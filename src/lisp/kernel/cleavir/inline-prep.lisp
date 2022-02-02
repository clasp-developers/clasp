(in-package :clasp-cleavir)

(defun proclaim-ftype (type fnames)
  (loop for fname in fnames
        do (setf (global-ftype fname) type)))

(defun proclaim-type (type vnames)
  (loop for vname in vnames
        do (setf (global-type vname) type)))

(defun proclaim-hook (decl)
  (let ((head (car decl)))
    (cond
      ((eq head 'cl:ftype)
       (destructuring-bind (type &rest fnames) (cdr decl)
         (proclaim-ftype type fnames)))
      ((eq head 'cl:type)
       (destructuring-bind (type &rest vnames) (cdr decl)
         (proclaim-type type vnames)))
      ((eq head 'cl:optimize)
       (setf *global-optimize*
             (cleavir-policy:normalize-optimize
              (append (rest decl) *global-optimize*)
              *clasp-env*)
             *global-policy*
             (cleavir-policy:compute-policy *global-optimize*
                                            *clasp-env*)))
      ;; Add other clauses here
      (t #+(or)(warn "Add support for proclaim ~s~%" decl)))))

(defvar *code-walker* nil)

(defmethod cleavir-cst-to-ast:convert :before (cst environment (system clasp-64bit))
  (declare (ignore system))
  (when *code-walker*
    (let ((form (cst:raw cst)))
      (funcall *code-walker* form environment))))

(defun code-walk-using-cleavir (code-walker-function form env)
  (let* ((cleavir-cst-to-ast:*compiler* 'cl:compile)
         (core:*use-cleavir-compiler* t)
         (*code-walker* code-walker-function))
    (handler-bind
        ((cleavir-cst-to-ast:no-variable-info
           (lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'cleavir-cst-to-ast:consider-special)))
         (cleavir-cst-to-ast:no-function-info
           (lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'cleavir-cst-to-ast:consider-global)))
         ;; No point printing warnings twice (now, and when the method body
         ;; is actually compiled)
         (warning #'muffle-warning)
         ;; Upon other error just give up.
         ;; (Returning NIL means code could not be walked.)
         (error (lambda (e)
                  (declare (ignore e))
                  (return-from code-walk-using-cleavir nil))))
      (cleavir-cst-to-ast:cst-to-ast (cst:cst-from-expression form) env *clasp-system*)))
  t)

(export 'code-walk-using-cleavir)

;;; Given a FUNCTION-AST, return the function-scope-info to insert into its body ASTs.
;;; or NIL if there's no source info.
(defun compute-fsi (ast)
  (let ((orig (let ((orig (cleavir-ast:origin ast)))
                (loop while (typep orig 'cst:cst)
                      do (setf orig (cst:source orig)))
                (cond ((consp orig) (car orig))
                      ((null orig)
                       ;; KLUDGE: If no source info, make one up
                       (core:make-source-pos-info))
                      (t orig)))))
    ;; See usage in cmp/debuginfo.lsp
    (list (cmp:jit-function-name (cleavir-ast:name ast))
          (core:source-pos-info-lineno orig)
          (core:source-pos-info-file-handle orig))))

;;; Stuff to put function scope infos into inline ast SPIs.
(defun insert-function-scope-info-into-spi (spi fsi)
  ;; If something already has an FSI, we're in a nested inline AST
  ;; and don't want to interfere with it, but need to hit the one that
  ;; doesn't deeper in.
  (if (core:source-pos-info-function-scope spi)
      (let ((next (core:source-pos-info-inlined-at spi)))
        (when next
          (insert-function-scope-info-into-spi next fsi)))
      (core:setf-source-pos-info-function-scope spi fsi)))
(defun insert-function-scope-info-into-ast (ast fsi)
  (let ((orig (cleavir-ast:origin ast)))
    (loop while (typep orig 'cst:cst)
          do (setf orig (cst:source orig)))
    (cond ((consp orig)
           (insert-function-scope-info-into-spi (car orig) fsi)
           (insert-function-scope-info-into-spi (cdr orig) fsi))
          ((null orig)
           (return-from insert-function-scope-info-into-ast ast))
          (t
           (insert-function-scope-info-into-spi orig fsi)))))
(defun fix-inline-ast (ast)
  (check-type ast cleavir-ast:function-ast)
  (let ((fsi (compute-fsi ast)))
    (unless (null fsi)
      (insert-function-scope-info-into-ast ast fsi)
      (labels ((aux (ast)
                 (typecase ast
                   (cleavir-ast:function-ast (fix-inline-ast ast))
                   (t (insert-function-scope-info-into-ast ast fsi)
                    (cleavir-ast:map-children #'aux ast)))))
        (cleavir-ast:map-children #'aux ast))))
  #+(or)
  (let ((fsi (compute-fsi ast)))
    (unless (null fsi)
      (cleavir-ast:map-ast-depth-first-preorder
       (lambda (ast)
         (insert-function-scope-info-into-ast ast fsi))
       ast)))
  #+(or)
  (let ((fsi (compute-fsi ast)))
    (unless (null fsi)
      (insert-function-scope-info-into-ast ast fsi)
      (dolist (child-ast (cleavir-ast:children ast))
        (cleavir-ast:map-ast-depth-first-preorder
         (lambda (ast)
           (if (typep ast 'cleavir-ast:function-ast)
               (fix-inline-ast ast)
               (insert-function-scope-info-into-ast ast fsi)))
         child-ast))))
  ast)

;;; Incorporated into DEFUN expansion (see lsp/evalmacros.lsp)
(defun defun-inline-hook (name function-form env)
  (declare (ignore env))
  (when (core:declared-global-inline-p name)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (when (core:declared-global-inline-p ',name)
         (setf (inline-ast ',name)
               (fix-inline-ast
                ;; Must use file compilation semantics here to compile
                ;; load-time-value correctly.
                (cleavir-primop:cst-to-ast ,function-form t)))))))

;; When we inline expand, the saved ast will be as if we had a
;; load-time-value ast. Fix those up if we are not file compiling.
(defun eval-load-time-value-asts (ast)
  (cleavir-ast:map-ast-depth-first-preorder
   (lambda (ast)
     (when (typep ast 'cleavir-ast:load-time-value-ast)
       ;; Fixup saved load-time-value asts by evaling them if need be.
       (unless (eq cleavir-cst-to-ast:*compiler* 'cl:compile-file)
         (change-class ast 'cleavir-ast:constant-ast
                       :value (eval (cleavir-ast:form ast))))))
   ast)
  ast)

(export '(*code-walker*))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq core:*proclaim-hook* 'proclaim-hook))

;;; The following code sets up the chain of inlined-at info in AST origins.

;;; Basically we want to recurse until we hit a SPI with no inlined-at,
;;; and set its inlined-at to the provided value. Also we clone everything,
;;; and memoize to avoid cloning too much.
(defun fix-inline-source-position (spi inlined-at table)
  (or (gethash spi table)
      (setf (gethash spi table)
            (let ((clone (core:source-pos-info-copy spi)))
              (core:setf-source-pos-info-inlined-at
               clone
               (let ((next (core:source-pos-info-inlined-at clone)))
                 (if next
                     (fix-inline-source-position next inlined-at table)
                     inlined-at)))
              clone))))

(defun %allocate-copy (origin inlined-at table)
  (etypecase origin
    (null nil)
    (cst:cons-cst (make-instance 'cst:cons-cst :raw (cst:raw origin)))
    (cst:atom-cst (make-instance 'cst:atom-cst :raw (cst:raw origin)))
    (cons
     (cons (copy-origin-fixing-sources (car origin) inlined-at table)
           (copy-origin-fixing-sources (cdr origin) inlined-at table)))
    (core:source-pos-info
     (let ((clone (core:source-pos-info-copy origin)))
       (core:setf-source-pos-info-inlined-at
        clone
        (let ((next (core:source-pos-info-inlined-at clone)))
          (if next
              (copy-origin-fixing-sources next inlined-at table)
              inlined-at)))
       clone))))

(defun %initialize-copy (origin copy inlined-at table)
  (typecase origin
    (cst:cons-cst
     (let ((car (copy-origin-fixing-sources (cst:first origin)
                                            inlined-at table))
           (cdr (copy-origin-fixing-sources (cst:rest origin)
                                            inlined-at table))
           (source (copy-origin-fixing-sources (cst:source origin)
                                               inlined-at table)))
       (reinitialize-instance copy :first car :rest cdr :source source)))
    (cst:atom-cst
     (let ((source (copy-origin-fixing-sources (cst:source origin)
                                               inlined-at table)))
       (reinitialize-instance copy :source source)))))

(defun copy-origin-fixing-sources (origin inlined-at table)
  (multiple-value-bind (copy presentp)
      (gethash origin table)
    (if presentp
        copy
        (let ((copy (%allocate-copy origin inlined-at table)))
          (setf (gethash origin table) copy)
          ;; For CSTs, initialize the copy now that subforms can refer to the
          ;; existing entry (done this way in case of cycles)
          (%initialize-copy origin copy inlined-at table)
          copy))))

(defun fix-inline-source-positions (ast inlined-at)
  (let ((new-origins (make-hash-table :test #'eq)))
    ;; NEW-ORIGINS is a memoization table.
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (ast)
       (let ((orig (cleavir-ast:origin ast)))
         (setf (cleavir-ast:origin ast)
               (copy-origin-fixing-sources orig inlined-at new-origins))))
     ast))
  ast)

(defun track-inline-counts (inlinee-names inlined-name)
  (let (inlinee-name)
    (loop for iname in inlinee-names
          when iname
            do (setf inlinee-name iname))
    (let ((inlinee-ht (gethash inlinee-name cmp:*track-inlined-functions*)))
      (unless inlinee-ht
        (setf inlinee-ht (make-hash-table :test #'equal))
        (setf (gethash inlinee-name cmp:*track-inlined-functions*) inlinee-ht))
      (incf (gethash inlined-name inlinee-ht 0))
      (when (core:global-inline-status inlinee-name)
        (setf (gethash :inline inlinee-ht) t)))))

(defmethod cleavir-cst-to-ast:convert-called-function-reference
    (cst info env (system clasp-64bit))
  (declare (ignore env))
  ;; FIXME: Duplicates cleavir.
  (when (not (eq (cleavir-env:inline info) 'cl:notinline))
    (let ((ast (cleavir-env:ast info)))
      (when ast
        (when (hash-table-p cmp:*track-inlined-functions*)
          (track-inline-counts cmp:*track-inlinee-name* (cleavir-environment:name info)))
        (return-from cleavir-cst-to-ast:convert-called-function-reference
          (eval-load-time-value-asts
           (fix-inline-source-positions
            (cleavir-ast-transformations:clone-ast ast)
            (let ((source (cst:source cst)))
              (loop while (typep source 'cst:cst)
                    do (setf source (cst:source source)))
              (cond ((consp source) (car source))
                    ((null source) core:*current-source-pos-info*)
                    (t source)))))))))
  (call-next-method))
