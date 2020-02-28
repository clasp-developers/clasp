(in-package :clasp-cleavir)

(defun proclaim-ftype (type fnames)
  (loop for fname in fnames
        do (setf (global-ftype fname) type)))

(defun proclaim-hook (decl)
  (let ((head (car decl)))
    (cond
      ((eq head 'cl:ftype)
       (destructuring-bind (type &rest fnames) (cdr decl)
         (proclaim-ftype type fnames)))
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

#+cst
(defmethod cleavir-cst-to-ast:convert :before (cst environment (system clasp-64bit))
  (declare (ignore system))
  (when *code-walker*
    (let ((form (cst:raw cst)))
      (funcall *code-walker* form environment))))

#-cst
(defmethod cleavir-generate-ast:convert :before (form environment (system clasp-64bit))
  (declare (ignore system))
  (when *code-walker*
    (funcall *code-walker* form environment)))

(defun code-walk-using-cleavir (code-walker-function form env)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
         (core:*use-cleavir-compiler* t)
         (*code-walker* code-walker-function))
    (handler-bind
        ((cleavir-env:no-variable-info
           (lambda (condition)
             (invoke-restart #+cst 'cleavir-cst-to-ast:consider-special
                             #-cst 'cleavir-generate-ast:consider-special)))
         (cleavir-env:no-function-info
           (lambda (condition)
             (invoke-restart #+cst 'cleavir-cst-to-ast:consider-global
                             #-cst 'cleavir-generate-ast:consider-global)))
         ;; No point printing warnings twice (now, and when the method body
         ;; is actually compiled)
         (warning #'muffle-warning)
         ;; Upon other error just give up.
         ;; (Returning NIL means code could not be walked.)
         (error (lambda (e)
                  (declare (ignore e))
                  (return-from code-walk-using-cleavir nil))))
      #+cst
      (cleavir-cst-to-ast:cst-to-ast (cst:cst-from-expression form) env *clasp-system*)
      #-cst
      (cleavir-generate-ast:generate-ast form env *clasp-system*)))
  t)

(export 'code-walk-using-cleavir)

;;; Given a FUNCTION-AST, return the function-scope-info to insert into its body ASTs.
;;; or NIL if there's no source info.
(defun compute-fsi (ast)
  (let ((orig (let ((orig (cleavir-ast:origin ast)))
                (cond ((consp orig) (car orig))
                      ((null orig)
                       ;; KLUDGE: If no source info, make one up
                       (core:make-source-pos-info "no-source-info-available" 0 0 0))
                      (t orig)))))
    ;; See usage in cmp/debuginfo.lsp
    (list (cmp:jit-function-name (clasp-cleavir-ast:lambda-name ast))
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
      (cleavir-ast:map-ast-depth-first-preorder
       (lambda (ast)
         (insert-function-scope-info-into-ast ast fsi))
       ast)))
  ast)

;;; Incorporated into DEFUN expansion (see lsp/evalmacros.lsp)
(defun defun-inline-hook (name function-form env)
  (when (core:declared-global-inline-p name)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (when (core:declared-global-inline-p ',name)
         (setf (inline-ast ',name)
               (fix-inline-ast
                (cleavir-primop:cst-to-ast ,function-form)))))))

(export '(*code-walker*))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq core:*proclaim-hook* 'proclaim-hook))

;;; The following code sets up the chain of inlined-at info in AST origins.
#+cst
(progn

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

(defun fix-inline-source-positions (ast inlined-at)
  (let ((new-origins (make-hash-table :test #'eq)))
    ;; NEW-ORIGINS is a memoization table.
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (ast)
       (let ((orig (cleavir-ast:origin ast)))
         (setf (cleavir-ast:origin ast)
               (cond ((consp orig)
                      (cons (fix-inline-source-position
                             (car orig) inlined-at new-origins)
                            (fix-inline-source-position
                             (cdr orig) inlined-at new-origins)))
                     ((null orig) nil)
                     (t (fix-inline-source-position
                         orig inlined-at new-origins))))))
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

(defmethod cleavir-cst-to-ast:convert-called-function-reference (cst info env (system clasp-64bit))
  (declare (ignore env))
  ;; FIXME: Duplicates cleavir.
  (when (not (eq (cleavir-env:inline info) 'cl:notinline))
    (let ((ast (cleavir-env:ast info)))
      (when ast
        (when (hash-table-p cmp:*track-inlined-functions*)
          (track-inline-counts cmp:*track-inlinee-name* (cleavir-environment:name info)))
        (return-from cleavir-cst-to-ast:convert-called-function-reference
          (fix-inline-source-positions
           (cleavir-ast-transformations:clone-ast ast)
           (let ((source (cst:source cst)))
             (cond ((consp source) (car source))
                   ((null source) core:*current-source-pos-info*)
                   (t source))))))))
  (call-next-method))

) ; #+cst (progn...)
