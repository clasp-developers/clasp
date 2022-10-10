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
             (policy:normalize-optimize
              (append (rest decl) *global-optimize*) *clasp-env*)
             *global-policy*
             (policy:compute-policy *global-optimize* *clasp-env*)))
      ;; Add other clauses here
      (t #+(or)(warn "Add support for proclaim ~s~%" decl)))))

(defmethod cleavir-cst-to-ast:convert :before (cst environment (system clasp-64bit))
  (declare (ignore system))
  (when cmp:*code-walker*
    (let ((form (cst:raw cst)))
      (funcall cmp:*code-walker* form environment))))

(defun code-walk-using-cleavir (code-walker-function form env)
  (let* ((cleavir-cst-to-ast:*compiler* 'cl:compile)
         (core:*use-cleavir-compiler* t)
         (cmp:*code-walker* code-walker-function))
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
  (let ((orig (let ((orig (origin-source (cleavir-ast:origin ast))))
                (cond ((consp orig) (car orig))
                      ((null orig) core:*current-source-pos-info*)
                      (t orig)))))
    ;; See usage in cmp/debuginfo.lisp
    (list (cmp:jit-function-name (cleavir-ast:name ast))
          (core:source-pos-info-lineno orig)
          (core:source-pos-info-file-handle orig))))

;;; Stuff to put function scope infos into inline ast SPIs.
(defun insert-function-scope-info-into-spi (spi fsi)
  ;; If something already has an FSI, we're in a nested inline AST
  ;; and don't want to interfere with it.
  (unless (core:source-pos-info-function-scope spi)
    (core:setf-source-pos-info-function-scope spi fsi)))
(defun insert-function-scope-info-into-ast (ast fsi)
  (let ((orig (origin-source (cleavir-ast:origin ast))))
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
  ast)

;;; erase all source info.
(defun unhome-inline-ast (ast)
  (cleavir-ast:map-ast-depth-first-preorder
   (lambda (ast) (setf (cleavir-ast:origin ast) nil))
   ast)
  ast)

;;; Incorporated into DEFUN expansion (see lsp/evalmacros.lisp)
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

(defun copy-origin-fixing-sources (origin inlined-at
                                   &optional (table
                                              (make-hash-table :test #'eq)))
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

(defmethod cleavir-ast-to-bir:inline-origin (origin inlined-at (system clasp))
  (let ((inlined-at (origin-spi (origin-source inlined-at))))
    (if inlined-at
        (progn
          (check-type inlined-at core:source-pos-info)
          (copy-origin-fixing-sources origin inlined-at))
        origin)))
