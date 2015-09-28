(in-package :clasp-cleavir)

(defun proclaim-hook (decl)
  (let ((head (car decl)))
    (cond
      ((eq head 'cl:ftype)
       (format t "*** Do something with proclaim ftype ~s~%" decl))
      ;; Add other clauses here
      (t (warn "Add support for proclaim ~s~%" decl)))))

(defun global-function-inline-ast (name)
  (gethash name *function-inline-asts*))

(defun do-inline-hook (name ast)
  (format t "Attaching to name: ~a ast: ~a~%" name ast)
  (when (core:declared-global-inline-p name)
    (core:setf-cleavir-ast (fdefinition name) ast)))
  
;; Generate an AST and save it for inlining if the
;; function is proclaimed as inline
(defun defun-inline-hook (name function-form)
  (let ((cleavir-generate-ast:*compiler* 'cl:compile))
    (let* ((ast (cleavir-generate-ast:generate-ast function-form *clasp-env* *clasp-system*))
           (ast-form (cleavir-ast-transformations:codegen-clone-ast ast))
           (astfn-gs (gensym "ASTFN")))
      (when (core:declared-global-inline-p name)
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (let ((,astfn-gs (lambda () ,ast-form)))
             (when core:*do-inline-hook*
               (funcall core:*do-inline-hook* (QUOTE ,name) (funcall ,astfn-gs)))))))))
    
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq core:*defun-inline-hook* 'defun-inline-hook)
  (setq core:*do-inline-hook* 'do-inline-hook)
  (setq core:*proclaim-hook* 'proclaim-hook))
