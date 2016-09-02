(in-package :clasp-cleavir)

(defun proclaim-hook (decl)
  (let ((head (car decl)))
    (cond
      ((eq head 'cl:ftype)
       #+verbose-compiler(warn "*** Do something with proclaim ftype ~s~%" decl))
      ;; Add other clauses here
      (t (warn "Add support for proclaim ~s~%" decl)))))

#+(or)
(defun global-function-inline-ast (name)
  (gethash name *function-inline-asts*))

(defun do-inline-hook (name ast)
  (when (core:declared-global-inline-p name)
    (if (fboundp name)
        (core:setf-cleavir-ast (fdefinition name) ast))))
  
;; Generate an AST and save it for inlining if the
;; function is proclaimed as inline
(defun defun-inline-hook (name function-form)
  (when (core:declared-global-inline-p name)
    (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
           (ast (cleavir-generate-ast:generate-ast function-form *clasp-env* *clasp-system*))
           (ast-form (cleavir-ast-transformations:codegen-clone-ast ast))
           (astfn-gs (gensym "ASTFN")))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (let ((,astfn-gs (lambda () ,ast-form)))
           (when core:*do-inline-hook*
             (funcall core:*do-inline-hook* (QUOTE ,name) (funcall ,astfn-gs))))))))
    
(setq core:*defun-inline-hook* 'defun-inline-hook)
(setq core:*do-inline-hook* 'do-inline-hook)
(setq core:*proclaim-hook* 'proclaim-hook)
