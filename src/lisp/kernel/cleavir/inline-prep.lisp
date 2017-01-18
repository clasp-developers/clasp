(in-package :clasp-cleavir)

(defun proclaim-hook (decl)
  (let ((head (car decl)))
    (cond
      ((eq head 'cl:ftype)
       #+verbose-compiler(warn "*** Do something with proclaim ftype ~s~%" decl))
      ((eq head 'cl:optimize)
       (setf *global-optimize*
             (cleavir-policy:normalize-optimize
              (append (rest decl) *global-optimize*)
              *clasp-env*)
             *global-policy*
             (cleavir-policy:compute-policy *global-optimize*
                                            *clasp-env*)))
      ;; Add other clauses here
      (t (warn "Add support for proclaim ~s~%" decl)))))

(defun mark-env-as-function ()
  (push 'si::function-boundary *simple-environment*))

(defun local-function-form-p (form)
  #+clc(warn "Convert this to use predicate ext:local_function_p")
  (and (listp form) (member (first form) '(flet labels))))

(defmethod cleavir-generate-ast:convert :around (form environment (system clasp-64bit))
  (declare (ignore system))
  (let ((*simple-environment* *simple-environment*))
    (when *code-walker*
      (when (local-function-form-p form)
        (mark-env-as-function))
      (funcall *code-walker* form *simple-environment*))
    (call-next-method)))

(defun code-walk-using-cleavir (form env &key code-walker-function)
  (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
         (core:*use-cleavir-compiler* t)
         (clasp-cleavir:*code-walker* code-walker-function))
    (handler-bind
        ((cleavir-env:no-variable-info
           (lambda (condition)
             (invoke-restart 'cleavir-generate-ast:consider-special)))
         (cleavir-env:no-function-info
           (lambda (condition)
             (invoke-restart 'cleavir-generate-ast:consider-global))))
      (cleavir-generate-ast:generate-ast form env *clasp-system*))))

(export 'code-walk-using-cleavir)


(defmethod make-load-form ((ast cleavir-ast:ast) &optional environment)
  (values `(allocate-instance ',(class-of ast))
          `(initialize-instance
            ,ast
            ,@(loop for (keyword reader)
                    in (cleavir-io:save-info ast)
                    for value = (funcall reader ast)
                    collect `(quote ,keyword)
                    collect `(quote ,value)))))



;; Generate an AST and save it for inlining if the
;; function is proclaimed as inline
(defun defun-inline-hook (name function-form)
  (when (core:declared-global-inline-p name)
    (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
           (ast (handler-bind
                    ((cleavir-env:no-variable-info
                       (lambda (condition)
                         (invoke-restart 'cleavir-generate-ast:consider-special)))
                     (cleavir-env:no-function-info
                       (lambda (condition)
                         (invoke-restart 'cleavir-generate-ast:consider-global))))
                  (cleavir-generate-ast:generate-ast function-form *clasp-env* *clasp-system*))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (when (core:declared-global-inline-p ',name)
           (when (fboundp ',name)
             (core:setf-cleavir-ast (fdefinition ',name) ,ast)))))))

  
(defparameter *simple-environment* nil)
(defvar *code-walker* nil)
(export '(*simple-environment* *code-walker*))
