(in-package :clasp-cleavir)

(defun proclaim-hook (decl)
  (let ((head (car decl)))
    (cond
      ((eq head 'cl:ftype)
       #+verbose-compiler(warn "*** Do something with proclaim ftype ~s~%" decl))
      ;; Add other clauses here
      (t (warn "Add support for proclaim ~s~%" decl)))))

(defun do-inline-hook (name ast)
  (when (core:declared-global-inline-p name)
    (if (fboundp name)
        (core:setf-cleavir-ast (fdefinition name) ast))))
  
;; Generate an AST and save it for inlining if the
;; function is proclaimed as inline
(defun defun-inline-hook (name function-form)
  (when (core:declared-global-inline-p name)
    (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
           (ast (cleavir-generate-ast:generate-ast function-form *clasp-env* *clasp-system*)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
           ;; I'm featuring out the *do-inline-hook* test
           ;; I don't think it's needed in addition to
           ;; *defun-inline-hook*
         (funcall do-inline-hook (QUOTE ,name) ,ast)
         #+(or)(when core:*do-inline-hook*
                 (funcall core:*do-inline-hook* (QUOTE ,name) ,ast))))))

;;; original
#+(or)
(defun defun-inline-hook (name function-form)
  (when (core:declared-global-inline-p name)
    (let* ((cleavir-generate-ast:*compiler* 'cl:compile)
           (ast (cleavir-generate-ast:generate-ast function-form *clasp-env* *clasp-system*))
           (ast-form (cleavir-ast-transformations:codegen-clone-ast ast))
           (astfn-gs (gensym "ASTFN")))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (let ((,astfn-gs (lambda () ,ast-form)))
           ;; I'm featuring out the *do-inline-hook* test
           ;; I don't think it's needed in addition to
           ;; *defun-inline-hook*
           (funcall do-inline-hook (QUOTE ,name) (funcall ,astfn-gs))
           #+(or)(when core:*do-inline-hook*
             (funcall core:*do-inline-hook* (QUOTE ,name) (funcall ,astfn-gs))))))))

(defun inline-on! ()
  (setq core:*inline-on* t))

(defun inline-off! ()
  (setq core:*inline-on* nil))

(eval-when (:compile-toplevel :execute :load-toplevel)
  #+(or)(setq core::*inline-on* t)
  (setq core:*defun-inline-hook* 'defun-inline-hook)
  (setq core:*proclaim-hook* 'proclaim-hook)
  ;; The following may not be needed in addition to *defun-inline-hook*
  ;; remove it from init.lsp if not
  #+(or)(setq core:*do-inline-hook* 'do-inline-hook) ; maybe depreciated
  )
  
(defparameter *simple-environment* nil)
(defvar *code-walker* nil)
(export '(*simple-environment* *code-walker*))

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
    (cleavir-generate-ast:generate-ast form env *clasp-system*)))

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
