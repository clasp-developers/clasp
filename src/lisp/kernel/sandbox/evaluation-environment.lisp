(defpackage #:evaluation-environment
  (:use #:cl)
  (:export #:evaluation-environment #:compilation-environment
           #:transferringp))

(in-package #:evaluation-environment)

(defclass evaluation-environment (sandbox-environment)
  ((%compilation-environment :initarg :compilation-environment :accessor compilation-environment)
   ;; This flag indicates whether the environment has been initialized.
   ;; If it has, than further definitions are during a compile, so we want to transfer them to
   ;; the compilation environment.
   (%transferringp :initform nil :accessor transferringp)))

;;; Some definitions being evaluated transfer to the compilation environment.
;;; E.g., (eval-when (:compile-toplevel) (setf (macro-function ...) ...))
;;; The compiler should thereafter be able to expand relevant macro forms.
(macrolet ((transfer-setf (accessor (&rest more-parameters))
             `(defmethod (setf ,accessor) :after (new ,@more-parameters (env evaluation-environment))
                (when (transferringp env)
                  (setf (,accessor ,@more-parameters (compilation-environment env)) new)))))
  (transfer-setf sicl-genv:macro-function (symbol))
  (transfer-setf sicl-genv:compiler-macro-function (function-name))
  (transfer-setf sicl-genv:find-class (symbol))
  (transfer-setf sicl-genv:function-type (function-name))
  (transfer-setf sicl-genv:function-inline (function-name))
  (transfer-setf sicl-genv:function-ast (function-name))
  (transfer-setf sicl-genv:constant-variable (symbol))
  (transfer-setf sicl-genv:symbol-macro (symbol))
  (transfer-setf sicl-genv:variable-type (symbol))
  (transfer-setf sicl-genv:setf-expander (symbol))
  (transfer-setf sicl-genv:default-setf-expander ())
  (transfer-setf sicl-genv:type-expander (symbol))
  (transfer-setf sicl-genv:declaration (name))
  (transfer-setf sicl-genv:optimize-quality-values ()))
;;; for special variables, ignore the value, we don't need it.
(defmethod (setf sicl-genv:special-variable) :after
    (value symbol (environment evaluation-environment) initialize-p)
  (declare (ignore initialize-p value))
  (when (transferringp environment)
    (setf (sicl-genv:special-variable symbol (compilation-environment environment) nil) nil)))

;;; since i'm doing this for building, inevitably a lot of CL macros and stuff will be defined.
;;; in this case we want to not replace the perfectly good definitions in the compiler environment.
;;; This is a different matter from actual redefinitions in the code, which are handled by the
;;; sicl-genv: definitions in compilation-environment.lisp.

;;; This might not be required since there are rejections of everything in compiler.lisp.
;;; not required for the particular project of building itself, anyway.
(macrolet ((reject-setf (accessor (&rest more-parameters))
             `(defmethod (setf ,accessor) (new ,@more-parameters (env evaluation-environment))
                (declare (ignore new))
                (if (and (transferringp env) (,accessor ,@more-parameters env))
                    (progn
                      #+(or)
                      (warn "Ignoring compiler redefinition: ~a" (list ',accessor ,@more-parameters env)))
                    (call-next-method)))))
  (reject-setf sicl-genv:macro-function (symbol))
  (reject-setf sicl-genv:compiler-macro-function (function-name))
  (reject-setf sicl-genv:find-class (symbol))
  (reject-setf sicl-genv:function-type (function-name))
  (reject-setf sicl-genv:function-inline (function-name))
  (reject-setf sicl-genv:function-ast (function-name))
  (reject-setf sicl-genv:constant-variable (symbol))
  (reject-setf sicl-genv:symbol-macro (symbol))
  (reject-setf sicl-genv:variable-type (symbol))
  (reject-setf sicl-genv:setf-expander (symbol))
  (reject-setf sicl-genv:type-expander (symbol))
  (reject-setf sicl-genv:declaration (name))
  (reject-setf sicl-genv:optimize-quality-values ()))
(defmethod (setf sicl-genv:special-variable)
    (value symbol (environment evaluation-environment) initialize-p)
  (declare (ignore value))
  (if (sicl-genv:special-variable symbol environment)
      (progn
        #+(or)
        (warn "Ignoring compiler redefinition: ~a"
              `(sicl-genv:special-variable ,symbol ,environment ,initialize-p)))
      (call-next-method)))
