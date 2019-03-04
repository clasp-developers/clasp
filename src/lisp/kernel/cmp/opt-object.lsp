(in-package #:cmp)

;;; Optimize FIND-CLASS with constant symbol argument.
;;; Every symbol used as a class-name gets a CLASS-HOLDER, which is basically
;;; just a cell. This class-holder can be looked up at load time.

(core:bclasp-define-compiler-macro find-class
    (&whole form classf &optional (errorpf 't) env &environment cenv)
  (if (and (constantp classf env) (null env))
    (let ((class (ext:constant-form-value classf cenv))
          (class-holder-gs (gensym "CLASS-HOLDER")))
      (if (symbolp class)
          ;; Okay, we can actually optimize.
          `(let ((,class-holder-gs (load-time-value (core:find-class-holder ',class))))
             (if (ext:class-unboundp ,class-holder-gs)
                 ;; Check what we do with an error condition
                 ,(if (constantp errorpf env)
                      (if (ext:constant-form-value errorpf cenv)
                          ;; errorp is constant true: signal an error.
                          `(error 'ext:undefined-class :name ',class)
                          ;; errorp is constant false: return NIL.
                          'nil)
                      ;; errorp is variable: check at runtime.
                      `(if ,errorpf
                           (error 'ext:undefined-class :name ',class)
                           nil))
                 ;; Class is bound, return it.
                 (ext:class-get ,class-holder-gs)))
          ;; Class is constant but not a symbol - type error. just let the call do it.
          form))
    ;; Class is not constant or we're in some weird environment - punt
    form))

;;; generic functions are usually hard to work with at compile time,
;;; but we can at least take care of the simple method on SYMBOL,
;;; which cannot be customized.
(core:bclasp-define-compiler-macro make-instance (&whole form class &rest initargs &environment env)
  (when (constantp class env)
    (let ((class (ext:constant-form-value class env)))
      (when (symbolp class)
        (return-from make-instance
          `(make-instance (find-class ',class) ,@initargs)))))
  form)

