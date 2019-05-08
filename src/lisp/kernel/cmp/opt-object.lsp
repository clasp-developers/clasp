(in-package #:cmp)

;;; Optimize FIND-CLASS with constant symbol argument.
;;; Every symbol used as a class-name gets a CLASS-HOLDER, which is basically
;;; just a cell. This class-holder can be looked up at load time, so all that's
;;; done at runtime is a boundedness check and a read from a known location.

(define-compiler-macro find-class
    (&whole form classf &optional (errorpf 't) env &environment cenv)
  (if (and (constantp classf cenv) (null env))
      (let ((class (ext:constant-form-value classf cenv))
            (class-holder-gs (gensym "CLASS-HOLDER"))
            (errorp-gs (gensym "ERRORP")))
        (if (symbolp class)
            ;; Okay, we can actually optimize.
            `(let ((,class-holder-gs (load-time-value (core:find-class-holder ',class)))
                   ;; note we have to evaluate errorpf if it was provided,
                   ;; in case of side effects. If we were smarter about
                   ;; other environments, we'd have to eval env too.
                   (,errorp-gs ,errorpf))
               (if (ext:class-unboundp ,class-holder-gs)
                   (if ,errorp-gs
                       (error 'ext:undefined-class :name ',class)
                       nil)
                   ;; Class is bound, return it.
                   (ext:class-get ,class-holder-gs)))
            ;; Class is constant but not a symbol - type error. just let the call do it.
            form))
      ;; Class is not constant or there's an environment argument - punt
      form))

;;; generic functions are usually hard to work with at compile time,
;;; but we can at least take care of the simple methods on SYMBOL,
;;; which cannot be customized. These methods just allow a class name to be used
;;; where a class would be.
;;; These macros allow the above FIND-CLASS optimization to be used, avoiding a runtime lookup.
;;; They're ordered from most to least important.

#+(or) ; see clos/static-gfs/compiler-macros.lisp
(define-compiler-macro make-instance (&whole form class &rest initargs &environment env)
  (if (constantp class env)
    (let ((class (ext:constant-form-value class env)))
      (if (symbolp class)
          `(make-instance (find-class ',class) ,@initargs)
          form))
    form))

(define-compiler-macro change-class
    (&whole form instance classf &rest initargs &environment env)
  (if (constantp classf env)
      (let ((class (ext:constant-form-value classf env)))
        (if (symbolp class)
            `(change-class ,instance (find-class ',class) ,@initargs)
            form))
      form))

(define-compiler-macro make-instances-obsolete (&whole form classf &environment env)
  (if (constantp classf env)
      (let ((class (ext:constant-form-value classf env)))
        (if (symbolp class)
            `(make-instances-obsolete (find-class ',class))
            form))
      form))
