(in-package #:cmp)

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
