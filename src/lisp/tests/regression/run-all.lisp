(defpackage #:clasp-tests
    (:use :cl :core)
  (:export #:test))

(in-package :clasp-tests)

(defmacro test (foo &key description )
  `(if ,foo
       (format t "Passed ~s~%" (if ,description ,description ',foo))
       (progn
         (format t "The test ~s failed~%" ',foo)
         (when ,description (format t "~s~%" ,description))
         (error "Regression test ~s failed!" ,description))))

(defun expand-test-expect-error (fn)
  (handler-case
      (progn
        (funcall fn)
        nil)
    (error (err) t)))

(defmacro test-expect-error (foo &key description)
  `(test (handler-case
             (progn
               ,foo
               nil)
           (error (err) t)) :description ,description))
       
;;; ------------------------------------------------------------
;;; Run tests
(load (compile-file "sys:tests;regression;array0.lisp"))
(load (compile-file "sys:tests;regression;tests01.lisp"))
(load (compile-file "sys:tests;regression;finalizers.lisp"))
(load (compile-file "sys:tests;regression;strings01.lisp"))
(load (compile-file "sys:tests;regression;sequences01.lisp"))
