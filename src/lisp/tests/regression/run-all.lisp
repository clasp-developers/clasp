(defpackage #:clasp-tests
    (:use :cl :core)
  (:export #:test))

(in-package :clasp-tests)

(defmacro test (foo &key description )
  `(if ,foo
       (format t "Passed ~a~%" (if ,description ,description "test"))
       (progn
         (format t "The test ~a failed~%" ',foo)
         (when ,description (format t "~a~%" ,description))
         (error "Regression test ~a failed!" ,description))))

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
(load "sys:tests;regression;tests01.lisp")
(load "sys:tests;regression;finalizers.lisp")
(load "sys:tests;regression;strings01.lisp")
(load "sys:tests;regression;sequences01.lisp")
