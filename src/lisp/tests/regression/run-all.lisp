(defpackage #:clasp-tests
    (:use :cl :core)
  (:export #:test))

(in-package :clasp-tests)

(defmacro test (foo &key description )
  `(if ,foo
       (format t "Passed ~a~%" ,description)
       (progn
         (format t "The test ~a failed~%" ,foo)
         (when ,description (format t "~a~%" ,description))
         (error "Regression test ~a failed!" ,description))))

;;; ------------------------------------------------------------
;;; Run tests
(load "sys:tests;regression;tests01.lisp")
(load "sys:tests;regression;finalizers.lisp")
