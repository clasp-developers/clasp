(defpackage #:clasp-tests
    (:use :cl :core)
  (:export #:test))

(in-package :clasp-tests)

(defparameter *passes* 0)
(defparameter *fails* 0)

(defmacro test (foo &key description )
  `(if ,foo
       (progn
         (format t "Passed ~s~%" (if ,description ,description ',foo))
         (incf *passes*))
       (progn
         (incf *fails*)
         (format t "The test ~s failed~%" ',foo)
         (when ,description (format t "~s~%" ,description))
         (format t "FAILED: test ~s!~%" ,description))))

(defmacro test-type (t1 t2)
  `(test (and (subtypep ,t1 ,t2) (subtypep ,t2 ,t1))))

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
(load (compile-file "sys:regression-tests;fastgf.lisp"))
(load (compile-file "sys:regression-tests;stamps.lisp"))
(load (compile-file "sys:regression-tests;array0.lisp"))
(load (compile-file "sys:regression-tests;tests01.lisp"))
(load (compile-file "sys:regression-tests;finalizers.lisp"))
(load (compile-file "sys:regression-tests;strings01.lisp"))
(load (compile-file "sys:regression-tests;sequences01.lisp"))
(load (compile-file "sys:regression-tests;clos.lisp"))
(format t "Passes: ~a~%" *passes*)
(format t "Fails:  ~a~%" *fails*)
