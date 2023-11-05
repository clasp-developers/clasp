(in-package #:clasp-tests)

(test compile-file-pathname-1
      (pathname-type
       (compile-file-pathname "test.lisp" :output-file "test.newfasl"))
      ("newfasl"))

;;; if output-file is passed, repsect the extension and don't overwrite it
(test-true compile-file-parallel
 (let ((cmp::*compile-file-parallel* t)
       (file "sys:src;lisp;regression-tests;framework.lisp"))
   (let ((fasl (compile-file file :output-file (make-pathname :type "newfasl" :defaults file) :verbose nil :print nil)))
     (and (probe-file fasl) (string-equal (pathname-type fasl) "newfasl")))))

(test-true compile-file-serial
 (let ((cmp::*compile-file-parallel* nil)
       (file "sys:src;lisp;regression-tests;framework.lisp"))
   (let ((fasl (compile-file file :output-file (make-pathname :type "newfasl" :defaults file) :verbose nil :print nil)))
     (and (probe-file fasl) (string-equal (pathname-type fasl) "newfasl")))))

;;; crosscompiling sbcl
(test-true compile-file-serial-no-faso
 (let ((cmp::*compile-file-parallel* nil)
       (cmp:*default-output-type* :faso)
       (file "sys:src;lisp;regression-tests;framework.lisp"))
   (let ((fasl (compile-file file :output-file (make-pathname :type "newfasl" :defaults file) :verbose nil :print nil)))
     (and (probe-file fasl) (string-equal (pathname-type fasl) "newfasl")))))

;;; there shoudn't be any output if verbose and print are nil
(test COMPILE-FILE.1.simplified
      (with-output-to-string
          (*standard-output*)
        (compile-file "sys:src;lisp;regression-tests;framework.lisp" :verbose nil :print nil))
      (""))
