(in-package #:clasp-tests)

(test compile-file-pathname-1
      (pathname-type
       (compile-file-pathname "test.lisp" :output-file "test.newfasl"))
      ("newfasl"))

#+(or)
(test
 compile-file-pathname-2a
 (let ((cmp:*compile-file-parallel* t))
   (string-equal
    (cmp::cfp-output-extension :fasl)
    (pathname-type (compile-file-pathname "test.lisp")))))

#+(or)
(test
 compile-file-pathname-2b
 (let ((cmp:*compile-file-parallel* nil))
   (string-equal
    (cmp::cfp-output-extension :fasl)
    (pathname-type (compile-file-pathname "test.lisp")))))

(test-true compile-file-pathname-3a
 (let ((cmp:*use-human-readable-bitcode* t))
   (string-equal
    (cmp::cfp-output-extension :bitcode)
    (pathname-type (compile-file-pathname "test.lisp" :output-file "test.newfasl" :output-type :bitcode)))))

(test-true compile-file-pathname-3b
 (let ((cmp:*use-human-readable-bitcode* nil))
   (string-equal
    (cmp::cfp-output-extension :bitcode)
    (pathname-type (compile-file-pathname "test.lisp" :output-file "test.newfasl" :output-type :bitcode)))))

;;; if output-file is passed, repsect the extension and don't overwrite it 
(test-true compile-file-parallel
 (let ((cmp::*compile-file-parallel* t)
       (file "sys:regression-tests;framework.lisp"))
   (let ((fasl (compile-file file :output-file (make-pathname :type "newfasl" :defaults file) :verbose nil :print nil)))
     (and (probe-file fasl) (string-equal (pathname-type fasl) "newfasl")))))

(test-true compile-file-serial
 (let ((cmp::*compile-file-parallel* nil)
       (file "sys:regression-tests;framework.lisp"))
   (let ((fasl (compile-file file :output-file (make-pathname :type "newfasl" :defaults file) :verbose nil :print nil)))
     (and (probe-file fasl) (string-equal (pathname-type fasl) "newfasl")))))

;;; crosscompiling sbcl
(test-true compile-file-serial-no-faso
 (let ((cmp::*compile-file-parallel* nil)
       (core:*clasp-build-mode* :faso)
       (file "sys:regression-tests;framework.lisp"))
   (let ((fasl (compile-file file :output-file (make-pathname :type "newfasl" :defaults file) :verbose nil :print nil)))
     (and (probe-file fasl) (string-equal (pathname-type fasl) "newfasl")))))

;;; there shoudn't be any output if verbose and print are nil
(test COMPILE-FILE.1.simplified
      (with-output-to-string
          (*standard-output*)
        (compile-file "sys:regression-tests;framework.lisp" :verbose nil :print nil))
      (""))
