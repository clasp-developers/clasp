
(in-package #:clasp-tests)

(test compile-2
      (with-output-to-string (*standard-output*)
        (compile-file "sys:regression-tests;lowlevel-source.lisp" :verbose nil :print nil))
      (""))
