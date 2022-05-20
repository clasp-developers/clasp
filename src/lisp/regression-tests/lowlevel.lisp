
(in-package #:clasp-tests)

(test compile-2-low-level
      (with-output-to-string (*standard-output*)
        (compile-file "sys:src;lisp;regression-tests;lowlevel-source.lisp" :verbose nil :print nil))
      (""))
