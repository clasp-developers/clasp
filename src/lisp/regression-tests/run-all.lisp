(in-package :cl-user)

(declaim (optimize (safety 3)))

(let ((compiled-file
        (compile-file "sys:src;lisp;regression-tests;framework.lisp")))
  (if compiled-file
       (load compiled-file)
       (error "Could not compile ~s~%" "sys:src;lisp;regression-tests;framework.lisp")))

(load "sys:src;lisp;regression-tests;set-unexpected-failures.lisp")

(in-package #:clasp-tests)

;;; ------------------------------------------------------------
;;; Run tests
(reset-clasp-tests)
(load-if-compiled-correctly "sys:src;lisp;regression-tests;defcallback-native.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;lowlevel.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;fastgf.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;array0.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;tests01.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;finalizers.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;strings01.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;cons01.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;sequences01.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;clos.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;mop.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;update-instance-abort.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;numbers.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;ehkiller.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;package.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;structures.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;symbol0.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;string-comparison0.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;bit-array0.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;bit-array1.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;character0.lisp")
#+unicode
(load-if-compiled-correctly "sys:src;lisp;regression-tests;unicode.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;hash-tables0.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;misc.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;read01.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;printer01.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;streams01.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;environment01.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;types01.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;control01.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;iteration.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;loop.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;numbers-core.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;unwind.lisp")
#+unicode
(load-if-compiled-correctly "sys:src;lisp;regression-tests;encodings.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;environment.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;conditions.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;float-features.lisp")
#+(or)(progn
        (load-if-compiled-correctly "sys:src;lisp;regression-tests;system-construction.lisp")
        (no-handler-case-load-if-compiled-correctly "sys:src;lisp;regression-tests;debug.lisp")
        )
#+(and)(load-if-compiled-correctly "sys:src;lisp;regression-tests;debug.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;mp.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;posix.lisp")
;;; system-construction should be last for now.
;;; When we have it before debug.lisp, debug.lisp will fail
(load-if-compiled-correctly "sys:src;lisp;regression-tests;system-construction.lisp")
(load-if-compiled-correctly "sys:src;lisp;regression-tests;extensions.lisp")
(sys:quit (if (show-test-summary) 0 1))
