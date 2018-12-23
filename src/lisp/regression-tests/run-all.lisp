(load (compile-file "sys:regression-tests;framework.lisp"))
(load "sys:regression-tests;set-unexpected-failures.lisp")

(in-package #:clasp-tests)

;;; ------------------------------------------------------------
;;; Run tests
(reset-clasp-tests)
(load-if-compiled-correctly "sys:regression-tests;fastgf.lisp")
(load-if-compiled-correctly "sys:regression-tests;stamps.lisp")
(load-if-compiled-correctly "sys:regression-tests;array0.lisp")
(load-if-compiled-correctly "sys:regression-tests;tests01.lisp")
#-use-mps
(load-if-compiled-correctly "sys:regression-tests;finalizers.lisp")
#+use-mps
(progn (princ "Skipping finalizer tests on MPS - fix!") (terpri))
(load-if-compiled-correctly "sys:regression-tests;strings01.lisp")
(load-if-compiled-correctly "sys:regression-tests;cons01.lisp")
(load-if-compiled-correctly "sys:regression-tests;sequences01.lisp")
(load-if-compiled-correctly "sys:regression-tests;clos.lisp")
(load-if-compiled-correctly "sys:regression-tests;numbers.lisp")
(load-if-compiled-correctly "sys:regression-tests;ehkiller.lisp")
(load-if-compiled-correctly "sys:regression-tests;package.lisp")
(load-if-compiled-correctly "sys:regression-tests;structures.lisp")
(load-if-compiled-correctly "sys:regression-tests;symbol0.lisp")
(load-if-compiled-correctly "sys:regression-tests;string-comparison0.lisp")
(load-if-compiled-correctly "sys:regression-tests;bit-array0.lisp")
(load-if-compiled-correctly "sys:regression-tests;bit-array1.lisp")
(load-if-compiled-correctly "sys:regression-tests;character0.lisp")
(load-if-compiled-correctly "sys:regression-tests;hash-tables0.lisp")
(load-if-compiled-correctly "sys:regression-tests;misc.lisp")
(load-if-compiled-correctly "sys:regression-tests;read01.lisp")
(load-if-compiled-correctly "sys:regression-tests;printer01.lisp")
(load-if-compiled-correctly "sys:regression-tests;streams01.lisp")
(load-if-compiled-correctly "sys:regression-tests;loop.lisp")

(progn
  (note-test-finished)
  (format t "Passes: ~a~%" *passes*)
  (format t "Fails:  ~a~%" *fails*)
  (show-failed-tests))
