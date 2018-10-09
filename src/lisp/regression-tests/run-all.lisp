(load (compile-file "sys:regression-tests;framework.lisp"))
(load "sys:regression-tests;set-unexpected-failures.lisp")

(in-package #:clasp-tests)
       
;;; ------------------------------------------------------------
;;; Run tests
(load (compile-file "sys:regression-tests;fastgf.lisp"))
(load (compile-file "sys:regression-tests;stamps.lisp"))
(load (compile-file "sys:regression-tests;array0.lisp"))
(load (compile-file "sys:regression-tests;tests01.lisp"))
#-use-mps
(load (compile-file "sys:regression-tests;finalizers.lisp"))
#+use-mps
(progn (princ "Skipping finalizer tests on MPS - fix!") (terpri))
(load (compile-file "sys:regression-tests;strings01.lisp"))
(load (compile-file "sys:regression-tests;cons01.lisp"))
(load (compile-file "sys:regression-tests;sequences01.lisp"))
(load (compile-file "sys:regression-tests;clos.lisp"))
(load (compile-file "sys:regression-tests;numbers.lisp"))
(load (compile-file "sys:regression-tests;ehkiller.lisp"))
(load (compile-file "sys:regression-tests;package.lisp"))
(load (compile-file "sys:regression-tests;structures.lisp"))
(load (compile-file "sys:regression-tests;symbol0.lisp"))
(load (compile-file "sys:regression-tests;string-comparison0.lisp"))
(load (compile-file "sys:regression-tests;bit-array0.lisp"))
(load (compile-file "sys:regression-tests;bit-array1.lisp"))
(load (compile-file "sys:regression-tests;character0.lisp"))
(load (compile-file "sys:regression-tests;hash-tables0.lisp"))
(load (compile-file "sys:regression-tests;misc.lisp"))
(load (compile-file "sys:regression-tests;read01.lisp"))
(load (compile-file "sys:regression-tests;printer01.lisp"))
(load (compile-file "sys:regression-tests;streams01.lisp"))

(progn
  (note-test-finished)
  (format t "Passes: ~a~%" *passes*)
  (format t "Fails:  ~a~%" *fails*)
  (show-failed-tests))
