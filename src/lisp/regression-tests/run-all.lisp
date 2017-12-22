(load (compile-file "sys:regression-tests;framework.lisp"))

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
(load (compile-file "sys:regression-tests;sequences01.lisp"))
(load (compile-file "sys:regression-tests;clos.lisp"))
(load (compile-file "sys:regression-tests;numbers.lisp"))
(format t "Passes: ~a~%" *passes*)
(format t "Fails:  ~a~%" *fails*)
