(format t "Building cleavir clasp full version - loading compile-cclasp.lisp~%")
(load "sys:kernel;cleavir;compile-cclasp.lisp")
(cclasp-build:compile-full-cclasp)
(core:quit)
