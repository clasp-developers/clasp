(format t "Building cleavir clasp full version - loading compile-cclasp.lisp~%")
(defpackage #:cclasp-build
  (:use #:common-lisp #:core))
(in-package :cclasp-build)
(load "sys:kernel;cleavir;compile-cclasp.lisp")
(format t "Loading cleavir-system.lsp~%")
(load "sys:kernel;cleavir-system.lsp")
(let ((cclasp-system (setup-cclasp-system
                      core:*init-files*
                      *cleavir-partial-system*)))
  (core:load-system :bclasp :cclasp
                    :system cclasp-system)
  (cclasp-build:compile-full-cclasp cclasp-system))
(core:quit)
