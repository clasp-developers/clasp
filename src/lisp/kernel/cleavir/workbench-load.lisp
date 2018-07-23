
(progn
  (load "sys:kernel;clasp-builder.lsp")
  (defparameter *system*
    (with-open-file (fin "source-dir:tools-for-build;cleavir-file-list.lisp" :direction :input)
      (read fin)))
  (core::load-system *system*))
