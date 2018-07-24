
(defun start-cleavir ()
  (load "sys:kernel;clasp-builder.lsp")
  (defparameter *system*
    (with-open-file (fin "source-dir:tools-for-build;cleavir-file-list.lisp" :direction :input)
      (read fin)))
  (core::load-system *system*))

(defun compile-stuff ()
  (dotimes (i 50)
    (format t "Compilation #~a~%" i)
    (compile-file "sys:kernel;lsp;setf.lsp" :output-file "/tmp/setf.fasl")))


