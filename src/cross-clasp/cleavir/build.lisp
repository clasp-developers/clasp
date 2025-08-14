(in-package #:cross-clasp.cleavir)

(defun build (input-files output-files source-pathnames)
  (let ((*compile-verbose* t) (*compile-print* t))
    (with-compilation-unit ()
      (loop for input in input-files
            for output in output-files
            for source in source-pathnames
            do (compile-file input :output-file output
                                   :output-type :faso
                                   :environment cross::*build-rte*
                                   :reader-client *reader-client*
                                   :source-debug-pathname source)))))
