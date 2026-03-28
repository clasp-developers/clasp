(in-package :clasp-cleavir)

(defun origin-source (origin)
  (loop while (typep origin 'cst:cst)
        do (setf origin (cst:source origin)))
  origin)
