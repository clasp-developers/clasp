(in-package :clasp-cleavir)

(defun global-ftype (name)
  (multiple-value-bind (value presentp) (gethash name *ftypes*)
    (if presentp
        value
        (load-time-value (cleavir-env:parse-type-specifier
                          '(function * *)
                          *clasp-env*
                          *clasp-system*)))))

(defun (setf global-ftype) (type name)
  (setf (gethash name *ftypes*)
        (cleavir-env:parse-type-specifier
         type
         clasp-cleavir::*clasp-env*
         clasp-cleavir::*clasp-system*)))
