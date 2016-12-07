(in-package :clasp-cleavir)

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'cleavir-typed-transforms:insert-type-checks))
     optimize
     (environment clasp-global-environment))
  (> (cleavir-policy:optimize-value optimize 'safety)
     (cleavir-policy:optimize-value optimize 'speed)))
