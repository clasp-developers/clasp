(in-package :clasp-cleavir)

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'cleavir-typed-transforms:insert-type-checks))
     optimize
     (environment clasp-cleavir::clasp-global-environment))
  (> (cleavir-policy:optimize-value optimize 'safety)
     (cleavir-policy:optimize-value optimize 'speed)))

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'cleavir-escape:trust-dynamic-extent))
     optimize
     (environment clasp-cleavir:clasp-global-environment))
  (> (cleavir-policy:optimize-value optimize 'space)
     (cleavir-policy:optimize-value optimize 'safety)))

(defmethod cleavir-policy:compute-policy-quality
    (quality optimize (environment null))
    (cleavir-policy:compute-policy-quality quality optimize *clasp-env*))
