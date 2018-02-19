(in-package #:compilation-environment)

;;;; Policy for the compilation of the target.
;;;; Could incorporate adjustable flags.

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'cleavir-kildall-type-inference:insert-type-checks))
     optimize
     (environment compilation-environment))
  (> (cleavir-policy:optimize-value optimize 'safety)
     (cleavir-policy:optimize-value optimize 'speed)))

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'cleavir-escape:trust-dynamic-extent))
     optimize
     (environment compilation-environment))
  (> (cleavir-policy:optimize-value optimize 'space)
     (cleavir-policy:optimize-value optimize 'safety)))
