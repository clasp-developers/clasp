(in-package :clasp-cleavir)

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'cleavir-kildall-type-inference:insert-type-checks))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy ANALYZE-FLOW.
;;;
;;; Since Kildall analyses are tragically slow at the moment,
;;; they need to be suppessed for reasonable compile times.
;;; If ANALYZE-FLOW is false, no Kildall is done.
;;; See MY-HIR-TRANSFORMATIONS for use.

(defmethod cleavir-policy:policy-qualities append ((env clasp-global-environment))
  '((analyze-flow boolean t)))
;;; FIXME: Can't just punt like normal since it's an APPEND method combo.
(defmethod cleavir-policy:policy-qualities append ((env null))
  '((analyze-flow boolean t)))

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'analyze-flow))
     optimize
     (environment clasp-global-environment))
  (or (> (cleavir-policy:optimize-value optimize 'space)
         (cleavir-policy:optimize-value optimize 'compilation-speed))
      (> (cleavir-policy:optimize-value optimize 'speed)
         (cleavir-policy:optimize-value optimize 'compilation-speed))))

;;; Kildall can only be done on whole functions, due to how control flow in
;;; HIR works. But do not affect the top level enter instruction most of the time.
;;; So we have this helper that sees if the policy is in place anywhere at all in
;;; the function.
;;; KLUDGE. It should be easier to limit optimizations to lexical regions.

(defun policy-anywhere-p (initial-instruction quality)
  (setf cl-user::*test* initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (i)
     (when (cleavir-policy:policy-value (cleavir-ir:policy i)
                                        quality)
       (return-from policy-anywhere-p t)))
   initial-instruction)
  nil)
