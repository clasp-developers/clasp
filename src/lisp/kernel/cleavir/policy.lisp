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

(defmethod cleavir-policy:policy-qualities append ((env clasp-global-environment))
  '((maintain-shadow-stack boolean t)
    (insert-array-bounds-checks boolean t)
    (do-type-inference boolean t)
    (do-dx-analysis boolean t)))
;;; FIXME: Can't just punt like normal since it's an APPEND method combo.
(defmethod cleavir-policy:policy-qualities append ((env null))
  '((maintain-shadow-stack boolean t)
    (insert-array-bounds-checks boolean t)
    (do-type-inference boolean t)
    (do-dx-analysis boolean t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy MAINTAIN-SHADOW-STACK.
;;;
;;; Clasp presently maintains a stack trace by doing things
;;; at runtime. This is expensive but doing otherwise is
;;; slow going. If MAINTAIN-SHADOW-STACK is false this stuff
;;; is not inserted, so functions so compiled won't show up
;;; in backtraces.

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'maintain-shadow-stack))
     optimize
     (environment clasp-global-environment))
  #-debug-cclasp-lisp(= (cleavir-policy:optimize-value optimize 'debug) 3)
  #+debug-cclasp-lisp t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policies DO-TYPE-INFERENCE, DO-DX-ANALYSIS.
;;;
;;; Since Kildall analyses are tragically slow at the moment,
;;; they need to be suppessed for reasonable compile times.
;;; If DO-TYPE-INFERENCE is false no type inference is done.
;;; If DO-DX-ANALYSIS is false no DX analysis is done.
;;; See MY-HIR-TRANSFORMATIONS for use.

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'do-type-inference))
     optimize
     (environment clasp-global-environment))
  (> (cleavir-policy:optimize-value optimize 'speed)
     (cleavir-policy:optimize-value optimize 'compilation-speed)))

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'do-dx-analysis))
     optimize
     (environment clasp-global-environment))
  (> (cleavir-policy:optimize-value optimize 'space)
     (cleavir-policy:optimize-value optimize 'compilation-speed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy INSERT-ARRAY-BOUNDS-CHECKS

(defmethod cleavir-policy:compute-policy-quality
    ((quality (eql 'insert-array-bounds-checks))
     optimize
     (environment clasp-global-environment))
  (> (cleavir-policy:optimize-value optimize 'safety) 0))

(defun has-policy-p (instruction quality)
  (cleavir-policy:policy-value (cleavir-ir:policy instruction) quality))

(defun environment-has-policy-p (environment quality)
  (cleavir-policy:policy-value
   (cleavir-env:policy (cleavir-env:optimize-info environment)) quality))

;;; Kildall can only be done on whole functions, due to how control flow in
;;; HIR works. But do not affect the top level enter instruction most of the time.
;;; So we have this helper that sees if the policy is in place anywhere at all in
;;; the function.
;;; KLUDGE. It should be easier to limit optimizations to lexical regions.

(defun policy-anywhere-p (initial-instruction quality)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (has-policy-p instruction quality)
       (return-from policy-anywhere-p t)))
   initial-instruction)
  nil)
