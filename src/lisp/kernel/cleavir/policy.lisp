(in-package :clasp-cleavir)

;;; With this policy on, the compiler tries to treat type declarations as
;;; assertions. They will be checked carefully and somewhat slowly.
(defmethod policy:compute-policy-quality
    ((quality (eql 'insert-type-checks))
     optimize
     (environment clasp-cleavir::clasp-global-environment))
  (> (policy:optimize-value optimize 'safety) 0))

;;; This policy is used in transform.lisp to determine whether to insert
;;; type checks enforcing basic safety. Without these checks, low level
;;; operators that do not check their inputs will be used unprotected, so
;;; very bad problems can occur (segfaults, crashes, etc.)
(defmethod policy:compute-policy-quality
    ((quality (eql 'insert-minimum-type-checks))
     optimize
     (environment clasp-global-environment))
  (> (policy:optimize-value optimize 'safety) 0))

;;; This policy is used in transform.lisp to determine whether to flush unused
;;; calls, even if this will not preserve some error that the call might signal.
;;; If this policy is not in place, such calls may be flushed.
(defmethod policy:compute-policy-quality
    ((quality (eql 'flush-safely))
     optimize
     (environment clasp-global-environment))
  (= (policy:optimize-value optimize 'safety) 3))

;;; Should the compiler insert code to signal step conditions? This has
;;; some overhead, so it's only done at debug 3.
(defmethod policy:compute-policy-quality
    ((quality (eql 'insert-step-conditions))
     optimize
     (environment clasp-global-environment))
  (>= (policy:optimize-value optimize 'debug) 3))

;;; This policy indicates that the compiler should note calls that could be
;;; transformed (i.e. eliminated by inlining, replacement with a primop, etc.)
;;; but couldn't be due to lack of information.
(defmethod policy:compute-policy-quality
    ((quality (eql 'note-untransformed-calls))
     optimize
     (environment clasp-global-environment))
  (declare (ignorable optimize))
  ;; at present, type inference is not good enough to handle actually
  ;; optimizable code (e.g. (and (consp x) (... (car x) ...))). as such, for
  ;; the time being at least, you have to manually request this policy
  ;; as by (declare (optimize clasp-cleavir::note-untransformed-calls))
  nil
  #+(or)
  (= (policy:optimize-value optimize 'speed) 3))

;;; This policy indicates that the compiler should note when it's forced
;;; to emit expensive boxing instructions. Note that this does not result
;;; in notes for calling functions that may box internally - FIXME?
(defmethod policy:compute-policy-quality
    ((quality (eql 'note-boxing))
     optimize
     (environment clasp-global-environment))
  (declare (ignorable optimize))
  nil
  #+(or)
  (= (policy:optimize-value optimize 'speed) 3))

;;; This policy tells the compiler to note when a &rest parameter must
;;; be consed into a list (i.e. the optimization in vaslist.lisp does not fire).
;;; It must also be specifically requested.
(defmethod policy:compute-policy-quality
    ((quality (eql 'note-consing-&rest))
     optimize
     (environment clasp-global-environment))
  (declare (ignorable optimize))
  ;; Must be specifically requested. In the future, maybe note on SPACE 3?
  nil)

(defmethod policy:compute-policy-quality
    ((quality (eql 'type-check-ftype-arguments))
     optimize
     (environment clasp-cleavir::clasp-global-environment))
  (= (policy:optimize-value optimize 'safety) 3))

(defmethod policy:compute-policy-quality
    ((quality (eql 'type-check-ftype-return-values))
     optimize
     (environment clasp-cleavir::clasp-global-environment))
  (= (policy:optimize-value optimize 'safety) 3))

(defmethod policy:compute-policy-quality
    (quality optimize (environment null))
    (policy:compute-policy-quality quality optimize *clasp-env*))

(defmethod policy:policy-qualities append ((env clasp-global-environment))
  '((save-register-args boolean t)
    (perform-optimization boolean t)
    (insert-type-checks boolean t)
    (insert-minimum-type-checks boolean t)
    (flush-safely boolean t)
    (insert-step-conditions boolean t)
    (note-untransformed-calls boolean t)
    (note-boxing boolean t)
    (note-consing-&rest boolean t)
    (core::insert-array-bounds-checks boolean t)
    (ext:assume-right-type boolean nil)
    (do-type-inference boolean t)
    (do-dx-analysis boolean t)
    (type-check-ftype-arguments boolean t)
    (type-check-ftype-return-values boolean t)))
;;; FIXME: Can't just punt like normal since it's an APPEND method combo.
(defmethod policy:policy-qualities append ((env null))
  '((save-register-args boolean t)
    (perform-optimization boolean t)
    (insert-type-checks boolean t)
    (insert-minimum-type-checks boolean t)
    (flush-safely boolean t)
    (insert-step-conditions boolean t)
    (note-untransformed-calls boolean t)
    (note-boxing boolean t)
    (note-consing-&rest boolean t)
    (core::insert-array-bounds-checks boolean t)
    (ext:assume-right-type boolean nil)
    (do-type-inference boolean t)
    (do-dx-analysis boolean t)
    (type-check-ftype-arguments boolean t)
    (type-check-ftype-return-values boolean t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy SAVE-REGISTER-ARGS.
;;;
;;; Clasp presently maintains a stack trace by doing things
;;; at runtime. This is expensive but doing otherwise is
;;; slow going. If SAVE-REGISTER-ARGS is false this stuff
;;; is not inserted, so functions so compiled won't show up
;;; in backtraces.

(defmethod policy:compute-policy-quality
    ((quality (eql 'save-register-args))
     optimize
     (environment clasp-global-environment))
  #-debug-cclasp-lisp(= (policy:optimize-value optimize 'debug) 3)
  #+debug-cclasp-lisp (> (policy:optimize-value optimize 'debug) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy PERFORM-OPTIMIZATION.
;;;
;;; If this policy is not on, Clasp will perform only minimal
;;; optimization. At present this just puts the "optnone" attribute
;;; on functions for LLVM.

(defmethod policy:compute-policy-quality
    ((quality (eql 'perform-optimization))
     optimize
     (environment clasp-global-environment))
  (< (policy:optimize-value optimize 'debug) 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policies DO-TYPE-INFERENCE, DO-DX-ANALYSIS.
;;;
;;; Since Kildall analyses are tragically slow at the moment,
;;; they need to be suppessed for reasonable compile times.
;;; If DO-TYPE-INFERENCE is false no type inference is done.
;;; If DO-DX-ANALYSIS is false no DX analysis is done.
;;; See MY-HIR-TRANSFORMATIONS for use.

(defmethod policy:compute-policy-quality
    ((quality (eql 'do-type-inference))
     optimize
     (environment clasp-global-environment))
  (> (policy:optimize-value optimize 'speed)
     (policy:optimize-value optimize 'compilation-speed)))

(defmethod policy:compute-policy-quality
    ((quality (eql 'do-dx-analysis))
     optimize
     (environment clasp-global-environment))
  (> (policy:optimize-value optimize 'space)
     (policy:optimize-value optimize 'compilation-speed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy CORE::INSERT-ARRAY-BOUNDS-CHECKS

;;; Should calls to aref and such do a bounds check?
;;; In CORE package so we can use it in earlier code.

(defmethod policy:compute-policy-quality
    ((quality (eql 'core::insert-array-bounds-checks))
     optimize
     (environment clasp-global-environment))
  (> (policy:optimize-value optimize 'safety) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; POLICY EXT:ASSUME-RIGHT-TYPE
;;;
;;; Should type declarations be trusted for unsafe transforms?
;;; If this is true, crashes can result if type declarations
;;; are wrong, so per the definition of "safe code" it must be
;;; false at safety 3.
;;; NOTE: This is only really necessary because of our non
;;; existent type inference. Policies need a FIXME rethink
;;; once things are less broken.

(defmethod policy:compute-policy-quality
    ((quality (eql 'ext:assume-right-type))
     optimize
     (environment clasp-global-environment))
  (let ((safety (policy:optimize-value optimize 'safety)))
    (and (zerop safety)
         (> (policy:optimize-value optimize 'speed) safety))))

;;;

(defun environment-has-policy-p (environment quality)
  (policy:policy-value
   (cleavir-env:policy (cleavir-env:optimize-info environment)) quality))
