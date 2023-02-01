(in-package :clasp-cleavir)

;;; The type policies work as follows.
;;; If the value is 0, type assertions of the given kind are believed by
;;; the compiler and are not checked. Very unsafe.
;;; If the value is 1, type assertions are noted by the compiler but
;;; not believed nor checked. The compiler will not use the information to
;;; perform optimizations, but can note it to e.g. warn about type conflicts.
;;; If 2 or 3, type assertions are checked and believed.

;;; This policy is used for THE as well as TYPE declarations on variables.
(defmethod policy:compute-policy-quality
    ((quality (eql 'type-check-the))
     optimize
     (environment clasp-cleavir::clasp-global-environment))
  (if (> (policy:optimize-value optimize 'safety) 0)
      3
      0))

;;; This policy is used for type assertions derived from arguments to a call
;;; to a function with declared FTYPE.
(defmethod policy:compute-policy-quality
    ((quality (eql 'type-check-ftype-arguments))
     optimize
     (environment clasp-cleavir::clasp-global-environment))
  (ecase (policy:optimize-value optimize 'safety)
    ((0) 0)
    ((1 2) 1)
    ((3) 3)))

;;; This policy is used for type assertions derived from the return values of
;;; a call to a function with declared FTYPE.
(defmethod policy:compute-policy-quality
    ((quality (eql 'type-check-ftype-return-values))
     optimize
     (environment clasp-cleavir::clasp-global-environment))
  (ecase (policy:optimize-value optimize 'safety)
    ((0) 0)
    ((1 2) 1)
    ((3) 3)))

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
    (quality optimize (environment null))
    (policy:compute-policy-quality quality optimize *clasp-env*))

(defmethod policy:policy-qualities append ((env clasp-global-environment))
  '((save-register-args boolean t)
    (perform-optimization boolean t)
    (type-check-the (integer 0 3) 3)
    (type-check-ftype-arguments (integer 0 3) 3)
    (type-check-ftype-return-values (integer 0 3) 3)
    (flush-safely boolean t)
    (insert-step-conditions boolean t)
    (note-untransformed-calls boolean t)
    (note-boxing boolean t)
    (note-consing-&rest boolean t)
    (core::insert-array-bounds-checks boolean t)))
;;; FIXME: Can't just punt like normal since it's an APPEND method combo.
(defmethod policy:policy-qualities append ((env null))
  '((save-register-args boolean t)
    (perform-optimization boolean t)
    (type-check-the (integer 0 3) 3)
    (type-check-ftype-arguments (integer 0 3) 3)
    (type-check-ftype-return-values (integer 0 3) 3)
    (flush-safely boolean t)
    (insert-step-conditions boolean t)
    (note-untransformed-calls boolean t)
    (note-boxing boolean t)
    (note-consing-&rest boolean t)
    (core::insert-array-bounds-checks boolean t)))

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
;;; Policy CORE::INSERT-ARRAY-BOUNDS-CHECKS

;;; Should calls to aref and such do a bounds check?
;;; In CORE package so we can use it in earlier code.

(defmethod policy:compute-policy-quality
    ((quality (eql 'core::insert-array-bounds-checks))
     optimize
     (environment clasp-global-environment))
  (> (policy:optimize-value optimize 'safety) 0))

;;;

(defun environment-has-policy-p (environment quality)
  (policy:policy-value
   (cleavir-env:policy (cleavir-env:optimize-info environment)) quality))
