(in-package :clasp-cleavir)

(core:defconstant-equal +optimize-qualities+
    '(compilation-speed speed space safety debug))
(defvar *policy-qualities* nil)
(defvar *policy-level-descriptions* nil)

(defmacro define-policy (name compute (&rest levels))
  `(progn
     (defmethod policy:compute-policy-quality
         ((quality (eql ',name)) optimize (env clasp-global-environment))
       (declare (ignorable optimize))
       (symbol-macrolet
           (,@(loop for qual in +optimize-qualities+
                    collect `(,qual (policy:optimize-value optimize ',qual))))
         (declare (ignorable ,@+optimize-qualities+))
         ,compute))
     (push '(,name ,@levels) *policy-level-descriptions*)
     (push '(,name (member ,@(mapcar #'car levels)) ,(caar (last levels)))
           *policy-qualities*)
     ',name))

(defmethod policy:compute-policy-quality
    (quality optimize (environment null))
  (policy:compute-policy-quality quality optimize *clasp-env*))

;;; The type policies work as follows.
;;; If the value is 0, type assertions of the given kind are believed by
;;; the compiler and are not checked. Very unsafe.
;;; If the value is 1, type assertions are noted by the compiler but
;;; not believed nor checked. The compiler will not use the information to
;;; perform optimizations, but can note it to e.g. warn about type conflicts.
;;; If 2 or 3, type assertions are checked and believed.

;;; This policy is used for THE as well as TYPE declarations on variables.
(define-policy type-check-the
    (if (> safety 0) 3 0)
  ((0 "believed-unsafely") (1 "noted")
                           (2 "believed-safely") (3 "believed-safely")))


;;; This policy is used for type assertions derived from arguments to a call
;;; to a function with declared FTYPE.
(define-policy type-check-ftype-arguments
    (ecase safety ((0) 0) ((1 2) 1) ((3) 3))
  ((0 "believed-unsafely") (1 "noted")
                           (2 "believed-safely") (3 "believed-safely")))

;;; This policy is used for type assertions derived from the return values of
;;; a call to a function with declared FTYPE.
(define-policy type-check-ftype-return-values
    (ecase safety ((0) 0) ((1 2) 1) ((3) 3))
  ((0 "believed-unsafely") (1 "noted")
                           (2 "believed-safely") (3 "believed-safely")))

;;; This policy is used in transform.lisp to determine whether to flush unused
;;; calls, even if this will not preserve some error that the call might signal.
;;; If this policy is not in place, such calls may be flushed.
(define-policy flush-safely
    (= safety 3)
  ((nil "no") (t "yes")))

;;; Should the compiler insert code to signal step conditions? This has
;;; some overhead, so it's only done at debug 3.
(define-policy insert-step-conditions
    (>= debug 3)
  ((nil "no") (t "yes")))

;;; This policy indicates that the compiler should note calls that could be
;;; transformed (i.e. eliminated by inlining, replacement with a primop, etc.)
;;; but couldn't be due to lack of information.
(define-policy note-untransformed-calls
  ;; at present, type inference is not good enough to handle actually
  ;; optimizable code (e.g. (and (consp x) (... (car x) ...))). as such, for
  ;; the time being at least, you have to manually request this policy
  ;; as by (declare (optimize clasp-cleavir::note-untransformed-calls))
  nil ; (= speed 3)
  ((nil "no") (t "yes")))

;;; This policy indicates that the compiler should note when it's forced
;;; to emit expensive boxing instructions. Note that this does not result
;;; in notes for calling functions that may box internally - FIXME?
(define-policy note-boxing
  nil ; (= speed 3)
  ((nil "no") (t "yes")))

;;; This policy tells the compiler to note when a &rest parameter must
;;; be consed into a list (i.e. the optimization in vaslist.lisp does not fire).
;;; It must also be specifically requested.
(define-policy note-consing-&rest
  nil ; (= space 3)
  ((nil "no") (t "yes")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy SAVE-REGISTER-ARGS.
;;;
;;; Clasp presently maintains a stack trace by doing things
;;; at runtime. This is expensive but doing otherwise is
;;; slow going. If SAVE-REGISTER-ARGS is false this stuff
;;; is not inserted, so functions so compiled won't show up
;;; in backtraces.

(define-policy save-register-args
  #-debug-cclasp-lisp (= debug 3)
  #+debug-cclasp-lisp (> debug 0)
  ((nil "no") (t "yes")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy PERFORM-OPTIMIZATION.
;;;
;;; If this policy is not on, Clasp will perform only minimal
;;; optimization. At present this just puts the "optnone" attribute
;;; on functions for LLVM.

(define-policy perform-optimization
    (< debug 3)
  ((nil "no") (t "yes")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy CORE::INSERT-ARRAY-BOUNDS-CHECKS

;;; Should calls to aref and such do a bounds check?
;;; In CORE package so we can use it in earlier code.

(define-policy core::insert-array-bounds-checks
    (> safety 0)
  ((nil "no") (t "yes")))

;;;

(defun environment-has-policy-p (environment quality)
  (policy:policy-value
   (cleavir-env:policy (cleavir-env:optimize-info environment)) quality))

(defmethod policy:policy-qualities append ((env clasp-global-environment))
  *policy-qualities*)
;;; FIXME: Can't just punt like normal since it's an APPEND method combo.
(defmethod policy:policy-qualities append ((env null))
  *policy-qualities*)

(defun ext:describe-compiler-policy (&optional optimize)
  (let* ((optimize
           (policy:normalize-optimize (append optimize cmp:*optimize*)
                                      *clasp-env*))
         (policy (policy:compute-policy optimize *clasp-env*)))
    (fresh-line)
    (format t "  Optimize qualities:~%")
    (dolist (quality +optimize-qualities+)
      (format t "~s = ~d~%" quality (policy:optimize-value optimize quality)))
    ;; Describe computed policies
    (format t "  Compilation policies:~%")
    (loop for (quality . value) in policy
          for level-info = (cdr (assoc quality *policy-level-descriptions*))
          for desc = (second (assoc value level-info))
          do (format t "~s = ~d" quality value)
          when desc
            do (format t " (~a)" desc)
          do (terpri))))
