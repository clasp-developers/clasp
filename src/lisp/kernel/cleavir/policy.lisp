(in-package :clasp-cleavir)

(core:defconstant-equal +optimize-qualities+
    '(compilation-speed speed space safety debug))
(defvar *policy-qualities* nil)
(defvar *policy-descriptions* nil)

(defmacro define-policy (name compute (&rest levels) &optional documentation)
  `(progn
     (defmethod policy:compute-policy-quality
         ((quality (eql ',name)) optimize (env clasp-global-environment))
       (declare (ignorable optimize))
       (symbol-macrolet
           (,@(loop for qual in +optimize-qualities+
                    collect `(,qual (policy:optimize-value optimize ',qual))))
         (declare (ignorable ,@+optimize-qualities+))
         ,compute))
     (push '(,name ,documentation ,@levels) *policy-descriptions*)
     (push '(,name (member ,@(mapcar #'car levels)) ,(caar (last levels)))
           *policy-qualities*)
     ',name))

(defmethod policy:compute-policy-quality
    (quality optimize (environment null))
  (policy:compute-policy-quality quality optimize *clasp-env*))

(defmethod documentation ((name symbol) (dt (eql 'cmp:policy)))
  (second (assoc name *policy-descriptions*)))

;;; 

(define-policy type-check-the
    (if (> safety 0) 3 0)
  ((0 "believed-unsafely") (1 "noted")
                           (2 "believed-safely") (3 "believed-safely"))
  "This policy is used for THE as well as TYPE declarations on variables.

If the value is 0, type assertions are believed by the compiler and are not checked. Very unsafe.
If the value is 1, type assertions are noted by the compiler but not believed nor checked.
The compiler will not use the information to perform optimizations, but can note it to e.g. warn about type conflicts.
If 2 or 3, type assertions are checked and believed.")

(define-policy type-check-ftype-arguments
    (ecase safety ((0) 0) ((1 2) 1) ((3) 3))
  ((0 "believed-unsafely") (1 "noted")
                           (2 "believed-safely") (3 "believed-safely"))
  "This policy is used for type assertions derived from arguments to a call to a function with declared FTYPE.

See TYPE-CHECK-THE for an explanation of the values.")

(define-policy type-check-ftype-return-values
    (ecase safety ((0) 0) ((1 2) 1) ((3) 3))
  ((0 "believed-unsafely") (1 "noted")
                           (2 "believed-safely") (3 "believed-safely"))
  "This policy is used for type assertions derived from the return values of a call to a function with declared FTYPE.

See TYPE-CHECK-THE for an explanation of the values.")

(define-policy flush-safely
    (= safety 3)
  ((nil "no") (t "yes"))
  "Should calls to pure functions with unused results be flushed, even if this will not preserve some error that the call might signal?
If this policy is not in place, such calls may be flushed.")

(define-policy core::insert-step-conditions
    (>= debug 3)
  ((nil "no") (t "yes"))
  "Should the compiler insert code to signal step conditions? This adds a bit of overhead to every call.")

(define-policy note-untransformed-calls
  ;; at present, type inference is not good enough to handle a lot of actually
  ;; optimizable code (e.g. (and (consp x) (... (car x) ...))). as such, for
  ;; the time being at least, you have to manually request this policy
  ;; as by (declare (optimize clasp-cleavir::note-untransformed-calls))
  nil ; (= speed 3)
  ((nil "no") (t "yes"))
  "Should the compiler note calls that could be transformed (eliminated by inlining, replaced by a primop, etc.) but couldn't be due to lack of information?")

(define-policy note-boxing
  nil ; (= speed 3)
  ((nil "no") (t "yes"))
  "Should the compiler note when it's forced to emit expensive boxing instructions?
Note that calls to functions that may box internally do not result in notes - FIXME?")

(define-policy note-consing-&rest
  nil ; (= space 3) ; must be specifically requested.
  ((nil "no") (t "yes"))
  "Should the compiler note when a &rest parameter must be consed into a list (i.e. the optimization in vaslist.lisp does not fire)?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy SAVE-REGISTER-ARGS.
;;;

(define-policy save-register-args
  #-debug-cclasp-lisp (= debug 3)
  #+debug-cclasp-lisp (> debug 0)
  ((nil "no") (t "yes"))
  "Clasp presently maintains a stack trace by saving registers when functions are called. This takes time. If SAVE-REGISTER-ARGS is false, code to save registers is not inserted into functions, so functions so compiled won't show up in backtraces.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy PERFORM-OPTIMIZATION.
;;;

(define-policy perform-optimization
    (< debug 3)
  ((nil "no") (t "yes"))
  "If this policy is not on, Clasp will perform only minimal optimization. At present this just puts the \"optnone\" attribute on functions for LLVM.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Policy CORE::INSERT-ARRAY-BOUNDS-CHECKS

;;; In CORE package so we can use it in earlier code.

(define-policy core::insert-array-bounds-checks
    (> safety 0)
  ((nil "no") (t "yes"))
  "Should calls to AREF and such do a bounds check? Note that this policy only affects calls that the compiler can inline.")

;;;

(defun environment-has-policy-p (environment quality)
  (policy:policy-value
   (cleavir-env:policy (cleavir-env:optimize-info *clasp-system* environment)) quality))

(defmethod policy:policy-qualities append ((env clasp-global-environment))
  *policy-qualities*)
;;; FIXME: Can't just punt like normal since it's an APPEND method combo.
(defmethod policy:policy-qualities append ((env null))
  *policy-qualities*)

(defun ext:describe-compiler-policy (&optional optimize)
  "Print the global compiler policy to standard output.

OPTIMIZE should be the arguments of a CL:OPTIMIZE declaration specifier, e.g. ((SPEED 3) (SAFETY 1)). The policy printed is that that would be in place with current global policy augmented by the specifier. If OPTIMIZE is not provided, the unaugmented current global policy is printed."
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
          for level-info = (cddr (assoc quality *policy-descriptions*))
          for desc = (second (assoc value level-info))
          do (format t "~s = ~d" quality value)
          when desc
            do (format t " (~a)" desc)
          do (terpri))))
