;; See the middle of the file




;;;; This file contains macro-like source transformations which
;;;; convert uses of certain functions into the canonical form desired
;;;; within the compiler. FIXME: and other IR1 transforms and stuff.

(in-package :core)

;;; These will be useful once inlining of the
;;; two-arg-XXX functions is possible

(define-source-transform logior (&rest args)
  (source-transform-transitive 'logior args 0 'integer))
(define-source-transform logxor (&rest args)
  (source-transform-transitive 'logxor args 0 'integer))
(define-source-transform logand (&rest args)
  (source-transform-transitive 'logand args -1 'integer))
(define-source-transform logeqv (&rest args)
  (source-transform-transitive 'logeqv args -1 'integer))
(define-source-transform gcd (&rest args)
  (source-transform-transitive 'gcd args 0 'integer '(abs)))
(define-source-transform lcm (&rest args)
  (source-transform-transitive 'lcm args 1 'integer '(abs)))







;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The code below is the same as that above but it has some newer features
;;; Replace the stuff above with the equivalents from below
;;;


(defun find-two-arg-function (name)
  (let ((full-name (bformat nil "TWO-ARG-%s" (string name))))
    (multiple-value-bind (sym foundp)
        (find-symbol full-name :core)
      (if foundp
          sym
          (error "Could not find symbol with name ~a" full-name)))))


(declaim (inline singleton-p))
(defun singleton-p (list)
  (and (listp list) (null (rest list)) list))

;;;; In Clasp source-transforms are implemented as compiler macros
(defmacro define-source-transform (name lambda-list &body body)
  (multiple-value-bind (func pprint doc-string)
      (sys::expand-defmacro name lambda-list body 'cl:core:bclasp-define-compiler-macro)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (compiler-macro-function ',name)
             (lambda (whole env)
               (declare (core:lambda-name
                         ,(intern (format nil "SOURCE-TRANSFORM-~a" name) :core)))
               (or (proper-list-p whole)
                   (error "Arguments for ~a must be a proper list" name))
               (multiple-value-bind (expansion done)
                   (funcall ,func whole env)
                 (if (or expansion (not done))
                     expansion
                     whole)))))))

(defmacro /show0 (&rest args) nil)

;;; LIST with one arg is an extremely common operation (at least inside
;;; SBCL itself); translate it to CONS to take advantage of common
;;; allocation routines.
(define-source-transform list (&rest args)
  (case (length args)
    (1 `(cons ,(first args) nil))
    (t (values nil t))))

;;; And similarly for LIST*.
(define-source-transform list* (arg &rest others)
  (cond ((not others) arg)
        ((not (cdr others)) `(cons ,arg ,(car others)))
        (t (values nil t))))


(define-source-transform nconc (&rest args)
  (case (length args)
    (0 ())
    (1 (car args))
    (t (values nil t))))


;;;; converting N-arg arithmetic functions
;;;;
;;;; N-arg arithmetic and logic functions are associated into two-arg
;;;; versions, and degenerate cases are flushed.

;;; Left-associate FIRST-ARG and MORE-ARGS using FUNCTION.
(declaim (ftype (sfunction (symbol t list t) list) associate-args))
(defun associate-args (fun first-arg more-args)
  (assert more-args)
  (let ((next (rest more-args))
        (arg (first more-args)))
    (if (null next)
        `(,fun ,first-arg ,arg)
        (associate-args fun `(,fun ,first-arg ,arg) next))))

;;; Reduce constants in ARGS list.
(declaim (ftype (sfunction (symbol list symbol) list) reduce-constants))
(defun reduce-constants (fun args one-arg-result-type)
  (let ((one-arg-constant-p
         (cond
           ((eq one-arg-result-type 'number) #'numberp)
           ((eq one-arg-result-type 'integer) #'integerp)
           (t (error "illegal one-arg-result-type ~a" one-arg-result-type))))
        (reduced-value)
        (reduced-p nil))
    (core::collect ((not-constants))
      (dolist (arg args)
        (let ((value (if (constantp arg)
                         arg ;;(constant-form-value arg)
                         arg)))
          (cond ((not (funcall one-arg-constant-p value))
                 (not-constants arg))
                (reduced-value
                 (setf reduced-value (funcall fun reduced-value value)
                       reduced-p t))
                (t
                 (setf reduced-value value)))))
      ;; It is tempting to drop constants reduced to identity here,
      ;; but if X is SNaN in (* X 1), we cannot drop the 1.
      (if (not-constants)
          (if reduced-p
              `(,reduced-value ,@(not-constants))
              args)
          `(,reduced-value)))))

;;; Do source transformations for transitive functions such as +.
;;; One-arg cases are replaced with the arg and zero arg cases with
;;; the identity. ONE-ARG-RESULT-TYPE is the type to ensure (with THE)
;;; that the argument in one-argument calls is.
(declaim (ftype (function (symbol list t &optional symbol list)
                          (values t &optional (member nil t)))
                source-transform-transitive))
(defun source-transform-transitive (fun args identity
                                    &optional (one-arg-result-type 'number)
                                      (one-arg-prefixes '(values)))
  (let ((two-arg-fun (find-two-arg-function fun)))
    (case (length args)
      (0 identity)
      (1 `(,@one-arg-prefixes (the ,one-arg-result-type ,(first args))))
      (2 (values `(,two-arg-fun ,@args) t))
      (t
       (let* ((reduced-args (reduce-constants fun args one-arg-result-type))
              (first (first reduced-args))
              (rest (rest reduced-args)))
         (if rest
             (associate-args two-arg-fun first rest)
             first))))))


(define-source-transform + (&rest args)
  (source-transform-transitive '+ args 0))
(define-source-transform * (&rest args)
  (source-transform-transitive '* args 1))




;;;; converting N-arg comparisons
;;;;
;;;; We convert calls to N-arg comparison functions such as < into
;;;; two-arg calls. This transformation is enabled for all such
;;;; comparisons in this file. If any of these predicates are not
;;;; open-coded, then the transformation should be removed at some
;;;; point to avoid pessimization.

;;; This function is used for source transformation of N-arg
;;; comparison functions other than inequality. We deal both with
;;; converting to two-arg calls and inverting the sense of the test,
;;; if necessary. If the call has two args, then we pass or return a
;;; negated test as appropriate. If it is a degenerate one-arg call,
;;; then we transform to code that returns true. Otherwise, we bind
;;; all the arguments and expand into a bunch of IFs.
(defun multi-compare (orig-predicate args not-p type &optional force-two-arg-p)
  (let ((nargs (length args))
        (predicate (find-two-arg-function orig-predicate)))
    (cond ((< nargs 1) (values nil t))
          ((= nargs 1) `(progn (the ,type ,@args) t))
          ((= nargs 2)
           (if not-p
               `(if (,predicate ,(first args) ,(second args)) nil t)
               (if force-two-arg-p
                   `(,predicate ,(first args) ,(second args))
                   (values nil t))))
          (t
           (do* ((i (1- nargs) (1- i))
                 (last nil current)
                 (current (gensym) (gensym))
                 (vars (list current) (cons current vars))
                 (result t (if not-p
                               `(if (,predicate ,current ,last)
                                    nil ,result)
                               `(if (,predicate ,current ,last)
                                    ,result nil))))
                ((zerop i)
                 `(let ,(mapcar #'list vars args) (declare (type ,type ,@vars)) ,result)))))))

(define-source-transform = (&rest args) (multi-compare '= args nil 'number))
(define-source-transform < (&rest args) (multi-compare '< args nil 'real))
(define-source-transform > (&rest args) (multi-compare '> args nil 'real))
;;; We cannot do the inversion for >= and <= here, since both
;;;   (< NaN X) and (> NaN X)
;;; are false, and we don't have type-information available yet. The
;;; deftransforms for two-argument versions of >= and <= takes care of
;;; the inversion to > and < when possible.
(define-source-transform <= (&rest args) (multi-compare '<= args nil 'real))
(define-source-transform >= (&rest args) (multi-compare '>= args nil 'real))


(declaim (inline canonicalize-test))
(defun canonicalize-test (test test-p test-not test-not-p default)
  (if test-p
      (if test-not-p
          (values nil nil)
          (values test nil))
      (if test-not-p
          (values test-not t)
          (values default nil))))

(defun name-from-function-form (function-form &optional env)
  (declare (ignore env))
  (if (constantp function-form)
      (let ((fun (eval function-form))) ; constant-form-value
        (cond ((symbolp fun) fun)
              ((and (consp fun)
                    (eq (car fun) 'function)
                    (consp (cdr fun))
                    (null (cddr fun)))
               ;; (function x)
               (second fun))
              (t nil)))
      nil))

(declaim (inline satisfies-two-arg-test-p))
(defun satisfies-two-arg-test-p (O Ei key test negate)
  (let* ((Zi (funcall key Ei))
         (test-result (funcall test O Zi)))
    (if negate (not test-result) test-result)))

(defun two-arg-test-form (O-form Ei-form key key-p test test-p test-not test-not-p)
  (multiple-value-bind (test negate)
      (canonicalize-test test test-p test-not test-not-p)
    (let* ((Zi-form
             (if key-p
                 (let ((maybe-key (name-from-function-form key)))
                   (if maybe-key
                       `(,maybe-key ,Ei-form)
                       ;; multiple evaluation possibility!
                       `(funcall ,key ,Ei-form)))
                 Ei-form))
           (maybe-test (name-from-function-form test))
           ;; multiple evaluation possibiliy!
           (test-form (if maybe-test
                          `(,maybe-test ,O-form ,Ei-form)
                          `(funcall ,test ,O-form ,Ei-form))))
      (if negate `(not ,test-form) test-form))))

(core:bclasp-define-compiler-macro assoc (item alist &key (key #'identity) (test nil test-p) (test-not nil test-not-p))
  (multiple-value-bind (test negate)
      (canonicalize-test test test-p test-not test-not-p '#'eql)
    (let ((S (gensym "S")) (pair (gensym "PAIR"))
          (stest (gensym "TEST")) (skey (gensym "KEY")) (sitem (gensym "ITEM")))
      `(do ((,S ,alist ,(cdr S))
            (,skey ,key)
            (,stest ,test))
            ((endp ,S) nil)
         (let ((,pair (car ,S)))
           (when ,(if negate
                      `(not (funcall ,stest ,sitem (funcall ,skey (car ,pair))))
                      `(funcall ,stest ,sitem (funcall ,skey (car ,pair))))
             (return ,pair)))))))
