(in-package #:ext)

;;; Must be synced with constantp in primitives.cc
(defun constant-form-value (form &optional env)
  (declare (ignore env)) ; FIXME: alternate envs!
  "If (constantp form env) is true, returns the constant value of the form in the environment."
  (cond ((symbolp form) (symbol-value form))
        ((consp form)
         ;; (assert (eql (first form) 'quote))
         (second form))
        ;; self-evaluating
        (t form)))

(in-package #:core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; proper-list-p code from Robert Strandh's Cleavir code
  (defun proper-list-p (object)
    (cond  ((null object) t)
           ((atom object) nil)
           (t (let ((slow object)
                    (fast (cdr object)))
                (declare (type cons slow))
                (tagbody
                 again
                   (unless (consp fast)
                     (return-from proper-list-p
                       (if (null fast) t nil)))
                   (when (eq fast slow)
                     (return-from proper-list-p nil))
                   (setq fast (cdr fast))
                   (unless (consp fast)
                     (return-from proper-list-p
                       (if (null fast) t nil)))
                   (setq fast (cdr fast))
                   (setq slow (cdr slow))
                   (go again))))))

  ;;; Some operators "should signal a type error", meaning that in safe code
  ;;; they _must_ signal a type error, and otherwise the behavior is undefined.
  ;;; The bytecode compiler is not smart enough to do this in a nuanced way,
  ;;; in that it just ignores THE rather than type checking (which is allowed),
  ;;; but Cleavir's is. So to implement this behavior we use
  ;;; this THE-SINGLE macro.
  ;;; The bytecode will have an actual call to a %the-single function,
  ;;; defined below, which just does a type test. So in safe and unsafe code
  ;;; there is a test.
  ;;; When compiling with Cleavir, this call will be transformed into a THE,
  ;;; so there's no actual call and the compiler can do its usual processing
  ;;; on THE forms based to the safety level.
  ;;; This setup also means we extract only the primary value, so e.g.
  ;;; (+ (values 1 2)) => 1 and not 1 2 as we want. Additionally it properly
  ;;; removes toplevelness.
  (defmacro the-single (type form &optional (return nil returnp))
    (if returnp
        `(%the-single-return ',type (values ,form) ,return)
        `(%the-single ',type (values ,form))))

  (defun %the-single (type value)
    (unless (typep value type)
      (error 'type-error :datum value :expected-type type))
    value)

  (defun %the-single-return (type value return)
    (unless (typep value type)
      (error 'type-error :datum value :expected-type type))
    return)

  (defun simple-associate-args (fun first-arg more-args)
    (or more-args (error "more-args cannot be nil"))
    (let ((next (rest more-args))
          (arg (first more-args)))
      (if (null next)
          `(,fun ,first-arg ,arg)
          (simple-associate-args fun `(,fun ,first-arg ,arg) next))))

  (defun expand-associative (fun two-arg-fun args identity
                             &optional (one-arg-result-type 'number))
    (declare (ignore fun))
    (case (length args)
      (0 identity)
      ;; Note that we only use the type information in the one argument
      ;; case because we need that check. With more arguments, the two-arg-fun
      ;; will do checks. This also applies to EXPAND-COMPARE below.
      (1 `(the-single ,one-arg-result-type ,(first args)))
      (2 (values `(,two-arg-fun ,@args) t))
      (t (simple-associate-args two-arg-fun (first args) (rest args)))))

  (defun simple-compare-args (fun first-arg more-args)
    (let ((next (rest more-args))
          (arg (first more-args)))
      (if (null next)
          `(,fun ,first-arg ,arg)
          `(if (,fun ,first-arg ,arg)
               ,(simple-compare-args fun arg next)
               nil))))

  (defun expand-compare (form fun args &optional (arg-type 't))
    (if (proper-list-p args)
        (case (length args)
          ((0)
           ;; need at least one argument. FIXME: warn?
           form)
          ((1)
           ;; preserve nontoplevelness and side effects
           `(the-single ,arg-type ,(first args) t))
          ((2)
           `(,fun ,(first args) ,(second args)))
          (otherwise
           ;; Evaluate arguments only once
           (let ((syms (mapcar (lambda (a) (declare (ignore a)) (gensym)) args)))
             `(let (,@(mapcar #'list syms args))
                ,(simple-compare-args fun (first syms) (rest syms))))))
        ;; bad syntax. warn?
        form))

  ;; /=, char/=, and so on have to compare every pair.
  ;; In general this results in order n^2 comparisons, requiring a loop etc.
  ;; For now we don't do that, and only inline the 1 and 2 arg cases.
  (defun expand-uncompare (form fun args &optional (arg-type 't))
    (if (proper-list-p args)
        (case (length args)
          ((1)
           ;; preserve nontoplevelness and side effects.
           `(the-single ,arg-type ,(first args) t))
          ((2) `(not (,fun ,(first args) ,(second args))))
          (otherwise form))
        form))
  (export '(expand-associative expand-compare expand-uncompare) "CORE")
  )
