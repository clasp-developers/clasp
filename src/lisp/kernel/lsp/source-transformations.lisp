(in-package #:ext)

;;; Must be synced with constantp in primitives.cc
(defun constant-form-value (form &optional env)
  "If (constantp form env) is true, returns the constant value of the form in the environment."
  (declare (ignore env))
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

  #+bytecode
  (defmacro the-single (type form &optional (return nil returnp))
    (let ((var (gensym)))
      `(let ((,var ,form))
         (check-type ,var ,type)
         ,(if returnp return var))))

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
      ;; We use these values &rest nil types here and below because they are
      ;; easier to check in cclasp (where THEs usually become checks).
      ;; See cleavir/convert-special.lisp.
      ;; Also note that we only use the type information in the one argument
      ;; case because we need that check. With more arguments, the two-arg-fun
      ;; will do checks. This also applies to EXPAND-COMPARE below.
      ;; Also also note that we have to use VALUES or else we'll get
      ;; (+ (values 1 nil)) => 1 NIL
      ;; which is unlikely in practice, but a bug.
      (1
       #+bytecode `(the-single ,one-arg-result-type ,(first args))
       #-bytecode `(the (values ,one-arg-result-type &rest nil) (values ,(first args))))
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
           #+bytecode `(the-single ,arg-type ,(first args) t)
           #-bytecode `(progn (the (values ,arg-type &rest nil) (values ,(first args))) t))
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
           #+bytecode `(the-single ,arg-type ,(first args) t)
           #-bytecode `(progn (the (values ,arg-type &rest nil) (values ,(first args))) t))
          ((2) `(not (,fun ,(first args) ,(second args))))
          (otherwise form))
        form))
  (export '(expand-associative expand-compare expand-uncompare) "CORE")
  )
