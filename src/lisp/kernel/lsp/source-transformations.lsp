(in-package :core)

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

  (defun simple-associate-args (fun first-arg more-args)
    (or more-args (error "more-args cannot be nil"))
    (let ((next (rest more-args))
          (arg (first more-args)))
      (if (null next)
          `(,fun ,first-arg ,arg)
          (simple-associate-args fun `(,fun ,first-arg ,arg) next))))

  (defun expand-associative (fun two-arg-fun args identity &optional (one-arg-result-type 'number))
    (case (length args)
      (0 identity)
      (1 `(values (the ,one-arg-result-type ,(first args))))
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

  (defun expand-compare (form fun args &optional (one-arg-result-type 'number))
    (if (proper-list-p args)
        (case (length args)
          ((0)
           ;; need at least one argument. FIXME: warn?
           form)
          ((1)
           ;; preserve nontoplevelness and side effects
           `(progn (the ,one-arg-result-type ,(first args)) t))
          ((2)
           `(,fun ,@args))
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
  (defun expand-uncompare (form fun args)
    (if (proper-list-p args)
        (case (length args)
          ((1)
           ;; preserve nontoplevelness and side effects.
           `(progn (the t ,(first args)) t))
          ((2) `(not (,fun ,@args)))
          (otherwise form))
        form))

  (define-compiler-macro aref (&whole whole array &rest indeces)
    (if (= (length indeces) 1)
        `(row-major-aref ,array ,(car indeces))
        whole))
  )
