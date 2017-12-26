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

  (core:bclasp-define-compiler-macro + (&rest numbers)
    (expand-associative '+ 'two-arg-+ numbers 0))

  (core:bclasp-define-compiler-macro * (&rest numbers)
    (expand-associative '* 'two-arg-* numbers 1))

  (core:bclasp-define-compiler-macro - (minuend &rest subtrahends)
    (if (proper-list-p subtrahends)
        (if subtrahends
            `(core:two-arg-- ,minuend ,(expand-associative '+ 'two-arg-+ subtrahends 0))
            `(core:negate ,minuend))
        (error "The - operator can not be part of a form that is a dotted list.")))

  (defun expand-compare (fun args)
    (if (consp args)
        (if (= (length args) 1)
            t
            (if (= (length args) 2)
                `(,fun ,(car args) ,(cadr args))
                `(if (,fun ,(car args) ,(cadr args))
                     ,(expand-compare fun (cdr args))
                     nil)))
        args))

  (core:bclasp-define-compiler-macro < (&rest numbers)
    (expand-compare 'two-arg-< numbers))

  (core:bclasp-define-compiler-macro <= (&rest numbers)
    (expand-compare 'two-arg-<= numbers))

  (core:bclasp-define-compiler-macro > (&rest numbers)
    (expand-compare 'two-arg-> numbers))

  (core:bclasp-define-compiler-macro >= (&rest numbers)
    (expand-compare 'two-arg->= numbers))

  (core:bclasp-define-compiler-macro = (&rest numbers)
    (expand-compare 'two-arg-= numbers))

  (core:bclasp-define-compiler-macro 1+ (x)
    `(core:two-arg-+ ,x 1))

  (core:bclasp-define-compiler-macro 1- (x)
    `(core:two-arg-- ,x 1))

  (core:bclasp-define-compiler-macro aref (&whole whole array &rest indeces)
    (if (= (length indeces) 1)
        `(row-major-aref ,array ,(car indeces))
        whole))
  )
