(in-package #:cmp)

(define-compiler-macro apply (&whole form function &rest arguments)
  (if (null arguments)
      form ; error, leave it to runtime
      (let* ((fixed (butlast arguments)) (last (first (last arguments)))
             (op (case (length fixed)
                   ((0) 'core:apply0)
                   ((1) 'core:apply1)
                   ((2) 'core:apply2)
                   ((3) 'core:apply3)
                   (otherwise 'core:apply4))))
        `(,op (core:coerce-fdesignator ,function) ,last ,@fixed))))

(define-compiler-macro eql (&whole form x y &environment env)
  (if (constantp x env)
      (when (constantp y env)
        ;; Both constant: Fold.
        (return-from eql
          (eql (ext:constant-form-value x env)
               (ext:constant-form-value y env))))
      (if (constantp y env)
          ;; y is constant but not x. swap for the rest of the code
          ;; (no order of evaluation problem, since constant)
          ;; We haven't loaded rotatef yet.
          (let ((w x) (z y))
            (setq x z y w))
          ;; Neither is constant - nothing to do
          (return-from eql form)))
  ;; OK now x is constant and y is not.
  (let ((xv (ext:constant-form-value x env)))
    (if (typep xv 'core::eq-incomparable)
        ;; X is a bignum or something - can't help that.
        form
        ;; X can be compared by EQ.
        `(eq ',xv ,y))))

(define-compiler-macro not (&whole form objectf)
  ;; Take care of (not (not x)), which code generates sometimes.
  (if (and (consp objectf)
           (eq (car objectf) 'not)
           (consp (cdr objectf))
           (null (cddr objectf)))
      `(if ,(second objectf) t nil)
      form))

;;; every, etc. defined in opt-sequence
