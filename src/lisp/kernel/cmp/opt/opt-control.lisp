(in-package #:cmp)

(define-compiler-macro apply (&whole form function &rest arguments
                                     &environment env)
  (if (null arguments)
      form ; error, leave it to runtime
      (let* ((fixed (butlast arguments))
             (last (first (last arguments)))
             (fsym (gensym "FUNCTION"))
             (syms (gensym-list fixed))
             (op (case (length fixed)
                   ((0) 'core:apply0)
                   ((1) 'core:apply1)
                   ((2) 'core:apply2)
                   ((3) 'core:apply3)
                   (otherwise 'core:apply4))))
        ;; Pick off (apply ... nil), which could be generated
        ;; (for example in CLOS).
        (if (and (constantp last env)
                 (null (ext:constant-form-value last env)))
            `(funcall ,function ,@fixed)
            ;; The LET is so that we evaluate the arguments to APPLY
            ;; in the correct order.
            `(let ((,fsym (core:coerce-called-fdesignator ,function))
                   ,@(mapcar #'list syms fixed))
               (,op ,fsym ,last ,@syms))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun function-form-p (form)
  (and (consp form)
       (eq (car form) 'function)
       (consp (cdr form))
       (null (cddr form))))
)

;;; Collapse (coerce-fdesignator #'foo) to #'foo,
;;; (coerce-fdesignator 'foo) to (fdefinition 'foo),
;;; and (coerce-fdesignator (lambda ...)) to (lambda ...).
;;; Note that cclasp should have more sophisticated IR-level analyses
;;; expanding on this.
(define-compiler-macro core:coerce-fdesignator (&whole form designator
                                                       &environment env)
  ;; In order to cover (lambda ...), among other possibilities, macroexpand.
  (let ((designator (macroexpand designator env)))
    (cond ((function-form-p designator) designator)
          ((constantp designator env)
           (let ((value (ext:constant-form-value designator env)))
             (cond ((symbolp value) `(fdefinition ,designator))
                   ((functionp value) value)
                   (t form))))
          (t form))))
(define-compiler-macro core:coerce-called-fdesignator
    (&whole form designator &environment env)
  (let ((designator (macroexpand designator env)))
    (cond ((function-form-p designator) designator)
          ((constantp designator env)
           (let ((value (ext:constant-form-value designator env)))
             (cond ((symbolp value) `(fdefinition ,designator))
                   ((functionp value) value)
                   (t form))))
          (t form))))

(define-compiler-macro not (objectf)
  ;; Take care of (not (not x)), which code generates sometimes.
  (if (and (consp objectf)
           (eq (car objectf) 'not)
           (consp (cdr objectf))
           (null (cddr objectf)))
      `(if ,(second objectf) t nil)
      ;; Or just use the obvious
      `(if ,objectf nil t)))

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

(define-compiler-macro identity (object)
  ;; ensure only the primary value is returned.
  `(prog1 ,object))

(define-compiler-macro constantly (object)
  (let ((s (gensym "CONSTANTLY-OBJECT")))
    `(let ((,s ,object))
       (lambda (&rest args)
         (declare (ignore args))
         ,s))))

;;; every, etc. defined in opt-sequence
