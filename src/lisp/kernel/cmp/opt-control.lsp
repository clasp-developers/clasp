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
            `(let ((,fsym (core:coerce-fdesignator ,function))
                   ,@(mapcar #'list syms fixed))
               (,op ,fsym ,last ,@syms))))))

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

(define-compiler-macro case (&whole form keyform &rest clauses)
  ;;; Check degenerate case
  (when (null clauses)
    (return-from case `(progn ,keyform nil)))
  ;;; Use CLEAVIR-PRIMOP:CASE if everything is immediate.
  (let* ((last (first (last clauses)))
         (default-provided-p (member (first last) '(t otherwise)))
         (default (if default-provided-p
                      last
                      '(otherwise nil)))
         (cases (if default-provided-p (butlast clauses) clauses)))
    (loop for (keything . body) in cases
          for keys = (if (listp keything) keything (list keything))
          unless (every #'core:create-tagged-immediate-value-or-nil keys)
            return form
          collect (cons keys body) into new-cases
          finally (return `(cleavir-primop:case ,keyform
                             ,@new-cases
                             ,default)))))

;;; every, etc. defined in opt-sequence
