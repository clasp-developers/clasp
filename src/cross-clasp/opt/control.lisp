(in-package #:cross-clasp)

(define-cross-compiler-macro apply
    (&whole form function &rest arguments &environment env)
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

(defun core:coerce-called-fdesignator (fdesignator)
  (etypecase fdesignator
    (function fdesignator)
    (symbol
     (clostrum:fdefinition m:*client* *build-rte* fdesignator))))
(defun core:apply0 (f last) (apply f last))
(defun core:apply1 (f last a0) (apply f a0 last))
(defun core:apply2 (f last a0 a1) (apply f a0 a1 last))
(defun core:apply3 (f last a0 a1 a2) (apply f a0 a1 a2 last))
(defun core:apply4 (f last &rest r)
  (multiple-value-call f (values-list r) (values-list last)))

(defun function-form-p (form)
  (and (consp form)
       (eq (car form) 'function)
       (consp (cdr form))
       (null (cddr form))))

;;; Collapse (coerce-fdesignator #'foo) to #'foo,
;;; (coerce-fdesignator 'foo) to (fdefinition 'foo),
;;; and (coerce-fdesignator (lambda ...)) to (lambda ...).
;;; Note that cclasp should have more sophisticated IR-level analyses
;;; expanding on this.
(define-cross-compiler-macro core:coerce-fdesignator
    (&whole form designator &environment env)
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

;;; Dummy macro for use by the bytecode compiler. Ignored by cclasp.
(defmacro cleavir-primop:case (keyform &rest clauses)
  `(case ,keyform ,@clauses))

(define-compiler-macro case (&whole form keyform &rest clauses)
  ;;; Check degenerate case
  (when (null clauses)
    (return-from case `(progn ,keyform nil)))
  ;;; Use CLEAVIR-PRIMOP:CASE if everything is immediate.
  ;;; In any case, check for redundant keys, and skip them in the
  ;;; expansion, while issuing a style warning.
  (let* ((last (first (last clauses)))
         (default-provided-p (member (first last) '(t otherwise)))
         (default (if default-provided-p
                      last
                      '(otherwise nil)))
         (cases (if default-provided-p (butlast clauses) clauses))
         (seen-keys (make-hash-table)))
    (flet ((filter-keys (keys)
             ;; Return two values: A list with the duplicates removed,
             ;; and the list of duplicates removed.
             (loop for k in keys
                   when (gethash k seen-keys)
                     collect k into redundant-keys
                   else collect k into cleaned-keys
                   do (setf (gethash k seen-keys) t)
                   finally (return (values cleaned-keys redundant-keys)))))
      (loop with redundantp = nil
            with optimizablep = t
            for case in cases
            for (keything . body) = case
            for keys = (cond
                         ;; Defaults in the middle: macro handles this.
                         ((eq keything 't) (return form))
                         ((eq keything 'otherwise) (return form))
                         ;; Normal case
                         ((listp keything) keything)
                         (t (list keything)))
            for (skeys rkeys) = (multiple-value-list (filter-keys keys))
            unless (every #'core:create-tagged-immediate-value-or-nil skeys)
              do (setf optimizablep nil)
            unless (null rkeys)
              do (ext:with-current-source-form (keys)
                   (warn 'core::simple-style-warning
                         :format-control "Redundant keys in CASE: ~a"
                         :format-arguments (list rkeys)))
                 (setf redundantp t optimizablep nil)
            unless (null skeys)
              collect (cons skeys body) into new-cases
            finally (return
                      (cond (optimizablep
                             `(cleavir-primop:case ,keyform
                                ,@new-cases
                                ,default))
                            (redundantp
                             ;; Since we expand into CASE again,
                             ;; this compiler macro will be
                             ;; triggered again, but this time
                             ;; there will be no redundancy.
                             ;; If this cmacro was part of the
                             ;; macro instead, this wouldn't happen.
                             `(case ,keyform ,@new-cases ,default))
                            (t form)))))))

;;; every, etc. defined in opt-sequence
