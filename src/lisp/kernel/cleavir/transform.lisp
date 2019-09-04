(in-package #:clasp-cleavir)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun values-type-primary (values-type)
    ;; VALUES-TYPE is an actual values type spec, i.e. a cons (values ...)
    ;; get the first thing in the values type - either a scalar type
    ;; or a lambda-list keyword. We assume validity.
    (let ((first (second values-type)))
      (case first
        ((&optional)
         (let ((second (third values-type)))
           (if (eq second '&rest)
               (fourth values-type)
               second)))
        ((&rest) (third values-type))
        (t first))))
  
  (defun function-type-result (type)
    ;; type is any type specifier
    (multiple-value-bind (head args) (core::normalize-type type)
      (if (eq head 'function)
          (let ((result (second args)))
            (cond ((null result) 't)
                  ((and (consp result) (eq (car result) 'values))
                   (values-type-primary result))
                  (t result)))
          't)))
  
  (defun form-type (form env)
    (cond ((constantp form env)
           `(eql ,(ext:constant-form-value form env)))
          ((consp form)
           (let ((operator (first form)))
             (if (symbolp operator)
                 (if (eq operator 'the)
                     (let ((type (second form)) (form (third form)))
                       `(and ,(if (and (consp type) (eq (first type) 'values))
                                  (values-type-primary type)
                                  type)
                             ,(form-type form env)))
                     (let ((info (cleavir-env:function-info nil operator)))
                       (if (typep info '(or cleavir-env:local-function-info
                                         cleavir-env:global-function-info))
                           (function-type-result (cleavir-env:type info))
                           't)))
                 't)))
          (t ; symbol (everything else covered by constantp and consp)
           (let ((info (cleavir-env:variable-info env form)))
             (if info
                 (cleavir-env:type info)
                 't)))))

  (defvar *transformers* (make-hash-table :test #'equal))

  (defun maybe-transform (form table args env)
    (let ((argtypes (mapcar (lambda (f) (form-type f env)) args)))
      (with-hash-table-iterator (next table)
        (loop
          (multiple-value-bind (present types transformer) (next)
            (if present
                (when (every (lambda (at ty) (subtypep at ty env)) argtypes types)
                  (let ((res (apply transformer args)))
                    (when res (return `(,res ,@args)))))
                (return form))))))))

(defmacro deftransformation (name)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *transformers*)
             (make-hash-table :test #'equalp)))
     (define-cleavir-compiler-macro ,name (&whole form &rest args &environment env)
       (maybe-transform form (gethash ',name *transformers*) args env))))

(defmacro deftransform (name (&rest lambda-list) &body body)
  (let ((params (loop for var in lambda-list
                      collect (if (consp var) (car var) var)))
        (types (loop for var in lambda-list
                     collect (if (consp var) (second var) 't))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',types (gethash ',name *transformers*))
             (lambda (,@params) (declare (ignorable ,@params)) ,@body)))))

;;;

(deftransformation eql)
(deftransform eql ((x (not core::eq-incomparable)) y) 'eq)
(deftransform eql (x (y (not core::eq-incomparable))) 'eq)

(deftransformation car) (deftransformation cdr)
(deftransform car ((x cons)) 'cleavir-primop:car)
(deftransform cdr ((x cons)) 'cleavir-primop:cdr)

(deftransformation primop:inlined-two-arg-+)
(deftransformation primop:inlined-two-arg--)
(deftransformation primop:inlined-two-arg-*)
(deftransformation primop:inlined-two-arg-/)
(deftransformation primop:inlined-two-arg-<)
(deftransformation primop:inlined-two-arg-<=)
(deftransformation primop:inlined-two-arg-=)
(deftransformation primop:inlined-two-arg->)
(deftransformation primop:inlined-two-arg->=)

(deftransform primop:inlined-two-arg-+ ((x fixnum) (y fixnum))
  'core:two-arg-+-fixnum-fixnum)

(macrolet ((def-float-op (name op)
             `(progn
                (deftransform ,name ((x single-float) (y single-float))
                  '(lambda (x y) (,op single-float x y)))
                (deftransform ,name ((x double-float) (y double-float))
                  '(lambda (x y) (,op double-float x y)))))
           (def-float-compare (name op)
             `(progn
                (deftransform ,name ((x single-float) (y single-float))
                  '(lambda (x y) (if (,op single-float x y) t nil)))
                (deftransform ,name ((x double-float) (y double-float))
                  '(lambda (x y) (if (,op double-float x y) t nil))))))
  (def-float-op primop:inlined-two-arg-+ cleavir-primop:float-add)
  (def-float-op primop:inlined-two-arg-- cleavir-primop:float-sub)
  (def-float-op primop:inlined-two-arg-* cleavir-primop:float-mul)
  (def-float-op primop:inlined-two-arg-/ cleavir-primop:float-div)
  (def-float-compare primop:inlined-two-arg-< cleavir-primop:float-less)
  (def-float-compare primop:inlined-two-arg-<= cleavir-primop:float-not-greater)
  (def-float-compare primop:inlined-two-arg-= cleavir-primop:float-equal)
  (def-float-compare primop:inlined-two-arg-> cleavir-primop:float-greater)
  (def-float-compare primop:inlined-two-arg->= cleavir-primop:float-not-less))

(deftransformation array-total-size)
(deftransform array-total-size ((a (simple-array * (*)))) 'core::vector-length)
;;(deftransform array-total-size ((a core:mdarray)) 'core::%array-total-size)

(deftransformation array-rank)
(deftransform array-rank ((a (simple-array * (*)))) '(lambda (a) (declare (ignore a)) 1))

(deftransformation svref/no-bounds-check)
(deftransformation (setf svref/no-bounds-check))
(deftransform svref/no-bounds-check ((a simple-vector) (index fixnum))
  '(lambda (vector index) (cleavir-primop:aref vector index t t t)))
(deftransform (setf svref/no-bounds-check) (value (a simple-vector) (index fixnum))
  '(lambda (value vector index)
    (cleavir-primop:aset vector index value t t t)
    value))

(deftransformation length)
(deftransform length ((s list)) '(lambda (x) (if (cleavir-primop:eq x nil) 0 (core:cons-length x))))
(deftransform length ((s vector)) 'core::vector-length)

(deftransformation elt) (deftransformation core:setf-elt)
(deftransform elt ((s vector) index) 'vector-read)
(deftransform core:setf-elt (value (s vector) index)
  '(lambda (value sequence index) (vector-set sequence index new-value)))

(deftransformation core::coerce-fdesignator)
(deftransform core::coerce-fdesignator ((fd symbol)) 'fdefinition)
(deftransform core::coerce-fdesignator ((fd function)) 'identity)
