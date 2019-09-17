(in-package #:clasp-cleavir)

;;;; TRANSFORMS are like compiler macros, but use the context (environment)
;;;; more heavily. Currently they are implemented through compiler macros,
;;;; but the intent is that in the future they will be used after the source
;;;; stage, when much more information has been made available through analysis.
;;;; Syntax is as follows:
;;;; deftransform (op-name (&rest lambda-list) &body body)
;;;; op-name is the name of a function or macro.
;;;; lambda-list is a typed lambda list, kind of like defmethod, but with
;;;;  types allowed as "specializers". Currently the lambda list can only have
;;;;  required parameters.
;;;; Semantics are as follows:
;;;; When the compiler sees a call to op-name, it will determine the types
;;;; of the argument forms as best it can. Then it will try to find a
;;;; transform such that the argument types are subtypes of the types of the
;;;; transform's lambda list. If it finds one, it calls the transform function
;;;; with the given argument forms. If the transform returns NIL, the compiler
;;;; tries another valid transform if there is one, or else gives up.
;;;; Otherwise, the compiler substitutes the result for the original op-name.
;;;; Here's a simple example:
;;;; (deftransform eql ((x symbol) y) 'eq)
;;;; Now when the compiler sees (eql 'foo x), this transform might be used
;;;; because it's easy to see 'FOO is a symbol. The transform unconditionally
;;;; returns EQ, so the compiler replaces the form with (eq 'foo x) and
;;;; compiles that instead.
;;;; More complicated examples return a lambda expression.
;;;: NOTE: The order in which transforms are tried is not defined.

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
                 (case operator
                   ((the)
                    (let ((type (second form)) (form (third form)))
                      `(and ,(if (and (consp type) (eq (first type) 'values))
                                 (values-type-primary type)
                                 type)
                            ,(form-type form env))))
                   ((function)
                    (if (symbolp (second form))
                        (let ((info (cleavir-env:function-info
                                     env (second form))))
                          (if (typep info '(or cleavir-env:local-function-info
                                            cleavir-env:global-function-info))
                              (cleavir-env:type info)
                              'function))
                        'function))
                   (otherwise
                    (let ((info (cleavir-env:function-info env operator)))
                      (if (typep info '(or cleavir-env:local-function-info
                                        cleavir-env:global-function-info))
                          (function-type-result (cleavir-env:type info))
                          't))))
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

;;; Set up an operator as having transforms
(defmacro deftransformation (name)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *transformers*)
             (make-hash-table :test #'equalp)))
     (define-cleavir-compiler-macro ,name (&whole form &rest args &environment env)
       (maybe-transform form (gethash ',name *transformers*) args env))))

;;; Main interface
(defmacro deftransform (name (&rest lambda-list) &body body)
  (let ((params (loop for var in lambda-list
                      collect (if (consp var) (car var) var)))
        (types (loop for var in lambda-list
                     collect (if (consp var) (second var) 't))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ;; If the operator hasn't been set up for transformation yet,
       ;; do so implicitly
       (unless (nth-value 1 (gethash ',name *transformers*))
         (deftransformation ,name))
       ;; Actually define the transform
       (setf (gethash ',types (gethash ',name *transformers*))
             (lambda (,@params) (declare (ignorable ,@params)) ,@body)))))

;;;

(deftransform eql ((x (not core::eq-incomparable)) y) 'eq)
(deftransform eql (x (y (not core::eq-incomparable))) 'eq)

(deftransform car ((x cons)) 'cleavir-primop:car)
(deftransform cdr ((x cons)) 'cleavir-primop:cdr)

(deftransform primop:inlined-two-arg-+ ((x fixnum) (y fixnum))
  'core:two-arg-+-fixnum-fixnum)

(macrolet ((def-float-op (name op)
             `(progn
                (deftransform ,name ((x single-float) (y single-float))
                  '(lambda (x y) (,op single-float x y)))
                (deftransform ,name ((x single-float) (y double-float))
                  '(lambda (x y)
                    (,op double-float
                     (cleavir-primop:coerce single-float double-float x) y)))
                (deftransform ,name ((x double-float) (y single-float))
                  '(lambda (x y)
                    (,op double-float
                     x (cleavir-primop:coerce single-float double-float y))))
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

(deftransform array-total-size ((a (simple-array * (*)))) 'core::vector-length)
;;(deftransform array-total-size ((a core:mdarray)) 'core::%array-total-size)

(deftransform array-rank ((a (simple-array * (*)))) '(lambda (a) (declare (ignore a)) 1))

(deftransform svref/no-bounds-check ((a simple-vector) (index fixnum))
  '(lambda (vector index) (cleavir-primop:aref vector index t t t)))
(deftransform (setf svref/no-bounds-check) (value (a simple-vector) (index fixnum))
  '(lambda (value vector index)
    (cleavir-primop:aset vector index value t t t)
    value))

(deftransform length ((s list)) '(lambda (x) (if (cleavir-primop:eq x nil) 0 (core:cons-length x))))
(deftransform length ((s vector)) 'core::vector-length)

(deftransform elt ((s vector) index) 'vector-read)
(deftransform core:setf-elt (value (s vector) index)
  '(lambda (value sequence index) (vector-set sequence index new-value)))

(deftransform core::coerce-fdesignator ((fd symbol)) 'fdefinition)
(deftransform core::coerce-fdesignator ((fd function)) 'identity)
