(in-package #:clasp-cleavir)

;;;; TRANSFORMS are like compiler macros, but use the context (environment)
;;;; more heavily. They can access inferred types and other information about
;;;; their parameters (mostly just types so far).
;;;; Syntax is as follows:
;;;; deftransform (op-name (&rest lambda-list) &body body)
;;;; op-name is the name of a function.
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
;;;; Transforms are tried most-specific first. A transform is more specific
;;;; than another if they work on calls with the same number of arguments, and
;;;; all types of the first are recognizable subtypes of the second.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *bir-transformers* (make-hash-table :test #'equal)))

(defun arg-subtypep (arg ctype)
  (ctype:subtypep (ctype:primary (bir:ctype arg) *clasp-system*)
                  ctype *clasp-system*))

(defun maybe-transform (call transforms)
  (loop with args = (rest (bir:inputs call))
        with nargs = (length args)
        for (transform . types) in transforms
        when (and (= (length types) nargs) (every #'arg-subtypep args types))
          do (funcall transform call)
          and return t))

(defmethod cleavir-bir-transformations:transform-call
    ((system clasp) key call)
  (let ((trans (gethash key *bir-transformers*)))
    (if trans
        (maybe-transform call trans)
        nil)))

(defmacro %deftransformation (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *bir-transformers*) nil)
     (setf (gethash ',name *fn-transforms*) '(,name))
     ',name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun more-specific-types-p (types1 types2)
    ;; True if the lists have the same number of types, and all types in
    ;; the first are recognizable subtypes of those in the second.
    (and (= (length types1) (length types2))
         (every (lambda (t1 t2) (ctype:subtypep t1 t2 *clasp-system*))
                types1 types2)))
  (defun %def-bir-transformer (name function param-types)
    ;; We just use a reverse alist (function . types).
    ;; EQUALP does not actually technically test type equality, which is what we
    ;; want, but it should be okay for now at least.
    (let* ((transformers (gethash name *bir-transformers*))
           (existing (rassoc param-types transformers :test #'equalp)))
      (if existing
          ;; replace
          (setf (car existing) function)
          ;; Merge in, respecting subtypep
          (setf (gethash name *bir-transformers*)
                (merge 'list (list (cons function param-types))
                       (gethash name *bir-transformers*)
                        #'more-specific-types-p
                       :key #'cdr))))))

(defmacro %deftransform (name (instparam) (&rest param-types)
                                &body body)
  (let ((param-types
          (loop for ty in param-types
                collect (cleavir-env:parse-type-specifier
                         ty nil *clasp-system*))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (unless (nth-value 1 (gethash ',name *bir-transformers*))
         (%deftransformation ,name))
       (%def-bir-transformer ',name (lambda (,instparam) ,@body) '(,@param-types))
       ',name)))

;;; Given an expression, make a CST for it.
;;; FIXME: This should be more sophisticated. I'm thinking the source info
;;; should be as for an inlined function.
(defun cstify-transformer (origin expression)
  (cst:cst-from-expression expression :source origin))

;;; FIXME: Only required parameters permitted here
(defmacro deftransform (name typed-lambda-list &body body)
  (let* ((params (loop for entry in typed-lambda-list
                       collect (if (consp entry) (first entry) entry)))
         (typespecs (loop for entry in typed-lambda-list
                          collect (if (consp entry) (second entry) 't)))
         (csym (gensym "CALL")))
    `(%deftransform ,name (,csym) (,@typespecs)
        (replace-callee-with-lambda ,csym
                                    (cstify-transformer
                                     (bir:origin ,csym)
                                     (list 'lambda '(,@params)
                                           '(declare (ignorable ,@params))
                                           (progn ,@body)))))))

(defmacro define-deriver (name deriver-name)
  `(setf (gethash ',name *derivers*) '(,deriver-name)))

(defmethod bir-transformations:derive-return-type ((inst bir:call) deriver
                                                   (system clasp))
  (funcall (fdefinition deriver) inst))

;;;

(defun replace-callee-with-lambda (call lambda-expression-cst)
  (let* (;; FIXME: We should be harsher with errors than cst->ast is here,
         ;; since deftransforms are part of the compiler, and not the
         ;; user's fault.
         (ast (cst->ast lambda-expression-cst))
         (module (bir:module (bir:function call)))
         (bir (cleavir-ast-to-bir:compile-into-module ast module
                                                      *clasp-system*)))
    ;; Run the first few transformations.
    ;; FIXME: Use a pass manager/reoptimize flags/something smarter.
    (bir-transformations:eliminate-catches bir)
    (bir-transformations:find-module-local-calls module)
    (bir-transformations:function-optimize-variables bir)
    ;; Now properly insert it.
    (change-class call 'bir:local-call
                  :inputs (list* bir (rest (bir:inputs call))))
    (bir-transformations:maybe-interpolate bir)))

;;; for folding identity operations.
(defun replace-call-with-argument (call idx)
  (let ((argument (nth idx (rest (bir:inputs call)))))
    (setf (bir:inputs call) nil)
    (bir:replace-uses argument (bir:output call))
    (bir:delete-instruction call)))

(defun reference-constant-before (inst value)
  (let* ((module (bir:module (bir:function inst)))
         (constant (bir:constant-in-module value module))
         (cv (make-instance 'bir:output
               :derived-type (ctype:single-value
                              (ctype:member *clasp-system* value)
                              *clasp-system*)))
         (cr (make-instance 'bir:constant-reference
               :policy (bir:policy inst) :origin (bir:origin inst)
               :inputs (list constant) :outputs (list cv))))
    (bir:insert-instruction-before cr inst)
    cv))

(defun replace-call-with-constant (call value)
  (let ((cv (reference-constant-before call value)))
    (bir:replace-uses cv (bir:output call))
    (bir:delete-instruction call)))

(defun replace-call-with-vprimop (call primop-name)
  (change-class call 'cleavir-bir:primop
                :inputs (rest (bir:inputs call)) ; don't need the function
                :info (cleavir-primop-info:info primop-name)))

;;; This is important to perform complex type derivations.
;;; I have serious reservations about doing it in this side effectual way,
;;; but the perfect is the enemy of the good.
(defun wrap-in-thei (inst ctype)
  (let* ((datum (bir:output inst))
         (new-datum (make-instance 'bir:output
                      :derived-type (bir:ctype datum)))
         (thei (make-instance 'bir:thei
                 :policy (bir:policy inst) :origin (bir:origin inst)
                 :asserted-type ctype :type-check-function :trusted
                 :outputs (list new-datum))))
    (bir:insert-instruction-after thei inst)
    (bir:replace-uses new-datum datum)
    (setf (bir:inputs thei) (list datum))
    thei))

(defun replace-with-vprimop-and-wrap (inst primop ctype)
  (wrap-in-thei inst (ctype:single-value ctype *clasp-system*))
  (replace-call-with-vprimop inst primop)
  t)

(defun wrap-coerce-sf-to-df (inst datum)
  (let* ((df (ctype:single-value
              (ctype:range 'double-float '* '* *clasp-system*)
              *clasp-system*))
         (new (make-instance 'bir:output
                :derived-type df))
         (coerce (make-instance 'bir:primop
                   :origin (bir:origin inst) :policy (bir:policy inst)
                   :info (cleavir-primop-info:info 'core::single-to-double)
                   :outputs (list new))))
    (bir:insert-instruction-before coerce inst)
    (bir:replace-uses new datum)
    (setf (bir:inputs coerce) (list datum)))
  (values))

(defun wrap-coerce-df-to-sf (inst datum)
  (let* ((sf (ctype:single-value
              (ctype:range 'single-float '* '* *clasp-system*)
              *clasp-system*))
         (new (make-instance 'bir:output
                :derived-type sf))
         (coerce (make-instance 'bir:primop
                   :origin (bir:origin inst) :policy (bir:policy inst)
                   :info (cleavir-primop-info:info 'core::double-to-single)
                   :outputs (list new))))
    (bir:insert-instruction-before coerce inst)
    (bir:replace-uses new datum)
    (setf (bir:inputs coerce) (list datum)))
  (values))

(macrolet ((define-two-arg-f (name sf-primop df-primop)
             (let ((sf (ctype:range 'single-float '* '* *clasp-system*))
                   (df
                     (ctype:range 'double-float '* '* *clasp-system*)))
               `(progn
                  (%deftransform ,name (call) (single-float single-float)
                    (replace-with-vprimop-and-wrap call ',sf-primop ',sf))
                  (%deftransform ,name (call) (double-float double-float)
                    (replace-with-vprimop-and-wrap call ',df-primop ',df))
                  (%deftransform ,name (call) (single-float double-float)
                    (wrap-coerce-sf-to-df call (first (rest (bir:inputs call))))
                    (replace-with-vprimop-and-wrap call ',df-primop ',df))
                  (%deftransform ,name (call) (double-float single-float)
                    (wrap-coerce-sf-to-df call (second (rest (bir:inputs call))))
                    (replace-with-vprimop-and-wrap call ',df-primop ',df)))))
           (define-two-arg-ff (name sf-primop df-primop)
             `(progn
                (define-two-arg-f ,name ,sf-primop ,df-primop)
                (deftransform ,name ((x fixnum) (y single-float))
                  '(core::primop ,sf-primop
                    (core::primop core::fixnum-to-single x) y))
                (deftransform ,name ((x single-float) (y fixnum))
                  '(core::primop ,sf-primop
                    x (core::primop core::fixnum-to-single y)))
                (deftransform ,name ((x fixnum) (y double-float))
                  '(core::primop ,df-primop
                    (core::primop core::fixnum-to-double x) y))
                (deftransform ,name ((x double-float) (y fixnum))
                  '(core::primop ,df-primop
                    x (core::primop core::fixnum-to-double y))))))
  (define-two-arg-ff core:two-arg-+ core::two-arg-sf-+ core::two-arg-df-+)
  (define-two-arg-ff core:two-arg-- core::two-arg-sf-- core::two-arg-df--)
  (define-two-arg-ff core:two-arg-* core::two-arg-sf-* core::two-arg-df-*)
  (define-two-arg-ff core:two-arg-/ core::two-arg-sf-/ core::two-arg-df-/)
  (define-two-arg-f expt           core::sf-expt      core::df-expt))

(%deftransform ftruncate (call) (single-float single-float)
  (wrap-in-thei call (cleavir-env:parse-values-type-specifier
                      '(values single-float single-float &rest nil)
                      nil *clasp-system*))
  (replace-call-with-vprimop call 'core::sf-ftruncate))
(%deftransform ftruncate (call) (double-float double-float)
  (wrap-in-thei call (cleavir-env:parse-values-type-specifier
                      '(values double-float double-float &rest nil)
                      nil *clasp-system*))
  (replace-call-with-vprimop call 'core::df-ftruncate))
(%deftransform ftruncate (call) (single-float double-float)
  ;; FIXME: i think our FTRUNCATE function has a bug: it should return doubles in
  ;; this case, by my reading.
  (wrap-coerce-sf-to-df call (first (rest (bir:inputs call))))
  (wrap-in-thei call (cleavir-env:parse-values-type-specifier
                      '(values double-float double-float &rest nil)
                      nil *clasp-system*))
  (replace-call-with-vprimop call 'core::df-ftruncate))
(%deftransform ftruncate (call) (double-float single-float)
  (wrap-coerce-sf-to-df call (second (rest (bir:inputs call))))
  (wrap-in-thei call (cleavir-env:parse-values-type-specifier
                      '(values double-float double-float &rest nil)
                      nil *clasp-system*))
  (replace-call-with-vprimop call 'core::df-ftruncate))
;; TODO: one-arg form

(macrolet ((define-float-conditional (name sf-primop df-primop)
             `(progn
                (deftransform ,name ((x single-float) (y single-float))
                  '(if (core::primop ,sf-primop x y) t nil))
                (deftransform ,name ((x double-float) (y double-float))
                  '(if (core::primop ,df-primop x y) t nil))
                (deftransform ,name ((x single-float) (y double-float))
                  '(if (core::primop ,df-primop
                        (core::primop core::single-to-double x) y)
                    t nil))
                (deftransform ,name ((x double-float) (y single-float))
                  '(if (core::primop ,df-primop
                        x (core::primop core::single-to-double y))
                    t nil)))))
  (define-float-conditional core:two-arg-=
    core::two-arg-sf-= core::two-arg-df-=)
  (define-float-conditional core:two-arg-<
    core::two-arg-sf-< core::two-arg-df-<)
  (define-float-conditional core:two-arg-<=
    core::two-arg-sf-<= core::two-arg-df-<=)
  (define-float-conditional core:two-arg->
    core::two-arg-sf-> core::two-arg-df->)
  (define-float-conditional core:two-arg->=
    core::two-arg-sf->= core::two-arg-df->=))

(deftransform zerop ((n single-float))
  '(if (core::primop core::two-arg-sf-= n 0f0) t nil))
(deftransform plusp ((n single-float))
  '(if (core::primop core::two-arg-sf-> n 0f0) t nil))
(deftransform minusp ((n single-float))
  '(if (core::primop core::two-arg-sf-< n 0f0) t nil))

(deftransform zerop ((n double-float))
  '(if (core::primop core::two-arg-df-= n 0d0) t nil))
(deftransform plusp ((n double-float))
  '(if (core::primop core::two-arg-df-> n 0d0) t nil))
(deftransform minusp ((n double-float))
  '(if (core::primop core::two-arg-df-< n 0d0) t nil))

(macrolet ((define-one-arg-f (name sf-primop df-primop)
             (let ((sf (ctype:range 'single-float '* '* *clasp-system*))
                   (df
                     (ctype:range 'double-float '* '* *clasp-system*)))
               `(progn
                  (%deftransform ,name (call) (single-float)
                    (replace-with-vprimop-and-wrap call ',sf-primop ',sf))
                  (%deftransform ,name (call) (double-float)
                    (replace-with-vprimop-and-wrap call ',df-primop ',df))))))
  (define-one-arg-f abs         core::sf-abs    core::df-abs)
  (define-one-arg-f sqrt        core::sf-sqrt   core::df-sqrt)
  (define-one-arg-f exp         core::sf-exp    core::df-exp)
  ;; there's probably some weird floating point reason to explain why
  ;; llvm has an fneg instruction but not a reciprocal, but i don't know it.
  (define-one-arg-f core:negate core::sf-negate core::df-negate)
  (define-one-arg-f cos         core::sf-cos    core::df-cos)
  (define-one-arg-f sin         core::sf-sin    core::df-sin)
  (define-one-arg-f tan         core::sf-tan    core::df-tan)
  (define-one-arg-f acos        core::sf-acos   core::df-acos)
  (define-one-arg-f asin        core::sf-asin   core::df-asin)
  (define-one-arg-f cosh        core::sf-cosh   core::df-cosh)
  (define-one-arg-f sinh        core::sf-sinh   core::df-sinh)
  (define-one-arg-f tanh        core::sf-tanh   core::df-tanh)
  (define-one-arg-f acosh       core::sf-acosh  core::df-acosh)
  (define-one-arg-f asinh       core::sf-asinh  core::df-asinh)
  (define-one-arg-f atanh       core::sf-atanh  core::df-atanh))

(%deftransform core:reciprocal (call) (single-float)
  (let ((onev (reference-constant-before call 1f0)))
    (change-class call 'cleavir-bir:primop
                  :inputs (list onev (first (rest (bir:inputs call))))
                  :info (cleavir-primop-info:info 'core::two-arg-sf-/))))
(%deftransform core:reciprocal (call) (double-float)
  (let ((onev (reference-constant-before call 1d0)))
    (change-class call 'cleavir-bir:primop
                  :inputs (list onev (first (rest (bir:inputs call))))
                  :info (cleavir-primop-info:info 'core::two-arg-df-/))))

;;; Transform log, but only one-argument log (which can be derived from the
;;; two argument case by the compiler macro in opt-number.lsp)
(%deftransform log (call) (single-float)
  (replace-with-vprimop-and-wrap call 'core::sf-log
                                 (ctype:range 'single-float '* '*
                                              *clasp-system*)))
(%deftransform log (call) (double-float)
  (replace-with-vprimop-and-wrap call 'core::df-log
                                 (ctype:range 'double-float '* '*
                                              *clasp-system*)))

(%deftransform float (call) (single-float)
  (replace-call-with-argument call 0))
(%deftransform float (call) (single-float single-float)
  (replace-call-with-argument call 0))
(%deftransform float (call) (double-float double-float)
  (replace-call-with-argument call 0))
(%deftransform float (call) (single-float double-float)
  (wrap-coerce-sf-to-df call (first (rest (bir:inputs call))))
  (replace-call-with-argument call 0))
(%deftransform float (call) (double-float single-float)
  (wrap-coerce-df-to-sf call (first (rest (bir:inputs call))))
  (replace-call-with-argument call 0))
(%deftransform float (call) (double-float)
  (wrap-coerce-df-to-sf call (first (rest (bir:inputs call))))
  (replace-call-with-argument call 0))

(deftransform float ((num fixnum) (proto single-float))
  `(core::primop core::fixnum-to-single num))
(deftransform float ((num fixnum))
  `(core::primop core::fixnum-to-single num))
(deftransform float ((num fixnum) (proto double-float))
  `(core::primop core::fixnum-to-double num))

(defun derive-float (call)
  (cleavir-ctype:single-value
   (let ((args (rest (bir:inputs call))))
     (if (rest args)
         (let ((proto (second args))
               #+(or) ; nonexistent
               (short (ctype:range 'short-float '* '* *clasp-system*))
               (single (ctype:range 'single-float '* '* *clasp-system*))
               (double (ctype:range 'double-float '* '* *clasp-system*))
               #+(or) ; nonexistent
               (long (ctype:range 'long-float '* '* *clasp-system*)))
           (cond #+(or)((arg-subtypep proto short) short)
                 ((arg-subtypep proto single) single)
                 ((arg-subtypep proto double) double)
                 #+(or)((arg-subtypep proto long) long)
                 (t (env:parse-type-specifier 'float nil *clasp-system*))))
         ;; FIXME: More sophisticated type operations would make this more
         ;; precise. For example, it would be good to derive that if the
         ;; argument is an (or single-float rational), the result is a
         ;; single float.
         (let ((arg (first args))
               (float (env:parse-type-specifier 'float nil *clasp-system*))
               (rat (env:parse-type-specifier 'rat nil *clasp-system*)))
           (cond ((arg-subtypep arg float)
                  (ctype:primary (bir:ctype arg) *clasp-system*))
                 ((arg-subtypep arg rat)
                  (ctype:range 'single-float '* '* *clasp-system*))
                 (t float)))))
   *clasp-system*))

(define-deriver float derive-float)

;;;

(%deftransform realpart (c) (real) (replace-call-with-argument c 0))
(%deftransform imagpart (c) (rational) (replace-call-with-constant c 0))
;; imagpart of a float is slightly complicated with negative zero
(%deftransform conjugate (c) (real) (replace-call-with-argument c 0))
(%deftransform numerator (c) (integer) (replace-call-with-argument c 0))
(%deftransform denominator (c) (integer)
  (replace-call-with-constant c 1))
(%deftransform rational (c) (rational) (replace-call-with-argument c 0))
(%deftransform rationalize (c) (rational)
  (replace-call-with-argument c 0))

(deftransform equalp ((x number) (y number)) '(= x y))

;;;

(deftransform equal ((x character) (y character)) '(char= x y))
(deftransform equalp ((x character) (y character)) '(char-equal x y))

;;;

(%deftransform aref (call) ((simple-array single-float (*)) t)
  (replace-with-vprimop-and-wrap call 'core::sf-vref
                                 (ctype:range 'single-float '* '*
                                              *clasp-system*)))
(%deftransform aref (call) ((simple-array double-float (*)) t)
  (replace-with-vprimop-and-wrap call 'core::df-vref
                                 (ctype:range 'double-float '* '*
                                              *clasp-system*)))
(%deftransform row-major-aref (call) ((simple-array single-float (*)) t)
  (replace-with-vprimop-and-wrap call 'core::sf-vref
                                 (ctype:range 'single-float '* '*
                                              *clasp-system*)))
(%deftransform row-major-aref (call) ((simple-array double-float (*)) t)
  (replace-with-vprimop-and-wrap call 'core::df-vref
                                 (ctype:range 'double-float '* '*
                                              *clasp-system*)))

(%deftransform core:row-major-aset (call)
  ((simple-array single-float (*)) t t)
  (wrap-in-thei call
                (ctype:single-value
                 (ctype:range 'single-float '* '* *clasp-system*)
                 *clasp-system*))
  (change-class call 'cleavir-bir:primop
                :inputs (let ((args (rest (bir:inputs call))))
                          ;; aset takes (array index value) while the intrinsic
                          ;; takes (value array index)
                          (list (third args) (first args) (second args)))
                :info (cleavir-primop-info:info 'core::sf-vset)))
(%deftransform core:row-major-aset (call)
  ((simple-array double-float (*)) t t)
  (wrap-in-thei call
                (ctype:single-value
                 (ctype:range 'double-float '* '* *clasp-system*)
                 *clasp-system*))
  (change-class call 'cleavir-bir:primop
                :inputs (let ((args (rest (bir:inputs call))))
                          ;; aset takes (array index value) while the intrinsic
                          ;; takes (value array index)
                          (list (third args) (first args) (second args)))
                :info (cleavir-primop-info:info 'core::df-vset)))

(%deftransform (setf aref) (call) (t (simple-array single-float (*)) t)
  (replace-with-vprimop-and-wrap call 'core::sf-vset
                                 (ctype:range 'single-float '* '*
                                              *clasp-system*)))
(%deftransform (setf aref) (call) (t (simple-array double-float (*)) t)
  (replace-with-vprimop-and-wrap call 'core::df-vset
                                 (ctype:range 'double-float '* '*
                                              *clasp-system*)))

(deftransform aref ((arr vector) (index t)) '(row-major-aref arr index))
(deftransform (setf aref) ((val t) (arr vector) (index t))
  '(setf (row-major-aref arr index) val))

(%deftransform array-rank (call) ((array * (*)))
  (replace-call-with-constant call 1))

(%deftransform array-total-size (call) ((simple-array * (*)))
  (replace-with-vprimop-and-wrap call 'core::vector-length
                                 (env:parse-type-specifier
                                  'valid-array-dimension
                                  nil *clasp-system*)))

(%deftransform length (call) (vector)
  (replace-with-vprimop-and-wrap call 'core::vector-length
                                 (env:parse-type-specifier
                                  'valid-array-dimension
                                  nil *clasp-system*)))

(defun derive-aref (call)
  (let* ((aarg (first (rest (bir:inputs call))))
         (ct (ctype:primary (bir:ctype aarg) *clasp-system*)))
    (ctype:single-value
     (if (and (consp ct)
              (member (first ct) '(array simple-array vector))
              (consp (cdr ct)))
         (second ct)
         (ctype:top *clasp-system*))
     *clasp-system*)))

(define-deriver aref derive-aref)
(define-deriver row-major-aref derive-aref)

#+(or) ; string= is actually slower atm due to keyword etc processing
(deftransform equal ((x string) (y string)) '(string= x y))

(deftransform string ((x symbol)) '(symbol-name x))
(deftransform string ((x string)) '(progn x))

;;;

(%deftransform lognot (call) (fixnum)
  (replace-with-vprimop-and-wrap call 'core::fixnum-lognot
                                 (ctype:range 'integer
                                              most-negative-fixnum
                                              most-positive-fixnum
                                              *clasp-system*)))

(macrolet ((deflog2 (name primop)
             (let ((fix (ctype:range 'integer
                                     most-negative-fixnum
                                     most-positive-fixnum
                                     *clasp-system*)))
               `(%deftransform ,name (call) (fixnum fixnum)
                  (replace-with-vprimop-and-wrap call ',primop ',fix)))))
  (deflog2 core:logand-2op core::fixnum-logand)
  (deflog2 core:logior-2op core::fixnum-logior)
  (deflog2 core:logxor-2op core::fixnum-logxor))

(defun lognot-before (before datum)
  (let* ((fix (ctype:single-value
               (ctype:range 'integer most-negative-fixnum
                            most-positive-fixnum *clasp-system*)
               *clasp-system*))
         (new (make-instance 'bir:output :derived-type fix))
         (not (make-instance 'bir:primop
                :origin (bir:origin before) :policy (bir:policy before)
                :info (cleavir-primop-info:info 'core::fixnum-lognot)
                :outputs (list new))))
    (bir:insert-instruction-before not before)
    (bir:replace-uses new datum)
    (setf (bir:inputs not) (list datum)))
  (values))
(defun lognot-after (after datum)
  (let* ((fix (ctype:single-value
               (ctype:range 'integer most-negative-fixnum
                            most-positive-fixnum *clasp-system*)
               *clasp-system*))
         (new (make-instance 'bir:output :derived-type fix))
         (not (make-instance 'bir:primop
                :origin (bir:origin after) :policy (bir:policy after)
                :info (cleavir-primop-info:info 'core::fixnum-lognot)
                :outputs (list new))))
    (bir:insert-instruction-after not after)
    (bir:replace-uses new datum)
    (setf (bir:inputs not) (list datum)))
  (values))

(macrolet ((deflog2c (name primop which)
             (let ((fix (ctype:range 'integer
                                     most-negative-fixnum
                                     most-positive-fixnum
                                     *clasp-system*)))
               `(%deftransform ,name (call) (fixnum fixnum)
                  (lognot-before call (,which (rest (bir:inputs call))))
                  (replace-with-vprimop-and-wrap call ',primop ',fix)))))
  (deflog2c logandc1 core::fixnum-logand first)
  (deflog2c logandc2 core::fixnum-logand second)
  (deflog2c logorc1 core::fixnum-logior first)
  (deflog2c logorc2 core::fixnum-logior second))

(macrolet ((deflog2r (name primop)
             (let ((fix (ctype:range 'integer
                                     most-negative-fixnum
                                     most-positive-fixnum
                                     *clasp-system*)))
               `(%deftransform ,name (call) (fixnum fixnum)
                  (let ((out (bir:output call)))
                    (replace-with-vprimop-and-wrap call ',primop ',fix)
                    (lognot-after call out))))))
  (deflog2r core:logeqv-2op core::fixnum-logxor)
  (deflog2r lognand core::fixnum-logand)
  (deflog2r lognor core::fixnum-logior))

;;; This is a very KLUDGEy way to find additions of fixnums whose result is
;;; a fixnum as well. Plus it hardcodes the number of fixnum bits. FIXME
(%deftransform core:two-arg-+ (call) ((signed-byte 60) (signed-byte 60))
  (replace-with-vprimop-and-wrap call 'core::fixnum-add
                                 (ctype:range 'integer
                                              most-negative-fixnum
                                              most-positive-fixnum
                                              *clasp-system*)))
(%deftransform core:two-arg-- (call) ((signed-byte 60) (signed-byte 60))
  (replace-with-vprimop-and-wrap call 'core::fixnum-sub
                                 (ctype:range 'integer
                                              most-negative-fixnum
                                              most-positive-fixnum
                                              *clasp-system*)))

(macrolet ((define-fixnum-conditional (name primop)
             `(deftransform ,name ((x fixnum) (y fixnum))
                '(if (core::primop ,primop x y) t nil))))
  (define-fixnum-conditional core:two-arg-=  core::two-arg-fixnum-=)
  (define-fixnum-conditional core:two-arg-<  core::two-arg-fixnum-<)
  (define-fixnum-conditional core:two-arg-<= core::two-arg-fixnum-<=)
  (define-fixnum-conditional core:two-arg->  core::two-arg-fixnum->)
  (define-fixnum-conditional core:two-arg->= core::two-arg-fixnum->=))

(deftransform zerop ((n fixnum))
  '(if (core::primop core::two-arg-fixnum-= n 0) t nil))
(deftransform plusp ((n fixnum))
  '(if (core::primop core::two-arg-fixnum-> n 0) t nil))
(deftransform minusp ((n fixnum))
  '(if (core::primop core::two-arg-fixnum-< n 0) t nil))

(deftransform logcount ((n (and fixnum unsigned-byte)))
  '(core::primop core::fixnum-positive-logcount n))

;;;

(%deftransform car (call) (cons)
  (replace-call-with-vprimop call 'cleavir-primop:car))
(%deftransform cdr (call) (cons)
  (replace-call-with-vprimop call 'cleavir-primop:cdr))

(deftransform length ((x null)) 0)
(deftransform length ((x cons)) '(core:cons-length x))
(deftransform length ((x list))
  `(if (null x)
       0
       (core:cons-length x)))

;; These transforms are unsafe, as NTH does not signal out-of-bounds.
#+(or)
(progn
(deftransform elt ((seq list) n) '(nth n seq))
(deftransform core:setf-elt ((seq list) n value) '(setf (nth n seq) value))
)

(deftransform reverse ((x list)) '(core:list-reverse x))
(deftransform nreverse ((x list)) '(core:list-nreverse x))
