(in-package #:clasp-cleavir)

#+(or)
(eval-when (:execute)
  (format t "Setting core:*echo-repl-read* to T~%")
  (setq core:*echo-repl-read* t))

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

(defun asserted-ctype (datum)
  (ctype:values-conjoin *clasp-system*
                        (bir:ctype datum) (bir:asserted-type datum)))

(defun arg-subtypep (arg ctype)
  (ctype:subtypep (ctype:primary (asserted-ctype arg) *clasp-system*)
                  ctype *clasp-system*))

(defun maybe-transform (call transforms)
  (loop with args = (rest (bir:inputs call))
        with nargs = (length args)
        for (transform . types) in transforms
        when (and (= (length types) nargs) (every #'arg-subtypep args types))
          do (funcall transform call)
          and return t))

(defmethod cleavir-bir-transformations:transform-call
    ((system clasp) key (call bir:call))
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

;; Useful below for inserting checks on arguments.
(defmacro ensure-the (type form)
  `(cleavir-primop:ensure-the (values ,type &rest t)
                              (lambda (&optional value &rest ign)
                                ;; This is going right into a primop,
                                ;; so the other values can be ignored
                                (declare (ignore ign))
                                (unless (typep value ',type)
                                  (error 'type-error :datum value
                                                     :expected-type ',type))
                                value)
                              ,form))

;; Also useful, for laziness reasons.
(defmacro truly-the (type form)
  `(cleavir-primop:truly-the (values ,type &rest nil) ,form))

;;; FIXME: Only required parameters permitted here
(defmacro deftransform (name typed-lambda-list &body body)
  (let* ((params (loop for entry in typed-lambda-list
                       collect (if (consp entry) (first entry) entry)))
         (typespecs (loop for entry in typed-lambda-list
                          collect (if (consp entry) (second entry) 't)))
         (insurances
           (mapcar (lambda (param typespec)
                     (if (ctype:top-p typespec *clasp-system*)
                         `(,param ,param)
                         `(,param (ensure-the ,typespec ,param))))
                   params typespecs))
         (csym (gensym "CALL")))
    `(%deftransform ,name (,csym) (,@typespecs)
        (replace-callee-with-lambda ,csym
                                    (cstify-transformer
                                     (bir:origin ,csym)
                                     (list 'lambda '(,@params)
                                           (list 'let
                                                 '(,@insurances)
                                                 '(declare
                                                   (ignorable ,@params))
                                                 (progn ,@body))))))))

(defmacro define-deriver (name deriver-name)
  `(setf (gethash ',name *derivers*) ',deriver-name))

(defmethod bir-transformations:derive-return-type ((inst bir:call) identity
                                                   argstype (system clasp))
  (declare (ignore argstype))
  (let ((deriver (gethash identity *derivers*)))
    (if deriver
        (funcall deriver inst)
        (call-next-method))))

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

(macrolet ((define-two-arg-f (name sf-primop df-primop)
             `(progn
                (deftransform ,name ((a1 single-float) (a2 single-float))
                  '(truly-the single-float (core::primop ,sf-primop a1 a2)))
                (deftransform ,name ((a1 double-float) (a2 double-float))
                  '(truly-the double-float (core::primop ,df-primop a1 a2)))
                (deftransform ,name ((a1 single-float) (a2 double-float))
                  '(truly-the double-float
                    (core::primop ,df-primop
                     (core::primop core::single-to-double a1)
                     a2)))
                (deftransform ,name ((a1 double-float) (a2 single-float))
                  '(truly-the double-float
                    (core::primop ,df-primop
                     a1
                     (core::primop core::single-to-double a2))))))
           (define-two-arg-ff (name sf-primop df-primop)
             `(progn
                (define-two-arg-f ,name ,sf-primop ,df-primop)
                (deftransform ,name ((x fixnum) (y single-float))
                  '(truly-the single-float
                    (core::primop ,sf-primop
                     (core::primop core::fixnum-to-single x) y)))
                (deftransform ,name ((x single-float) (y fixnum))
                  '(truly-the single-float
                    (core::primop ,sf-primop
                     x (core::primop core::fixnum-to-single y))))
                (deftransform ,name ((x fixnum) (y double-float))
                  '(truly-the double-float
                    (core::primop ,df-primop
                     (core::primop core::fixnum-to-double x) y)))
                (deftransform ,name ((x double-float) (y fixnum))
                  '(truly-the double-float
                    (core::primop ,df-primop
                     x (core::primop core::fixnum-to-double y)))))))
  (define-two-arg-ff core:two-arg-+ core::two-arg-sf-+ core::two-arg-df-+)
  (define-two-arg-ff core:two-arg-- core::two-arg-sf-- core::two-arg-df--)
  (define-two-arg-ff core:two-arg-* core::two-arg-sf-* core::two-arg-df-*)
  (define-two-arg-ff core:two-arg-/ core::two-arg-sf-/ core::two-arg-df-/)
  (define-two-arg-f expt           core::sf-expt      core::df-expt))

(deftransform ftruncate ((dividend single-float) (divisor single-float))
  '(cleavir-primop:truly-the (values single-float single-float &rest nil)
    (core::primop core::sf-ftruncate dividend divisor)))
(deftransform ftruncate ((dividend double-float) (divisor double-float))
  '(cleavir-primop:truly-the (values double-float double-float &rest nil)
    (core::primop core::df-truncate dividend divisor)))
;; FIXME: i think our FTRUNCATE function has a bug: it should return doubles in
;; this case, by my reading.
(deftransform ftruncate ((dividend single-float) (divisor double-float))
  '(cleavir-primop:truly-the (values double-float double-float &rest nil)
    (core::primop core::df-ftruncate
     (core::primop core::single-to-double dividend)
     divisor)))
(deftransform ftruncate ((dividend double-float) (divisor single-float))
  '(cleavir-primop:truly-the (values double-float double-float &rest nil)
    (core::primop core::df-ftruncate
     dividend
     (core::primop core::single-to-double divisor))))
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
             `(progn
                (deftransform ,name ((arg single-float))
                  '(truly-the single-float (core::primop ,sf-primop arg)))
                (deftransform ,name ((arg double-float))
                  '(truly-the double-float (core::primop ,df-primop arg))))))
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

(deftransform core:reciprocal ((v single-float))
  '(truly-the single-float (core::two-arg-sf-/ 1f0 v)))
(deftransform core:reciprocal ((v double-float))
  '(truly-the double-float (core::two-arg-df-/ 1d0 v)))

;;; Transform log, but only one-argument log (which can be derived from the
;;; two argument case by the compiler macro in opt-number.lsp)
(deftransform log ((v single-float))
  '(truly-the single-float (core::primop core::sf-log v)))
(deftransform log ((v double-float))
  '(truly-the double-float (core::primop core::df-log v)))

(deftransform float ((v single-float)) 'v)
(deftransform float ((v single-float) (proto single-float)) 'v)
(deftransform float ((v double-float) (proto double-float)) 'v)
(deftransform float ((v single-float) (proto double-float))
  '(truly-the double-float (core::primop core::single-to-double v)))
(deftransform float ((v double-float) (proto single-float))
  '(truly-the single-float (core::primop core::double-to-single v)))
(deftransform float ((v double-float))
  '(truly-the double-float (core::primop core::double-to-single v)))

(deftransform float ((num fixnum) (proto single-float))
  '(truly-the single-float (core::primop core::fixnum-to-single num)))
(deftransform float ((num fixnum))
  '(truly-the single-float (core::primop core::fixnum-to-single num)))
(deftransform float ((num fixnum) (proto double-float))
  '(truly-the double-float (core::primop core::fixnum-to-double num)))

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

(deftransform realpart ((r real)) 'r)
(deftransform imagpart ((r rational)) 0)
;; imagpart of a float is slightly complicated with negative zero
(deftransform conjugate ((r real)) 'r)
(deftransform numerator ((r integer)) 'r)
(deftransform denominator ((r integer)) 1)
(deftransform rational ((r rational)) 'r)
(deftransform rationalize ((r rational)) 'r)

(deftransform equal ((x number) (y number)) '(eql x y))
(deftransform equalp ((x number) (y number)) '(= x y))

;;;

(deftransform equal ((x character) (y character)) '(char= x y))
(deftransform equalp ((x character) (y character)) '(char-equal x y))

;;;

(deftransform aref ((arr (simple-array single-float (*))) i)
  '(truly-the single-float (core::primop core::sf-vref arr i)))
(deftransform aref ((arr (simple-array double-float (*))) i)
  '(truly-the double-float (core::primop core::df-vref arr i)))
(deftransform row-major-aref ((arr (simple-array single-float (*))) i)
  '(truly-the single-float (core::primop core::sf-vref arr i)))
(deftransform row-major-aref ((arr (simple-array double-float (*))) i)
  '(truly-the double-float (core::primop core::df-vref arr i)))

(deftransform core:row-major-aset ((arr (simple-array single-float (*)))
                                   idx value)
  '(truly-the single-float (core::primop core::sf-vset value arr idx)))
(deftransform core:row-major-aset ((arr (simple-array double-float (*)))
                                   idx value)
  '(truly-the double-float (core::primop core::df-vset value arr idx)))

(deftransform (setf aref) (value (arr (simple-array single-float (*))) idx)
  '(truly-the single-float
    (core::primop core::sf-vset value arr idx)))
(deftransform (setf aref) (value (arr (simple-array double-float (*))) idx)
  '(truly-the double-float
    (core::primop core::df-vset value arr idx)))

(deftransform aref ((arr vector) (index t))
  '(row-major-aref arr index))
(deftransform (setf aref) ((val t) (arr vector) (index t))
  '(setf (row-major-aref arr index) val))

(deftransform array-rank ((arr (array * (*)))) 1)
(deftransform array-total-size ((arr (simple-array * (*))))
  '(truly-the valid-array-dimension (core::primop core::vector-length arr)))
(deftransform length ((arr vector))
  '(truly-the valid-array-dimension (core::primop core::vector-length arr)))

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

;;; FIXME: Maybe should be a compiler macro not specializing on fixnum.
;;;        And maybe should use LOGTEST, but I'm not sure what the best way
;;;        to optimize that is yet.
(deftransform evenp ((f fixnum))
  '(zerop (truly-the fixnum (core::primop core::fixnum-logand f 1))))
(deftransform oddp ((f fixnum))
  '(not (zerop (truly-the fixnum (core::primop core::fixnum-logand f 1)))))

(deftransform lognot ((f fixnum))
  '(truly-the fixnum (core::primop core::fixnum-lognot f)))

(macrolet ((deflog2 (name primop)
             `(deftransform ,name ((a fixnum) (b fixnum))
                '(truly-the fixnum (core::primop ,primop a b)))))
  (deflog2 core:logand-2op core::fixnum-logand)
  (deflog2 core:logior-2op core::fixnum-logior)
  (deflog2 core:logxor-2op core::fixnum-logxor))

(deftransform logandc1 ((n fixnum) (b fixnum))
  '(truly-the fixnum (core::primop core::fixnum-logand
                      (core::primop core::fixnum-lognot n) b)))
(deftransform logandc2 ((a fixnum) (n fixnum))
  '(truly-the fixnum (core::primop core::fixnum-logand
                      a (core::primop core::fixnum-lognot n))))
(deftransform logorc1 ((n fixnum) (b fixnum))
  '(truly-the fixnum (core::primop core::fixnum-logior
                      (core::primop core::fixnum-lognot n) b)))
(deftransform logorc2 ((a fixnum) (n fixnum))
  '(truly-the fixnum (core::primop core::fixnum-logior
                      a (core::primop core::fixnum-lognot n))))

(macrolet ((deflog2r (name primop)
             `(deftransform ,name ((a fixnum) (b fixnum))
                '(truly-the fixnum
                  (core::primop core::fixnum-lognot
                   (core::primop ,primop a b))))))
  (deflog2r core:logeqv-2op core::fixnum-logxor)
  (deflog2r lognand core::fixnum-logand)
  (deflog2r lognor core::fixnum-logior))

;;; This is a very KLUDGEy way to find additions of fixnums whose result is
;;; a fixnum as well. Plus it hardcodes the number of fixnum bits. FIXME
(deftransform core:two-arg-+ ((a (signed-byte 60)) (b (signed-byte 60)))
  '(truly-the fixnum (core::primop core::fixnum-add a b)))
(deftransform core:two-arg-- ((a (signed-byte 60)) (b (signed-byte 60)))
  '(truly-the fixnum (core::primop core::fixnum-sub a b)))

;; assuming 2's complement, most-negative-fixnum, uniquely among fixnums,
;; has a bignum negation.
(deftransform core:negate ((n (integer #.(1+ most-negative-fixnum)
                                       #.most-positive-fixnum)))
  '(truly-the fixnum (core::primop core::fixnum-sub 0 n)))

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
  '(truly-the fixnum (core::primop core::fixnum-positive-logcount n)))

;; right shift of a fixnum
(deftransform ash ((int fixnum) (count (integer * 0)))
  '(truly-the fixnum (core::primop core::fixnum-ashr int (min (- count) 63))))

;;;

(deftransform car ((cons cons)) '(cleavir-primop:car cons))
(deftransform cdr ((cons cons)) '(cleavir-primop:cdr cons))

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
