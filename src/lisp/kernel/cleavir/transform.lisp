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

(defun lambda->birfun (module lambda-expression-cst)
  (let* (;; FIXME: We should be harsher with errors than cst->ast is here,
         ;; since deftransforms are part of the compiler, and not the
         ;; user's fault.
         (ast (cst->ast lambda-expression-cst))
         (bir (cleavir-ast-to-bir:compile-into-module ast module
                                                      *clasp-system*)))
    ;; Run the first few transformations.
    ;; FIXME: Use a pass manager/reoptimize flags/something smarter.
    (bir-transformations:eliminate-come-froms bir)
    (bir-transformations:find-module-local-calls module)
    (bir-transformations:function-optimize-variables bir)
    bir))

(defun replace-callee-with-lambda (call lambda-expression-cst)
  (let ((bir (lambda->birfun (bir:module (bir:function call))
                             lambda-expression-cst)))
    ;; Now properly insert it.
    (change-class call 'bir:local-call
                  :inputs (list* bir (rest (bir:inputs call))))
    ;; KLUDGEish: maybe-interpolate misbehaves when the flow order is invalid.
    ;; See #1260.
    (bir:compute-iblock-flow-order (bir:function call))
    (bir-transformations:maybe-interpolate bir)))

(defmacro with-transform-declining (&body body)
  `(catch '%decline-transform ,@body))

(defmacro decline-transform (reason &rest arguments)
  (declare (ignore reason arguments)) ; maybe later
  `(throw '%decline-transform nil))

(defun maybe-transform (call transforms)
  (flet ((arg-primary (arg)
           (ctype:primary (bir:asserted-type arg) *clasp-system*)))
    (loop with args = (rest (bir:inputs call))
          with argstype
            = (ctype:values (mapcar #'arg-primary args) nil
                            (ctype:bottom *clasp-system*) *clasp-system*)
          for (transform . vtype) in transforms
          when (ctype:values-subtypep argstype vtype *clasp-system*)
            do (with-transform-declining
                   (replace-callee-with-lambda call
                                               (funcall transform call))
                 (return t)))))

(defmethod cleavir-bir-transformations:transform-call
    ((system clasp) key (call bir:call))
  (let ((trans (gethash key *bir-transformers*)))
    (if trans
        (maybe-transform call trans)
        nil)))

(define-condition failed-transform (ext:compiler-note)
  ((%call :initarg :call :reader failed-transform-call)
   (%opname :initarg :opname :reader failed-transform-opname)
   ;; A list of transform "criteria". For now, a criterion is just the list
   ;; of types a transform can require. In the future there may be other
   ;; criteria, such as being a constant.
   (%available :initarg :available :reader failed-transform-available))
  (:report (lambda (condition stream)
             (format stream "Unable to optimize call to ~s:
The compiler only knows the arguments to be of types ~a.
Optimizations are available for any of:
~{~s~%~}"
                     (failed-transform-opname condition)
                     (loop with call = (failed-transform-call condition)
                           with sys = *clasp-system*
                           for arg in (rest (bir:inputs call))
                           for vtype = (asserted-ctype arg)
                           for svtype = (ctype:primary vtype sys)
                           collect svtype)
                     (mapcar #'cdr (failed-transform-available condition))))))

;;; Note a missed optimization to the programmer.
;;; called in translate, not here, since transform-call may be called
;;; multiple times during meta-evaluation.
(defun maybe-note-failed-transforms (call)
  (when (policy:policy-value (bir:policy call) 'note-untransformed-calls)
    (let ((identities (cleavir-attributes:identities (bir:attributes call))))
      (dolist (id identities)
        (let ((trans (gethash id *bir-transformers*)))
          (when trans
            (cmp:note 'failed-transform
                      :call call :opname id :available trans
                      :origin (origin-source (bir:origin call)))))))))

(defmacro %deftransformation (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *bir-transformers*) nil)
     (setf (gethash ',name *fn-transforms*) '(,name))
     ',name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vtype= (vtype1 vtype2)
    (and (ctype:values-subtypep vtype1 vtype2 *clasp-system*)
         (ctype:values-subtypep vtype2 vtype1 *clasp-system*)))
  (defun vtype< (vtype1 vtype2)
    (and (ctype:values-subtypep vtype1 vtype2 *clasp-system*)
         ;; This also includes NIL NIL, but that probably won't happen
         ;; if the first subtypep returns true
         (not (ctype:values-subtypep vtype2 vtype1 *clasp-system*))))
  (defun %def-bir-transformer (name function argstype)
    ;; We just use a reverse alist (function . argstype).
    (let* ((transformers (gethash name *bir-transformers*))
           (existing (rassoc argstype transformers :test #'vtype=)))
      (if existing
          ;; replace
          (setf (car existing) function)
          ;; Merge in, respecting subtypep
          (setf (gethash name *bir-transformers*)
                (merge 'list (list (cons function argstype))
                       (gethash name *bir-transformers*)
                        #'vtype< :key #'cdr))))))

(defmacro %deftransform (name (instparam) argstype
                         &body body)
  (let ((argstype (env:parse-values-type-specifier argstype nil *clasp-system*)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (unless (nth-value 1 (gethash ',name *bir-transformers*))
         (%deftransformation ,name))
       (%def-bir-transformer ',name (lambda (,instparam) ,@body) ',argstype)
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

;;; A deftransform lambda list is like a method lambda list, except with
;;; types instead of specializers, and &optional and &rest can have types.
;;; And there's no &key yet.
;;; &optional parameters can be specified as ((var type) default var-p).
;;; This function returns six values: Three for the required, optional, and
;;; rest parts of the lambda list, and three for the corresponding types.
;;; This function returns two values: An ordinary lambda list and an
;;; unparsed values type representing the arguments.
(defun process-deftransform-lambda-list (lambda-list)
  (loop with state = :required
        with sys = *clasp-system*
        with reqparams = nil
        with optparams = nil
        with restparam = nil
        with reqtypes = nil
        with opttypes = nil
        with resttype = nil
        for item in lambda-list
        do (cond ((member item '(&optional &rest))
                  (assert (or (eq state :required)
                              (and (eq item '&rest) (eq state '&optional))))
                  (setf state item))
                 ((eq state :required)
                  (cond ((listp item)
                         (push (first item) reqparams)
                         (push (second item) reqtypes))
                        (t (push item reqparams)
                           (push 't reqtypes))))
                 ((eq state '&optional)
                  (cond ((and (listp item) (listp (first item)))
                         (push (list (caar item) (second item) (third item))
                               optparams)
                         (push (cadar item) opttypes))
                        (t (push item optparams)
                           (push t opttypes))))
                 ((eq state '&rest)
                  (cond ((listp item)
                         (setf restparam (first item))
                         (setf resttype (second item)))
                        (t (setf restparam item) (setf resttype 't)))
                  (setf state :done))
                 ((eq state :done) (error "Bad deftransform ll ~a" lambda-list)))
        finally (return (values (nreverse reqparams) (nreverse optparams)
                                restparam
                                (nreverse reqtypes) (nreverse opttypes)
                                resttype))))

;;; FIXME: Only required parameters permitted here
(defmacro deftransform (name typed-lambda-list &body body)
  (multiple-value-bind (req opt rest reqt optt restt)
      (process-deftransform-lambda-list typed-lambda-list)
    (assert (or (null restt) (eq restt t))) ; we're limitd at the moment.
    (let* ((ignorable (append req opt (when rest (list rest))))
           (ll `(,@req &optional ,@opt ,@(when rest `((&rest ,rest)))))
           (vt `(values ,@reqt &optional ,@optt &rest ,restt))
           (insurances
             (flet ((insurance (param typespec)
                      (if (ctype:top-p typespec *clasp-system*)
                          `(,param ,param)
                          `(,param (ensure-the ,typespec ,param)))))
               (append (mapcar #'insurance req reqt)
                       (mapcar #'insurance opt optt)
                       (when rest (list `(,rest ,rest))))))
           (csym (gensym "CALL")) (bodysym (gensym "BODY")))
      `(%deftransform ,name (,csym) ,vt
         (let ((,bodysym (progn ,@body)))
           (cstify-transformer
            (bir:origin ,csym)
            ;; double backquotes carefully designed piece by piece
            (if (policy:policy-value (bir:policy ,csym)
                                     'insert-minimum-type-checks)
                `(lambda (,@',ll)
                   (let (,@',insurances)
                     (declare (ignorable ,@',ignorable))
                     ,,bodysym))
                `(lambda (,@',ll)
                   (declare (ignorable ,@',ignorable))
                   ,,bodysym))))))))

;;;

(defmethod cleavir-bir-transformations:generate-type-check-function
    ((module bir:module) origin ctype (system clasp))
  (lambda->birfun module
                  (cstify-transformer origin
                                      `(lambda (&optional v &rest ign)
                                         (declare (ignore ign))
                                         (if (typep v ',(discrimination-type
                                                         ctype))
                                             v
                                             (error 'type-error
                                                    :datum v
                                                    :expected-type ',ctype))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (5) DATA AND CONTROL FLOW

(deftransform equal ((x number) (y t)) '(eql x y))
(deftransform equal ((x t) (y number)) '(eql x y))
(deftransform equalp ((x number) (y number)) '(= x y))

(deftransform equal ((x character) (y character)) '(char= x y))
(deftransform equalp ((x character) (y character)) '(char-equal x y))

#+(or) ; string= is actually slower atm due to keyword etc processing
(deftransform equal ((x string) (y string)) '(string= x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (12) NUMBERS

(macrolet ((define-two-arg-f (name sf-primop df-primop)
             `(progn
                (deftransform ,name ((a1 single-float) (a2 single-float))
                  '(core::primop ,sf-primop a1 a2))
                (deftransform ,name ((a1 double-float) (a2 double-float))
                  '(core::primop ,df-primop a1 a2))
                (deftransform ,name ((a1 single-float) (a2 double-float))
                  '(core::primop ,df-primop
                    (core::primop core::single-to-double a1)
                    a2))
                (deftransform ,name ((a1 double-float) (a2 single-float))
                  '(core::primop ,df-primop
                    a1
                    (core::primop core::single-to-double a2)))))
           (define-two-arg-ff (name sf-primop df-primop)
             `(progn
                (define-two-arg-f ,name ,sf-primop ,df-primop)
                (deftransform ,name ((x fixnum) (y single-float))
                  '(core::primop ,sf-primop
                    (core::primop core::fixnum-to-single x) y))
                (deftransform ,name ((x single-float) (y fixnum))
                  '(core::primop ,sf-primop
                     (core::primop core::fixnum-to-single y)))
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

(deftransform ftruncate ((dividend single-float) (divisor single-float))
  '(core::primop core::sf-ftruncate dividend divisor))
(deftransform ftruncate ((dividend double-float) (divisor double-float))
  '(core::primop core::df-truncate dividend divisor))
;; FIXME: i think our FTRUNCATE function has a bug: it should return doubles in
;; this case, by my reading.
(deftransform ftruncate ((dividend single-float) (divisor double-float))
  '(core::primop core::df-ftruncate
    (core::primop core::single-to-double dividend)
    divisor))
(deftransform ftruncate ((dividend double-float) (divisor single-float))
  '(core::primop core::df-ftruncate
    dividend
    (core::primop core::single-to-double divisor)))
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

(macrolet ((define-irratf (name sf-primop df-primop)
             `(progn
                (deftransform ,name ((arg single-float))
                  '(core::primop ,sf-primop arg))
                (deftransform ,name ((arg double-float))
                  '(core::primop ,df-primop arg))
                (deftransform ,name ((arg fixnum))
                  '(core::primop ,sf-primop
                    (core::primop core::fixnum-to-single arg))))))
  (define-irratf exp         core::sf-exp    core::df-exp)
  (define-irratf cos         core::sf-cos    core::df-cos)
  (define-irratf sin         core::sf-sin    core::df-sin)
  (define-irratf tan         core::sf-tan    core::df-tan)
  (define-irratf cosh        core::sf-cosh   core::df-cosh)
  (define-irratf sinh        core::sf-sinh   core::df-sinh)
  (define-irratf tanh        core::sf-tanh   core::df-tanh)
  (define-irratf asinh       core::sf-asinh  core::df-asinh))

(deftransform sqrt ((arg (single-float 0f0)))
  '(core::primop core::sf-sqrt arg))
(deftransform sqrt ((arg (double-float 0d0)))
  '(core::primop core::df-sqrt arg))
(deftransform sqrt ((arg (integer 0 #.most-positive-fixnum)))
  '(core::primop core::sf-sqrt
    (core::primop core::fixnum-to-single arg)))

;; Only transform the one-argument case.
;; The compiler macro in opt-number.lisp should reduce two-arg to one-arg.
(deftransform log ((arg (single-float (0f0))))
  '(core::primop core::sf-log arg))
(deftransform log ((arg (double-float (0d0))))
  '(core::primop core::df-log arg))
(deftransform log ((arg (integer 1 #.most-positive-fixnum)))
  '(core::primop core::sf-log
    (core::primop core::fixnum-to-single arg)))

(deftransform acos ((arg (single-float -1f0 1f0)))
  '(core::primop core::sf-acos arg))
(deftransform acos ((arg (double-float -1d0 1d0)))
  '(core::primop core::df-acos arg))
;; Don't bother with fixnums in such a small range

(deftransform asin ((arg (single-float -1f0 1f0)))
  '(core::primop core::sf-asin arg))
(deftransform asin ((arg (double-float -1d0 1d0)))
  '(core::primop core::df-asin arg))

(deftransform acosh ((arg (single-float 1f0)))
  '(core::primop core::sf-acosh arg))
(deftransform acosh ((arg (double-float 1d0)))
  '(core::primop core::df-acosh arg))
(deftransform acosh ((arg (integer 1 #.most-positive-fixnum)))
  '(core::primop core::sf-acosh
    (core::primop core::fixnum-to-single arg)))

(deftransform atanh ((arg (single-float (-1f0) (1f0))))
  '(core::primop core::sf-atanh arg))
(deftransform atanh ((arg (double-float (-1d0) (1d0))))
  '(core::primop core::df-atanh arg))

(deftransform abs ((arg single-float))
  '(core::primop core::sf-abs arg))
(deftransform abs ((arg double-float))
  '(core::primop core::df-abs arg))

(deftransform core:negate ((arg single-float))
  '(core::primop core::sf-negate arg))
(deftransform core:negate ((arg double-float))
  '(core::primop core::df-negate arg))

(deftransform core:reciprocal ((v single-float))
  '(core::two-arg-sf-/ 1f0 v))
(deftransform core:reciprocal ((v double-float))
  '(core::two-arg-df-/ 1d0 v))

(deftransform float ((v single-float)) 'v)
(deftransform float ((v single-float) (proto single-float)) 'v)
(deftransform float ((v double-float) (proto double-float)) 'v)
(deftransform float ((v single-float) (proto double-float))
  '(core::primop core::single-to-double v))
(deftransform float ((v double-float) (proto single-float))
  '(core::primop core::double-to-single v))
(deftransform float ((v double-float))
  '(core::primop core::double-to-single v))

(deftransform float ((num fixnum) (proto single-float))
  '(core::primop core::fixnum-to-single num))
(deftransform float ((num fixnum))
  '(core::primop core::fixnum-to-single num))
(deftransform float ((num fixnum) (proto double-float))
  '(core::primop core::fixnum-to-double num))

;;;

(deftransform realpart ((r real)) 'r)
(deftransform imagpart ((r rational)) 0)
;; imagpart of a float is slightly complicated with negative zero
(deftransform conjugate ((r real)) 'r)
(deftransform numerator ((r integer)) 'r)
(deftransform denominator ((r integer)) 1)
(deftransform rational ((r rational)) 'r)
(deftransform rationalize ((r rational)) 'r)

;;; FIXME: Maybe should be a compiler macro not specializing on fixnum.
;;;        And maybe should use LOGTEST, but I'm not sure what the best way
;;;        to optimize that is yet.
(deftransform evenp ((f fixnum))
  '(zerop (truly-the fixnum (core::primop core::fixnum-logand f 1))))
(deftransform oddp ((f fixnum))
  '(not (zerop (truly-the fixnum (core::primop core::fixnum-logand f 1)))))

(deftransform lognot ((f fixnum))
  '(core::primop core::fixnum-lognot f))

(macrolet ((deflog2 (name primop)
             `(deftransform ,name ((a fixnum) (b fixnum))
                '(core::primop ,primop a b))))
  (deflog2 core:logand-2op core::fixnum-logand)
  (deflog2 core:logior-2op core::fixnum-logior)
  (deflog2 core:logxor-2op core::fixnum-logxor))

(deftransform logandc1 ((n fixnum) (b fixnum))
  '(core::primop core::fixnum-logand
    (core::primop core::fixnum-lognot n) b))
(deftransform logandc2 ((a fixnum) (n fixnum))
  '(core::primop core::fixnum-logand
    a (core::primop core::fixnum-lognot n)))
(deftransform logorc1 ((n fixnum) (b fixnum))
  '(core::primop core::fixnum-logior
    (core::primop core::fixnum-lognot n) b))
(deftransform logorc2 ((a fixnum) (n fixnum))
  '(core::primop core::fixnum-logior
    a (core::primop core::fixnum-lognot n)))

(macrolet ((deflog2r (name primop)
             `(deftransform ,name ((a fixnum) (b fixnum))
                '(core::primop core::fixnum-lognot
                  (core::primop ,primop a b)))))
  (deflog2r core:logeqv-2op core::fixnum-logxor)
  (deflog2r lognand core::fixnum-logand)
  (deflog2r lognor core::fixnum-logior))

;;; This is a very KLUDGEy way to find additions of fixnums whose result is
;;; a fixnum as well. Plus it hardcodes the number of fixnum bits. FIXME
(deftransform core:two-arg-+ ((a (signed-byte 60)) (b (signed-byte 60)))
  '(core::primop core::fixnum-add a b))
(deftransform core:two-arg-- ((a (signed-byte 60)) (b (signed-byte 60)))
  '(core::primop core::fixnum-sub a b))

;; assuming 2's complement, most-negative-fixnum, uniquely among fixnums,
;; has a bignum negation.
(deftransform core:negate ((n (integer #.(1+ most-negative-fixnum)
                                       #.most-positive-fixnum)))
  '(core::primop core::fixnum-sub 0 n))

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

;; right shift of a fixnum
(deftransform ash ((int fixnum) (count (integer * 0)))
  '(core::primop core::fixnum-ashr int (min (- count) 63)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (14) CONSES

(deftransform car ((cons cons)) '(cleavir-primop:car cons))
(deftransform cdr ((cons cons)) '(cleavir-primop:cdr cons))

(deftransform length ((x null)) 0)
(deftransform length ((x cons)) '(core:cons-length x))
(deftransform length ((x list))
  `(if (null x)
       0
       (core:cons-length x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (15) ARRAYS

(deftransform aref ((arr (simple-array single-float (*))) i)
  '(core::primop core::sf-vref arr i))
(deftransform aref ((arr (simple-array double-float (*))) i)
  '(core::primop core::df-vref arr i))
(deftransform row-major-aref ((arr (simple-array single-float (*))) i)
  '(core::primop core::sf-vref arr i))
(deftransform row-major-aref ((arr (simple-array double-float (*))) i)
  '(core::primop core::df-vref arr i))

(deftransform core:row-major-aset ((arr (simple-array single-float (*)))
                                   idx value)
  '(core::primop core::sf-vset value arr idx))
(deftransform core:row-major-aset ((arr (simple-array double-float (*)))
                                   idx value)
  '(core::primop core::df-vset value arr idx))

(deftransform (setf aref) (value (arr (simple-array single-float (*))) idx)
  '(core::primop core::sf-vset value arr idx))
(deftransform (setf aref) (value (arr (simple-array double-float (*))) idx)
  '(core::primop core::df-vset value arr idx))

(deftransform aref ((arr vector) (index t))
  '(row-major-aref arr index))
(deftransform (setf aref) ((val t) (arr vector) (index t))
  '(setf (row-major-aref arr index) val))

(deftransform array-rank ((arr (array * (*)))) 1)
(deftransform array-total-size ((arr (simple-array * (*))))
  '(core::primop core::vector-length arr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (16) STRINGS

(deftransform string ((x symbol)) '(symbol-name x))
(deftransform string ((x string)) '(progn x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (17) SEQUENCES

;; These transforms are unsafe, as NTH does not signal out-of-bounds.
#+(or)
(progn
(deftransform elt ((seq list) n) '(nth n seq))
(deftransform core:setf-elt ((seq list) n value) '(setf (nth n seq) value))
)

(deftransform length ((arr vector))
  '(core::primop core::vector-length arr))

(deftransform reverse ((x list)) '(core:list-reverse x))
(deftransform nreverse ((x list)) '(core:list-nreverse x))
