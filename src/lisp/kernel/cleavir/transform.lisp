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

;;; We can't ever inline mv local calls (yet! TODO, should be possible sometimes)
;;; so this is a bit simpler than the above.
(defun replace-mvcallee-with-lambda (call lambda-expression-cst)
  (let ((bir (lambda->birfun (bir:module (bir:function call))
                             lambda-expression-cst)))
    ;; Now properly insert it.
    (change-class call 'bir:mv-local-call
                  :inputs (list* bir (rest (bir:inputs call))))))

(defmacro with-transform-declining (&body body)
  `(catch '%decline-transform ,@body))

(defmacro decline-transform (reason &rest arguments)
  (declare (ignore reason arguments)) ; maybe later
  `(throw '%decline-transform nil))

(defun maybe-transform (call transforms)
  (flet ((arg-primary (arg)
           (ctype:primary (asserted-ctype arg) *clasp-system*)))
    (loop with args = (rest (bir:inputs call))
          with argstype
            = (ctype:values (mapcar #'arg-primary args) nil
                            (ctype:bottom *clasp-system*) *clasp-system*)
          for (transform . vtype) in transforms
          when (ctype:values-subtypep argstype vtype *clasp-system*)
            do (with-transform-declining
                   (replace-callee-with-lambda
                    call (funcall transform :origin (bir:origin call)
                                            :argstype argstype
                                            :policy (bir:policy call)))
                 (return t)))))

(defun transform-values-call-to-ftm (call)
  (change-class call 'bir:fixed-to-multiple
                :inputs (rest (bir:inputs call))))

(defmethod cleavir-bir-transformations:transform-call
    ((system clasp) key (call bir:call))
  ;; FUNKY SPECIAL CASE: If we find calls to VALUES, we replace them
  ;; with FIXED-TO-MULTIPLE directly. KLUDGE.
  (when (eq key 'values)
    (transform-values-call-to-ftm call)
    (return-from cleavir-bir-transformations:transform-call t))
  (let ((trans (gethash key *bir-transformers*)))
    (if trans
        (maybe-transform call trans)
        nil)))

(defmethod cleavir-bir-transformations:transform-call
    ((system clasp) key (call bir:mv-call))
  (let ((transforms (gethash key *bir-transformers*)))
    (if transforms
        (loop with argstype = (bir:ctype (second (bir:inputs call)))
              for (transform . vtype) in transforms
              when (ctype:values-subtypep argstype vtype *clasp-system*)
                do (with-transform-declining
                       (replace-mvcallee-with-lambda
                        call (funcall transform :origin (bir:origin call)
                                                :argstype argstype
                                                :policy (bir:policy call)))
                     (return t)))
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

(defmacro %deftransform (name lambda-list argstype
                         &body body)
  (let ((argstype (env:parse-values-type-specifier argstype nil *clasp-system*)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (unless (nth-value 1 (gethash ',name *bir-transformers*))
         (%deftransformation ,name))
       (%def-bir-transformer ',name (lambda ,lambda-list ,@body) ',argstype)
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

(defmacro with-transformer-types (lambda-list argstype &body body)
  `(with-types ,lambda-list ,argstype
     (:default (decline-transform "type mismatch"))
     ,@body))

;;; A deftransform lambda list is like a method lambda list, except with
;;; types instead of specializers, and &optional and &rest can have types.
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

(defmacro deftransform (name (typed-lambda-list
                              &key (argstype (gensym "ARGSTYPE") argstypep)
                              (policy (gensym "POLICY") policyp))
                        &body body)
  (multiple-value-bind (req opt rest reqt optt restt)
      (process-deftransform-lambda-list typed-lambda-list)
    (assert (or (null restt) (eq restt t))) ; we're limitd at the moment.
    (let* ((ignorable (append req opt (when rest (list rest))))
           (ll `(,@req &optional ,@opt ,@(when rest `(&rest ,rest))))
           (vt `(values ,@reqt &optional ,@optt &rest ,restt))
           (osym (gensym "ORIGIN")) (bodysym (gensym "BODY")))
      `(%deftransform ,name (&key ((:origin ,osym))
                                  ((:argstype ,argstype))
                                  ((:policy ,policy))) ,vt
         ,@(unless argstypep `((declare (ignore ,argstype))))
         ,@(unless policyp `((declare (ignore ,policy))))
         (let ((,bodysym (progn ,@body)))
           (cstify-transformer
            ,osym
            ;; double backquotes carefully designed piece by piece
            `(lambda (,@',ll)
               (declare (ignorable ,@',ignorable))
               ,,bodysym)))))))

(defmacro deftransform-type-predicate (name type)
  `(progn (deftransform ,name (((object ,type))) 't)
          (deftransform ,name (((object (not ,type)))) 'nil)))

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
;;; (4) TYPES AND CLASSES

;;; With constant typespec, fold when able.
;;; TODO? Could expand into semi-constant types, as in
;;; (typep foo `(cons ...))

(deftransform typep (((object t) (tspec t))
                     :argstype args)
  (with-transformer-types (object tspec &optional env) args
    (declare (ignore env))
    (let ((sys *clasp-system*))
      (if (and (ctype:member-p sys tspec)
               (= (length (ctype:member-members sys tspec)) 1))
          (let* ((tspec (first (ctype:member-members sys tspec)))
                 (type (env:parse-type-specifier tspec nil sys)))
            (cond ((ctype:subtypep object type sys) 't)
                  ((ctype:disjointp object type sys) 'nil)
                  (t
                   (decline-transform "TODO")
                   #+(or)
                   (maybe-expand-typep type 'object))))
          (decline-transform "non-constant type specifier")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (5) DATA AND CONTROL FLOW

(deftransform-type-predicate functionp function)

(deftransform-type-predicate compiled-function-p compiled-function)

(deftransform equal (((x number) (y t))) '(eql x y))
(deftransform equal (((x t) (y number))) '(eql x y))
(deftransform equalp (((x number) (y number))) '(= x y))

(deftransform equal (((x character) (y character))) '(char= x y))
(deftransform equalp (((x character) (y character))) '(char-equal x y))

#+(or) ; string= is actually slower atm due to keyword etc processing
(deftransform equal ((x string) (y string)) '(string= x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (10) SYMBOLS

(deftransform-type-predicate symbolp symbol)
(deftransform-type-predicate keywordp keyword)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (11) PACKAGES

(deftransform-type-predicate packagep package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (12) NUMBERS

(deftransform-type-predicate numberp number)
(deftransform-type-predicate complexp complex)
(deftransform-type-predicate realp real)
(deftransform-type-predicate rationalp rational)
(deftransform-type-predicate floatp float)
(deftransform-type-predicate core:single-float-p single-float)
(deftransform-type-predicate core:double-float-p double-float)
(deftransform-type-predicate integerp integer)
(deftransform-type-predicate core:fixnump fixnum)

(deftransform-type-predicate random-state-p random-state)

(macrolet ((define-two-arg-f (name)
             `(progn
                (deftransform ,name (((a1 single-float) (a2 double-float)))
                  '(,name (core:to-double-float a1) a2))
                (deftransform ,name (((a1 double-float) (a2 single-float)))
                  '(,name a1 (core:to-double-float a2)))))
           (define-two-arg-ff (name)
             `(progn
                (define-two-arg-f ,name)
                (deftransform ,name (((x rational) (y single-float)))
                  '(,name (core:to-single-float x) y))
                (deftransform ,name (((x single-float) (y rational)))
                  '(,name x (core:to-single-float y)))
                (deftransform ,name (((x rational) (y double-float)))
                  '(,name (core:to-double-float x) y))
                (deftransform ,name (((x double-float) (y rational)))
                  '(,name x (core:to-double-float y))))))
  (define-two-arg-ff core:two-arg-+)
  (define-two-arg-ff core:two-arg--)
  (define-two-arg-ff core:two-arg-*)
  (define-two-arg-ff core:two-arg-/)
  (define-two-arg-f  expt))

(deftransform expt (((x (eql 2)) (y (integer 0)))) '(ash 1 y))

;; FIXME: i think our FTRUNCATE function has a bug: it should return doubles in
;; this case, by my reading.
(deftransform ftruncate (((dividend single-float) (divisor double-float)))
  '(ftruncate (core:to-double-float dividend) divisor))
(deftransform ftruncate (((dividend double-float) (divisor single-float)))
  '(ftruncate dividend (core:to-single-float divisor)))

(macrolet ((define-float-conditional (name)
             `(progn
                (deftransform ,name (((x single-float) (y double-float)))
                  '(,name (core:to-double-float x) y))
                (deftransform ,name (((x double-float) (y single-float)))
                  '(,name x (core:to-double-float y))))))
  (define-float-conditional core:two-arg-=)
  (define-float-conditional core:two-arg-<)
  (define-float-conditional core:two-arg-<=)
  (define-float-conditional core:two-arg->)
  (define-float-conditional core:two-arg->=))

(deftransform zerop (((n single-float))) '(= n 0f0))
(deftransform plusp (((n single-float))) '(> n 0f0))
(deftransform minusp (((n single-float))) '(< n 0f0))

(deftransform zerop (((n double-float))) '(= n 0d0))
(deftransform plusp (((n double-float))) '(> n 0d0))
(deftransform minusp (((n double-float))) '(< n 0d0))

(macrolet ((define-irratf (name)
             `(deftransform ,name (((arg rational)))
                '(,name (core:to-single-float arg))))
           (define-irratfs (&rest names)
             `(progn ,@(loop for name in names collect `(define-irratf ,name)))))
  (define-irratfs exp cos sin tan cosh sinh tanh asinh sqrt
    ;; Only transform the one-argument case.
    ;; The compiler macro in opt-number.lisp should reduce two-arg to one-arg.
    log
    acos asin acosh atanh))

(deftransform core:reciprocal (((v single-float))) '(/ 1f0 v))
(deftransform core:reciprocal (((v double-float))) '(/ 1d0 v))

(deftransform float (((v float))) 'v)
(deftransform float (((v (not float)))) '(core:to-single-float v))
(deftransform float (((v single-float) (proto single-float))) 'v)
(deftransform float ((v (proto single-float))) '(core:to-single-float v))
(deftransform core:to-single-float (((v single-float))) 'v)
(deftransform float (((v double-float) (proto double-float))) 'v)
(deftransform float ((v (proto double-float))) '(core:to-double-float v))
(deftransform core:to-double-float (((v double-float))) 'v)

;;;

(deftransform realpart (((r real))) 'r)
(deftransform imagpart (((r rational))) 0)
;; imagpart of a float is slightly complicated with negative zero
(deftransform conjugate (((r real))) 'r)
(deftransform numerator (((r integer))) 'r)
(deftransform denominator (((r integer))) 1)
(deftransform rational (((r rational))) 'r)
(deftransform rationalize (((r rational))) 'r)

;;; FIXME: Maybe should be a compiler macro not specializing on fixnum.
;;;        And maybe should use LOGTEST, but I'm not sure what the best way
;;;        to optimize that is yet.
(deftransform evenp (((f fixnum)))
  '(zerop (logand f 1)))
(deftransform oddp (((f fixnum)))
  '(not (zerop (logand f 1))))

(deftransform logandc1 (((n fixnum) (b fixnum))) '(logand (lognot n) b))
(deftransform logandc2 (((a fixnum) (n fixnum))) '(logand a (lognot n)))
(deftransform logorc1 (((n fixnum) (b fixnum))) '(logior (lognot n) b))
(deftransform logorc2 (((a fixnum) (n fixnum))) '(logior a (lognot n)))

(macrolet ((deflog2r (name neg)
             `(deftransform ,name (((a fixnum) (b fixnum))) '(lognot (,neg a b)))))
  (deflog2r core:logeqv-2op core:logxor-2op)
  (deflog2r lognand core:logand-2op)
  (deflog2r lognor core:logior-2op))

(deftransform core:negate (((n fixnum))) '(- 0 n))

(deftransform zerop (((n fixnum))) '(= n 0))
(deftransform plusp (((n fixnum))) '(> n 0))
(deftransform minusp (((n fixnum))) '(< n 0))

;; really obvious case, but it comes up in e.g. (ldb (byte 8 0) ...)
(deftransform ash (((int integer) (count (eql 0)))) 'int)
;; also obvious
(deftransform ash (((int (eql 0)) (count integer))) 'int)

;; transform shifts into direct use of shift-left, shift-right
;; the efficiency gain is negligible, but we can go from there into
;; better optimizations; see bir-to-bmir
(deftransform ash (((int t) (count (integer 0)))) '(core:ash-left int count))
(deftransform ash (((int t) (count (integer * 0))))
  ;; A weakness of meta-evaluate reveals itself here - if we do the more obvious
  ;; (- count), that will be transformed into (- 0 count) by the above, but then
  ;; our transformations may be spent and it will remain there, and we'll do a
  ;; full call to ash-right. Which would be pretty subpar.
  '(core:ash-right int (- 0 count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (13) CHARACTERS

(define-deriver characterp (object)
  (derive-type-predicate object 'character *clasp-system*))

(deftransform standard-char-p (((object standard-char))) 't)
(deftransform standard-char-p (((object (and character (not standard-char)))))
  'nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (14) CONSES

(deftransform consp (((x cons))) 't)
(deftransform consp (((x atom))) 'nil)
(deftransform atom (((o atom))) 't)
(deftransform atom (((o cons))) 'nil)

(deftransform-type-predicate listp list)

(deftransform length (((x null))) 0)
(deftransform length (((x cons))) '(core:cons-length x))
(deftransform length (((x list)))
  `(if (null x)
       0
       (core:cons-length x)))

(deftransform-type-predicate null null)

(deftransform endp (((x (not list))))
  '(error 'type-error :datum x :expected-type 'list))
(deftransform endp (((x list))) '(null x))
;; These two are technically redundant since null has a transform,
;; but we might as well simplify in one shot, no?
(deftransform endp (((x null))) t)
(deftransform endp (((x cons))) nil)
;; If all else fails,
(deftransform endp (((x t)))
  '(null (the (values list &rest nil) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (15) ARRAYS

;;; FIXME: The &key stuff should be integrated into deftransform itself. Easier.
(deftransform make-array (((dimensions (integer 0 #.array-dimension-limit)) &rest keys)
                          :argstype args)
  (with-transformer-types (dimensions &key (element-type (eql t))
                                      (initial-element null iesp)
                                      (initial-contents null icsp)
                                      (adjustable null)
                                      (fill-pointer null)
                                      (displaced-to null)
                                      (displaced-index-offset (eql 0) diosp))
    args
    (declare (ignore dimensions displaced-index-offset
                     initial-element initial-contents))
    (let* ((sys *clasp-system*) (null (ctype:member sys nil)))
      (if (and (ctype:member-p sys element-type)
               (= (length (ctype:member-members sys element-type)) 1)
               (ctype:subtypep adjustable null sys)
               (ctype:subtypep fill-pointer null sys)
               (ctype:subtypep displaced-to null sys)
               (not diosp)
               ;; Handle these later. TODO. For efficiency,
               ;; this will probably mean inlining lambdas with &key.
               ;; Or replacing the make-array with a reqargs-only function,
               ;; more likely.
               (and (null iesp) (null icsp)))
          (let* ((uaet (upgraded-array-element-type
                        (first (ctype:member-members sys element-type))))
                 (make-sv (cmp::uaet-info uaet)))
            `(,make-sv dimensions nil nil))
          (decline-transform "making a complex array")))))

(deftransform aref (((arr vector) (index t))) '(row-major-aref arr index))
(deftransform (setf aref) (((val t) (arr vector) (index t)))
  '(setf (row-major-aref arr index) val))

;;; Move bounds check outside the callee
(defun bounds-check-form (array index policy)
  (if (cleavir-policy:policy-value policy 'core::insert-array-bounds-checks)
      ;; Note that this LENGTH is on a known simple-vector, so it will be inlined.
      `(core:check-bound ,array (length ,array) ,index)
      index))
(defmacro define-vector-transforms (element-type)
  `(progn
     (deftransform aref (((arr (simple-array ,element-type (*))) (index t))
                         :policy policy)
       (list 'core:vref 'arr (bounds-check-form 'arr 'index policy)))
     (deftransform (setf aref) (((val t)
                                 (arr (simple-array ,element-type (*)))
                                 (index t))
                                :policy policy)
       (list 'setf (list 'core:vref 'arr (bounds-check-form 'arr 'index policy))
             (list 'the (list 'values ',element-type '&rest 'nil) 'val)))
     (deftransform row-major-aref (((arr (simple-array ,element-type (*))) (index t))
                                   :policy policy)
       (list 'core:vref 'arr (bounds-check-form 'arr 'index policy)))
     (deftransform (setf row-major-aref) (((val t)
                                           (arr (simple-array ,element-type (*)))
                                           (index t))
                                          :policy policy)
       (list 'setf (list 'core:vref 'arr (bounds-check-form 'arr 'index policy))
             (list 'the (list 'values ',element-type '&rest 'nil) 'val)))))
;;; These are the ones we have underlying primops for at the moment.
;;; Doesn't seem to be worth it otherwise.
(define-vector-transforms t)
(define-vector-transforms single-float)
(define-vector-transforms double-float)
(define-vector-transforms base-char)
(define-vector-transforms character)

(deftransform array-rank (((arr (array * (*))))) 1)
(deftransform array-dimension (((arr (simple-array * (*))) (dimension (eql 0))))
  '(length arr))

(deftransform-type-predicate arrayp array)

(deftransform-type-predicate vectorp vector)
(deftransform vectorp (((o array))) '(eql (array-rank o) 1))

(deftransform-type-predicate bit-vector-p bit-vector)
(deftransform-type-predicate simple-bit-vector-p simple-bit-vector)

(deftransform-type-predicate core:data-vector-p core:abstract-simple-vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (16) STRINGS

(deftransform-type-predicate simple-string-p simple-string)

(deftransform string (((x symbol))) '(symbol-name x))
(deftransform string (((x string))) '(progn x))

(deftransform-type-predicate stringp string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (17) SEQUENCES

;; These transforms are unsafe, as NTH does not signal out-of-bounds.
#+(or)
(progn
(deftransform elt ((seq list) n) '(nth n seq))
(deftransform (setf elt) (value (seq list) n) '(setf (nth n seq) value))
)

(deftransform reverse (((x list))) '(core:list-reverse x))
(deftransform nreverse (((x list))) '(core:list-nreverse x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (18) HASH TABLES

(deftransform-type-predicate hash-table-p hash-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Flushing calls
;;;
;;; Flushing unused calls has to be done carefully even if they are seemingly
;;; side-effect-free, because many (in fact almost all) functions sometimes have
;;; to signal errors. In particular, in safe code, we are required to signal a
;;; program error on argument count mismatch. Beyond that, we have to worry
;;; about things like not deleting arithmetic code that could signal overflow.

(defvar *flushable* (make-hash-table :test #'equal))

(defmethod bir-transformations:flushable-call-p
    ((call bir:abstract-call) identity (system clasp))
  (let ((test (gethash identity *flushable*)))
    (if test (funcall test call) nil)))

(defmacro defflusher (name (call-param) &body body)
  (let ((fname (make-symbol (format nil "~a-FLUSHER" (write-to-string name)))))
    `(progn
       (defun ,fname (,call-param)
         (block ,(core:function-block-name name)
           ;; If we're not on heightened safety, go ahead and flush.
           (unless (policy:policy-value (bir:policy ,call-param)
                                        'flush-safely)
             (return-from ,(core:function-block-name name) t))
           ,@body))
       (setf (gethash ',name *flushable*) ',fname)
       ',name)))

(defun call-argstype (call)
  (etypecase call
    ((or bir:mv-call bir:mv-local-call)
     (bir:ctype (second (bir:inputs call))))
    ((or bir:call bir:local-call)
     (let ((sys *clasp-system*))
       (ctype:values
        (loop for arg in (rest (bir:inputs call))
              collect (ctype:primary (bir:ctype arg) sys))
        nil (ctype:bottom sys) sys)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-flusher-type (type)
    (let ((opt (member '&optional type))
          (rest (member '&rest type))
          (sys *clasp-system*))
      (flet ((parse1 (ty) (env:parse-type-specifier ty nil sys)))
        (ctype:values
         (mapcar #'parse1 (ldiff type (or opt rest)))
         (mapcar #'parse1 (rest (ldiff opt rest)))
         (if rest (parse1 (second rest)) (ctype:bottom sys))
         sys)))))

;;; Flush unless we're in safe code.
(defmacro defflusher-unsafe (name) `(defflusher ,name (call) nil))

(defmacro defflushers-unsafe (&rest names)
  `(progn ,@(loop for name in names collect `(defflusher-unsafe ,name))))

;;; Flush even in safe code unconditionally.
;;; This is only really valid for functions that accept any arguments.
(defmacro defflusher-always (name) `(defflusher ,name (call) t))

;;; Flush iff the arguments are a recognizable subtype of some given type.
;;; The type is specified as the &rest of the macro, and is interpreted strictly,
;;; i.e. without the &optional padding and stuff inserted.
;;; However if no &rest is specified, that's put in automatically.
(defmacro defflusher-type (name &rest type)
  `(defflusher ,name (call)
     (values (ctype:values-subtypep
              (call-argstype call)
              ',(parse-flusher-type type)
              *clasp-system*))))

(defflusher-type special-operator-p symbol)
(defflusher-type constantp t &optional t)

(defflusher-type type-of t)
(defflusher-type type-error-datum type-error)
(defflusher-type type-error-expected-type type-error)

(defflusher-type functionp t)
(defflusher-type compiled-function-p t)
(defflusher-type not t)
(defflusher-type eq t t)
(defflusher-type eql t t)
(defflusher-type equal t t)
(defflusher-type equalp t t)
(defflusher-type identity t)
(defflusher-type complement function)
(defflusher-type constantly t)
(defflusher-always values)

(defflusher-type class-of t)

(defflusher-type copy-structure structure-object)

(defflusher-type symbolp t)
(defflusher-type keywordp t)
(defflusher-type make-symbol string)
(defflusher-type copy-symbol symbol &optional t)
(defflusher-type gensym &optional (or string (integer 0)))
(defflusher-type symbol-name symbol)
(defflusher-type symbol-package symbol)
(defflusher-type symbol-plist symbol)
(defflusher-type boundp symbol)

;;; TODO: Some of this might be okay to flush in safe code.
;;; But I haven't thought hard about it, so no flushing in safe code for now.
(defflushers-unsafe = /= < > <= >= max min minusp plusp zerop
  floor ffloor ceiling fceiling truncate ftruncate round fround
  sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh
  * + - / core:two-arg-* core:two-arg-+ core:two-arg-- core:two-arg-/
  core:negate core:reciprocal 1+ 1- abs evenp oddp exp expt gcd lcm
  log mod rem signum sqrt isqrt cis conjugate phase realpart imagpart
  upgraded-complex-part-type numerator denominator rational rationalize
  ash integer-length parse-integer boole logand logandc1 logandc2
  logeqv logior lognand lognor lognot logorc1 logorc2 logxor
  logbitp logcount logtest byte byte-size byte-position
  deposit-byte dpb ldb ldb-test mask-field
  decode-float scale-float float-radix float-sign float-digits
  float-precision integer-decode-float)
(defflusher-type make-random-state &optional (or random-state null (eql t)))
(defflusher-type random-state-p t)
(defflusher-type numberp t)
(defflusher-type complexp t)
(defflusher-type complex real &optional real)
(defflusher-type realp t)
(defflusher-type rationalp t)
(defflusher-type integerp t)
(defflusher-type floatp t)
(defflusher-type arithmetic-error-operands arithmetic-error)
(defflusher-type arithmetic-error-operation arithmetic-error)

(macrolet ((defchars (&rest names)
             `(progn
                ,@(loop for name in names
                        collect `(defflusher-type ,name &rest character)))))
  (defchars char= char/= char< char> char<= char>=
    char-equal char-not-equal char-lessp char-greaterp
    char-not-greaterp char-not-lessp))
(defflusher-type characterp t)
(defflusher-type alpha-char-p character)
(defflusher-type alphanumericp character)
(defflusher-type digit-char (integer 0) &optional (integer 2 36))
(defflusher-type digit-char-p character &optional (integer 2 36))
(defflusher-type graphic-char-p character)
(defflusher-type standard-char-p character)
(defflusher-type char-upcase character)
(defflusher-type char-downcase character)
(defflusher-type upper-case-p character)
(defflusher-type lower-case-p character)
(defflusher-type both-case-p character)
(defflusher-type char-code character)
(defflusher-type char-int character)
(defflusher-type code-char (integer 0 (#.char-code-limit)))
(defflusher-type char-name character)
(defflusher-type name-char string)

(defflusher-type cons t t)
(defflusher-type consp t)
(defflusher-type atom t)
(defflusher-type car list)
(defflusher-type cdr list)
(defflusher-type first list)
(defflusher-type rest list)
(defflusher-type listp t)
(defflusher-type endp list)
(defflusher-type null t)
(defflusher-always list)

(defflusher-type array-dimensions array)
(defflusher-type array-element-type array)
(defflusher-type array-has-fill-pointer-p array)
(defflusher-type array-displacement array)
(defflusher-type array-rank array)
(defflusher-type arrayp t)
(defflusher-type simple-vector-p t)
(defflusher-always vector)
(defflusher-type vectorp t)
(defflusher-type bit-vector-p t)
(defflusher-type simple-bit-vector-p t)

(defflusher-type simple-string-p t)
(defflusher-type stringp t)

(defflusher-type hash-table-p t)
(defflusher-type hash-table-count hash-table)
(defflusher-type hash-table-rehash-size hash-table)
(defflusher-type hash-table-rehash-threshold hash-table)
(defflusher-type hash-table-size hash-table)
(defflusher-type hash-table-test hash-table)
(defflusher-type gethash t hash-table &optional t)
(defflusher-type sxhash t)

(defflusher-type pathnamep t)
