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
  
  (defun function-type-result (type &optional env)
    ;; type is any type specifier
    (multiple-value-bind (head args) (core::normalize-type type env)
      (if (eq head 'function)
          (let ((result (second args)))
            (cond ((null result) 't)
                  ((and (consp result) (eq (car result) 'values))
                   (values-type-primary result))
                  (t result)))
          't)))

  ;;; Given a type, find what type (aref (the TYPE ...) ...) is.
  (defun array-type-element-type (type env)
    (let ((types
            (loop for et in core::+upgraded-array-element-types+
                  ;; Exclude any element type that is CERTAINLY not included.
                  unless (subtypep `(and (array ,et) ,type) nil env)
                    collect et)))
      ;; Now simplify a bit for common stupid cases
      (if (member t types)
          't
          (cons 'or (remove nil types)))))

  ;;; Because some transforms relying on type information are unsafe,
  ;;; we ignore type declarations unless the TRUST-TYPE-DECLARATIONS policy
  ;;; is in place (at low SAFETY). See policy.lisp.
  (defun form-type (form env)
    (let ((trust-type-decls-p
            (environment-has-policy-p env 'ext:assume-right-type)))
      (cond ((constantp form env)
             `(eql ,(ext:constant-form-value form env)))
            ((consp form)
             (let ((operator (first form)))
               (if (symbolp operator)
                   (case operator
                     ((the)
                      (if trust-type-decls-p
                          (let ((type (second form)) (form (third form)))
                            `(and ,(if (and (consp type) (eq (first type) 'values))
                                       (values-type-primary type)
                                       type)
                                  ,(form-type form env)))
                          't))
                     ((function)
                      (if (and (symbolp (second form)) trust-type-decls-p)
                          (let ((info (cleavir-env:function-info
                                       clasp-cleavir:*clasp-system*
                                       env (second form))))
                            (if (typep info '(or cleavir-env:local-function-info
                                              cleavir-env:global-function-info))
                                (cleavir-env:type info)
                                'function))
                          'function))
                     ;; This could be expanded.
                     ((lambda) 'function)
                     ;; This is really KLUDGEy, but then this whole thing kind of is.
                     ((aref)
                      (if (and (consp form) (consp (cdr form)))
                          (let* ((array-form (second form))
                                 (array-form-type (form-type array-form env)))
                            (array-type-element-type array-form-type env))
                          ;; malformed
                          't))
                     (otherwise
                      (if trust-type-decls-p
                          (let ((info (cleavir-env:function-info
                                       clasp-cleavir:*clasp-system*
                                       env operator)))
                            (if (typep info '(or cleavir-env:local-function-info
                                              cleavir-env:global-function-info))
                                (function-type-result (cleavir-env:type info) env)
                                't))
                          't)))
                   't)))
            (t ; symbol (everything else covered by constantp and consp)
             (if trust-type-decls-p
                 (let ((info (cleavir-env:variable-info
                              clasp-cleavir:*clasp-system* env form)))
                   (if info
                       (cleavir-env:type info)
                       't))
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

(deftransform rplaca ((cons cons) value)
  '(lambda (cons value)
    (cleavir-primop:rplaca cons value)
    cons))
(deftransform rplacd ((cons cons) value)
  '(lambda (cons value)
    (cleavir-primop:rplacd cons value)
    cons))

(deftransform primop:inlined-two-arg-+ ((x fixnum) (y fixnum))
  'core:two-arg-+-fixnum-fixnum)

#+(or)
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
  (def-float-compare primop:inlined-two-arg-<  cleavir-primop:float-less)
  (def-float-compare primop:inlined-two-arg-<= cleavir-primop:float-not-greater)
  (def-float-compare primop:inlined-two-arg-=  cleavir-primop:float-equal)
  (def-float-compare primop:inlined-two-arg->  cleavir-primop:float-greater)
  (def-float-compare primop:inlined-two-arg->= cleavir-primop:float-not-less))

#+(or)
(macrolet ((def-fixnum-compare (name op)
             `(deftransform ,name ((x fixnum) (y fixnum))
                '(lambda (x y) (if (,op x y) t nil)))))
  (def-fixnum-compare primop:inlined-two-arg-<  cleavir-primop:fixnum-less)
  (def-fixnum-compare primop:inlined-two-arg-<= cleavir-primop:fixnum-not-greater)
  (def-fixnum-compare primop:inlined-two-arg-=  cleavir-primop:fixnum-equal)
  (def-fixnum-compare primop:inlined-two-arg->  cleavir-primop:fixnum-greater)
  (def-fixnum-compare primop:inlined-two-arg->= cleavir-primop:fixnum-not-less))

#+(or)
(deftransform minusp ((number fixnum))
  '(lambda (n) (if (cleavir-primop:fixnum-less n 0) t nil)))
#+(or)
(deftransform plusp ((number fixnum))
  '(lambda (n) (if (cleavir-primop:fixnum-greater n 0) t nil)))

(deftransform array-total-size ((a (simple-array * (*)))) 'core::vector-length)
;;(deftransform array-total-size ((a core:mdarray)) 'core::%array-total-size)

(deftransform array-rank ((a (simple-array * (*)))) '(lambda (a) (declare (ignore a)) 1))

#+(or)
(deftransform svref/no-bounds-check ((a simple-vector) (index fixnum))
  '(lambda (vector index) (cleavir-primop:aref vector index t t t)))
#+(or)
(deftransform (setf svref/no-bounds-check) (value (a simple-vector) (index fixnum))
  '(lambda (value vector index)
    (cleavir-primop:aset vector index value t t t)
    value))

(deftransform length ((s list)) '(lambda (x) (if x (core:cons-length x) 0)))
(deftransform length ((s vector)) 'core::vector-length)

#+(or)
(deftransform elt ((s vector) index) 'vector-read)
#+(or)
(deftransform core:setf-elt (value (s vector) index)
  '(lambda (value sequence index) (vector-set sequence index new-value)))

(deftransform core:coerce-fdesignator ((fd symbol)) 'fdefinition)
(deftransform core:coerce-fdesignator ((fd function)) 'identity)

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *bir-transformers* (make-hash-table :test #'equal)))

(defmacro define-bir-transformation (name)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *bir-transformers*) nil)
       (setf (gethash ',name *fn-transforms*)
             (list
              (lambda (call)
                (maybe-bir-transform call
                                     (gethash ',name *bir-transformers*))))))
     ',name))

(defmacro define-bir-transform (name (instparam) (&rest param-types)
                                &body body)
  (let ((param-types
          (loop for ty in param-types
                collect (cleavir-env:parse-type-specifier
                         ty nil *clasp-system*))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (unless (nth-value 1 (gethash ',name *bir-transformers*))
         (define-bir-transformation ,name))
       (pushnew (list* (lambda (,instparam) ,@body) '(,@param-types))
                (gethash ',name *bir-transformers*)
                :key #'cdr
                ;; The test is type equality. equalp is... not really there,
                ;; but should mostly work.
                :test #'equalp)
       ',name)))

(defun arg-subtypep (arg ctype)
  (cleavir-ctype:subtypep (cleavir-ctype:primary (bir:ctype arg) *clasp-system*)
                          ctype *clasp-system*))

(defun maybe-bir-transform (call transforms)
  (loop with args = (rest (bir:inputs call))
        with nargs = (length args)
        for (transform . types) in transforms
        when (and (= (length types) nargs) (every #'arg-subtypep args types))
          do (funcall transform call)
          and return t))

(defun replace-call-with-vprimop (call primop-name)
  (change-class call 'cleavir-bir:vprimop
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
  (wrap-in-thei inst (cleavir-ctype:single-value ctype *clasp-system*))
  (replace-call-with-vprimop inst primop)
  t)

(defun wrap-coerce-sf-to-df (inst datum)
  (let* ((df (cleavir-ctype:single-value
              (cleavir-ctype:range 'double-float '* '* *clasp-system*)
              *clasp-system*))
         (new (make-instance 'bir:output
                :derived-type df))
         (coerce (make-instance 'bir:vprimop
                   :origin (bir:origin inst) :policy (bir:policy inst)
                   :info (cleavir-primop-info:info 'core::single-to-double)
                   :outputs (list new))))
    (bir:insert-instruction-before coerce inst)
    (bir:replace-uses new datum)
    (setf (bir:inputs coerce) (list datum)))
  (values))

;;; This is basically a massive KLUDGE.
;;; The deal is, we essentially want to replace (= x y) with
;;; (if (primitive-float-= x y) t nil). meta evaluate can then collapse that
;;; in the common (if (= ...) ...) case. We have to construct the IR ourselves
;;; because there's no good mechanism to do so, and that's verbose.
;;; FIXME: Two things to do here: One, allow introducing a new function into a
;;; module and then inlining it - then we can just keep the IR of
;;; (lambda (x y) (if (primitive-float-= x y) t nil)) around and copy it into
;;; wherever - but that also requires a copier. Second, maybe think about
;;; an IR assembler syntax.

(defun replace-with-test-primop (call primop-name)
  (multiple-value-bind (fore aft) (bir:split-block-after call)
    (let* ((args (rest (bir:inputs call)))
           (boolt
             (cleavir-ctype:single-value
              (cleavir-ctype:member *clasp-system* t nil)
              *clasp-system*))
           (phi (make-instance 'bir:phi
                  :name primop-name :iblock aft :derived-type boolt))
           (dynenv (bir:dynamic-environment fore))
           (function (bir:function fore))
           (module (bir:module function))
           (true (bir:constant-in-module t module))
           (false (bir:constant-in-module nil module))
           (truet (cleavir-ctype:single-value
                   (cleavir-ctype:member *clasp-system* t)
                   *clasp-system*))
           (falset (cleavir-ctype:single-value
                    (cleavir-ctype:member *clasp-system* nil)
                    *clasp-system*))
           (truev (make-instance 'bir:output :derived-type truet))
           (falsev (make-instance 'bir:output :derived-type falset))
           (trueb (make-instance 'bir:iblock
                    :predecessors (cleavir-set:make-set fore)
                    :inputs ()
                    :dynamic-environment dynenv :function function
                    :name (make-symbol
                           (concatenate 'string (symbol-name primop-name)
                                        "-TRUE"))))
           (falseb (make-instance 'bir:iblock
                     :predecessors (cleavir-set:make-set fore)
                     :inputs ()
                     :dynamic-environment dynenv :function function
                     :name (make-symbol
                            (concatenate 'string (symbol-name primop-name)
                                         "-FALSE"))))
           (truer (make-instance 'bir:constant-reference
                    :policy (bir:policy call) :origin (bir:origin call)
                    :predecessor nil :iblock trueb
                    :inputs (list true) :outputs (list truev)))
           (falser (make-instance 'bir:constant-reference
                     :policy (bir:policy call) :origin (bir:origin call)
                     :predecessor nil :iblock falseb
                     :inputs (list false) :outputs (list falsev)))
           (truej (make-instance 'bir:jump
                    :policy (bir:policy call) :origin (bir:origin call)
                    :inputs (list truev) :outputs (list phi) :iblock trueb
                    :predecessor truer :next (list aft)))
           (falsej (make-instance 'bir:jump
                     :policy (bir:policy call) :origin (bir:origin call)
                     :inputs (list falsev) :outputs (list phi) :iblock falseb
                     :predecessor falser :next (list aft)))
           (info (cleavir-primop-info:info primop-name))
           (ifi (make-instance 'bir:ifi
                  :policy (bir:policy call) :origin (bir:origin call)
                  :next (list trueb falseb)))
           (cout (bir:output call)))
      ;; Fill the blocks - they just read a constant to the phi
      (setf (bir:start trueb) truer (bir:successor truer) truej
            (bir:end trueb) truej
            (bir:start falseb) falser (bir:successor falser) falsej
            (bir:end falseb) falsej
            (bir:inputs aft) (list phi))
      ;; Replace the call
      (change-class call 'cleavir-bir:vprimop
                    :info info :inputs args)
      (cleavir-set:nadjoinf (bir:predecessors aft) trueb)
      (cleavir-set:nadjoinf (bir:predecessors aft) falseb)
      (cleavir-set:nremovef (bir:predecessors aft) fore)
      (cleavir-set:nadjoinf (bir:scope dynenv) trueb)
      (cleavir-set:nadjoinf (bir:scope dynenv) falseb)
      (bir:replace-terminator ifi (bir:end fore))
      (bir:replace-uses phi cout)
      (setf (bir:inputs ifi) (list cout))))
  t)

(macrolet ((define-two-arg-f (name sf-primop df-primop)
             (let ((sf (cleavir-ctype:range 'single-float '* '* *clasp-system*))
                   (df
                     (cleavir-ctype:range 'double-float '* '* *clasp-system*)))
             `(progn
                (define-bir-transform ,name (call) (single-float single-float)
                  (replace-with-vprimop-and-wrap call ',sf-primop ',sf))
                (define-bir-transform ,name (call) (double-float double-float)
                  (replace-with-vprimop-and-wrap call ',df-primop ',df))
                (define-bir-transform ,name (call) (single-float double-float)
                  (wrap-coerce-sf-to-df call (first (rest (bir:inputs call))))
                  (replace-with-vprimop-and-wrap call ',df-primop ',df))
                (define-bir-transform ,name (call) (double-float single-float)
                  (wrap-coerce-sf-to-df call (second (rest (bir:inputs call))))
                  (replace-with-vprimop-and-wrap call ',df-primop ',df))))))
  (define-two-arg-f core:two-arg-+ core::two-arg-sf-+ core::two-arg-df-+)
  (define-two-arg-f core:two-arg-- core::two-arg-sf-- core::two-arg-df--)
  (define-two-arg-f core:two-arg-* core::two-arg-sf-* core::two-arg-df-*)
  (define-two-arg-f core:two-arg-/ core::two-arg-sf-/ core::two-arg-df-/)
  (define-two-arg-f expt           core::sf-expt      core::df-expt))

(macrolet ((define-float-conditional (name sf-primop df-primop)
             `(progn
                (define-bir-transform ,name (call) (single-float single-float)
                  (replace-with-test-primop call ',sf-primop))
                (define-bir-transform ,name (call) (double-float double-float)
                  (replace-with-test-primop call ',df-primop))
                (define-bir-transform ,name (call) (single-float double-float)
                  (wrap-coerce-sf-to-df call
                                        (first (rest (bir:inputs call))))
                  (replace-with-test-primop call ',df-primop))
                (define-bir-transform ,name (call) (double-float single-float)
                  (wrap-coerce-sf-to-df call
                                        (second (rest (bir:inputs call))))
                  (replace-with-test-primop call ',df-primop)))))
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

(macrolet ((define-one-arg-f (name sf-primop df-primop)
             (let ((sf (cleavir-ctype:range 'single-float '* '* *clasp-system*))
                   (df
                     (cleavir-ctype:range 'double-float '* '* *clasp-system*)))
               `(progn
                  (define-bir-transform ,name (call) (single-float)
                    (replace-with-vprimop-and-wrap call ',sf-primop ',sf))
                  (define-bir-transform ,name (call) (double-float)
                    (replace-with-vprimop-and-wrap call ',df-primop ',df))))))
  (define-one-arg-f cos         core::sf-cos    core::df-cos)
  (define-one-arg-f sin         core::sf-sin    core::df-sin)
  (define-one-arg-f abs         core::sf-abs    core::df-abs)
  (define-one-arg-f sqrt        core::sf-sqrt   core::df-sqrt)
  (define-one-arg-f exp         core::sf-exp    core::df-exp)
  ;; there's probably some weird floating point reason to explain why
  ;; llvm has an fneg instruction but not a reciprocal, but i don't know it.
  (define-one-arg-f core:negate core::sf-negate core::df-negate))

(define-bir-transform core:reciprocal (call) (single-float)
  (let* ((arguments (rest (bir:inputs call)))
         (module (bir:module (bir:function call)))
         (one (bir:constant-in-module 1f0 module))
         (onet (cleavir-ctype:range 'single-float 1f0 1f0 *clasp-system*))
         (onett (cleavir-ctype:single-value onet *clasp-system*))
         (onev (make-instance 'bir:output :derived-type onett :name '#:one))
         (cr (make-instance 'bir:constant-reference
               :policy (bir:policy call) :origin (bir:origin call)
               :inputs (list one) :outputs (list onev))))
    (bir:insert-instruction-before cr call)
    (change-class call 'cleavir-bir:vprimop
                  :inputs (list onev (first arguments))
                  :info (cleavir-primop-info:info 'core::two-arg-sf-/))))
(define-bir-transform core:reciprocal (call) (double-float)
  (let* ((arguments (rest (bir:inputs call)))
         (module (bir:module (bir:function call)))
         (one (bir:constant-in-module 1d0 module))
         (onet (cleavir-ctype:range 'double-float 1d0 1d0 *clasp-system*))
         (onett (cleavir-ctype:single-value onet *clasp-system*))
         (onev (make-instance 'bir:output :derived-type onett :name '#:one))
         (cr (make-instance 'bir:constant-reference
               :policy (bir:policy call) :origin (bir:origin call)
               :inputs (list one) :outputs (list onev))))
    (bir:insert-instruction-before cr call)
    (change-class call 'cleavir-bir:vprimop
                  :inputs (list onev (first arguments))
                  :info (cleavir-primop-info:info 'core::two-arg-df-/))))

;;; Transform log, but only one-argument log (which can be derived from the
;;; two argument case by the compiler macro in opt-number.lsp)
(define-bir-transform log (call) (single-float)
  (replace-with-vprimop-and-wrap call 'core::sf-log
                                 (cleavir-ctype:range 'single-float '* '*
                                                      *clasp-system*)))
(define-bir-transform log (call) (double-float)
  (replace-with-vprimop-and-wrap call 'core::df-log
                                 (cleavir-ctype:range 'double-float '* '*
                                                      *clasp-system*)))
