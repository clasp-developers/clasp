(defpackage #:interpret-ast
  (:use #:cl)
  (:export #:interpret)
  (:export #:cannot-interpret #:cannot-interpret-ast)
  (:export #:can-interpret-p)
  (:shadow #:variable))

;;;; NOTE: Some methods in this file must be compiled with cleavir,
;;;; as they use cleavir special operators - metacircularity, eh?
;;;; These are surrounded with (eval-when (:load-toplevel) ...) to
;;;; prevent bclasp from loading them.

(in-package #:interpret-ast)

;;; environment definition
;;; it's a list of hash tables. car is the most recent

(defun empty-environment ()
  (list (make-hash-table :test #'eq)))

(defun augment-environment (env)
  (cons (make-hash-table :test #'eq) env))

(defun variable (var env)
  (multiple-value-bind (value presentp)
      (gethash var (car env))
    (cond
      (presentp value)
      ((null (cdr env))
       (error "BUG: Unbound ~a" var))
      (t (variable var (cdr env))))))

(defun variable-frame (var env)
  (cond ((null env) nil)
        ((nth-value 1 (gethash var (car env))) (car env))
        (t (variable-frame var (cdr env)))))

(defun (setf variable) (new var env)
  (let ((frame (variable-frame var env)))
    (if (null frame) ; var not found, set it locally
        (setf (gethash var (car env)) new)
        (setf (gethash var frame) new))))

;; interface

(defun interpret (ast)
  (let ((env (empty-environment)))
    ;; Do actual work
    (interpret-ast ast env)))

(define-condition cannot-interpret (error)
  ((ast :reader cannot-interpret-ast :initarg :ast))
  (:report (lambda (condition stream)
             (format stream "Interpreter not implemented for AST: ~a"
                     (cannot-interpret-ast condition)))))

;;; meat

(defgeneric interpret-ast (ast env))

(defmethod interpret-ast (ast env)
  (declare (ignore env))
  (error 'cannot-interpret :ast ast))

;;; distinguished only to make sure the input ast is correct
(defgeneric interpret-boolean-ast (condition env))

(defmethod interpret-boolean-ast (condition env)
  (declare (ignore then else env))
  (error 'cannot-interpret :ast condition))

;;; Some we don't bother with, such as all the arithmetic.
(defgeneric can-interpret-p (ast))
(defmethod can-interpret-p (ast) nil)

(defmacro defcan (name)
  `(defmethod can-interpret-p ((ast ,name)) t))

(defun can-interpret-ast-p (ast)
  (cleavir-ast:map-ast-depth-first-preorder
   (lambda (ast)
     (unless (can-interpret-ast ast)
       (return-from can-interpret-ast-p nil)))
   ast)
  t)

(defcan cleavir-ast:immediate-ast)
(defmethod interpret-ast ((ast cleavir-ast:immediate-ast) env)
  (declare (ignore env))
  (let* ((val (cleavir-ast:value ast))
         (_ (or val (error "AST immediate from ~a is not possible" val)))
         (imm (core:value-from-tagged-immediate val)))
    (or imm (error "AST immediate ~a produced nil" val)))        
  #+(or)
  (error "AST produced for interpretation cannot include immediates: ~a" ast))

(defcan cleavir-ast:constant-ast)
(defmethod interpret-ast ((ast cleavir-ast:constant-ast) env)
  (declare (ignore env))
  (cleavir-ast:value ast))

(defcan cleavir-ast:lexical-ast)
(defmethod interpret-ast ((ast cleavir-ast:lexical-ast) env)
  (variable ast env))

(defcan cleavir-ast:symbol-value-ast)
(defmethod interpret-ast ((ast cleavir-ast:symbol-value-ast) env)
  (symbol-value (interpret-ast (cleavir-ast:symbol-ast ast) env)))

(defcan cleavir-ast:set-symbol-value-ast)
(defmethod interpret-ast ((ast cleavir-ast:set-symbol-value-ast) env)
  (setf (symbol-value (interpret-ast (cleavir-ast:symbol-ast ast) env))
        (interpret-ast (cleavir-ast:value-ast ast) env)))

(defcan cleavir-ast:fdefinition-ast)
(defmethod interpret-ast ((ast cleavir-ast:fdefinition-ast) env)
  (fdefinition (interpret-ast (cleavir-ast:name-ast ast) env)))

(defcan cleavir-ast:call-ast)
(defmethod interpret-ast ((ast cleavir-ast:call-ast) env)
  (let ((fn (interpret-ast (cleavir-ast:callee-ast ast) env))
        (args (loop for arg in (cleavir-ast:argument-asts ast)
                    collecting (interpret-ast arg env))))
    (apply fn args)))

;;; assumes correctness, so as to maintain my sanity
(defun parse-lambda-list (ll)
  (let (required optional rest (va-rest-p nil) keyp key aok-p)
    (loop with state = :required
          for item in ll
          do (case state
               ((:required)
                (case item
                  ((&optional &rest)
                   (setf state item))
                  ((core:&va-rest)
                   (setf state '&rest va-rest-p t))
                  ((&key) (setf state item keyp t))
                  (otherwise (push item required))))
               ((&optional)
                (case item
                  ((&rest) (setf state item))
                  ((core:&va-rest) (setf state '&rest va-rest-p t))
                  ((&key) (setf state item keyp t))
                  (otherwise (push item optional))))
               ((&rest)
                (if (eq item '&key)
                    (setf state item keyp t)
                    (setf rest item)))
               ((&key)
                (if (member item '(&allow-other-keys))
                    (setf aok-p item)
                    (push item key)))))
    (values (nreverse required) (nreverse optional)
            rest va-rest-p keyp (nreverse key) aok-p)))

;;; given a vaslist of arguments, an env, and the shredded viscera of a lambda list,
;;; fill the env with the appropriate bindings.
(defun bind-list (arguments env required optional rest va-rest-p keyp key aok-p)
  (loop for r in required
        if (zerop (core:vaslist-length arguments))
          do (error "Not enough arguments") ; FIXME: message
        else do (setf (variable r env) (core:vaslist-pop arguments)))
  (loop for (ovar o-p) in optional
        if (zerop (core:vaslist-length arguments))
          do (setf (variable o-p env) nil)
        else do (setf (variable ovar env) (core:vaslist-pop arguments)
                      (variable o-p env) t))
  (when rest
    (setf (variable rest env)
          (if va-rest-p arguments (core:list-from-va-list arguments))))
  (when keyp
    (unless (evenp (core:vaslist-length arguments))
      (error "Odd number of keyword arguments")))
  (when (and (not rest) (not keyp) (plusp (core:vaslist-length arguments)))
    (error "Too many arguments"))
  (loop with indicator = (list nil) ; arbitrary unique thing
        with arguments = (core:list-from-va-list arguments)
        for (k var var-p) in key
        for value = (getf arguments k indicator)
        if (eq value indicator) ; not present
          do (setf (variable var-p env) nil)
        else do (setf (variable var env) value
                      (variable var-p env) t))
  ;; TODO: aokp check blabla
  (values))

(defcan cleavir-ast:function-ast)
(defmethod interpret-ast ((ast cleavir-ast:function-ast) env)
  (let ((body (cleavir-ast:body-ast ast))
        (ll (cleavir-ast:lambda-list ast)))
    (cleavir-ast:map-local-ast-depth-first-preorder
     (lambda (ast)
       (unless (can-interpret-p ast)
         (error 'interpret-ast:cannot-interpret :ast ast)))
     body)
    (multiple-value-bind (required optional rest va-rest-p keyp key aok-p)
        (parse-lambda-list ll)
      (lambda (core:&va-rest arguments)
        (declare (core:lambda-name ast-interpreted-closure))
        (let ((env (augment-environment env)))
          (bind-list arguments env
                     required optional rest va-rest-p keyp key aok-p)
          ;; ok body now
          (interpret-ast body env))))))

(defcan cleavir-ast:progn-ast)
(defmethod interpret-ast ((ast cleavir-ast:progn-ast) env)
  (let ((form-asts (cleavir-ast:form-asts ast)))
    (if (null form-asts)
        nil
        (loop for (form-ast . rest) on form-asts
              when (null rest)
                return (interpret-ast form-ast env)
              else do (interpret-ast form-ast env)))))

(defcan cleavir-ast:block-ast)
(defmethod interpret-ast ((ast cleavir-ast:block-ast) env)
  ;; We need to disambiguate things if the block is entered
  ;; more than once. Storing things in the environment
  ;; lets it work with closures.
  (let ((catch-tag (gensym)))
    (setf (variable ast env) catch-tag)
    (catch catch-tag
      (interpret-ast (cleavir-ast:body-ast ast) env))))

(defcan cleavir-ast:return-from-ast)
(defmethod interpret-ast ((ast cleavir-ast:return-from-ast) env)
  (let ((catch-tag (variable (cleavir-ast:block-ast ast) env)))
    (throw catch-tag
      (interpret-ast (cleavir-ast:form-ast ast) env))))

(defcan cleavir-ast:setq-ast)
(defmethod interpret-ast ((ast cleavir-ast:setq-ast) env)
  (setf (variable (cleavir-ast:lhs-ast ast) env)
        (interpret-ast (cleavir-ast:value-ast ast) env)))

(defcan cleavir-ast:multiple-value-setq-ast)
(defmethod interpret-ast ((ast cleavir-ast:multiple-value-setq-ast) env)
  (let ((values (multiple-value-list
                 (interpret-ast (cleavir-ast:form-ast ast) env))))
    (loop with rvalues = values
          for var in (cleavir-ast:lhs-asts ast)
          do (setf (variable var env) (pop rvalues)))
    (values-list values)))

(defcan cleavir-ast:tag-ast)
(defmethod interpret-ast ((ast cleavir-ast:tag-ast) env)
  ;; nop
  (declare (ignore env)))

(defcan cleavir-ast:tagbody-ast)
(defmethod interpret-ast ((ast cleavir-ast:tagbody-ast) env)
  ;; We loop through the item-asts interpreting them.
  ;; If we hit a GO, the GO throws a new list of ASTs to interpret, set up
  ;; beforehand. We catch that and set it as the new to-interpret list.
  (let* ((catch-tag (gensym))
         (items (cleavir-ast:item-asts ast)))
    ;; Set up the tags
    (loop for (item . rest) on items
          when (typep item 'cleavir-ast:tag-ast)
            do (setf (variable item env) (cons catch-tag rest)))
    ;; Go
    (loop for to-interpret = items
            then (catch catch-tag
                   ;; if we hit a GO to here, it'll throw and the
                   ;; (cdr to-interpret) will be unused.
                   (interpret-ast (car to-interpret) env)
                   (cdr to-interpret))
          until (null to-interpret)))
  nil)

(defcan cleavir-ast:go-ast)
(defmethod interpret-ast ((ast cleavir-ast:go-ast) env)
  (destructuring-bind (catch-tag . asts)
      (variable (cleavir-ast:tag-ast ast) env)
    (throw catch-tag asts)))

(defcan cleavir-ast:the-ast)
(defmethod interpret-ast ((ast cleavir-ast:the-ast) env)
  ;; ignore the declaration.
  (interpret-ast (cleavir-ast:form-ast ast) env))

(defcan cleavir-ast:typeq-ast)
(defmethod interpret-boolean-ast ((condition cleavir-ast:typeq-ast) env)
  (typep (interpret-ast (cleavir-ast:form-ast condition) env)
         (cleavir-ast:type-specifier condition)))

(defcan cleavir-ast:load-time-value-ast)
(defmethod interpret-ast ((ast cleavir-ast:load-time-value-ast) env)
  (eval (cleavir-ast:form ast)))

(defcan cleavir-ast:if-ast)
(defmethod interpret-ast ((ast cleavir-ast:if-ast) env)
  (if (interpret-boolean-ast (cleavir-ast:test-ast ast) env)
      (interpret-ast (cleavir-ast:then-ast ast) env)
      (interpret-ast (cleavir-ast:else-ast ast) env)))

(defcan cleavir-ast:multiple-value-call-ast)
(defmethod interpret-ast ((ast cleavir-ast:multiple-value-call-ast) env)
  (let ((fn (interpret-ast (cleavir-ast:function-form-ast ast) env))
        (values (loop for ast in (cleavir-ast:form-asts ast)
                      nconcing (multiple-value-list (interpret-ast ast env)))))
    (apply fn values)))

(defcan cleavir-ast:values-ast)
(defmethod interpret-ast ((ast cleavir-ast:values-ast) env)
  (values-list (loop for ast in (cleavir-ast:argument-asts ast)
                     collecting (interpret-ast ast env))))

(defcan cleavir-ast:multiple-value-prog1-ast)
(defmethod interpret-ast ((ast cleavir-ast:multiple-value-prog1-ast) env)
  (multiple-value-prog1 (interpret-ast (cleavir-ast:first-form-ast ast) env)
    (loop for ast in (cleavir-ast:form-asts ast)
          do (interpret-ast ast env))))

(defcan cleavir-ast:dynamic-allocation-ast)
(defmethod interpret-ast ((ast cleavir-ast:dynamic-allocation-ast) env)
  ;; ignore declaration
  (interpret-ast (cleavir-ast:form-ast ast) env))

(defcan cleavir-ast:unreachable-ast)
(defmethod interpret-ast ((ast cleavir-ast:unreachable-ast) env)
  (error "BUG: Unreachable"))

(defcan cleavir-ast:eq-ast)
(defmethod interpret-boolean-ast ((ast cleavir-ast:eq-ast) env)
  (eq (interpret-ast (cleavir-ast:arg1-ast ast) env)
      (interpret-ast (cleavir-ast:arg2-ast ast) env)))

;;; array-related-asts.lisp

(defcan cleavir-ast:aref-ast)
(defmethod interpret-ast ((ast cleavir-ast:aref-ast) env)
  (aref (interpret-ast (cleavir-ast:array-ast ast) env)
        (interpret-ast (cleavir-ast:index-ast ast) env)))

(defcan cleavir-ast:aset-ast)
(defmethod interpret-ast ((ast cleavir-ast:aset-ast) env)
  (setf (aref (interpret-ast (cleavir-ast:array-ast ast) env)
              (interpret-ast (cleavir-ast:index-ast ast) env))
        (interpret-ast (cleavir-ast:element-ast ast) env)))

;;; cons-related-asts.lisp

(defcan cleavir-ast:car-ast)
(defmethod interpret-ast ((ast cleavir-ast:car-ast) env)
  (car (the cons (interpret-ast (cleavir-ast:cons-ast ast) env))))

(defcan cleavir-ast:cdr-ast)
(defmethod interpret-ast ((ast cleavir-ast:cdr-ast) env)
  (cdr (the cons (interpret-ast (cleavir-ast:cons-ast ast) env))))

(defcan cleavir-ast:rplaca-ast)
(defmethod interpret-ast ((ast cleavir-ast:rplaca-ast) env)
  (setf (car (the cons (interpret-ast (cleavir-ast:cons-ast ast) env)))
        (interpret-ast (cleavir-ast:object-ast ast) env)))

(defcan cleavir-ast:rplacd-ast)
(defmethod interpret-ast ((ast cleavir-ast:rplacd-ast) env)
  (setf (cdr (the cons (interpret-ast (cleavir-ast:cons-ast ast) env)))
        (interpret-ast (cleavir-ast:object-ast ast) env)))

;;; fixnum-related-asts.lisp

;; Given how the interpreter is used, we don't expect much arithmetic.
;; fixnum-add and -sub are especially annoying to do - their semantics
;; are hard to do without metacircularity, and with metacircularity
;; they introduce boot issues - so we punt on those.

(defmacro define-fixnum-comparison-interpreter (name op)
  `(progn
     (defcan ,name)
     (defmethod interpret-boolean-ast ((ast ,name) env)
       (,op (the fixnum (interpret-ast (cleavir-ast:arg1-ast ast) env))
            (the fixnum (interpret-ast (cleavir-ast:arg2-ast ast) env))))))

(define-fixnum-comparison-interpreter cleavir-ast:fixnum-less-ast <)
(define-fixnum-comparison-interpreter cleavir-ast:fixnum-not-greater-ast <=)
(define-fixnum-comparison-interpreter cleavir-ast:fixnum-greater-ast >)
(define-fixnum-comparison-interpreter cleavir-ast:fixnum-not-less-ast >=)
(define-fixnum-comparison-interpreter cleavir-ast:fixnum-equal-ast =)

;;; simple-float-related-asts.lisp

(defmacro define-one-arg-float-ast-interpreter (name op)
  `(progn
     (defcan ,name)
     (defmethod interpret-ast ((ast ,name) env)
       (,op (the float (interpret-ast (cleavir-ast:arg-ast ast) env))))))

(defmacro define-two-arg-float-ast-interpreter (name op)
  `(progn
     (defcan ,name)
     (defmethod interpret-ast ((ast ,name) env)
       (,op (the float (interpret-ast (cleavir-ast:arg1-ast ast) env))
            (the float (interpret-ast (cleavir-ast:arg2-ast ast) env))))))

(defmacro define-float-comparison-ast-interpreter (name op)
  `(progn
     (defcan ,name)
     (defmethod interpret-boolean-ast ((ast ,name) env)
       (,op (the float (interpret-ast (cleavir-ast:arg1-ast ast) env))
            (the float (interpret-ast (cleavir-ast:arg2-ast ast) env))))))

(define-two-arg-float-ast-interpreter cleavir-ast:float-add-ast +)
(define-two-arg-float-ast-interpreter cleavir-ast:float-sub-ast -)
(define-two-arg-float-ast-interpreter cleavir-ast:float-mul-ast *)
(define-two-arg-float-ast-interpreter cleavir-ast:float-div-ast /)

(define-float-comparison-ast-interpreter cleavir-ast:float-less-ast <)
(define-float-comparison-ast-interpreter cleavir-ast:float-not-greater-ast <=)
(define-float-comparison-ast-interpreter cleavir-ast:float-greater-ast >)
(define-float-comparison-ast-interpreter cleavir-ast:float-not-less-ast >=)
(define-float-comparison-ast-interpreter cleavir-ast:float-equal-ast =)

(define-one-arg-float-ast-interpreter cleavir-ast:float-sin-ast sin)
(define-one-arg-float-ast-interpreter cleavir-ast:float-cos-ast cos)
(define-one-arg-float-ast-interpreter cleavir-ast:float-sqrt-ast sqrt)

(defcan cleavir-ast:coerce-ast)
(defmethod interpret-ast ((ast cleavir-ast:coerce-ast) env)
  (coerce (interpret-ast (cleavir-ast:arg-ast ast) env)
          (cleavir-ast:to-type ast)))

;;; standard-object-related-asts.lisp
;;; clasp only, replace clos: with your mop package 

(defcan cleavir-ast:slot-read-ast)
(defmethod interpret-ast ((ast cleavir-ast:slot-read-ast) env)
  (clos:standard-instance-access
   (interpret-ast (cleavir-ast:object-ast ast) env)
   (interpret-ast (cleavir-ast:slot-number-ast ast) env)))

(defcan cleavir-ast:slot-write-ast)
(defmethod interpret-ast ((ast cleavir-ast:slot-write-ast) env)
  (setf (clos:standard-instance-access
         (interpret-ast (cleavir-ast:object-ast ast) env)
         (interpret-ast (cleavir-ast:slot-number-ast ast) env))
        (interpret-ast (cleavir-ast:value-ast ast) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLASP-SPECIFIC ASTS FOLLOW

;;; Unimplemented:
;;; debug-message, debug-break, m-v-foreign-call, foreign-call, foreign-call-pointer, defcallback,
;;; precalc-whatever

(defcan cc-ast:setf-fdefinition-ast)
(defmethod interpret-ast ((ast cc-ast:setf-fdefinition-ast) env)
  (fdefinition `(setf ,(interpret-ast (cleavir-ast:name-ast ast) env))))

(defcan cc-ast:throw-ast)
(defmethod interpret-ast ((ast cc-ast:throw-ast) env)
  (throw (interpret-ast (cc-ast:tag-ast ast) env)
    (interpret-ast (cc-ast:result-ast ast) env)))

;; The array access ASTs, like vector-length, are annoying to do non-metacircularly, so we don't.

(defcan cc-ast:vaslist-pop-ast)
(defmethod interpret-ast ((ast cc-ast:vaslist-pop-ast) env)
  (core:vaslist-pop (interpret-ast (cleavir-ast:arg-ast ast) env)))

#-cst (defcan cc-ast:bind-va-list-ast)
#-cst ; bind-va-list doesn't inline right - FIXME
(defmethod interpret-ast ((ast cc-ast:bind-va-list-ast) env)
  (let ((lambda-list (cleavir-ast:lambda-list ast))
        (va-list-ast (cc-ast:va-list-ast ast))
        (body-ast (cleavir-ast:body-ast ast)))
    (multiple-value-bind (required optional rest va-rest-p keyp key aok-p)
        (parse-lambda-list lambda-list)
      ;; We need to copy the vaslist for bind-va-list semantics.
      ;; This is the only way I know how, and yes, it's kind of silly.
      (core:bind-va-list (core:&va-rest vaslist-copy)
          (interpret-ast va-list-ast env)
        (bind-list vaslist-copy env
                   required optional rest va-rest-p keyp key aok-p)
        (interpret-ast body-ast env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLASP-SPECIFIC MODS

;;; Provide convenience function.

(in-package #:clasp-cleavir)

;; KLUDGE: If the closure has ASTs we can't interpret, we have
;; to give up immediately, because of inner closures.
;; We check proactively.
(defun can-interpret-ast-p (ast)
  (cleavir-ast:map-ast-depth-first-preorder
   (lambda (ast)
     (unless (interpret-ast:can-interpret-p ast)
       (return-from can-interpret-ast-p nil)))
   ast)
  t)

(defun ast-interpret-form (form env)
  (let ((ast #+cst(cst->ast (cst:cst-from-expression form) env)
             #-cst(generate-ast form env)))
    (if (can-interpret-ast-p ast)
        (interpret-ast:interpret ast)
        (cclasp-eval-with-env `(cleavir-primop:ast ,ast) env))))

(defun ast-interpret-cst (cst env)
  (let ((ast #+cst(cst->ast cst env)
             #-cst(generate-ast (cst:raw cst) env)))
    (if (can-interpret-ast-p ast)
        (interpret-ast:interpret ast)
        (cclasp-eval-with-env `(cleavir-primop:ast ,ast) env))))
