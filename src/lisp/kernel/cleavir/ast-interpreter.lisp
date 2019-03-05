(defpackage #:interpret-ast
  (:use #:cl)
  (:export #:interpret)
  ;; FIXME: lose this asap
  (:export #:*system*))

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
    (interpret-ast ast env)))

;;; meat

(defgeneric interpret-ast (ast env))

(defmethod interpret-ast (ast env)
  (declare (ignore env))
  (error "AST interpreter not implemented for ~a" ast))

;;; distinguished only to make sure the input ast is correct
(defgeneric interpret-boolean (condition env))

(defmethod interpret-boolean (condition env)
  (declare (ignore then else env))
  (error "AST boolean interpreter not implemented for ~a" condition))

(defmethod interpret-ast ((ast cleavir-ast:immediate-ast) env)
  (declare (ignore env))
  (let* ((val (cleavir-ast:value ast))
         (_ (or val (error "AST immediate from ~a is not possible" val)))
         (imm (core:value-from-tagged-immediate val)))
    (or imm (error "AST immediate ~a produced nil" val)))        
  #+(or)
  (error "AST produced for interpretation cannot include immediates: ~a" ast))

(defmethod interpret-ast ((ast cleavir-ast:constant-ast) env)
  (declare (ignore env))
  (cleavir-ast:value ast))

(defmethod interpret-ast ((ast cleavir-ast:lexical-ast) env)
  (variable ast env))

(defmethod interpret-ast ((ast cleavir-ast:symbol-value-ast) env)
  (symbol-value (interpret-ast (cleavir-ast:symbol-ast ast) env)))

(defmethod interpret-ast ((ast cleavir-ast:set-symbol-value-ast) env)
  (setf (symbol-value (interpret-ast (cleavir-ast:symbol-ast ast) env))
        (interpret-ast (cleavir-ast:value-ast ast) env)))

(defmethod interpret-ast ((ast cleavir-ast:fdefinition-ast) env)
  (fdefinition (interpret-ast (cleavir-ast:name-ast ast) env)))

(defmethod interpret-ast ((ast cleavir-ast:call-ast) env)
  (let ((fn (interpret-ast (cleavir-ast:callee-ast ast) env))
        (args (loop for arg in (cleavir-ast:argument-asts ast)
                    collecting (interpret-ast arg env))))
    (apply fn args)))

;;; assumes correctness, so as to maintain my sanity
(defun parse-lambda-list (ll)
  (let (required optional rest keyp key aok-p)
    (loop with state = :required
          for item in ll
          do (case state
               ((:required)
                (case item
                  ((&optional &rest) (setf state item))
                  ((&key) (setf state item keyp t))
                  (otherwise (push item required))))
               ((&optional)
                (case item
                  ((&rest) (setf state item))
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
    (values (nreverse required) (nreverse optional) rest keyp (nreverse key) aok-p)))

(defmethod interpret-ast ((ast cleavir-ast:function-ast) env)
  (let ((body (cleavir-ast:body-ast ast))
        (ll (cleavir-ast:lambda-list ast))
        (indicator (gensym)))
    (multiple-value-bind (required optional rest keyp key aok-p)
        (parse-lambda-list ll)
      (lambda (&rest arguments)
        (let ((env (augment-environment env)))
          (loop for r in required
                if (null arguments)
                  do (error "Not enough arguments") ; FIXME: message
                else do (setf (variable r env) (pop arguments)))
          (loop for (ovar o-p) in optional
                if (null arguments)
                  do (setf (variable o-p env) nil)
                else do (setf (variable ovar env) (pop arguments)
                              (variable o-p env) t))
          (when rest
            (setf (variable rest env) arguments))
          (when keyp
            (unless (evenp (length arguments))
              (error "Odd number of keyword arguments")))
          (when (and (not rest) (not keyp) (not (null arguments)))
            (error "Too many arguments"))
          (loop for (k var var-p) in key
                for value = (getf arguments k indicator)
                if (eq value indicator) ; not present
                  do (setf (variable var-p env) nil)
                else do (setf (variable var env) value
                              (variable var-p env) t))
          ;; TODO: aokp check blabla
          ;; ok body now
          (interpret-ast body env))))))

(defmethod interpret-ast ((ast cleavir-ast:progn-ast) env)
  (let ((form-asts (cleavir-ast:form-asts ast)))
    (if (null form-asts)
        nil
        (loop for (form-ast . rest) on form-asts
              when (null rest)
                return (interpret-ast form-ast env)
              else do (interpret-ast form-ast env)))))

(defmethod interpret-ast ((ast cleavir-ast:block-ast) env)
  ;; We need to disambiguate things if the block is entered
  ;; more than once. Storing things in the environment
  ;; lets it work with closures.
  (let ((catch-tag (gensym)))
    (setf (variable ast env) catch-tag)
    (catch catch-tag
      (interpret-ast (cleavir-ast:body-ast ast) env))))

(defmethod interpret-ast ((ast cleavir-ast:return-from-ast) env)
  (let ((catch-tag (variable (cleavir-ast:block-ast ast) env)))
    (throw catch-tag
      (interpret-ast (cleavir-ast:form-ast ast) env))))

(defmethod interpret-ast ((ast cleavir-ast:setq-ast) env)
  (setf (variable (cleavir-ast:lhs-ast ast) env)
        (interpret-ast (cleavir-ast:value-ast ast) env)))

(defmethod interpret-ast ((ast cleavir-ast:multiple-value-setq-ast) env)
  (let ((values (multiple-value-list
                 (interpret-ast (cleavir-ast:form-ast ast) env))))
    (loop for var in (cleavir-ast:lhs-asts ast)
          do (setf (variable var env) (pop values)))))

(defmethod interpret-ast ((ast cleavir-ast:tag-ast) env)
  ;; nop
  (declare (ignore env)))

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

(defmethod interpret-ast ((ast cleavir-ast:go-ast) env)
  (destructuring-bind (catch-tag . asts)
      (variable (cleavir-ast:tag-ast ast) env)
    (throw catch-tag asts)))

(defmethod interpret-ast ((ast cleavir-ast:the-ast) env)
  ;; ignore the declaration.
  (interpret-ast (cleavir-ast:form-ast ast) env))

(defmethod interpret-boolean-ast ((condition cleavir-ast:typeq-ast) env)
  (typep (interpret-ast (cleavir-ast:form-ast condition) env)
         (cleavir-ast:type-specifier condition)))

(defmethod interpret-ast ((ast cleavir-ast:load-time-value-ast) env)
  (eval (cleavir-ast:form ast)))

(defmethod interpret-ast ((ast cleavir-ast:if-ast) env)
  (if (interpret-boolean-ast (cleavir-ast:test-ast ast) env)
      (interpret-ast (cleavir-ast:then-ast ast) env)
      (interpret-ast (cleavir-ast:else-ast ast) env)))

(defmethod interpret-ast ((ast cleavir-ast:multiple-value-call-ast) env)
  (let ((fn (interpret-ast (cleavir-ast:function-form-ast ast) env))
        (values (loop for ast in (cleavir-ast:form-asts ast)
                      nconcing (multiple-value-list (interpret-ast ast env)))))
    (apply fn values)))

(defmethod interpret-ast ((ast cleavir-ast:values-ast) env)
  (values-list (loop for ast in (cleavir-ast:argument-asts ast)
                     collecting (interpret-ast ast env))))

(defmethod interpret-ast ((ast cleavir-ast:multiple-value-prog1-ast) env)
  (multiple-value-prog1 (interpret-ast (cleavir-ast:first-form-ast ast) env)
    (loop for ast in (cleavir-ast:form-asts ast)
          do (interpret-ast ast env))))

(defmethod interpret-ast ((ast cleavir-ast:dynamic-allocation-ast) env)
  ;; ignore declaration
  (interpret-ast (cleavir-ast:form-ast ast) env))

(defmethod interpret-ast ((ast cleavir-ast:unreachable-ast) env)
  (error "BUG: Unreachable"))

(defmethod interpret-boolean-ast ((ast cleavir-ast:eq-ast) env)
  (eq (interpret-ast (cleavir-ast:arg1-ast ast) env)
      (interpret-ast (cleavir-ast:arg2-ast ast) env)))

;;; array-related-asts.lisp

(defmethod interpret-ast ((ast cleavir-ast:aref-ast) env)
  (aref (interpret-ast (cleavir-ast:array-ast ast) env)
        (interpret-ast (cleavir-ast:index-ast ast) env)))

(defmethod interpret-ast ((ast cleavir-ast:aset-ast) env)
  (setf (aref (interpret-ast (cleavir-ast:array-ast ast) env)
              (interpret-ast (cleavir-ast:index-ast ast) env))
        (interpret-ast (cleavir-ast:element-ast ast) env)))

;;; cons-related-asts.lisp

(defmethod interpret-ast ((ast cleavir-ast:car-ast) env)
  (car (the cons (interpret-ast (cleavir-ast:cons-ast ast) env))))

(defmethod interpret-ast ((ast cleavir-ast:cdr-ast) env)
  (cdr (the cons (interpret-ast (cleavir-ast:cons-ast ast) env))))

(defmethod interpret-ast ((ast cleavir-ast:rplaca-ast) env)
  (setf (car (the cons (interpret-ast (cleavir-ast:cons-ast ast) env)))
        (interpret-ast (cleavir-ast:object-ast ast) env)))

(defmethod interpret-ast ((ast cleavir-ast:rplacd-ast) env)
  (setf (cdr (the cons (interpret-ast (cleavir-ast:cons-ast ast) env)))
        (interpret-ast (cleavir-ast:object-ast ast) env)))

;;; fixnum-related-asts.lisp

(eval-when (:load-toplevel)
(defmethod interpret-boolean-ast ((ast cleavir-ast:fixnum-add-ast) env)
  ;; Doing this without metacicularity is just annoying.
  (cleavir-primop:let-uninitialized (z)
    (prog1
        (if (cleavir-primop:fixnum-add
             (interpret-ast (cleavir-ast:arg1-ast ast) env)
             (interpret-ast (cleavir-ast:arg2-ast ast) env)
             z)
            t nil)
      (setf (variable (cleavir-ast:variable-ast ast) env) z))))

(defmethod interpret-boolean-ast ((ast cleavir-ast:fixnum-sub-ast) env)
  (cleavir-primop:let-uninitialized (z)
    (prog1
        (if (cleavir-primop:fixnum-sub
             (interpret-ast (cleavir-ast:arg1-ast ast) env)
             (interpret-ast (cleavir-ast:arg2-ast ast) env)
             z)
            t nil)
      (setf (variable (cleavir-ast:variable-ast ast) env) z))))
) ; eval-when

(defmacro define-fixnum-comparison-interpreter (name op)
  `(defmethod interpret-boolean-ast ((ast ,name) env)
     (,op (the fixnum (interpret-ast (cleavir-ast:arg1-ast ast) env))
          (the fixnum (interpret-ast (cleavir-ast:arg2-ast ast) env)))))

(define-fixnum-comparison-interpreter cleavir-ast:fixnum-less-ast <)
(define-fixnum-comparison-interpreter cleavir-ast:fixnum-not-greater-ast <=)
(define-fixnum-comparison-interpreter cleavir-ast:fixnum-greater-ast >)
(define-fixnum-comparison-interpreter cleavir-ast:fixnum-not-less-ast >=)
(define-fixnum-comparison-interpreter cleavir-ast:fixnum-equal-ast =)

;;; simple-float-related-asts.lisp

(defmacro define-one-arg-float-ast-interpreter (name op)
  `(defmethod interpret-ast ((ast ,name) env)
     (,op (the float (interpret-ast (cleavir-ast:arg-ast ast) env)))))

(defmacro define-two-arg-float-ast-interpreter (name op)
  `(defmethod interpret-ast ((ast ,name) env)
     (,op (the float (interpret-ast (cleavir-ast:arg1-ast ast) env))
          (the float (interpret-ast (cleavir-ast:arg2-ast ast) env)))))

(defmacro define-float-comparison-ast-interpreter (name op)
  `(defmethod interpret-boolean-ast ((ast ,name) env)
     (,op (the float (interpret-ast (cleavir-ast:arg1-ast ast) env))
          (the float (interpret-ast (cleavir-ast:arg2-ast ast) env)))))

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

(defmethod interpret-ast ((ast cleavir-ast:coerce-ast) env)
  (coerce (interpret-ast (cleavir-ast:arg-ast ast) env)
          (cleavir-ast:to-type ast)))

;;; standard-object-related-asts.lisp
;;; clasp only, replace clos: with your mop package 

(defmethod interpret-ast ((ast cleavir-ast:slot-read-ast) env)
  (clos:standard-instance-access
   (interpret-ast (cleavir-ast:object-ast ast) env)
   (interpret-ast (cleavir-ast:slot-number-ast ast) env)))

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
;;; precalc-whatever, bind-va-listr

(defmethod interpret-ast ((ast cc-ast:setf-fdefinition-ast) env)
  (fdefinition `(setf ,(interpret-ast (cleavir-ast:name-ast ast) env))))

(defmethod interpret-ast ((ast cc-ast:throw-ast) env)
  (throw (interpret-ast (cc-ast:tag-ast ast) env)
    (interpret-ast (cc-ast:result-ast ast) env)))

(eval-when (:load-toplevel)
(defmethod interpret-ast ((ast cc-ast:vector-length-ast) env)
  (core::vector-length (interpret-ast (cc-ast:vl-ast-vector ast) env)))

(defmethod interpret-ast ((ast cc-ast:displacement-ast) env)
  (core::%displacement
   (interpret-ast (cc-ast:displacement-ast-mdarray ast) env)))

(defmethod interpret-ast ((ast cc-ast:displaced-index-offset-ast) env)
  (core::%displaced-index-offset
   (interpret-ast (cc-ast:displaced-index-offset-ast-mdarray ast) env)))

(defmethod interpret-ast ((ast cc-ast:array-total-size-ast) env)
  (core::%array-total-size
   (interpret-ast (cc-ast:array-total-size-ast-mdarray ast) env)))

(defmethod interpret-ast ((ast cc-ast:array-rank-ast) env)
  (core::%array-rank
   (interpret-ast (cc-ast:array-rank-ast-mdarray ast) env)))

(defmethod interpret-ast ((ast cc-ast:array-dimension-ast) env)
  (core::%array-dimension
   (interpret-ast (cc-ast:array-dimension-ast-mdarray ast) env)
   (interpret-ast (cc-ast:array-dimension-ast-axis ast) env)))
) ; eval-when

(defmethod interpret-ast ((ast cc-ast:vaslist-pop-ast) env)
  (core:vaslist-pop (interpret-ast (cleavir-ast:arg-ast ast) env)))

(defmethod interpret-ast ((ast cc-ast:instance-stamp-ast) env)
  (core:instance-stamp (interpret-ast (cleavir-ast:arg-ast ast) env)))

(defmethod interpret-ast ((ast cc-ast:bind-va-list-ast) env)
  (let ((lambda-list (cleavir-ast:lambda-list ast))
        (va-list-ast (cc-ast:va-list-ast ast))
        (body-ast (cleavir-ast:body-ast ast)))
    (multiple-value-bind (required optional rest keyp key aok-p)
        (parse-lambda-list lambda-list)
      (let ((vaslist (interpret-ast va-list-ast env)))
        ;; mostly copied from enclose, above
        (loop for r in required
              if (zerop (core:vaslist-length vaslist))
                do (error "Not enough arguments") ; FIXME: message
              else do (setf (variable r env) (core:vaslist-pop vaslist)))
        (loop for (ovar o-p) in optional
              if (zerop (core:vaslist-length vaslist))
                do (setf (variable o-p env) nil)
              else do (setf (variable ovar env) (core:vaslist-pop vaslist)
                            (variable o-p env) t))
        (when rest
          (setf (variable rest env) (core:vaslist-as-list vaslist)))
        (when keyp
          (unless (evenp (core:vaslist-length vaslist))
            (error "Odd number of keyword arguments")))
        (when (and (not rest) (not keyp)
                   (not (zerop (core:vaslist-length vaslist))))
          (error "Too many arguments"))
        (loop with arguments = (core:vaslist-as-list vaslist)
              with indicator = (list nil)
              for (k var var-p) in key
              for value = (getf arguments k indicator)
              if (eq value indicator) ; not present
                do (setf (variable var-p env) nil)
              else do (setf (variable var env) value
                            (variable var-p env) t))
        ;; variables bound, now just interpret
        (interpret-ast body-ast env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLASP-SPECIFIC MODS

;;; Provide convenience function.

(in-package #:clasp-cleavir)

(defun ast-interpret-form (form env)
  (interpret-ast:interpret
   #-cst
   (cleavir-generate-ast:generate-ast
    form env clasp-cleavir:*clasp-system*)
   #+cst
   (cleavir-cst-to-ast:cst-to-ast
    (cst:cst-from-expression form)
    env clasp-cleavir:*clasp-system*)))
