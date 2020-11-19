(in-package :cc-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define a "macro" for the Cleavir compiler.
;;; This shouldn't be necessary, but I'd like to
;;; avoid overwriting any bclasp definitions.
;;; Don't use &environment or &whole.

(defmacro def-convert-macro (name lambda-list &body body)
  (let ((head (gensym "HEAD"))
        (cst (gensym "CST"))
        (environment (gensym "ENVIRONMENT"))
        (system (gensym "SYSTEM")))
    `(defmethod cleavir-cst-to-ast:convert-special
         ((,head (eql ',name)) ,cst ,environment (,system clasp-cleavir:clasp))
       (cleavir-cst-to-ast:convert
        (destructuring-bind ,lambda-list (cst:raw (cst:rest ,cst))
          (cst:reconstruct (progn ,@body) ,cst ,system))
        ,environment ,system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define how to convert a special form that's "functionlike".
;;; This means it evaluates all its arguments normally, and
;;; left to right.
;;; NAME is the operator. AST is the name of the AST class.
;;; INITARGS is a list of initargs to make-instance that class.

(defmacro define-functionlike-special-form (name ast (&rest initargs))
  (let ((syms (loop for i in initargs collect (gensym (symbol-name i)))))
    `(defmethod cleavir-cst-to-ast:convert-special
         ((head (eql ',name)) cst env (system clasp-cleavir:clasp))
       (cst:db origin (op ,@syms) cst
               (declare (ignore op))
               (make-instance
                ',ast
                ,@(loop for i in initargs
                        for s in syms
                        collect i
                        collect `(cleavir-cst-to-ast:convert ,s env system))
                :origin origin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-CALL.
;;;
;;; In the case where there is one multiple-value form this gets converted
;;; into a multiple-value-call-ast.  In the general case with multiple forms
;;; it gets converted into a function call to CORE:MULTIPLE-VALUE-FUNCALL.
;;;

(def-convert-macro multiple-value-call (function-form &rest forms)
  ;;; Technically we could convert the 0-forms case to FUNCALL, but it's
  ;;; probably not a big deal.
  (if (eql (length forms) 1)
      `(cleavir-primop:multiple-value-call (core:coerce-fdesignator ,function-form) ,@forms)
      `(core:multiple-value-funcall
        (core:coerce-fdesignator ,function-form)
        ,@(mapcar (lambda (x)
                    `#'(lambda ()
                         (declare (core:lambda-name core::mvc-argument-lambda))
                         (progn ,x)))
                  forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dealing with type checks.

;;; Convert a type spec to something acceptable to typep.
;;; Which just means reducing (function ...) to function.
;;; TODO: Move to cleavir?
(defun discrimination-type (ctype)
  (etypecase ctype
    ((or symbol class) ctype)
    (cons
     (case (first ctype)
       ((function) 'function)
       ((and complex cons not or)
        `(,(first ctype)
          ,@(mapcar #'discrimination-type (rest ctype))))
       (t ctype)))))

(defmethod cleavir-cst-to-ast:type-wrap
    (ast ctype origin env (system clasp-cleavir:clasp))
  ;; We unconditionally insert a declaration,
  ;; and insert a type check as well on high safety,
  ;; unless the type is top in which case we do nothing.
  ;; NOTE that the type check doesn't check &optional and &rest. FIXME?
  (let ((insert-type-checks
          (cleavir-policy:policy-value
           (cleavir-env:policy (cleavir-env:optimize-info env))
           'clasp-cleavir::insert-type-checks))
        (required (cleavir-ctype:values-required ctype system)))
    (cond
      ((or (every (lambda (ty) (cleavir-ctype:top-p ty system)) required)
           (null required))
        ;; We don't even generate an m-v-extract, because while semantically
        ;; meaningless, saving and loading values constantly slows things down
        ;; very hard.
       ast)
      (insert-type-checks
       (let ((check
               (cleavir-cst-to-ast:convert
                (cst:cst-from-expression
                 (let ((vars (loop repeat (length required)
                                   collect (gensym "CHECKED"))))
                   `(let (,@vars)
                      (cleavir-primop:multiple-value-extract (,@vars)
                          (cleavir-primop:ast ,ast)
                        ,@(loop for var in vars
                                for ty in required
                                unless (cleavir-ctype:top-p ty system)
                                  collect `(if (cleavir-primop:typew
                                                ,var ,ty
                                                (typep ,var
                                                       ',(discrimination-type
                                                          ty)))
                                               nil
                                               (error 'type-error
                                                      :datum ,var
                                                      :expected-type ',ty)))))))
                env system)))
         (cleavir-ast:make-the-ast check ctype :origin origin)))
      (t (cleavir-ast:make-the-ast ast ctype :origin origin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:DEBUG-MESSAGE
;;;
;;; This is converted into a call to print a message
;;;

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core:debug-message)) cst environment (system clasp-cleavir:clasp))
  (declare (ignore environment))
  (assert (typep (cst:raw (cst:second cst)) 'string))
  (make-instance 'clasp-cleavir-ast:debug-message-ast
                 :debug-message (cst:raw (cst:second cst))
                 :origin (cst:source cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:DEBUG-BREAK
;;;
;;; This is converted into a call to invoke the debugger
;;;

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core:debug-break)) cst environment (system clasp-cleavir:clasp))
  (declare (ignore environment))
  (make-instance 'clasp-cleavir-ast:debug-break-ast
                 :origin (cst:source cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:multiple-value-foreign-CALL
;;;
;;; This is converted into an intrinsic call
;;;

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core:multiple-value-foreign-call)) cst environment (system clasp-cleavir:clasp))
  (assert (stringp (cst:raw (cst:second cst))))
  (make-instance 'clasp-cleavir-ast:multiple-value-foreign-call-ast
                 :function-name (cst:raw (cst:second cst))
                 :argument-asts (cleavir-cst-to-ast::convert-sequence (cst:rest (cst:rest cst)) environment system)
                 :origin (cst:source cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:FOREIGN-call
;;;
;;; This is converted into a pointer call
;;;

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core:foreign-call)) cst environment (system clasp-cleavir:clasp))
                                        ;  (format t "convert-special form: ~a~%"  cst)
  (assert (listp (cst:raw (cst:second cst))))
  (assert (stringp (cst:raw (cst:third cst))))
  (make-instance 'clasp-cleavir-ast:foreign-call-ast
                 :foreign-types (cst:raw (cst:second cst))
                 :function-name (cst:raw (cst:third cst))
                 :argument-asts (cleavir-cst-to-ast::convert-sequence (cst:rest (cst:rest (cst:rest cst))) environment system)
                 :origin (cst:source cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:foreign-call-pointer
;;;
;;; This is converted into a pointer call
;;;

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core:foreign-call-pointer)) cst environment (system clasp-cleavir:clasp))
  (assert (listp (cst:raw (cst:second cst))))
  (make-instance 'clasp-cleavir-ast:foreign-call-pointer-ast
                 :foreign-types (cst:raw (cst:second cst))
                 :argument-asts (cleavir-cst-to-ast::convert-sequence (cst:rest (cst:rest cst)) environment system)
                 :origin (cst:source cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:DEFCALLBACK
;;;

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core:defcallback)) form env (system clasp-cleavir:clasp))
  (cst:db origin (name convention rtype rtrans atypes atrans params placeholder lisp-callback)
      (cst:rest form)
    (let ((args (list (cst:raw name) (cst:raw convention)
                      (cst:raw rtype) (cst:raw rtrans)
                      (cst:raw atypes) (cst:raw atrans)
                      (cst:raw params) (cst:raw placeholder))))
      (make-instance 'cc-ast:defcallback-ast
                     :args args
                     :callee (cleavir-cst-to-ast:convert lisp-callback env system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functionlikes
;;; See their AST classes for more info probably
;;;

(define-functionlike-special-form core::vector-length cc-ast:vector-length-ast
  (:vector))
(define-functionlike-special-form core::%displacement cc-ast:displacement-ast
  (:mdarray))
(define-functionlike-special-form core::%displaced-index-offset
  cc-ast:displaced-index-offset-ast
  (:mdarray))
(define-functionlike-special-form core::%array-total-size
  cc-ast:array-total-size-ast
  (:mdarray))
(define-functionlike-special-form core::%array-rank cc-ast:array-rank-ast
  (:mdarray))
(define-functionlike-special-form core::%array-dimension cc-ast:array-dimension-ast
  (:mdarray :axis))
(define-functionlike-special-form core:vaslist-pop cc-ast:vaslist-pop-ast
  (:vaslist))
(define-functionlike-special-form core:vaslist-length cc-ast:vaslist-length-ast
  (:vaslist))

(define-functionlike-special-form core::header-stamp cc-ast:header-stamp-ast (:arg))
(define-functionlike-special-form core::rack-stamp cc-ast:rack-stamp-ast (:arg))
(define-functionlike-special-form core::wrapped-stamp cc-ast:wrapped-stamp-ast (:arg))
(define-functionlike-special-form core::derivable-stamp cc-ast:derivable-stamp-ast (:arg))

(define-functionlike-special-form core:cas-car cc-ast:cas-car-ast
  (:cmp-ast :value-ast :cons-ast))
(define-functionlike-special-form core:cas-cdr cc-ast:cas-cdr-ast
  (:cmp-ast :value-ast :cons-ast))
(define-functionlike-special-form core::instance-cas cc-ast:slot-cas-ast
  (:cmp-ast :value-ast :object-ast :slot-number-ast))
(define-functionlike-special-form core:instance-rack cc-ast:instance-rack-ast
  (:object-ast))
(define-functionlike-special-form core:instance-rack-set cc-ast:instance-rack-set-ast
  (:object-ast :value-ast))
(define-functionlike-special-form core:rack-ref cc-ast:rack-read-ast
  (:object-ast :slot-number-ast))
(define-functionlike-special-form core:rack-set cc-ast:rack-write-ast
  (:object-ast :slot-number-ast :value-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE::ACAS
;;;

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core::acas)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (acas array index cmp value type simple-p boxed-p) cst
    (declare (ignore acas))
    (make-instance 'cc-ast:acas-ast
      :array-ast (cleavir-cst-to-ast:convert array env system)
      :index-ast (cleavir-cst-to-ast:convert index env system)
      :cmp-ast   (cleavir-cst-to-ast:convert cmp env system)
      :value-ast (cleavir-cst-to-ast:convert value env system)
      :element-type (cst:raw type)
      :simple-p (cst:raw simple-p)
      :boxed-p (cst:raw boxed-p)
      :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE::HEADER-STAMP-CASE
;;;

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core::header-stamp-case)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (stamp derivable rack wrapped header) (cst:rest cst)
    (cleavir-ast:make-branch-ast
     (clasp-cleavir-ast:make-header-stamp-case-ast
      (cleavir-cst-to-ast:convert stamp env system)
      origin)
     (list (cleavir-cst-to-ast:convert derivable env system)
           (cleavir-cst-to-ast:convert rack env system)
           (cleavir-cst-to-ast:convert wrapped env system))
     (cleavir-cst-to-ast:convert header env system)
     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting core::local-tagbody to tagbody
;;;        and core::local-block to block
;;;

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core::local-tagbody)) cst environment (system clasp-cleavir:clasp))
  (cleavir-cst-to-ast:convert-special 'tagbody cst environment system))

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core::local-block)) cst environment (system clasp-cleavir:clasp))
  (cleavir-cst-to-ast:convert-special 'block cst environment system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THROW
;;;
;;; Convert throw into a call

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'cl:throw)) cst environment (system clasp-cleavir:clasp))
  (cst:db origin (tag result-cst) 
      (cst:rest cst)
    ;; If I don't use a throw-ast node use the following
    #+(or)
    (cleavir-cst-to-ast::convert `(core:throw-function ,tag ,result-cst) environment system)
    ;; If I decide to go with a throw-ast node use the following
    (clasp-cleavir-ast:make-throw-ast
     (cleavir-cst-to-ast:convert tag environment system)
     (cleavir-cst-to-ast:convert result-cst environment system)
     origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting UNWIND-PROTECT
;;;

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'cl:unwind-protect)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (protected . cleanup) (cst:rest cst)
    (make-instance 'cc-ast:unwind-protect-ast
      :body-ast (cleavir-cst-to-ast:convert protected env system)
      :cleanup-ast (let ((cleanup-source (cst:source cleanup)))
                     (cleavir-cst-to-ast:convert
                      (cst:cons
                       (make-instance 'cst:atom-cst
                         :raw 'lambda :source cleanup-source)
                       (cst:cons (make-instance 'cst:atom-cst
                                   :raw nil :source cleanup-source)
                                 cleanup
                                 :source cleanup-source)
                       :source cleanup-source)
                      env system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE::BIND-VA-LIST
;;;

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core::bind-va-list)) cst
     environment (system clasp-cleavir:clasp))
  ;; (bind-va-list lambda-list va . body)
  ;; => `(apply (lambda ,lambda-list . ,body) ,va)
  (cst:db origin (op lambda-list-cst va-list-cst . body-cst) cst
    (declare (ignore op))
    (cleavir-cst-to-ast:convert
     (cst:list (cst:cst-from-expression 'apply)
               (cst:cons (cst:cst-from-expression 'lambda)
                         (cst:cons lambda-list-cst body-cst
                                   :source origin)
                         :source origin)
               va-list-cst)
     environment system)))

#+(or) ;;#+cst
(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core::bind-va-list)) cst environment (system clasp-cleavir:clasp))
  (cst:db origin (op lambda-list-cst va-list-cst . body-cst) cst
    (declare (ignore op))
    (let ((parsed-lambda-list
            (cst:parse-ordinary-lambda-list system lambda-list-cst :error-p nil)))
      (when (null parsed-lambda-list)
        (error 'cleavir-cst-to-ast::malformed-lambda-list
               :expr (cst:raw lambda-list-cst)
               :origin (cst:source lambda-list-cst)))
      (multiple-value-bind (declaration-csts documentation forms-cst)
          (cst:separate-function-body body-cst)
        (declare (ignore documentation))
        (let* ((declaration-specifiers
                 (loop for declaration-cst in declaration-csts
                       append (cdr (cst:listify declaration-cst))))
               (canonicalized-dspecs
                 (cst:canonicalize-declaration-specifiers
                  system
                  (cleavir-env:declarations environment)
                  declaration-specifiers)))
          (multiple-value-bind (idspecs rdspecs)
              (cleavir-cst-to-ast::itemize-declaration-specifiers-by-parameter-group
               (cleavir-cst-to-ast::itemize-lambda-list parsed-lambda-list)
               canonicalized-dspecs)
            (multiple-value-bind (lexical-lambda-list entries)
                (cleavir-cst-to-ast::lambda-list-from-parameter-groups
                 (cst:children parsed-lambda-list))
              (let ((ast (cleavir-cst-to-ast::process-parameter-groups
                          (cst:children parsed-lambda-list)
                          idspecs entries
                          (cleavir-cst-to-ast::make-body
                           rdspecs
                           (cleavir-cst-to-ast::cst-for-body forms-cst nil origin))
                          environment system)))
                (cc-ast:make-bind-va-list-ast
                 lexical-lambda-list
                 (cleavir-cst-to-ast::convert va-list-cst environment system)
                 ast
                 nil ; FIXME: handle rest-alloc (parse &rest from lambda list)
                 :origin origin)))))))))

(defmethod cleavir-cst-to-ast:convert-global-function-reference (cst info global-env (system clasp-cleavir:clasp))
  (declare (ignore global-env))
  (let ((name (cleavir-env:name info))
        (source (cst:source cst)))
    (cond
      ((and (consp name) (eq (car name) 'cl:setf))
       (clasp-cleavir-ast:make-setf-fdefinition-ast
        (cleavir-ast:make-constant-ast (cadr name) :origin source)
        :origin source))
      ((consp name)
       (error "Illegal name for function - must be (setf xxx)"))
      (t
       (cleavir-ast:make-fdefinition-ast
        (cleavir-ast:make-constant-ast name :origin (cst:source cst))
        :origin source)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:INSTANCE-REF/SET
;;;
;;; FIXME: Maybe just use the primops instead.

(defmethod cleavir-cst-to-ast:convert-special
    ((symbol (eql 'core:instance-ref)) cst environment (system clasp-cleavir:clasp))
  (cleavir-cst-to-ast:convert-special 'cleavir-primop:slot-read cst environment system))

;;; For -set it's mildly more complicated, as slot-write returns no values.
(def-convert-macro core:instance-set (instance index value)
  (let ((ig (gensym)) (ing (gensym)) (vg (gensym)))
    `(let ((,ig ,instance) (,ing ,index) (,vg ,value))
       (cleavir-primop:slot-write ,ig ,ing ,vg)
       ,vg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bind special variables.
;;;

(defmethod cleavir-cst-to-ast:convert-special-binding
    (variable-cst value-ast next-ast env (system clasp-cleavir:clasp))
  (make-instance 'cc-ast:bind-ast
    :name-ast (cleavir-cst-to-ast:convert-constant variable-cst env system)
    :value-ast value-ast
    :body-ast next-ast
    :origin (cst:source variable-cst)))
