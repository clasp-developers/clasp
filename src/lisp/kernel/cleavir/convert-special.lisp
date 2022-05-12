(in-package :clasp-cleavir)

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
    `(defmethod cst-to-ast:convert-special
         ((,head (eql ',name)) ,cst ,environment (,system clasp-cleavir:clasp))
       (cst-to-ast:convert
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
    `(defmethod cst-to-ast:convert-special
         ((head (eql ',name)) cst env (system clasp-cleavir:clasp))
       (cst:db origin (op ,@syms) cst
               (declare (ignore op))
               (make-instance
                   ',ast
                 ,@(loop for i in initargs
                         for s in syms
                         collect i
                         collect `(cst-to-ast:convert ,s env system))
                 :origin cst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-CALL.
;;;

(def-convert-macro multiple-value-call (function-form &rest forms)
  (let ((cf `(core:coerce-fdesignator ,function-form)))
    (case (length forms)
      (0
       `(cleavir-primop:funcall ,cf))
      (t
       `(cleavir-primop:multiple-value-call ,cf ,@forms)))))

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

(defun make-type-check-form (required system)
  (let ((vars (loop repeat (length required)
                    collect (gensym "CHECKED"))))
    `(lambda (,@vars &rest rest)
       ;; We don't want to insert type checks for typep and error, or
       ;; else we risk getting trapped in an infinite recursion.
       (declare (optimize (safety 0)))
       ,@(loop for var in vars
               for ty in required
               unless (cleavir-ctype:top-p ty system)
                 collect `(if (typep ,var
                                     ',(discrimination-type ty))
                              ,var
                              (error 'type-error
                                     :datum ,var
                                     :expected-type ',ty)))
       (apply #'values ,@vars rest))))

(defmethod cst-to-ast:type-wrap
    (ast ctype origin env (system clasp-cleavir:clasp))
  ;; We unconditionally insert a declaration,
  ;; and insert a type check as well on high safety,
  ;; unless the type is top in which case we do nothing.
  ;; NOTE that the type check doesn't check &optional and &rest. FIXME?
  (let ((insert-type-checks
          (policy:policy-value
           (cleavir-env:policy (cleavir-env:optimize-info env))
           'insert-type-checks))
        (required (cleavir-ctype:values-required ctype system)))
    (cond
      ((or (every (lambda (ty) (cleavir-ctype:top-p ty system)) required)
           (null required))
       ast)
      ((some (lambda (ty) (cleavir-ctype:bottom-p ty system)) required)
       ;; KLUDGE: This isn't actually a check; if the restriction is violated
       ;; bad things will happen. The right thing to do would be to call ERROR,
       ;; but I don't want to worry about the recursion (as ERROR is declared
       ;; as returning NIL type) right this second.
       (cleavir-ast:make-progn-ast
        (list ast (cleavir-ast:make-unreachable-ast :origin origin))))
      (insert-type-checks
       (let ((check
               (cst-to-ast:convert
                (cst:cst-from-expression
                 (make-type-check-form required system)
                 :source origin)
                env system)))
         (cleavir-ast:make-the-ast ast ctype check :origin origin)))
      (t (cleavir-ast:make-the-ast ast ctype nil :origin origin)))))

(defmethod cst-to-ast:type-wrap-argument
    (ast ctype origin env (system clasp-cleavir:clasp))
  ;; Insert an untrusted THE at non-high safety and insert type checks
  ;; on high safety, unless the type is top in which case we do nothing.
  ;; NOTE that the type check doesn't check &optional and &rest. FIXME?
  (let ((insert-type-checks
          (policy:policy-value
           (cleavir-env:policy (cleavir-env:optimize-info env))
           'type-check-ftype-arguments))
        (required (cleavir-ctype:values-required ctype system)))
    (declare (ignore insert-type-checks))
    (cond ((or (every (lambda (ty) (cleavir-ctype:top-p ty system)) required)
               (null required))
           ast)
          ((some (lambda (ty) (cleavir-ctype:bottom-p ty system)) required)
           ;; KLUDGE: This isn't actually a check; if the restriction is
           ;; violated bad things will happen. The right thing to do would be
           ;; to call ERROR, but I don't want to worry about the recursion
           ;; (as ERROR is declared as returning NIL type) right this second.
           (cleavir-ast:make-progn-ast
            (list ast (cleavir-ast:make-unreachable-ast :origin origin))))
          #+(or) ;; FIXME: doesn't work yet for some reason.
          (insert-type-checks
           (let ((check
                   (cst-to-ast:convert
                    (cst:cst-from-expression
                     (make-type-check-form ctype system))
                    env system)))
             (cleavir-ast:make-the-ast ast ctype check :origin origin)))
          (t (cleavir-ast:make-the-ast ast ctype nil :origin origin)))))

(defmethod cst-to-ast:type-wrap-return-values
    (ast ctype origin env (system clasp-cleavir:clasp))
  ;; FIXME: same semantics as TYPE-WRAP but should probably use the
  ;; different policy value. didn't want to copy and paste though.
  (cst-to-ast:type-wrap ast ctype origin env system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:DEBUG-MESSAGE
;;;
;;; This is converted into a call to print a message
;;;

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core:debug-message)) cst environment
     (system clasp-cleavir:clasp))
  (declare (ignore environment))
  (assert (typep (cst:raw (cst:second cst)) 'string))
  (make-instance 'clasp-cleavir-ast:debug-message-ast
    :debug-message (cst:raw (cst:second cst))
    :origin cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:DEBUG-BREAK
;;;
;;; This is converted into a call to invoke the debugger
;;;

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core:debug-break)) cst environment
     (system clasp-cleavir:clasp))
  (declare (ignore environment))
  (make-instance 'clasp-cleavir-ast:debug-break-ast :origin cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:multiple-value-foreign-CALL
;;;
;;; This is converted into an intrinsic call
;;;

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core:multiple-value-foreign-call)) cst environment
     (system clasp-cleavir:clasp))
  (assert (stringp (cst:raw (cst:second cst))))
  (make-instance 'clasp-cleavir-ast:multiple-value-foreign-call-ast
    :function-name (cst:raw (cst:second cst))
    :argument-asts (cst-to-ast::convert-sequence (cst:rest (cst:rest cst)) environment system)
    :origin cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:FOREIGN-call
;;;
;;; This is converted into a pointer call
;;;

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core:foreign-call)) cst environment
     (system clasp-cleavir:clasp))
  (assert (listp (cst:raw (cst:second cst))))
  (assert (stringp (cst:raw (cst:third cst))))
  (make-instance 'clasp-cleavir-ast:foreign-call-ast
    :foreign-types (cst:raw (cst:second cst))
    :function-name (cst:raw (cst:third cst))
    :argument-asts (cst-to-ast::convert-sequence (cst:rest (cst:rest (cst:rest cst))) environment system)
    :origin cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:foreign-call-pointer
;;;
;;; This is converted into a pointer call
;;;

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core:foreign-call-pointer)) cst environment (system clasp-cleavir:clasp))
  (assert (listp (cst:raw (cst:second cst))))
  (make-instance 'clasp-cleavir-ast:foreign-call-pointer-ast
    :foreign-types (cst:raw (cst:second cst))
    :argument-asts (cst-to-ast::convert-sequence (cst:rest (cst:rest cst)) environment system)
    :origin cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:DEFCALLBACK
;;;

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core:defcallback)) form env (system clasp-cleavir:clasp))
  (cst:db origin (name convention rtype rtrans atypes atrans params placeholder lisp-callback)
          (cst:rest form)
          (let ((args (list (cst:raw name) (cst:raw convention)
                            (cst:raw rtype) (cst:raw rtrans)
                            (cst:raw atypes) (cst:raw atrans)
                            (cst:raw params) (cst:raw placeholder))))
            (make-instance 'cc-ast:defcallback-ast
              :args args
              :callee (cst-to-ast:convert lisp-callback env system)
              :origin form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functionlikes
;;; See their AST classes for more info probably
;;;

(define-functionlike-special-form core::header-stamp cc-ast:header-stamp-ast (:arg))
(define-functionlike-special-form core::rack-stamp cc-ast:rack-stamp-ast (:arg))
(define-functionlike-special-form core::wrapped-stamp cc-ast:wrapped-stamp-ast (:arg))
(define-functionlike-special-form core::derivable-stamp cc-ast:derivable-stamp-ast (:arg))

(define-functionlike-special-form core::instance-cas cc-ast:slot-cas-ast
  (:cmp-ast :value-ast :object-ast :slot-number-ast))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CASs and other atomic ops are mostly functionlike, except for the order.

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::fence)) cst env (system clasp-cleavir:clasp))
  (declare (ignore env))
  (cst:db origin (f order) cst
    (declare (ignore f))
    (make-instance 'cc-ast:fence-ast :order (cst:raw order) :origin cst)))

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::car-atomic)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (cr order cons) cst
    (declare (ignore cr))
    (make-instance 'cc-ast:atomic-car-ast
      :order (cst:raw order) :origin cst
      :cons-ast (cst-to-ast:convert cons env system))))
(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::cdr-atomic)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (cr order cons) cst
    (declare (ignore cr))
    (make-instance 'cc-ast:atomic-cdr-ast
      :order (cst:raw order) :origin cst
      :cons-ast (cst-to-ast:convert cons env system))))
(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::rplaca-atomic)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (rp order nv cons) cst
    (declare (ignore rp))
    (make-instance 'cc-ast:atomic-rplaca-ast
      :order (cst:raw order) :origin cst
      :object-ast (cst-to-ast:convert nv env system)
      :cons-ast (cst-to-ast:convert cons env system))))
(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::rplacd-atomic)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (rp order nv cons) cst
    (declare (ignore rp))
    (make-instance 'cc-ast:atomic-rplacd-ast
      :order (cst:raw order) :origin cst
      :object-ast (cst-to-ast:convert nv env system)
      :cons-ast (cst-to-ast:convert cons env system))))
(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::cas-car)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (cas order cmp value cons) cst
    (declare (ignore cas))
    (make-instance 'cc-ast:cas-car-ast
      :order (cst:raw order) :origin cst
      :cmp-ast (cst-to-ast:convert cmp env system)
      :value-ast (cst-to-ast:convert value env system)
      :cons-ast (cst-to-ast:convert cons env system))))
(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::cas-cdr)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (cas order cmp value cons) cst
    (declare (ignore cas))
    (make-instance 'cc-ast:cas-cdr-ast
      :order (cst:raw order) :origin cst
      :cmp-ast (cst-to-ast:convert cmp env system)
      :value-ast (cst-to-ast:convert value env system)
      :cons-ast (cst-to-ast:convert cons env system))))

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::atomic-rack-read)) cst env
     (system clasp-cleavir:clasp))
  (cst:db origin (read order rack index) cst
    (declare (ignore read))
    (make-instance 'cc-ast:atomic-rack-read-ast
      :order (cst:raw order) :origin cst
      :rack-ast (cst-to-ast:convert rack env system)
      :slot-number-ast (cst-to-ast:convert index env system))))
(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::atomic-rack-write)) cst env
     (system clasp-cleavir:clasp))
  (cst:db origin (wr order nv rack index) cst
    (declare (ignore wr))
    (make-instance 'cc-ast:atomic-rack-write-ast
      :order (cst:raw order) :origin cst
      :value-ast (cst-to-ast:convert nv env system)
      :rack-ast (cst-to-ast:convert rack env system)
      :slot-number-ast (cst-to-ast:convert index env system))))
(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::cas-rack)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (cas order cmp new rack index) cst
    (declare (ignore cas))
    (make-instance 'cc-ast:cas-rack-ast
      :order (cst:raw order) :origin cst
      :cmp-ast (cst-to-ast:convert cmp env system)
      :value-ast (cst-to-ast:convert new env system)
      :rack-ast (cst-to-ast:convert rack env system)
      :slot-number-ast (cst-to-ast:convert index env system))))

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::atomic-vref)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (vr order uaet vector index) cst
    (declare (ignore vr))
    (make-instance 'cc-ast:atomic-vref-ast
      :order (cst:raw order) :element-type (cst:raw uaet) :origin cst
      :array-ast (cst-to-ast:convert vector env system)
      :index-ast (cst-to-ast:convert index env system))))
(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::atomic-vset)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (vr order uaet nv vector index) cst
    (declare (ignore vr))
    (make-instance 'cc-ast:atomic-vset-ast
      :order (cst:raw order) :element-type (cst:raw uaet) :origin cst
      :value-ast (cst-to-ast:convert nv env system)
      :array-ast (cst-to-ast:convert vector env system)
      :index-ast (cst-to-ast:convert index env system))))
(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::vcas)) cst env (system clasp-cleavir:clasp))
  (cst:db origin (vr order uaet cmp nv vector index) cst
    (declare (ignore vr))
    (make-instance 'cc-ast:vcas-ast
      :order (cst:raw order) :element-type (cst:raw uaet) :origin cst
      :cmp-ast (cst-to-ast:convert cmp env system)
      :value-ast (cst-to-ast:convert nv env system)
      :array-ast (cst-to-ast:convert vector env system)
      :index-ast (cst-to-ast:convert index env system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE::HEADER-STAMP-CASE
;;;

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::header-stamp-case)) cst env
     (system clasp-cleavir:clasp))
  (cst:db origin (stamp derivable rack wrapped header) (cst:rest cst)
    (cleavir-ast:make-branch-ast
     (clasp-cleavir-ast:make-header-stamp-case-ast
      (cst-to-ast:convert stamp env system)
      cst)
     (list (cst-to-ast:convert derivable env system)
           (cst-to-ast:convert rack env system)
           (cst-to-ast:convert wrapped env system))
     (cst-to-ast:convert header env system)
     :origin cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting core::local-tagbody to tagbody
;;;        and core::local-block to block
;;;

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::local-tagbody)) cst environment
     (system clasp-cleavir:clasp))
  (cst-to-ast:convert-special 'tagbody cst environment system))

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::local-block)) cst environment
     (system clasp-cleavir:clasp))
  (cst-to-ast:convert-special 'block cst environment system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THROW
;;;
;;; Convert throw into a call

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'cl:throw)) cst environment (system clasp-cleavir:clasp))
  (cst:db origin (tag result-cst) 
      (cst:rest cst)
    ;; If I don't use a throw-ast node use the following
    #+(or)
    (cst-to-ast::convert `(core:throw-function ,tag ,result-cst) environment system)
    ;; If I decide to go with a throw-ast node use the following
    (clasp-cleavir-ast:make-throw-ast
     (cst-to-ast:convert tag environment system)
     (cst-to-ast:convert result-cst environment system)
     cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE::BIND-VASLIST
;;;

(defmethod cst-to-ast:convert-special ((symbol (eql 'core::bind-vaslist)) cst
                                       environment (system clasp-cleavir:clasp))
  ;; (bind-vaslist lambda-list va . body)
  ;; => `(apply (lambda ,lambda-list . ,body) ,va)
  (cst:db origin (op lambda-list-cst vaslist-cst . body-cst) cst
    (declare (ignore op))
    (cst-to-ast:convert
     (cst:quasiquote origin
                     (apply (lambda (cst:unquote lambda-list-cst)
                              (cst:unquote-splicing body-cst))
                            (cst:unquote vaslist-cst)))
     environment system)))

#+(or) ;;#+cst
(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core::bind-vaslist)) cst environment (system clasp-cleavir:clasp))
  (cst:db origin (op lambda-list-cst vaslist-cst . body-cst) cst
          (declare (ignore op))
          (let ((parsed-lambda-list
                  (cst:parse-ordinary-lambda-list system lambda-list-cst :error-p nil)))
            (when (null parsed-lambda-list)
              (error 'cst-to-ast::malformed-lambda-list
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
                    (cst-to-ast::itemize-declaration-specifiers-by-parameter-group
                     (cst-to-ast::itemize-lambda-list parsed-lambda-list)
                     canonicalized-dspecs)
                  (multiple-value-bind (lexical-lambda-list entries)
                      (cst-to-ast::lambda-list-from-parameter-groups
                       (cst:children parsed-lambda-list))
                    (let ((ast (cst-to-ast::process-parameter-groups
                                (cst:children parsed-lambda-list)
                                idspecs entries
                                (cst-to-ast::make-body
                                 rdspecs
                                 (cst-to-ast::cst-for-body forms-cst nil origin))
                                environment system)))
                      (cc-ast:make-bind-vaslist-ast
                       lexical-lambda-list
                       (cst-to-ast::convert vaslist-cst environment system)
                       ast
                       nil ; FIXME: handle rest-alloc (parse &rest from lambda list)
                       :origin origin)))))))))

(defmethod cst-to-ast:convert-global-function-reference
    (cst info global-env (system clasp-cleavir:clasp))
  (declare (ignore global-env))
  (let ((name (cleavir-env:name info)))
    (cond
      ((and (consp name) (eq (car name) 'cl:setf))
       (clasp-cleavir-ast:make-setf-fdefinition-ast
        (cleavir-ast:make-constant-ast (cadr name) :origin cst)
        :attributes (cleavir-env:attributes info) :origin cst))
      ((consp name)
       (error "Illegal name for function - must be (setf xxx)"))
      (t
       (cleavir-ast:make-fdefinition-ast
        (cleavir-ast:make-constant-ast name :origin cst)
        :attributes (cleavir-env:attributes info) :origin cst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CORE:INSTANCE-REF/SET
;;;
;;; FIXME: Maybe just use the primops instead.

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core:instance-ref)) cst environment
     (system clasp-cleavir:clasp))
  (cst-to-ast:convert-special 'cleavir-primop:slot-read cst environment system))

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

(defmethod cst-to-ast:convert-special-binding
    (variable-cst value-ast next-ast env (system clasp-cleavir:clasp))
  (make-instance 'cc-ast:bind-ast
    :name-ast (cst-to-ast:convert-constant variable-cst env system)
    :value-ast value-ast
    :body-ast next-ast
    :origin variable-cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Enhance source locations.

;;; Called by ext::special-operator-source-locations in source-location.lisp
(defun special-operator-source-locations (name)
  ;; We check for explicit specialization rather than using
  ;; compute-applicable-methods so that general around methods etc. are not
  ;; included; they're not really germane to the particular operator.
  (ignore-errors
   (mapcan #'ext::method-source-location
           (remove-if-not
            (lambda (method)
              (let ((spec (first (clos:method-specializers method))))
                (and (typep spec 'clos:eql-specializer)
                     (eq (clos:eql-specializer-object spec) name))))
            (clos:generic-function-methods
             #'cleavir-cst-to-ast:convert-special)))))
