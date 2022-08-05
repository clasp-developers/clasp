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

(defun make-general-type-check-fun (ctype system)
  (let* ((req (ctype:values-required ctype system))
         (lreq (length req))
         (opt (ctype:values-optional ctype system))
         (lopt (length opt))
         (rest (ctype:values-rest ctype system))
         (rest-top-p (ctype:top-p rest system))
         (rest-bot-p (ctype:bottom-p rest system))
         (reqvars (loop repeat lreq collect (gensym "CHECKED")))
         (reqp (loop repeat lreq collect (gensym "CHECKED-PRESENT-P")))
         (optvars (loop repeat lopt collect (gensym "CHECKED")))
         (optp (loop repeat lopt collect (gensym "CHECKED-PRESENT-P")))
         (restvar (gensym "REST")))
    (flet ((one (var type)
             (let ((dtype (discrimination-type type)))
               `(unless (typep ,var ',dtype)
                  ;; We use the discrimination type in the error as well.
                  ;; The ANSI tests expect to be able to use it as a discrimination
                  ;; type, and we might as well comply.
                  (error 'type-error :datum ,var :expected-type ',dtype))))
           (mash (var -p) `(,var nil ,-p)))
      `(lambda (&optional ,@(mapcar #'mash reqvars reqp)
                  ,@(mapcar #'mash optvars optp)
                &rest ,restvar)
         (declare (core:lambda-name ,(make-symbol (format nil "~a-CHECKER" ctype)))
                  (ignorable ,@reqp)
                  (optimize (safety 0)))
         ,@(loop for reqv in reqvars
                 for -p in reqp
                 for ty in req
                 ;; If the type doesn't include NIL, we can skip checking -p since
                 ;; the type check will be (typep nil whatever) which will fail.
                 unless (ctype:disjointp ty 'null system)
                   ;; FIXME: Better error
                   collect `(unless ,-p
                              (error "Required parameter of type ~s not provided"
                                     ',ty))
                 collect (one reqv ty))
         ,@(loop for optv in optvars
                 for -p in optp
                 for ty in opt
                 ;; Our default is NIL, so if the type includes NIL we don't need
                 ;; to check the presence, but otherwise we do.
                 collect (if (ctype:disjointp ty 'null system)
                             `(when ,-p ,(one optv ty))
                             (one optv ty)))
         ,@(cond (rest-top-p nil)
                 (rest-bot-p
                  `((when ,restvar
                      (error 'type-error :datum (first ,restvar) :expected-type 'nil))))
                 (t
                  (let ((rv (gensym "CHECKED")))
                    `((loop for ,rv in ,restvar do ,(one rv rest))))))
         ;; Check successful, now return the values
         ,(if rest-bot-p
              `(cond ,@(loop for optvs on (reverse optvars) for -p in (reverse optp)
                             collect `(,-p (values ,@reqvars ,@(reverse optvs))))
                     (t (values ,@reqvars)))
              `(multiple-value-call #'values
                 ,@reqvars
                 (cond
                   ,@(loop for optvs on (reverse optvars) for -p in (reverse optp)
                           collect `(,-p (values ,@(reverse optvs))))
                   (t (values)))
                 (values-list ,restvar)))))))

(defun make-type-check-fun (context ctype system)
  (flet ((one (var type)
           (let ((dtype (discrimination-type type)))
             `(unless (typep ,var ',dtype)
                (error 'type-error :datum ,var :expected-type ',dtype)))))
    (ecase context
      ((:variable)
       (let ((var (gensym "CHECKED")))
         `(lambda (,var)
            (declare (core:lambda-name ,(make-symbol (format nil "~a-CHECKER" ctype)))
                     (optimize (safety 0)))
            ,(one var ctype)
            ,var)))
      ((:setq :argument)
       (let ((var (gensym "CHECKED")) (ignore (gensym "IGNORE")))
         `(lambda (&optional ,var &rest ,ignore)
            (declare (core:lambda-name ,(make-symbol (format nil "~a-CHECKER" ctype)))
                     (ignore ,ignore) (optimize (safety 0)))
            ,(one var ctype)
            ,var)))
      ((:the :return)
       (make-general-type-check-fun ctype system)))))

(defun unreachability-error (ast origin policy env system)
  ;; FIXME: Better error (condition type at least)
  (ast:make-progn-ast
   (list ast
         (cst-to-ast:convert
          (cst:cst-from-expression
           '(locally
             ;; Avoid recursion
             (declare (optimize (type-check-ftype-return-values 0)))
             (error "Unreachable"))
           :source origin)
          env system)
         (ast:make-unreachable-ast :origin origin :policy policy))
   :origin origin :policy policy))

(defmethod cst-to-ast:type-wrap
    (ast ctype context origin env (system clasp-cleavir:clasp))
  (let ((sv-ctype-p (member context '(:variable :argument :setq))))
    (if (if sv-ctype-p
            (ctype:top-p ctype system)
            (and (null (ctype:values-required ctype system))
                 (loop for ct in (ctype:values-optional ctype system)
                       always (ctype:top-p ct system))
                 (ctype:top-p (ctype:values-rest ctype system) system)))
        ;; The type is too boring to note under any policy.
        ast
        ;; Do something.
        (let* ((policy (env:policy (env:optimize-info env)))
               (insert-type-checks
                 (policy:policy-value
                  policy
                  (ecase context
                    ((:the :variable :setq) 'type-check-the)
                    ((:argument) 'type-check-ftype-arguments)
                    ((:return) 'type-check-ftype-return-values))))
               (vctype (ecase context
                         ((:the :return) ctype)
                         ((:variable) (ctype:single-value ctype system))
                         ((:argument :setq) (ctype:coerce-to-values ctype system))))
               (botp (if sv-ctype-p
                         (ctype:bottom-p ctype system)
                         (some (lambda (ct) (ctype:bottom-p ct system))
                               (ctype:values-required ctype system)))))
          (ecase insert-type-checks
            ((0) ; trust without checking. Unsafe! 
             (if botp
                 ;; Type is bottom, so rather than THE, mark unreachable.
                 (ast:make-progn-ast
                  (list ast (cleavir-ast:make-unreachable-ast
                             :origin origin :policy policy))
                  :origin origin :policy policy)
                 ;; Trusted THE
                 (ast:make-the-ast ast vctype :trusted :origin origin :policy policy)))
            ((1) ; note but don't use, i.e. make an untrusted+unchecked THE.
             (ast:make-the-ast ast vctype nil :origin origin :policy policy))
            ((2 3) ; Check
             (if botp
                 ;; This is unreachable, so insert an error.
                 (unreachability-error ast origin policy env system)
                 ;; Type check
                 (ast:make-the-ast ast vctype
                                   (cst-to-ast:convert
                                    (cst:cst-from-expression
                                     (make-type-check-fun context ctype system)
                                     :source origin)
                                    env system)
                                   :origin origin :policy policy))))))))

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
