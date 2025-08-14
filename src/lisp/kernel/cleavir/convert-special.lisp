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
          (cst:reconstruct ,system (progn ,@body) ,cst))
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
  (let ((cf `(core:coerce-called-fdesignator ,function-form)))
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

(defun values-top-p (ctype system)
  (and (null (ctype:values-required ctype system))
       (loop for ct in (ctype:values-optional ctype system)
             always (ctype:top-p ct system))
       (ctype:top-p (ctype:values-rest ctype system) system)))

(defun values-bottom-p (ctype system)
  (some (lambda (ct) (ctype:bottom-p ct system))
        (ctype:values-required ctype system)))

(defun insert-type-checks-level (policy context)
  (policy:policy-value
   policy
   (ecase context
     ((:the :variable :setq) 'type-check-the)
     ((:argument) 'type-check-ftype-arguments)
     ((:return) 'type-check-ftype-return-values))))

(defmethod cst-to-ast:type-wrap
    (ast ctype context origin env (system clasp-cleavir:clasp))
  (let ((sv-ctype-p (member context '(:variable :argument :setq))))
    (if (if sv-ctype-p
            (ctype:top-p ctype system)
            (values-top-p ctype system))
        ;; The type is too boring to note under any policy.
        ast
        ;; Do something.
        (let* ((policy (env:policy (env:optimize-info system env)))
               (insert-type-checks
                 (insert-type-checks-level policy context))
               (vctype (ecase context
                         ((:the :return) ctype)
                         ((:variable) (ctype:single-value ctype system))
                         ((:argument :setq) (ctype:coerce-to-values ctype system))))
               (botp (if sv-ctype-p
                         (ctype:bottom-p ctype system)
                         (values-bottom-p ctype system))))
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
;;; Converting CORE:INSTANCE-REF
;;;
;;; FIXME: Maybe just use the primops instead.
;;; FIXME: Is this code even relevant any more? If it is, update for
;;; (setf si:instance-ref).

(defmethod cst-to-ast:convert-special
    ((symbol (eql 'core:instance-ref)) cst environment
     (system clasp-cleavir:clasp))
  (cst-to-ast:convert-special 'cleavir-primop:slot-read cst environment system))

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
