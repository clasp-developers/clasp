;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

(defgeneric make-method-lambda (generic-function method lambda-expression environment))
(defgeneric function-keywords (method))

;;; Add type declarations for the arguments of a METHOD. This implies
;;; copying the method arguments because the arguments may be modified.
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defparameter *add-method-argument-declarations* nil)
)

(defun wrap-contf-lexical-function-binds (form contsym cnm-p nnmp-p
                                          default-cnm-form)
  `(macrolet (,@(when (eq cnm-p 't)
                  `((call-next-method (&rest args)
                      (if (null args)
                          ',default-cnm-form
                          (list* 'funcall ',contsym args)))))
              ,@(when (eq nnmp-p 't)
                  `((next-method-p ()
                      '(typep ,contsym '(not %no-next-method-continuation))))))
     (flet (,@(when (eq cnm-p 'function)
                `((call-next-method (&rest cnm-args)
                    (if (null cnm-args)
                        ,default-cnm-form
                        (apply ,contsym cnm-args)))))
            ,@(when (eq nnmp-p 'function)
                `((next-method-p ()
                    (typep ,contsym '(not %no-next-method-continuation))))))
       ,form)))

(defun contf-lambda (lambda-list lambda-name decls doc body
                     call-next-method-p no-next-method-p-p)
  (let ((contsym (gensym "METHOD-CONTINUATION")))
    (multiple-value-bind (req opt rest keyf keys aok-p aux)
        (core:process-lambda-list lambda-list 'function)
      (declare (ignore keys aok-p))
      (if (or (not (zerop (car opt))) rest keyf)
          `(lambda (,contsym &rest .method-args.)
             (declare (core:lambda-name ,lambda-name))
             ,@(when doc (list doc))
             ,(wrap-contf-lexical-function-binds
               `(apply (lambda ,lambda-list ,@decls ,@body) .method-args.)
               contsym call-next-method-p no-next-method-p-p
               `(apply ,contsym .method-args.)))
          ;; We have only required parameters. This allows us to use a function
          ;; that doesn't APPLY so much.
          ;; We have to rebind the required parameters so that call-next-method
          ;; can get at the originals, e.g. when the method body SETQs a param.
          (let ((req-aliases (loop for r in (rest req)
                                   collect (gensym (symbol-name r))))
                (aux-binds
                  (loop for (var init) on aux by #'cddr
                        collect `(,var ,init))))
            `(lambda (,contsym ,@req-aliases)
               (declare (core:lambda-name ,lambda-name))
               ,@(when doc (list doc))
               ,(wrap-contf-lexical-function-binds
                 `(let* (,@(mapcar #'list (rest req) req-aliases)
                         ,@aux-binds)
                    ,@decls
                    ,@body)
                 contsym call-next-method-p no-next-method-p-p
                 `(funcall ,contsym ,@req-aliases))))))))


;;; ----------------------------------------------------------------------
;;; DEFMETHOD
;;;

(defun maybe-augment-generic-function-lambda-list (gf method-lambda-list)
  "Add any &key parameters from method-lambda-list that are missing
in the generic function lambda-list to the generic function lambda-list"
  (let ((gf-lambda-list-all (ext:function-lambda-list gf)))
    (if (eq gf-lambda-list-all (core:unbound)) ; uninitialized
        (core:setf-lambda-list gf method-lambda-list)
        (let* ((has-aok (member '&allow-other-keys gf-lambda-list-all))
               (gf-lambda-list (if has-aok
                                   (butlast gf-lambda-list-all 1)
                                   gf-lambda-list-all)))
          (flet ((match-key (entry)
                   (cond
                     ((symbolp entry)
                      (intern (string entry) :keyword))
                     ((and (consp entry) (consp (car entry)))
                      (car (car entry)))
                     ((consp entry)
                      (intern (string (car entry)) :keyword))
                     (t (error "Illegal keyword parameter ~a" entry)))))
            (let* ((keys-gf (member '&key gf-lambda-list)))
              (when keys-gf
                (let ((keys-method (member '&key method-lambda-list))
                      append-keys)
                  (dolist (k (cdr keys-method))
                    (when (not (member (match-key k) keys-gf :key #'match-key))
                      (push k append-keys)))
                  (when append-keys
                    (let ((new-ll (append gf-lambda-list append-keys
                                          (if has-aok (list '&allow-other-keys) nil))))
                      (core:setf-lambda-list gf new-ll)))))))))))

(defun prototypes-for-make-method-lambda (name)
  (let ((gf? (and (fboundp name) (fdefinition name))))
    (if (or (null gf?) (not (si:instancep gf?)))
        (values (class-prototype #.(find-class 'standard-generic-function))
                (class-prototype #.(find-class 'standard-method)))
        (values gf?
                (class-prototype (or (generic-function-method-class gf?)
                                   #.(find-class 'standard-method)))))))

;;; Is this lambda form one returned by our make-method-lambda method (below)?
;;; If it is, we know what it does so we can kind of ignore it.
;;; See method-function.lisp
(defun our-method-lambda-p (method-lambda)
  ;; This is pretty KLUDGEy. But make-method-lambda pretty specifically has
  ;; to return a lambda expression, so there's only so much we can do as far
  ;; as magical indications go.
  (and (consp method-lambda)
       (eq (car method-lambda) 'lambda)
       (consp (cdr method-lambda))
       (equal (second method-lambda) '(.method-args. .next-methods.))))

;;; Does the work of calling make-method-lambda, and returns the same values.
;;; In the event the method lambda is the one returned by the standard method,
;;; instead of a lambda expression, this will return a form to create a
;;; %method-function with some quicker to use internal function (see above).
(defun method-lambda (name lambda-expression env
                      lambda-name lambda-list body declarations documentation)
  (multiple-value-bind (generic-function method)
      (prototypes-for-make-method-lambda name)
    (multiple-value-bind (fn-form options)
        (make-method-lambda generic-function method lambda-expression env)
      (let* ((cnm-p* (or (second (member ''call-next-method-p options
                                         :test #'equal))
                       'function))
             (nmp-p* (or (second (member ''next-method-p-p options
                                         :test #'equal))
                       'function))
             ;; account for extra quoting for evaluation
             (cnm-p (cond ((equal cnm-p* ''nil) nil)
                          ((equal cnm-p* ''t) t)
                          (t 'function)))
             (nmp-p (cond ((equal nmp-p* ''nil) nil)
                          ((equal nmp-p* ''t) t)
                          (t 'function))))
        (values
         (if (our-method-lambda-p fn-form)
             (if (not (or cnm-p nmp-p))
                 `(make-%leaf-method-function ,lambda-expression)
                 `(make-%contf-method-function
                   ,(contf-lambda lambda-list lambda-name declarations documentation body
                                  cnm-p nmp-p)))
             fn-form)
         options)))))

(defmacro defmethod (name &rest args &environment env)
  (let* ((qualifiers (loop while (and args (not (listp (first args))))
			collect (pop args)))
	 (specialized-lambda-list
	  (if args
	      (pop args)
	      (error "Illegal defmethod form: missing lambda list")))
	 (body args))
    (multiple-value-bind (lambda-list
                          required-parameters specializers specializedps)
        (parse-specialized-lambda-list specialized-lambda-list)
      (multiple-value-bind (lambda-expression lambda-name fn-lambda-list body
                            declarations documentation)
	  (make-raw-lambda name lambda-list
                           required-parameters specializers specializedps
                           body qualifiers)
        (multiple-value-bind (fn-form options)
            (method-lambda
             name lambda-expression env
             lambda-name fn-lambda-list body declarations documentation)
          `(progn
             (eval-when (:compile-toplevel)
               (cmp:register-global-function-def 'defmethod ',name))
             (install-method ',name ',qualifiers
                             ,(specializers-expression specializers)
                             ',lambda-list
                             ,fn-form
                             ;; Note that we do not quote the options returned by
                             ;; make-method-lambda. MOP is in my view ambiguous about whether
                             ;; they're supposed to be quoted. There's an example that sort of
                             ;; implies they are, but the extra flexibility is pretty convenient,
                             ;; and matches that the primary value is of course evaluated.
                             ,@(when documentation
                                 `(:documentation ',documentation))
                             ,@options)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun specializers-expression (specializers)
  `(list
    ,@(loop for spec in specializers
            collect (ext:with-current-source-form (spec)
                      (etypecase spec
                        (symbol `(find-class ',spec))
                        ((cons (eql eql) (cons t null)) ; (eql #<anything>)
                         `(intern-eql-specializer ,(second spec)))
                        ;; CLHS DEFMETHOD seems to say literal specializers are
                        ;; not allowed, but i'm not sure...
                        (specializer spec))))))

;;; Given a list of specializer expressions, return something that can be
;;; put in a function name.
(defun fixup-specializers (specializers)
  (mapcar (lambda (spec)
            (if (consp spec) ; eql specializer
                (let ((form (second spec)))
                  ;; try to display the most common case, (eql 'foo), nicely.
                  (if (and (consp form) (eq (car form) 'quote)
                           (consp (cdr form)) (null (cddr form)))
                      (let ((val (ext:constant-form-value form)))
                        (if (or (symbolp val) (numberp val))
                            `(eql ',val)
                            `(eql ,(make-symbol (prin1-to-string form)))))
                      `(eql ,(make-symbol (prin1-to-string form)))))
                spec))
          specializers))
)

(defun fixup-method-lambda-list (lambda-list)
  ;; According to CLHS 7.6.4.,
  ;; "The use of &allow-other-keys need not be consistent across lambda lists.
  ;;  If &allow-other-keys is mentioned in the lambda list of any applicable method
  ;;   or of the generic function, any keyword arguments may be mentioned in the call
  ;;   to the generic function."
  ;; and "The checking of the validity of keyword names is done in the generic function,
  ;;       not in each method."
  ;; Conceptually, this means that effective methods have a set of valid keywords
  ;; but the individual method _functions_ don't. Though the methods still have the
  ;; lambda list provided to them.
  ;; Long story short, we insert an &allow-other-keys if &key is present without it.
  (multiple-value-bind (required optional restvar
                        keyflag keys allow-other-keys aux va-rest-p)
      (core:process-lambda-list lambda-list 'function)
    (if (or (not keyflag) allow-other-keys) ; nothing to be done
        lambda-list
        ;; Reconstruct the lambda list.
        `(,@(rest required)
          ,@(unless (zerop (first optional)) '(&optional))
          ,@(loop for (var default -p) on (rest optional) by #'cdddr
                  collect `(,var ,default ,@(when -p (list -p))))
          ,@(when restvar
              `(,(if va-rest-p 'core:&va-rest '&rest) ,restvar))
          &key
          ,@(loop for (key var default -p) on (rest keys) by #'cddddr
                  collect `((,key ,var) ,default ,@(when -p (list -p))))
          &allow-other-keys
          ,@(when aux '(&aux))
          ,@(loop for (var default) on aux by #'cddr
                  collect `(,var ,default))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-raw-lambda (name lambda-list
                        required-parameters specializers specializedps
                        body qualifiers)
  (multiple-value-bind (declarations real-body documentation)
      (sys::find-declarations body t)
    (let* ((qualifiers (if qualifiers (list qualifiers)))
           (copied-variables '())
           (class-declarations
             (when *add-method-argument-declarations*
               (loop for name in required-parameters
                     for type in specializers
                     for specializedp in specializedps
                     when (and specializedp (symbolp type))
                       do (push `(,name ,name) copied-variables) and
                     nconc `((type ,type ,name)
                             (si::no-check-type ,name)))))
           (ignorability-declaration
             `(ignorable ,@(loop for name in required-parameters
                                 for specializedp
                                   in specializedps
                                 when specializedp
                                   collect name)))
           (total-declarations
             (list* `(declare ,@class-declarations)
                    `(declare ,ignorability-declaration)
                    declarations))
           (block `(block ,(si::function-block-name name) ,@real-body))
           (lambda-list (fixup-method-lambda-list lambda-list))
           (lambda-name `(method ,name ,@qualifiers ,(fixup-specializers specializers)))
           (method-lambda
            ;; Remove the documentation string and insert the
            ;; appropriate class declarations.  The documentation
            ;; string is removed to make it easy for us to insert
            ;; new declarations later, they will just go after the
            ;; second of the method lambda.  The class declarations
            ;; are inserted to communicate the class of the method's
            ;; arguments to the code walk.
             `(lambda ,lambda-list
                (declare (core:lambda-name ,lambda-name))
                ,@total-declarations
                ,(if copied-variables
                     `(let* ,copied-variables
                        (declare
                         (ignorable ,@(mapcar #'first copied-variables)))
                        ,block)
                     block))))
      (values method-lambda lambda-name lambda-list (list block)
              total-declarations documentation))))
)

(defmethod make-method-lambda ((gf standard-generic-function)
                               (method standard-method) method-lambda env)
  (declare (ignore gf method))
  (multiple-value-bind (call-next-method-p next-method-p-p)
      (walk-method-lambda method-lambda env)
    (multiple-value-bind (declarations body doc)
        (si:process-declarations (cddr method-lambda) t) ; We expect docstring
      ;; source location here?
      (let ((lambda-list (second method-lambda))
            (lambda-name-declaration (or (find 'core::lambda-name declarations :key #'car)
                                       '(core:lambda-name make-method-lambda.lambda))))
        ;; Note that this specific (.method-args. .next-methods.) lambda list is used
        ;; above to identify our method lambdas, so be conscientious if you change it.
        (values `(lambda (.method-args. .next-methods.)
                   (declare ,lambda-name-declaration)
                   ,@(when doc (list doc))
                   ,(gen-lexical-method-function-binds
                     call-next-method-p next-method-p-p
                     ;; FIXME: This might not work if the user is perverse enough to
                     ;; name a variable &whole, or something like that?
                     `(destructuring-bind ,lambda-list .method-args.
                        (declare ,@declarations)
                        ,@body)))
                ;; double quotes as per evaluation, explained above in defmethod.
                (list ''call-next-method-p `',call-next-method-p
                      ''next-method-p-p `',next-method-p-p))))))

;;; We want to avoid consing closures for call-next-method and next-method-p when possible,
;;; which is most of the time. We don't need a closure for just (call-next-method).
;;; The right way to do this would be to flet unconditionally, declare the functions inline,
;;; and let the compiler handle it. We're getting there, but right now the compiler isn't
;;; smart enough. So what we do is use a macrolet in the case that the walker only found
;;; (call-next-method ...) at worst.
;;; NOTE: We still cons a closure for (call-next-method actual-args...) because of how
;;; &va-rest works, so that's suboptimal.
(defun gen-lexical-method-function-binds (call-next-method-p next-method-p-p form)
  (cond ((or (eq call-next-method-p 'function) (eq next-method-p-p 'function))
         `(flet ((call-next-method (&rest args)
                   (if (null .next-methods.)
                       ;; FIXME: should call no-next-method.
                       ;; This is hard, because the method doesn't exist when this
                       ;; function is created.
                       (error "No next method")
                       (funcall (method-function (car .next-methods.))
                                (if (null args) .method-args. args)
                                (cdr .next-methods.))))
                 (next-method-p () (and .next-methods. t)))
            ,form))
        ((or call-next-method-p next-method-p-p)
         `(macrolet ((call-next-method (&rest args)
                       `(if (null .next-methods.)
                            (error "No next method")
                            (funcall (method-function (car .next-methods.))
                                     ,(if args `(list ,@args) '.method-args.)
                                     (cdr .next-methods.))))
                     (next-method-p () '(and .next-methods. t)))
            ,form))
        (t form)))

(defun walk-method-lambda (method-lambda env)
  (let ((call-next-method-p nil)
        (next-method-p-p nil))
    (flet ((code-walker (form env)
             (declare (ignore env))
	     (unless (atom form)
	       (let ((name (first form)))
		 (case name
		   (CALL-NEXT-METHOD
		    (setf call-next-method-p
			  (or call-next-method-p T)))
		   (NEXT-METHOD-P
                    (setf next-method-p-p t))
		   (FUNCTION
		    (when (eq (second form) 'CALL-NEXT-METHOD)
                      (setf call-next-method-p 'FUNCTION))
		    (when (eq (second form) 'NEXT-METHOD-P)
                      (setf next-method-p-p 'FUNCTION))))))
	     form))
      ;; CODE-WALK returns NIL if the walk could not be completed,
      ;; e.g. due to an error. Otherwise we just use the side effects.
      ;; If the walk can't be completed, we assume the worst.
      (unless (cmp:code-walk #'code-walker method-lambda env)
        (setq call-next-method-p 'function
              next-method-p-p 'function)))
    (values call-next-method-p next-method-p-p)))
                                   


;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun extract-lambda-list (specialized-lambda-list)
  (values (parse-specialized-lambda-list specialized-lambda-list)))

(defun extract-specializer-names (specialized-lambda-list)
  (nth-value 2 (parse-specialized-lambda-list specialized-lambda-list)))

(defun parse-specialized-lambda-list (specialized-lambda-list)
  "This function takes a method lambda list and outputs a new lambda list
where the specializers have disappeared, the list of required arguments, the
list of specializers, and a list where each element is true iff that
argument was specialized.
(The last is useful for implementing IGNORE behavior.)"
  ;; That is, clhs defmethod says that a specialized parameter is
  ;; ignorable, essentially.
  (ext:with-current-source-form (specialized-lambda-list)
    ;; SI:PROCESS-LAMBDA-LIST will ensure that the lambda list is
    ;; syntactically correct and will output as a second value
    ;; list of required arguments. We use this list to extract the
    ;; specializers and build a lambda list without specializers.
    (do* ((arglist (rest (si::process-lambda-list specialized-lambda-list 'METHOD))
                   (rest arglist))
          (lambda-list (copy-list specialized-lambda-list))
          (ll lambda-list (rest ll))
          (required-parameters '())
          (specializers '())
          (specializedps '())
          arg variable specializer specializedp)
         ((null arglist)
          (values lambda-list
                  (nreverse required-parameters)
                  (nreverse specializers)
                  (nreverse specializedps)))
      (setf arg (first arglist))
      (ext:with-current-source-form (arg)
        (cond
          ;; Just a variable
          ((atom arg)
           (setf variable arg specializer T specializedp nil))
          ;; List contains more elements than variable and specializer
          ((not (endp (cddr arg)))
           (si::simple-program-error "Syntax error in method specializer ~A" arg))
          ;; Specializer is NIL
          ((null (setf variable (first arg)
                       specializedp t
                       specializer (second arg)))
           (si::simple-program-error
            "NIL is not a valid specializer in a method lambda list"))
          ;; Specializer looks like a class name
          ((atom specializer))
          ;; Specializer is (EQL value)
          ((and (eql (first specializer) 'EQL)
             (cdr specializer)
             (endp (cddr specializer))))
          ;; Otherwise, syntax error
          (t
           (si::simple-program-error "Syntax error in method specializer ~A" arg)))
        (setf (first ll) variable)
        (push variable required-parameters)
        (push specializer specializers)
        (push specializedp specializedps)))))

(defun declaration-specializers (arglist declarations)
  (do ((argscan arglist (cdr argscan))
       (declist (when declarations (cdr declarations))))
      ((or
	(null argscan)
	(member (first argscan) '(&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX)))
       `(DECLARE ,@declist))
      (when (listp (first argscan))
	    (push `(TYPE ,(cadar argscan) ,(caar argscan)) declist))))

;;; ----------------------------------------------------------------------
;;;                                                         initialization

(defmethod shared-initialize :before
    ((method standard-method) slot-names &rest initargs
     &key (specializers nil spec-supplied-p)
       (lambda-list nil lambda-supplied-p)
     ;; these options are only here to legitimize them being passed back from
     ;; make-method-lambda; they are not actually used.
     ;; (We can't simply remove them in method-lambda, as a user may bypass that
     ;;  function with their own method definitions.)
     ;; our custom initargs are internal symbols, as per MOP "The defmethod macros"
       ((call-next-method-p call-next-method-p))
       ((next-method-p-p next-method-p-p)))
  (declare (ignore initargs call-next-method-p next-method-p-p))
  (when slot-names
    (unless spec-supplied-p
      (error "Specializer list not supplied in method initialization"))
    (unless lambda-supplied-p
      (error "Lambda list not supplied in method initialization"))
    (unless (= (first (si::process-lambda-list lambda-list 'method))
	       (length specializers))
      (error "The list of specializers does not match the number of required arguments in the lambda list ~A"
	     lambda-list)))
  (when spec-supplied-p
    (loop for s in specializers
       unless (typep s 'specializer)
         do (error "Object ~A is not a valid specializer" s))))

(defmethod shared-initialize :after
    ((method standard-method) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (setf (values (method-keywords method) (method-allows-other-keys-p method))
        (compute-method-keywords (method-lambda-list method))))


;;; ----------------------------------------------------------------------
;;;                                                             operations

(defun compute-method-keywords (lambda-list)
  (multiple-value-bind (reqs opts rest key-flag keywords allow-other-keys)
      (si::process-lambda-list lambda-list t)
    (declare (ignore reqs opts rest key-flag))
    (values
     (loop for k in (rest keywords) by #'cddddr
	   collect k)
     allow-other-keys)))

(defmethod function-keywords ((method standard-method))
  (values (method-keywords method) (method-allows-other-keys-p method)))

(defun install-method (name qualifiers specializers lambda-list fun &rest options)
  (let* ((gf (ensure-generic-function name))
	 (method (make-method (generic-function-method-class gf)
			      qualifiers specializers lambda-list
			      fun options)))
    (add-method gf method)
    method))

(defun make-method (method-class qualifiers specializers lambda-list fun options)
  (multiple-value-bind (keys aok-p)
      (compute-method-keywords lambda-list)
    (apply #'make-instance
           (if (classp method-class)
               method-class
               (find-class method-class))
           :generic-function nil
           :lambda-list lambda-list
           :function fun
           :specializers specializers
           :qualifiers qualifiers
           :keywords keys
           :aok-p aok-p
           options)))

(defun compile-method (method)
  (let ((mf (method-function method)))
    ;; TODO: Maybe also compile the slow method function?
    (typecase mf
      (%leaf-method-function
       (multiple-value-bind (new-fmf warningsp failurep)
           (compile nil (fmf mf))
         (unless failurep
           (setf (fmf mf) new-fmf))
         (values mf warningsp failurep)))
      (%contf-method-function
       (multiple-value-bind (new-contf warningsp failurep)
           (compile nil (contf mf))
         (unless failurep
           (setf (contf mf) new-contf))
         (values mf warningsp failurep))))))
