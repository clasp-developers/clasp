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

;;; ----------------------------------------------------------------------

(defparameter *method-size* 32)		; Size of methods hash tables

;;; This holds fake methods created during bootstrap.
;;; It is  an alist of:
;;;	(method-name {method}+)
(defparameter *early-methods* nil)

;;;
;;; This is used by combined methods to communicate the next methods to
;;; the methods they call.
;;;
(defparameter *next-methods* nil)

;;; Add type declarations for the arguments of a METHOD. This implies
;;; copying the method arguments because the arguments may be modified.
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defparameter *add-method-argument-declarations* nil)
)

;;; ----------------------------------------------------------------------
;;; %METHOD-FUNCTIONs
;;;
;;; These are funcallable instances used as method functions.
;;; The idea is we hang extra info, such as how to call the method with
;;; our own faster convention, on the method function itself. This ensures
;;; things don't get out of sync.
;;; See pseudo class definition in hierarchy.lsp. Idea from SBCL. 

;;; First, fast method functions: for leaf methods (i.e. methods that don't
;;; use call-next-method or next-method-p). They are therefore just
;;; functions, accepting the generic function's arguments. This means they
;;; also double as effective method functions.

(defun make-%method-function-fast (fmf)
  (with-early-make-funcallable-instance +%method-function-slots+
    (%mf (find-class '%method-function)
         :fmf fmf)
    (setf-function-name %mf 'slow-method-function)
    (set-funcallable-instance-function
     %mf
     (lambda (arguments next-methods)
       (declare (core:lambda-name slow-method-function.fmf)
                (ignore next-methods))
       ;; FIXME: Avoid coerce-fdesignator in apply here
       (apply fmf arguments)))
    %mf))

(defun fast-method-function (method)
  (let ((mf (method-function method)))
    ;; Internal class that is never subclassed, so just
    (and (eq (class-of mf) (find-class '%method-function))
         (with-early-accessors (+%method-function-slots+)
           (%mf-fast-method-function mf)))))

(defun early-fast-method-function (method)
  (with-early-accessors (+standard-method-slots+
                         +%method-function-slots+)
    (let ((mf (method-function method)))
      (and (eq (class-of mf) (find-class '%method-function))
           (%mf-fast-method-function mf)))))

;;; Continuation method functions (contfs) can be put in place for
;;; anything, unless there's a user make-method-lambda method.
;;; A contf takes one argument, the continuation, and then the arguments
;;; of the generic function as the rest. The continuation is the effective
;;; method function executed by call-next-method.
;;; So it's either a closure with another contf or method-function, or a
;;; fast method function, or a special %no-next-method-continuation.
;;; The %no-next-method-continuation is a somewhat magical funcallable
;;; instance with an instance function that just calls no-next-method.
;;; But it's its own class so that next-method-p can distinguish it.

(defun make-%no-next-method-continuation (method)
  (with-early-make-funcallable-instance nil ; class has no slots.
    (%nnmc (find-class '%no-next-method-continuation))
    (set-funcallable-instance-function
     %nnmc
     (if (null method)
         (lambda (core:&va-rest args)
           (declare (core:lambda-name %no-next-method-continuation.slow.bad))
           (error "No next method"))
         (let ((gf (method-generic-function method)))
           (lambda (core:&va-rest args)
             (declare (core:lambda-name %no-next-method-continuation.lambda))
             (apply #'no-next-method gf method args)))))
    %nnmc))

(defun make-%method-function-contf (contf)
  (with-early-make-funcallable-instance +%method-function-slots+
    (%mf (find-class '%method-function)
         :contf contf)
    (setf-function-name %mf 'slow-method-function)
    (set-funcallable-instance-function
     %mf
     (let (;; FIXME: Method not available yet :(
           (nnmc (make-%no-next-method-continuation nil)))
       (lambda (.method-args. next-methods)
         (declare (core:lambda-name slow-method-function.contf))
         ;; FIXME: Avoid coerce-fdesignator in apply here
         (apply contf
                (if (null next-methods)
                    nnmc
                    (lambda (core:&va-rest .method-args.)
                      (declare (core:lambda-name slot-method-function.contf.lambda))
                      (funcall (first next-methods)
                               .method-args.
                               (rest next-methods))))
                .method-args.))))
    %mf))

(defun contf-method-function (method)
  (let ((mf (method-function method)))
    ;; Internal class that is never subclassed, so just
    (and (eq (class-of mf) (find-class '%method-function))
         (with-early-accessors (+%method-function-slots+)
           (%mf-contf mf)))))

(defun early-contf-method-function (method)
  (with-early-accessors (+standard-method-slots+
                         +%method-function-slots+)
    (let ((mf (method-function method)))
      (and (eq (class-of mf) (find-class '%method-function))
           (%mf-contf mf)))))

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
                `((call-next-method (core:&va-rest cnm-args)
                    (if (> (vaslist-length cnm-args) 0)
                        (apply ,contsym cnm-args)
                        ,default-cnm-form))))
            ,@(when (eq nnmp-p 'function)
                `((next-method-p ()
                    (typep ,contsym '(not %no-next-method-continuation))))))
       ,form)))

(defun contf-lambda (lambda-list lambda-name decls doc body
                     call-next-method-p no-next-method-p-p)
  (let ((contsym (gensym "METHOD-CONTINUATION")))
    (multiple-value-bind (req opt rest keyf keys aok-p aux va-p)
        (core:process-lambda-list lambda-list 'function)
      (if (or (not (zerop (car opt))) rest keyf)
          `(lambda (,contsym core:&va-rest .method-args.)
             (declare (core:lambda-name ,lambda-name))
             ,@(when doc (list doc))
             ,(wrap-contf-lexical-function-binds
               `(core::bind-va-list ,lambda-list .method-args. ,@decls ,@body)
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

(defun generic-function-method-class (generic-function)
  (if *clos-booted*
      (slot-value generic-function 'method-class)
      (find-class 'standard-method)))

(defun maybe-augment-generic-function-lambda-list (gf method-lambda-list)
  "Add any &key parameters from method-lambda-list that are missing
in the generic function lambda-list to the generic function lambda-list"
  (let ((gf-lambda-list-all (ext:function-lambda-list gf)))
    (if (eq gf-lambda-list-all (core:unbound)) ; uninitialized
        (setf-lambda-list gf method-lambda-list)
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
                      (setf-lambda-list gf new-ll)))))))))))

(defun prototypes-for-make-method-lambda (name)
  (if (not *clos-booted*)
      (values nil nil)
      (let ((gf? (and (fboundp name) (fdefinition name))))
        (if (or (null gf?) (not (si:instancep gf?)))
            (values (class-prototype (find-class 'standard-generic-function))
                    (class-prototype (find-class 'standard-method)))
            (values gf?
                    (class-prototype (or (generic-function-method-class gf?)
                                         (find-class 'standard-method))))))))

;;; Is this lambda form one returned by our make-method-lambda method (below)?
;;; If it is, we know what it does so we can kind of ignore it.
;;; See %method-function above.
(defun our-method-lambda-p (method-lambda)
  ;; This is pretty KLUDGEy. But make-method-lambda pretty specifically has
  ;; to return a lambda expression, so there's only so much we can do as far
  ;; as magical indications go.
  (and (consp method-lambda)
       (eq (car method-lambda) 'lambda)
       (consp (cdr method-lambda))
       (equal (second method-lambda) '(.method-args. .next-methods.))))

;;; These are used to pass information obtained from walking the method body
;;; up to method-lambda. This is kind of ugly, but I think the walking really
;;; has to be done in make-method-lambda to work properly with user methods,
;;; and we don't want to pass it back as an option because they're not actual
;;; options.
(defvar *call-next-method-p*)
(defvar *next-method-p-p*)

;;; Does the work of calling make-method-lambda, and returns the same values.
;;; In the event the method lambda is the one returned by the standard method,
;;; instead of a lambda expression, this will return a form to create a
;;; %method-function with some quicker to use internal function (see above).
(defun method-lambda (name lambda-expression env
                      lambda-name lambda-list body declarations documentation)
  (multiple-value-bind (generic-function method)
      (prototypes-for-make-method-lambda name)
    (let ((*call-next-method-p* 'function)
          (*next-method-p-p* 'function))
      (multiple-value-bind (fn-form options)
          (make-method-lambda generic-function method lambda-expression env)
        (values
         (if (our-method-lambda-p fn-form)
             (if (not (or *call-next-method-p* *next-method-p-p*))
                 `(make-%method-function-fast ,lambda-expression)
                 `(make-%method-function-contf
                   ,(contf-lambda lambda-list lambda-name declarations documentation body
                                  *call-next-method-p* *next-method-p-p*)))
             fn-form)
         options)))))

(defmacro defmethod (&whole whole name &rest args &environment env)
  (let* ((qualifiers (loop while (and args (not (listp (first args))))
			collect (pop args)))
	 (specialized-lambda-list
	  (if args
	      (pop args)
	      (error "Illegal defmethod form: missing lambda list")))
	 (body args))
    (multiple-value-bind (lambda-list required-parameters specializers)
        (parse-specialized-lambda-list specialized-lambda-list)
      (multiple-value-bind (lambda-expression lambda-name fn-lambda-list body
                            declarations documentation)
	  (make-raw-lambda name lambda-list required-parameters specializers body env qualifiers)
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

(defun fixup-specializers (specializers)
  (mapcar (lambda (spec)
            (if (consp spec)
                (if (or (symbolp (second spec)) (numberp (second spec)))
                    spec
                    `(eql ,(make-symbol (format nil "~s" (second spec)))))
                spec))
          specializers))

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

(defun make-raw-lambda (name lambda-list required-parameters specializers body env qualifiers)
  (multiple-value-bind (declarations real-body documentation)
      (sys::find-declarations body t)
    (let* ((qualifiers (if qualifiers (list qualifiers)))
           (copied-variables '())
           (class-declarations
            (nconc (when *add-method-argument-declarations*
                     (loop for name in required-parameters
                        for type in specializers
                        when (and (not (eq type t)) (symbolp type))
                        do (push `(,name ,name) copied-variables) and
                        nconc `((type ,type ,name)
                                (si::no-check-type ,name))))
                   (cdar declarations)))
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
                ,@(and class-declarations `((declare ,@class-declarations)))
                ,(if copied-variables
                     `(let* ,copied-variables ,block)
                     block))))
      (values method-lambda lambda-name lambda-list (list block) declarations documentation))))

(defun make-method-lambda (gf method method-lambda env)
  (declare (ignore gf method))
  (multiple-value-bind (call-next-method-p next-method-p-p)
      (walk-method-lambda method-lambda env)
    (setf *call-next-method-p* call-next-method-p
          *next-method-p-p* next-method-p-p)
    (let ((leaf-method-p (null (or call-next-method-p next-method-p-p))))
      (multiple-value-bind (declarations body doc)
          (process-declarations (cddr method-lambda) t) ; We expect docstring
        ;; source location here?
        (let ((lambda-list (second method-lambda))
              (lambda-name-declaration (or (find 'core::lambda-name declarations :key #'car)
                                           '(core:lambda-name make-method-lambda.lambda))))
          (values `(lambda (.method-args. .next-methods.)
                     (declare ,lambda-name-declaration)
                     ,doc
                     ,(gen-lexical-method-function-binds
                       call-next-method-p next-method-p-p
                       ;; Per CLHS 7.6.4, methods do not do keyword argument checking- the gf does.
                       ;; BIND-VA-LIST is therefore set up to pass :safep nil to the argument parser
                       ;; generator, which essentially implies &allow-other-keys.
                       `(core::bind-va-list ,lambda-list .method-args.
                          (declare ,@declarations)
                          ,@body)))
                  ;; double quotes as per evaluation, explained above in defmethod.
                  (list ''leaf-method-p (not (not leaf-method-p)))))))))

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
         `(flet ((call-next-method (&va-rest args)
                   (if (null .next-methods.)
                       ;; FIXME: should call no-next-method.
                       ;; This is hard, because the method doesn't exist when this
                       ;; function is created.
                       (error "No next method")
                       (let ((use-args (if (> (vaslist-length args) 0) args .method-args.)))
                         (funcall (car .next-methods.)
                                  use-args
                                  (cdr .next-methods.)))))
                 (next-method-p () (and .next-methods. t)))
            ,form))
        ((or call-next-method-p next-method-p-p)
         `(macrolet ((call-next-method (&rest args)
                       `(if (null .next-methods.)
                            (error "No next method")
                            ,(if args
                                 `(let ((next (car .next-methods.))
                                        (more (cdr .next-methods.)))
                                    ((lambda (&va-rest args)
                                       (funcall next args more))
                                     ,@args))
                                 `(funcall (car .next-methods.)
                                           .method-args.
                                           (cdr .next-methods.)))))
                     (next-method-p () '(and .next-methods. t)))
            ,form))
        (t form)))

(defun walk-method-lambda (method-lambda env)
  (let ((call-next-method-p nil)
        (next-method-p-p nil))
    (flet ((code-walker (form env)
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

(defun legal-generic-function-name-p (name)
  (si::valid-function-name-p name))

(defun extract-lambda-list (specialized-lambda-list)
  (values (parse-specialized-lambda-list specialized-lambda-list)))

(defun extract-specializer-names (specialized-lambda-list)
  (nth-value 2 (parse-specialized-lambda-list specialized-lambda-list)))


;; For some reason clasp needs this at compile time but ecl does not
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun parse-specialized-lambda-list (specialized-lambda-list)
    "This function takes a method lambda list and outputs the list of required
arguments, the list of specializers and a new lambda list where the specializer
have disappeared."
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
            arg variable specializer)
           ((null arglist)
            (values lambda-list
                    (nreverse required-parameters)
                    (nreverse specializers)))
        (setf arg (first arglist))
        (ext:with-current-source-form (arg)
          (cond
            ;; Just a variable
            ((atom arg)
             (setf variable arg specializer T))
            ;; List contains more elements than variable and specializer
            ((not (endp (cddr arg)))
             (si::simple-program-error "Syntax error in method specializer ~A" arg))
            ;; Specializer is NIL
            ((null (setf variable (first arg)
                         specializer (second arg)))
             (si::simple-program-error
              "NIL is not a valid specializer in a method lambda list"))
            ;; Specializer is a class name
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
          (push specializer specializers))))))

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
;;;                                                             operations

(defun compute-method-keywords (lambda-list)
  (multiple-value-bind (reqs opts rest key-flag keywords allow-other-keys)
      (si::process-lambda-list lambda-list t)
    (declare (ignore reqs opts rest key-flag))
    (values
     (loop for k in (rest keywords) by #'cddddr
	   collect k)
     allow-other-keys)))

(defun make-method (method-class qualifiers specializers lambda-list fun options)
  (multiple-value-bind (keys aok-p)
      (compute-method-keywords lambda-list)
    (with-early-make-instance
      ;; We choose the largest list of slots
      +standard-accessor-method-slots+
      (method (if (classp method-class)
                  method-class
                  (find-class method-class))
              :generic-function nil
              :lambda-list lambda-list
              :function fun
              :specializers specializers
              :qualifiers qualifiers
              :keywords keys
              :aok-p aok-p
              leaf-method-p (getf options 'leaf-method-p nil)
              fast-method-function (getf options 'fast-method-function nil))
      method)))

;;; early version used during bootstrap
(defun method-p (x)
  (si::instancep x))

;;; early version used during bootstrap
(defun add-method (gf method)
  (with-early-accessors (+standard-method-slots+ +standard-generic-function-slots+ +standard-class-slots+)
    (let* ((name (core:function-name gf))
	   (method-entry (assoc name *early-methods*)))
      (unless method-entry
	(setq method-entry (list name))
	(push method-entry *early-methods*))
      (push method (cdr method-entry))
      (push method (generic-function-methods gf))
      (setf (method-generic-function method) gf)
      (unless (si::sl-boundp (generic-function-lambda-list gf))
	(setf (generic-function-lambda-list gf) (method-lambda-list method))
	(setf (generic-function-argument-precedence-order gf)
	      (rest (si::process-lambda-list (method-lambda-list method) t))))
      (maybe-augment-generic-function-lambda-list gf (method-lambda-list method))
      (compute-gf-specializer-profile gf)
      (compute-a-p-o-function gf)
      (invalidate-discriminating-function gf)
      gf)))

;; Upgraded into method in fixup.
(defun find-method (gf qualifiers specializers &optional (errorp t))
  (declare (notinline method-qualifiers))
  (flet ((filter-specializer (name)
	   (cond ((typep name 'specializer)
		  name)
		 ((atom name)
		  (let ((class (find-class name nil)))
		    (unless class
		      (error "~A is not a valid specializer name" name))
		    class))
		 ((and (eq (first name) 'EQL)
		       (null (cddr name)))
		  (cdr name))
		 (t
		  (error "~A is not a valid specializer name" name))))
	 (specializer= (cons-or-class specializer)
	   (if (consp cons-or-class)
	       (and (eql-specializer-flag specializer)
		    (eql (car cons-or-class)
			 (eql-specializer-object specializer)))
	       (eq cons-or-class specializer))))
    (when (/= (length specializers)
	      (length (generic-function-argument-precedence-order gf)))
      (error
       "The specializers list~%~A~%does not match the number of required arguments in ~A"
       specializers (generic-function-name gf)))
    (loop with specializers = (mapcar #'filter-specializer specializers)
       for method in (generic-function-methods gf)
       when (and (equal qualifiers (method-qualifiers method))
		 (every #'specializer= specializers (method-specializers method)))
       do (return-from find-method method))
    ;; If we did not find any matching method, then the list of
    ;; specializers might have the wrong size and we must signal
    ;; an error.
    (when errorp
      (error "There is no method on the generic function ~S that agrees on qualifiers ~S and specializers ~S"
	     (generic-function-name gf)
	     qualifiers specializers)))
  nil)

;;; ----------------------------------------------------------------------
;;;                                                         with-accessors

(defmacro with-accessors (slot-accessor-pairs instance-form &body body)
  (let* ((temp (gensym))
	 (accessors (do ((scan slot-accessor-pairs (cdr scan))
                         (res))
                        ((null scan) (nreverse res))
                      (let ((entry (car scan)))
                        (ext:with-current-source-form (entry)
                          (unless (and (listp entry)
                                       (= (length entry) 2))
                            (error "Malformed WITH-ACCESSORS syntax."))
                          (push `(,(car entry) (,(cadr entry) ,temp)) res))))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))
