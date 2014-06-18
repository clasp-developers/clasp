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
(defparameter *add-method-argument-declarations* nil)


;;; ----------------------------------------------------------------------
;;; DEFMETHOD
;;;

(defun generic-function-method-class (generic-function)
  (if *clos-booted*
      (slot-value generic-function 'method-class)
      (find-class 'standard-method)))

(defmacro defmethod (&whole whole name &rest args &environment env)
  (declare (notinline make-method-lambda))
  (let* ((*print-length* 3)
	 (*print-depth* 2)
	 (qualifiers (loop while (and args (not (listp (first args))))
			collect (pop args)))
	 (specialized-lambda-list
	  (if args
	      (pop args)
	      (error "Illegal defmethod form: missing lambda list")))
	 (body args))
    (multiple-value-bind (lambda-list required-parameters specializers)
	(parse-specialized-lambda-list specialized-lambda-list)
      (multiple-value-bind (lambda-form declarations documentation)
	  (make-raw-lambda name lambda-list required-parameters specializers body env)
	(let* ((generic-function (ensure-generic-function name))
	       (method-class (generic-function-method-class generic-function))
	       method)
	  (when *clos-booted*
	    (when (symbolp method-class)
	      (setf method-class (find-class method-class nil)))
	    (if method-class
		(setf method (class-prototype method-class))
		(error "Cannot determine the method class for generic functions of type ~A"
		       (type-of generic-function))))
	  (multiple-value-bind (fn-form options)
	      (make-method-lambda generic-function method lambda-form env)
	    (when documentation
	      (setf options (list* :documentation documentation options)))
	    (multiple-value-bind (wrapped-lambda wrapped-p)
		(simplify-lambda name fn-form)
	      (unless wrapped-p
		(error "Unable to unwrap function"))
	      (ext:register-with-pde
	       whole
	       `(install-method ',name ',qualifiers
				,(specializers-expression specializers)
				',lambda-list
				,(maybe-remove-block wrapped-lambda)
				,wrapped-p
				,@(mapcar #'si::maybe-quote options))))))))))

(defun specializers-expression (specializers)
  (declare (si::c-local))
  ;; The list of specializers contains either types or (EQL value)
  ;; expressions. The 'value' has to be obtained by evaluating a lisp form
  ;; We save some space/time by including directly the value of those
  ;; forms which do not involve any evaluations, such as literals or QUOTE
  ;; expressions. Note that we cannot use CONSTANTP because this would extract
  ;; the value of constant symbols, which would not be EQL to the value of
  ;; those same constants when the code is reloaded.
  (list 'si::quasiquote
	(loop for spec in specializers
	   collect (if (atom spec)
		       spec
		       `(eql ,(let ((value (second spec)))
				(if (if (atom value)
					(not (symbolp value))
					(eq (car value) 'quote))
				    (ext:constant-form-value value)
				    (list 'si::unquote value))))))))

(defun maybe-remove-block (method-lambda)
  (when (eq (first method-lambda) 'lambda)
    (multiple-value-bind (declarations body documentation)
	(si::find-declarations (cddr method-lambda))
      (let (block)
	(when (and (null (rest body))
		   (listp (setf block (first body)))
		   (eq (first block) 'block))
	  (setf method-lambda `(ext:lambda-block ,(second block) ,(second method-lambda)
				,@declarations
				,@(cddr block)))
	  ))))
  method-lambda)

(defun simplify-lambda (method-name fn-form)
  (let ((aux fn-form))
    (if (and (eq (pop aux) 'lambda)
	     (equalp (pop aux) '(.combined-method-args. *next-methods*))
	     (equalp (pop aux) '(declare (special .combined-method-args. *next-methods*)))
	     (null (rest aux))
	     (= (length (setf aux (first aux))) 3)
	     (eq (first aux) 'apply)
	     (eq (third aux) '.combined-method-args.)
	     (listp (setf aux (second aux)))
	     (eq (first aux) 'lambda))
	(values aux t)
	(values fn-form nil))))

(defun make-raw-lambda (name lambda-list required-parameters specializers body env)
  (declare (si::c-local))
  (multiple-value-bind (declarations real-body documentation)
      (sys::find-declarations body)
    ;; FIXME!! This deactivates the checking of keyword arguments
    ;; inside methods. The reason is that this checking must be
    ;; supplemented the knowledge of the keyword arguments of all
    ;; applicable methods (X3J13 7.6.5). Therefore, we should insert
    ;; that check, either in the method itself so that it is done
    ;; incrementally, or in COMPUTE-EFFECTIVE-METHOD.
    (when (and (member '&key lambda-list)
	       (not (member '&allow-other-keys lambda-list)))
      (let ((x (position '&aux lambda-list)))
	(setf lambda-list
		(append (subseq lambda-list 0 x)
			'(&allow-other-keys)
			(and x (subseq lambda-list x))
                        nil))))
    (let* ((copied-variables '())
	   (ignorable `(declare (ignorable ,@required-parameters)))
	   (class-declarations
	    (nconc (when *add-method-argument-declarations*
		     (loop for name in required-parameters
			for type in specializers
			when (and (not (eq type t)) (symbolp type))
			do (push `(,name ,name) copied-variables) and
			nconc `((type ,type ,name)
				(si::no-check-type ,name))))
		   (list (list 'si::function-block-name name))
		   (cdar declarations)))
	   (block `(block ,(si::function-block-name name) ,@real-body))
	   (method-lambda
	    ;; Remove the documentation string and insert the
	    ;; appropriate class declarations.  The documentation
	    ;; string is removed to make it easy for us to insert
	    ;; new declarations later, they will just go after the
	    ;; second of the method lambda.  The class declarations
	    ;; are inserted to communicate the class of the method's
	    ;; arguments to the code walk.
	    `(lambda ,lambda-list
	       ,@(and class-declarations `((declare ,@class-declarations)))
	       ,ignorable
	       ,(if copied-variables
		    `(let* ,copied-variables
		       ,ignorable
		       ,block)
		     block))))
      (values method-lambda declarations documentation))))

(defun make-method-lambda (gf method method-lambda env)
  (multiple-value-bind (call-next-method-p next-method-p-p in-closure-p)
      (walk-method-lambda method-lambda env)
    (values `(lambda (.combined-method-args. *next-methods*)
	       (declare (special .combined-method-args. *next-methods*))
	       (apply ,(if in-closure-p
			   (add-call-next-method-closure method-lambda)
			   method-lambda)
		      .combined-method-args.))
	    nil)))

(defun add-call-next-method-closure (method-lambda)
  (multiple-value-bind (declarations real-body documentation)
      (si::find-declarations (cddr method-lambda))
    `(lambda ,(second method-lambda)
       (let* ((.closed-combined-method-args.
	       (if (listp .combined-method-args.)
		   .combined-method-args.
		   (apply #'list .combined-method-args.)))
	      (.next-methods. *next-methods*))
	 (flet ((call-next-method (&rest args)
		  (unless .next-methods.
		    (error "No next method"))
		  (funcall (car .next-methods.)
			   (or args .closed-combined-method-args.)
			   (rest .next-methods.)))
		(next-method-p ()
		  .next-methods.))
	   ,@real-body)))))

(defun environment-contains-closure (env)
  ;;
  ;; As explained in compiler.d (make_lambda()), we use a symbol with
  ;; name "FUNCTION-BOUNDARY" to mark the beginning of a function. If
  ;; we find that symbol twice, it is quite likely that this form will
  ;; end up in a closure.
  ;;
  (let ((counter 0))
    (declare (fixnum counter))
    (dolist (item (car env))
      (when (and (consp item)
		 (eq (first (the cons item)) 'si::function-boundary)
		 (> (incf counter) 1)) 
	(return t)))))

(defun walk-method-lambda (method-lambda env)
  (declare (si::c-local))
  (let ((call-next-method-p nil)
	(next-method-p-p nil)
	(in-closure-p nil))
    (flet ((code-walker (form env)
	     (unless (atom form)
	       (let ((name (first form)))
		 (case name
		   (CALL-NEXT-METHOD
		    (setf call-next-method-p
			  (or call-next-method-p T)
			  in-closure-p
			  (or in-closure-p (environment-contains-closure env))))
		   (NEXT-METHOD-P
		    (setf next-method-p-p t
			  in-closure-p (or in-closure-p (environment-contains-closure env))))
		   (FUNCTION
		    (when (eq (second form) 'CALL-NEXT-METHOD)
		      (setf in-closure-p t
			    call-next-method-p 'FUNCTION))
		    (when (eq (second form) 'NEXT-METHOD-P)
		      (setf next-method-p-p 'FUNCTION
			    in-closure-p t))))))
	     form))
      (let ((si::*code-walker* #'code-walker))
	;; Instead of (coerce method-lambda 'function) we use
        ;; explicitely the bytecodes compiler with an environment, no
        ;; stepping, compiler-env-p = t and execute = nil, so that the
        ;; form does not get executed.
        (si::eval-with-env method-lambda env nil t t)))
    (values call-next-method-p
	    next-method-p-p
	    in-closure-p)))

;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun legal-generic-function-name-p (name)
  (si::valid-function-name-p name))

(defun parse-defmethod (args)
  (declare (si::c-local))
  ;; This function has to extract the name of the method, a list of
  ;; possible qualifiers (identified by not being lists), the lambda
  ;; list of the method (which might be empty!) and the body of the
  ;; function.
  (let* (name)
    (unless args
      (error "Illegal defmethod form: missing method name"))
    (setq name (pop args))
    (unless (legal-generic-function-name-p name)
      (error "~A cannot be a generic function specifier.~%~
             It must be either a non-nil symbol or ~%~
             a list whose car is setf and whose second is a non-nil symbol."
	     name))
    (do ((qualifiers '()))
	((progn
	   (when (endp args)
	     (error "Illegal defmethod form: missing lambda-list"))
	   (listp (first args)))
	 (values name (nreverse qualifiers) (first args) (rest args)))
      (push (pop args) qualifiers))))

(defun extract-lambda-list (specialized-lambda-list)
  (values (parse-specialized-lambda-list specialized-lambda-list)))

(defun extract-specializer-names (specialized-lambda-list)
  (nth-value 2 (parse-specialized-lambda-list specialized-lambda-list)))

(defun parse-specialized-lambda-list (specialized-lambda-list)
  "This function takes a method lambda list and outputs the list of required
arguments, the list of specializers and a new lambda list where the specializer
have disappeared."
  (declare (si::c-local))
  ;; SI:PROCESS-LAMBDA-LIST will ensure that the lambda list is
  ;; syntactically correct and will output as a first argument the
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
    (push specializer specializers)))

(defun declaration-specializers (arglist declarations)
  (declare (si::c-local))
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
    (if allow-other-keys
	't
	(loop for k in (rest keywords) by #'cddddr
	   collect k))))

(defun make-method (method-class qualifiers specializers lambda-list fun options)
  (declare (ignore options))
  (with-early-make-instance
      ;; We choose the largest list of slots
      +standard-accessor-method-slots+
    (method (if (si::instancep method-class)
		method-class
		(find-class method-class))
	    :generic-function nil
	    :lambda-list lambda-list
	    :function fun
	    :specializers specializers
	    :qualifiers qualifiers
	    :keywords (compute-method-keywords lambda-list))
    method))

;;; early version used during bootstrap
(defun method-p (x)
  (si::instancep x))

;;; early version used during bootstrap
(defun add-method (gf method)
  (with-early-accessors (+standard-method-slots+ +standard-generic-function-slots+ +standard-class-slots+)
    (let* ((name (slot-value gf 'name))
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
      (compute-g-f-spec-list gf)
      (set-generic-function-dispatch gf)
      method)))

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
		       (push `(,(caar scan) (,(cadar scan) ,temp)) res))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))

;;; Force the compiler into optimizing use of gethash inside methods:
(setf (symbol-function 'SLOT-INDEX) (symbol-function 'GETHASH))