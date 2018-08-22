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
;;; DEFMETHOD
;;;

(defun generic-function-method-class (generic-function)
  (if *clos-booted*
      (slot-value generic-function 'method-class)
      (find-class 'standard-method)))

(defun maybe-augment-generic-function-lambda-list (name method-lambda-list)
  "Add any &key parameters from method-lambda-list that are missing
in the generic function lambda-list to the generic function lambda-list"
  (when method-lambda-list
    (let* ((gf (fdefinition name))
           (gf-lambda-list-all (ext:function-lambda-list gf))
           (has-aok (member '&allow-other-keys gf-lambda-list-all))
           (gf-lambda-list (if has-aok
                               (butlast gf-lambda-list-all 1)
                               gf-lambda-list-all)))
      (if (null gf-lambda-list)
          (core:function-lambda-list-set gf method-lambda-list)
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
                      (core:function-lambda-list-set gf new-ll)))))))))))

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
      (multiple-value-bind (lambda-form declarations documentation)
	  (make-raw-lambda name lambda-list required-parameters specializers body env qualifiers)
	(multiple-value-bind (generic-function method)
            (prototypes-for-make-method-lambda name)
	  (multiple-value-bind (fn-form options)
	      (make-method-lambda generic-function method lambda-form env)
	    (when documentation
	      (setf options (list* :documentation documentation options)))
            `(progn
               (eval-when (:compile-toplevel)
                 (cmp:register-global-function-def 'defmethod ',name))
               (prog1
                   (install-method ',name ',qualifiers
                                   ,(specializers-expression specializers)
                                   ',lambda-list
                                   ,fn-form
                                   ;; Note that we do not quote the options returned by make-method-lambda.
                                   ;; This is essentially to make the fast method function easier.
                                   ;; MOP is in my view ambiguous about whether they're supposed to be quoted.
                                   ;; There's an example that sort of implies they are, but the extra
                                   ;; flexibility is pretty convenient, and matches that the primary value is
                                   ;; of course evaluated.
                                   ,@options)
                 (maybe-augment-generic-function-lambda-list ',name ',lambda-list)))))))))

(defun specializers-expression (specializers)
  `(list ,@(loop for spec in specializers
                 collect (etypecase spec
                           (symbol `(find-class ',spec))
                           ((cons (eql eql) (cons t null)) ; (eql #<anything>)
                            `(intern-eql-specializer ,(second spec)))
                           ;; CLHS DEFMETHOD seems to say literal specializers are
                           ;; not allowed, but i'm not sure...
                           (specializer spec)))))

(defun fixup-specializers (specializers)
  (mapcar (lambda (spec)
            (if (consp spec)
                (if (or (symbolp (second spec)) (numberp (second spec)))
                    spec
                    `(eql ,(make-symbol (format nil "~s" (second spec)))))
                spec))
          specializers))

(defun make-raw-lambda (name lambda-list required-parameters specializers body env qualifiers)
  (multiple-value-bind (declarations real-body documentation)
      (sys::find-declarations body)
    (setf qualifiers (if qualifiers (list qualifiers)))
    (let* ((copied-variables '())
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
           (method-lambda
            ;; Remove the documentation string and insert the
            ;; appropriate class declarations.  The documentation
            ;; string is removed to make it easy for us to insert
            ;; new declarations later, they will just go after the
            ;; second of the method lambda.  The class declarations
            ;; are inserted to communicate the class of the method's
            ;; arguments to the code walk.
             `(lambda ,lambda-list
                (declare (core:lambda-name (method ,name ,@qualifiers ,(fixup-specializers specializers))))
                ,@(and class-declarations `((declare ,@class-declarations)))
                ,(if copied-variables
                     `(let* ,copied-variables ,block)
                     block))))
      (values method-lambda declarations documentation))))

(defun lambda-list-fast-callable-p (lambda-list)
  ;; We only put in an FMF if we have only required parameters, and few enough
  ;; that they can be passed in registers (that's what number-of-fixed-arguments is).
  (and
   (<= (length lambda-list) core:+number-of-fixed-arguments+)
   (not (find-if (lambda (sym)
                   (member sym lambda-list-keywords))
                 lambda-list))))

(defun make-method-lambda (gf method method-lambda env)
  (multiple-value-bind (call-next-method-p next-method-p-p)
      (walk-method-lambda method-lambda env)
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
                     (flet (,@(and call-next-method-p
                                `((call-next-method (&va-rest args)    #|DANGER|#
                                    (if (not .next-methods.)
                                        ;; But how do I generate code that can be compiled
                                        ;; that will get access to the current method and
                                        ;; current generic function? The method does not yet exist.
                                        (error "No next method") ;; FIXME: should call no-next-method.
                                        (let ((use-args (if (> (vaslist-length args) 0) args .method-args.)))
                                          (funcall (car .next-methods.)
                                                   use-args ; (or args .method-args.)
                                                   (cdr .next-methods.)))))))
                            ,@(and next-method-p-p
                                `((next-method-p ()
                                                 (and .next-methods. t)))))
                       ;; Per CLHS 7.6.4, methods do not do keyword argument checking- the gf does.
                       ;; BIND-VA-LIST is therefore set up to pass :safep nil to the argument parser
                       ;; generator, which essentially implies &allow-other-keys.
                       (core::bind-va-list ,lambda-list .method-args.
                                           (declare ,@declarations)
                                           ,@body)))
                  ;; double quotes as per evaluation, explained above in defmethod.
                  (list ''leaf-method-p `',leaf-method-p
                        ;; FIXME: This is kind of a messy way of arranging things. Both the lambda list check
                        ;; criterion, and the fmf initarg. But I'm not sure what would be preferable.
                        ''fast-method-function (if (and leaf-method-p
                                                        (lambda-list-fast-callable-p lambda-list))
                                                  `(lambda ,lambda-list
                                                     (declare ,@declarations)
                                                     ,doc
                                                     ,@body)
                                                  nil))))))))

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
      ;; Determine how to walk the code in clasp
      ;; Either use the bclasp compiler or if clasp-cleavir is available
      ;; then use the clasp-cleavir compiler
      (unless (cmp:code-walk method-lambda env :code-walker-function #'code-walker :errorp nil)
        (setq call-next-method-p t
              next-method-p-p t)))
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
  )

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
      (invalidate-discriminating-function gf)
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




