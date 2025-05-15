;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.


(in-package "CLOS")

;; ----------------------------------------------------------------------
;; EFFECTIVE METHOD FUNCTIONS
;;
;; Effective method functions are the functional version of effective
;; methods (effective methods being the forms returned by
;; compute-effective-method). On Clasp, they are functions that accept the
;; same arguments as the generic function.
;; In general we can simply compile the effective method, but the compiler
;; is slow, so we go to some effort to special case common effective
;; methods.
;; Note that we more often go through this mechanism than putting the
;; effective methods in the discriminating function directly. See
;; *inline-effective-methods* in discriminate.lisp.
;; The main entry to this section is EFFECTIVE-METHOD-FUNCTION, which
;; returns a function for a given effective method.
;; The ARG-INFO threaded throughout here is used to skip some APPLYing.
;; See closfastgf.lisp, gf-arg-info function.

(defvar *avoid-compiling* nil)

(defun emf-maybe-compile (form)
  (if (or *avoid-compiling* (not cmp:*cleavir-compile-hook*))
      (coerce form 'function)
      (let ((*avoid-compiling* t))
        (compile nil form))))

(defun emf-default (form &optional (arg-info '(t)))
  (let ((restp (car arg-info)) (vars (cdr arg-info)))
    (emf-maybe-compile
     `(lambda (,@vars ,@(when restp '(core:&va-rest emf-more)))
        (declare (core:lambda-name effective-method-function.lambda))
        (with-effective-method-parameters ((,@vars) ,(if restp 'emf-more nil))
          ,form)))))

(defun std-method-p (method)
  (let ((mc (class-of method)))
    (or (eq mc (find-class 'standard-method))
        (eq mc (find-class 'standard-reader-method))
        (eq mc (find-class 'standard-writer-method))
        ;; Not remotely standard, but the same as standard-reader/writer
        ;; for our purposes.
        (eq mc (find-class 'effective-reader-method))
        (eq mc (find-class 'effective-writer-method)))))

(defun make-method-form-p (form)
  (and (consp form)
       (eq (first form) 'make-method)
       (consp (cdr form))))

(defun emf-from-contf (contf method next-methods
                       &optional (arg-info '(t)))
  (let ((next (if (null next-methods)
                  (make-%no-next-method-continuation method)
                  (emf-call-method
                   (first next-methods) (list (rest next-methods))
                   arg-info))))
    (lambda (core:&va-rest .method-args.)
      (declare (core:lambda-name emf-from-contf.lambda))
      (apply contf next .method-args.))))

(defun emf-call-method (method rest &optional (arg-info '(t)))
  (cond ((and
          (std-method-p method)
          ;; This next form will return NIL if the method does not have
          ;; an FMF or CONTF, an unusual situation indicating the user
          ;; has manually made a method with whatever function.
          ;; In this scenario we go to the default case down there.
          ;; NOTE that we use the early readers because we can call this
          ;; very early due to satiation. And the early readers are valid
          ;; because we just checked that this is a std method.
          (destructuring-bind (&optional ((&rest next-methods))) rest
            (or (early-fast-method-function method) ; FMFs are valid EMFs
                (let ((contf (early-contf-method-function method)))
                  (when contf
                    (emf-from-contf
                     contf method next-methods arg-info)))))))
        ((make-method-form-p method)
         ;; FIXME: Should call-next-method etc be bound
         (effective-method-function (second method) arg-info))
        ;; Could be a nonstandard method with its own EXPAND-APPLY-METHOD.
        (t (emf-default `(call-method ,method ,@rest) arg-info))))

(defun effective-method-function (form &optional (arg-info '(t)))
  ;; emf-default is always valid, but let's pick off a few cases
  ;; so that we can avoid using the compiler, which is slow.
  (if (consp form)
      (case (first form)
        ;; Note that MAKE-METHOD is not valid outside of a CALL-METHOD,
        ;; so form shouldn't be a MAKE-METHOD form.
        ((call-method) (emf-call-method (second form) (cddr form)
                                        arg-info))
        (otherwise (emf-default form arg-info)))
      (emf-default form arg-info)))

;;; Used for early satiation.

(defun early-emf-from-contf (contf method next-methods)
  (let ((next (if (null next-methods)
                  (make-%no-next-method-continuation method)
                  (early-emf-call-method
                   (first next-methods) (rest next-methods)))))
    (lambda (core:&va-rest .method-args.)
      (declare (core:lambda-name emf-from-contf.lambda))
      (apply contf next .method-args.))))

(defun early-emf-call-method (method next-methods)
  (cond ((method-p method)
         (or (early-fast-method-function method)
             (let ((contf (early-contf-method-function method)))
               (when contf (early-emf-from-contf contf method next-methods)))
             (error "BUG: early effective-method-function hit nonstandard method")))
        (t (error "BUG: early CALL-METHOD hit unusual method: ~a" method))))

(defun early-effective-method-function (form)
  (if (and (consp form) (eq (first form) 'call-method))
      (let ((method (second form))
            (next-methods (third form)))
        (early-emf-call-method method next-methods))
      (error "BUG: early effective-method-function hit unusual case")))

;; ----------------------------------------------------------------------
;; CALL-METHOD

(defun argforms-to-arg-info (argforms &optional env)
  (let* ((final (first (rest argforms)))
         (butl (butlast argforms)))
    (cons (and (constantp final env)
               (null (ext:constant-form-value final env)))
          (loop for s in butl
                if (symbolp s) collect (make-symbol (symbol-name s))
                  else collect (gensym "REQ-ARG")))))

;;; Convert an element of the second argument of a usual call-method
;;; into a method or form producing a method.
(defun call-method-aux (gf method &optional (arg-info '(t)))
  (cond ((method-p method) method)
        ((make-method-form-p method)
         `(make-instance ,(generic-function-method-class gf)
            ;; FIXME?: These are of course lies.
            ;; Our own method on shared-initialize will signal an error
            ;; without these initargs, though.
            :specializers '()
            :qualifiers '()
            :lambda-list '()
            ;; FIXME: Should call-next-method etc be available?
            :function (make-%method-function-fast
                       (effective-method-function
                        ',(second method) ',arg-info))))
        ;; FIXME: Delay this? Right now this error occurs during
        ;; macroexpansion of CALL- or APPLY-METHOD.
        (t (error "Invalid argument to CALL-METHOD: ~a" method))))

;;; Convert the second argument of a usual call-method into a list
;;; of methods.
(defun call-method-next-methods (gf next-methods &optional (arg-info '(t)))
  (declare (ignore arg-info))
  (loop for nmethod in next-methods
        collect (call-method-aux gf nmethod)))

(defun std-expand-apply-method (method method-arguments arguments env)
  (destructuring-bind (&optional ((&rest next-methods))) method-arguments
    (let ((arg-info (argforms-to-arg-info arguments env)))
      (cond
        ;; Inline effective accessors.
        ;; TODO: General inlining mechanism might be good.
        ((and (eq (class-of method) (find-class 'effective-reader-method))
              (> (length arguments) 1)) ; need the first argument.
         (let* ((location (with-early-accessors (+effective-accessor-method-slots+)
                            (effective-accessor-method-location method)))
                (sname (slot-definition-name
                        (accessor-method-slot-definition method)))
                (valuef
                  (cond ((si:fixnump location)
                         ;; instance location- easy
                         `(core:instance-ref ,(first arguments) ',location))
                        ((consp location)
                         ;; class location. we need to find the new cell at load time.
                         `(car ,(class-cell-form sname
                                                 (first (method-specializers method)))))
                        (t
                         (error "BUG: Slot location ~a is not a fixnum or cons" location)))))
           `(let ((value ,valuef))
              (if (cleavir-primop:eq value (core:unbound))
                  (slot-unbound (class-of ,(first arguments))
                                ,(first arguments)
                                ',sname)
                  value))))
        ((and (eq (class-of method) (find-class 'effective-writer-method))
              (> (length arguments) 2))
         (let ((location (with-early-accessors (+effective-accessor-method-slots+)
                           (effective-accessor-method-location method)))
               (sname (slot-definition-name
                       (accessor-method-slot-definition method)))
               (class (second (method-specializers method))))
           (cond ((si:fixnump location)
                  `(setf (si:instance-ref ,(second arguments) ,location)
                         ,(first arguments)))
                 ((consp location)
                  ;; class location
                  ;; Note we don't actually need the instance.
                  `(setf (car ,(class-cell-form sname class)) ,(first arguments)))
                  (t (error "BUG: Slot location ~a is not a fixnum or cons" location)))))
        ;; Standard methods
        ((fast-method-function method)
         `(apply
           ;; have to maybe do early- in case we're satiating early.
           (load-time-value (,(if (std-method-p method)
                                  'early-fast-method-function
                                  'fast-method-function)
                             ,method)
                            t)
           ,@arguments))
        ((contf-method-function method)
         `(apply
           (load-time-value (,(if (std-method-p method)
                                  'early-contf-method-function
                                  'contf-method-function)
                             ,method)
                            t)
           (load-time-value
            ,(if (null next-methods)
                 `(make-%no-next-method-continuation
                   ,method)
                 `(emf-call-method
                   ',(first next-methods)
                   '(,(rest next-methods)) ',arg-info))
            t)
           ,@arguments))
        ;; Default: AMOP protocol.
        (t `(funcall (load-time-value (method-function ,method) t)
                     ;; last element might be a vaslist
                     (apply #'list ,@arguments)
                     (load-time-value
                      (list ,@(call-method-next-methods
                               (method-generic-function method)
                               next-methods arg-info))
                      t)))))))

(defmacro apply-method (method (&rest method-arguments) &rest arguments
                        &environment env)
  "Call the given method. METHOD-ARGUMENTS are the unevaluated arguments
passed in a CALL-METHOD form after the method.
ARGUMENTS is a list of forms that will evaluate to a spreadable
argument list designator."
  ;; Pick off the standard case without calling the generic function,
  ;; for metacircularity reasons.
  (if (std-method-p method)
      (std-expand-apply-method method method-arguments arguments env)
      (expand-apply-method method method-arguments arguments env)))

(defmacro call-method (method &rest method-arguments &environment env)
  (if (make-method-form-p method)
      (second method) ; FIXME: should we try to bind CALL-NEXT-METHOD etc?
      (multiple-value-bind (required-arguments more-args)
          (effective-method-parameters env)
        `(apply-method ,method (,@method-arguments)
                       ,@required-arguments ,more-args))))

;; ----------------------------------------------------------------------
;; DEFINE-METHOD-COMBINATION
;;
;; METHOD-COMBINATION objects are instances defined in hierarchy.lisp.
;; They have slots for the name, compiler, and options. Name is obvious,
;; and the options are those provided to the thing.
;; The "compiler" is somewhat misleadingly named; it's the function that
;; outptus the effective method form.
;; These functions are stored in the global *method-combinations* hash
;; table. (the standard method on) FIND-METHOD-COMBINATION ignores the gf,
;; and makes a new METHOD-COMBINATION instance with the "compiler" looked
;; up in the hash table, and the name and options.
;; The "compiler" functions take two arguments, plus the lambda-list from
;; the define-method-combination. The first argument is the generic function
;; (used for the :generic-function option of D-M-C), the second is the sorted
;; list of applicable methods, and the rest are the method combination options.
;;

#+threads
(defparameter *method-combinations-lock* (mp:make-lock :name 'find-method-combination))
(defparameter *method-combinations* (make-hash-table :size 32 :test 'eq))


(defun search-method-combination (name)
  (mp:with-lock (*method-combinations-lock*)
    (gethash name *method-combinations*)))

(defun install-method-combination (name function)
  (mp:with-lock (*method-combinations-lock*)
    (setf (gethash name *method-combinations*) function))
  name)

(defun make-method-combination (name compiler options)
  (with-early-make-instance +method-combination-slots+
    (o (find-class 'method-combination)
       :name name
       :compiler compiler
       :options options)
    o))

;; Will be upgraded into a generic function later.
(defun find-method-combination (gf method-combination-type-name method-combination-options)
  (declare (ignore gf))
  (make-method-combination method-combination-type-name
			   (or (search-method-combination method-combination-type-name)
                               (error "~A does not name a method combination"
                                      method-combination-type-name))
			   method-combination-options))

(defun define-simple-method-combination (name &key documentation
					 identity-with-one-argument
					 (operator name))
  `(define-method-combination
     ,name (&optional (order :MOST-SPECIFIC-FIRST))
     ((around (:AROUND))
      (principal (,name) :REQUIRED t))
     ,documentation
     (let ((main-effective-method
             `(,',operator ,@(mapcar #'(lambda (x)
                                         (declare (core:lambda-name define-simple-method-combination.lambda))
                                         `(CALL-METHOD ,x NIL))
				    (if (eql order :MOST-SPECIFIC-LAST)
					(reverse principal)
					principal)))))
       (cond (around
	      `(call-method ,(first around)
		(,@(rest around) (make-method ,main-effective-method))))
	     (,(if identity-with-one-argument
		   '(rest principal)
		   t)
	      main-effective-method)
	     (t (second main-effective-method))))))

;;; See comment below.
(defmacro %magic-no-required-method (group-name)
  `(em-apply #'no-required-method .generic-function. ',group-name))

(defun define-complex-method-combination (form)
  (flet ((syntax-error ()
	   (error "~S is not a valid DEFINE-METHOD-COMBINATION form"
		  form)))
    (destructuring-bind (name lambda-list method-groups &rest body &aux
			      (group-names '())
			      (group-checks '())
			      (group-after '())
			      (generic-function '.generic-function.))
	form
      (unless (symbolp name) (syntax-error))
      (let ((x (first body)))
	(when (and (consp x) (eql (first x) :ARGUMENTS))
          (warn "Option :ARGUMENTS is not supported in DEFINE-METHOD-COMBINATION.")
	  (return-from define-complex-method-combination
            `(error "Option :ARGUMENTS is not supported in DEFINE-METHOD-COMBINATION."))))
      (let ((x (first body)))
	(when (and (consp x) (eql (first x) :GENERIC-FUNCTION))
	  (setf body (rest body))
	  (unless (symbolp (setf generic-function (second x)))
	    (syntax-error))))
      (dolist (group method-groups)
	(destructuring-bind (group-name predicate &key description
                                                    (order :most-specific-first)
                                                    (required nil))
	    group
          (declare (ignore description)) ; FIXME?
	  (if (symbolp group-name)
	      (push group-name group-names)
	      (syntax-error))
	  (let ((condition
                  (cond ((eql predicate '*) 'T)
                        ((null predicate) `(null .method-qualifiers.))
                        ((symbolp predicate)
                         `(,predicate .METHOD-QUALIFIERS.))
                        ((consp predicate)
                         (let* ((q (last predicate 0))
                                (p (copy-list (butlast predicate 0))))
                           (when (every #'symbolp p)
                             (if (eql q '*)
                                 `(every #'equal ',p .METHOD-QUALIFIERS.)
                                 `(equal ',p .METHOD-QUALIFIERS.)))))
                        (t (syntax-error)))))
	    (push `(,condition (push .METHOD. ,group-name)) group-checks))
	  (when required
	    (push `(unless ,group-name
                     ;; Effective methods can be computed in other situations than being
                     ;; about to call them. As such, compute-effective-method should not
                     ;; signal an error unless the computation is impossible. Lacking a
                     ;; required method is by contrast a problem that only needs to be
                     ;; signaled when the function is actually being called. So we return
                     ;; an error form. ...but because we want an independent function for
                     ;; the dtree interpreter, we return something specially recognizable
                     ;; by compute-outcome, so the generic function etc. can be hooked up.
                     (return-from ,name '(%magic-no-required-method ,group-name)))
		  group-after))
	  (case order
	    (:most-specific-first
	     (push `(setf ,group-name (nreverse ,group-name)) group-after))
	    (:most-specific-last)
	    (otherwise
             (let ((order-var (gensym)))
               (setf group-names (append group-names (list (list order-var order)))
                     group-after (list* `(when (eq ,order-var :most-specific-first)
                                           (setf ,group-name (nreverse ,group-name)))
                                        group-after)))))))
      `(install-method-combination ',name
				   (lambda (,generic-function .methods-list. ,@lambda-list)
                                     (declare (core:lambda-name ,name)
                                              (ignorable ,generic-function))
                                     (block ,name 
                                       (let (,@group-names)
                                         (dolist (.method. .methods-list.)
                                           (let ((.method-qualifiers. (method-qualifiers .method.)))
                                             (cond ,@(nreverse group-checks)
                                                   (t (invalid-method-error .method.
                                                                            "Method qualifiers ~S are not allowed in the method ~
			      combination ~S." .method-qualifiers. ',name)))))
                                         ,@group-after
                                         ,@body)))))))

(defmacro define-method-combination (name &body body)
  (if (and body (listp (first body)))
      (define-complex-method-combination (list* name body))
      (apply #'define-simple-method-combination name body)))

(defun method-combination-error (format-control &rest args)
  ;; FIXME! We should emit a more detailed error!
  (error "Method-combination error:~%~S"
	 (apply #'format nil format-control args)))

(defun invalid-method-error (method format-control &rest args)
  (error "Invalid method error for ~A~%~S"
	 method
	 (apply #'format nil format-control args)))



;;; ----------------------------------------------------------------------
;;; COMPUTE-EFFECTIVE-METHOD
;;;

(defun compute-effective-method-function (gf method-combination applicable-methods)
  (effective-method-function
   (compute-effective-method gf method-combination applicable-methods)))

;; will be upgraded into being the standard method on compute-effective-method in fixup.
(defun std-compute-effective-method (gf method-combination applicable-methods)
  (declare (type method-combination method-combination)
	   (type generic-function gf)
	   (optimize speed (safety 0)))
  ;; FIXME: early accessors here could technically be bad, if someone subclasses method-combination
  ;; On the other hand, I've never seen anyone do that. D-M-C already has arbitrary code, and
  ;; method combinations have no defined accessors - all you could do is add methods to
  ;; compute-effective-method, itself unusual because, again, arbitrary code.
  (with-early-accessors (+method-combination-slots+)
    (let* ((compiler (method-combination-compiler method-combination))
	   (options (method-combination-options method-combination)))
      (if options
	  (apply compiler gf applicable-methods options)
	  (funcall compiler gf applicable-methods)))))

(define-method-combination standard ()
    ((around (:around))
     (before (:before))
     (primary () :required t)
     (after (:after)))
  (flet ((call-methods (methods)
           (mapcar (lambda (method)
                     `(call-method ,method))
                   methods)))
    ;; We're a bit more hopeful about avoiding make-method and m-v-p1 than
    ;; the example in CLHS define-method-combination.
    ;; Performance impact is likely to be marginal at best, but why not try?
    (let* ((call-primary `(call-method ,(first primary) ,(rest primary)))
           (call-before (if before
                            `(progn ,@(call-methods before) ,call-primary)
                            call-primary))
           (call-after (if after
                           `(multiple-value-prog1 ,call-before
                              ,@(call-methods (reverse after)))
                           call-before))
           (call-around (if around
                            (if (and (null before) (null after))
                                `(call-method ,(first around)
                                              (,@(rest around)
                                               ,@primary))
                                `(call-method ,(first around)
                                              (,@(rest around)
                                               (make-method ,call-after))))
                            call-after)))
      call-around)))

(define-method-combination progn :identity-with-one-argument t)
(define-method-combination and :identity-with-one-argument t)
(define-method-combination max :identity-with-one-argument t)
(define-method-combination + :identity-with-one-argument t)
(define-method-combination nconc :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument nil)
(define-method-combination list :identity-with-one-argument nil)
(define-method-combination min :identity-with-one-argument t)
(define-method-combination or :identity-with-one-argument t)
