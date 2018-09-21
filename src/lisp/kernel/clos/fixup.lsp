;; Should be commented out
#+(or)
(eval-when (:execute)
  (setq core:*echo-repl-read* t))

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
#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *echo-repl-read* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For debugging this file
;;; (Which happens a fair amount, because it's where CLOS begins use.)

;;; This will print every form as its compiled
#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "Starting fixup.lsp")
  (setq *echo-repl-tpl-read* t)
  (setq *load-print* t)
  (setq *echo-repl-read* t))

#+mlog
(eval-when (:compile-toplevel :execute)
  (setq core::*debug-dispatch* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define generics for core functions.

(defun function-to-method (name lambda-list specializers
                           &optional (function (fdefinition name)))
  (mlog "function-to-method: name -> %s specializers -> %s  lambda-list -> %s%N" name specializers lambda-list)
  (mlog "function-to-method:  function -> %s%N" function)
  ;; since we still have method.lsp's add-method in place, it will try to add
  ;; the function-to-method-temp entry to *early-methods*. but then we unbind
  ;; that, so things are a bit screwy. We do it more manually.
  (let* ((f (ensure-generic-function 'function-to-method-temp)) ; FIXME: just make an anonymous one?
         (method
           ;; we're still using the old add-method, which adds things to *early-methods*.
           ;; We don't want to do that here, so we rebind and discard.
           (let ((*early-methods* nil))
             (add-method f
                         (make-method (find-class 'standard-method)
                                      nil
                                      (mapcar #'find-class specializers)
                                      lambda-list
                                      (lambda (.method-args. .next-methods.)
                                        (declare (core:lambda-name function-to-method.lambda))
                                        (mlog "In function-to-method.lambda  about to call %s with args %s%N"
                                              function (core:list-from-va-list .method-args.))
                                        (apply function .method-args.))
                                      (list
                                       'leaf-method-p t
                                       'fast-method-function (if (lambda-list-fast-callable-p lambda-list)
                                                                 function nil)))))))
    (mlog "function-to-method: installed method%N")
    (core:function-lambda-list-set f lambda-list) ; hook up the introspection
    (setf (fdefinition name) f
          (generic-function-name f) name)
    (when (boundp '*early-methods*)
      (push (cons name (list method)) *early-methods*)))
  (fmakunbound 'function-to-method-temp))

(function-to-method 'compute-applicable-methods
                    '(generic-function arguments)
                    '(standard-generic-function t)
                    #'std-compute-applicable-methods)

(function-to-method 'compute-applicable-methods-using-classes
                    '(generic-function classes)
                    '(standard-generic-function t)
                    #'std-compute-applicable-methods-using-classes)

(function-to-method 'compute-effective-method
                    '(generic-function method-combination applicable-methods)
                    '(standard-generic-function method-combination t)
                    #'std-compute-effective-method)

(function-to-method 'generic-function-method-class '(gf)
                    '(standard-generic-function))

(function-to-method 'find-method-combination
                    '(gf method-combination-type-name method-combination-options)
                    '(standard-generic-function t t))

(mlog "done with the first function-to-methods%N")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Satiate

;;; Every gf needs a specializer profile, not just satiated ones
;;; They pretty much all need one, and before any gf calls, so we do this
;;; before calling add-direct-method below

(dolist (method-info *early-methods*)
  (satiation-setup-specializer-profile
   (fdefinition (car method-info))))

(mlog "About to satiate%N")

(eval-when (:execute)
  (satiate-minimal-generic-functions))
(eval-when (:load-toplevel)
  (defun early-find-method (gf qualifiers specializers &optional (errorp t))
    (declare (notinline method-qualifiers))
    (with-early-accessors (+eql-specializer-slots+
                           +standard-generic-function-slots+
                           +standard-method-slots+)
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
                do (return-from early-find-method method))
        ;; If we did not find any matching method, then the list of
        ;; specializers might have the wrong size and we must signal
        ;; an error.
        (when errorp
          (error "There is no method on the generic function ~S that agrees on qualifiers ~S and specializers ~S"
                 (generic-function-name gf)
                 qualifiers specializers)))
      nil))
  (macrolet ((find-method (&rest args)
               `(early-find-method ,@args)))
    (with-early-accessors (+standard-method-slots+)
      (satiate class-slots
               (standard-class)
               (funcallable-standard-class))
      (satiate compute-applicable-methods-using-classes
               (standard-generic-function cons)
               (standard-generic-function null))
      (satiate compute-applicable-methods
               (standard-generic-function cons)
               (standard-generic-function null))
      (satiate compute-effective-method
               (standard-generic-function method-combination cons)
               (standard-generic-function method-combination null))
      (satiate method-qualifiers     ; called by method combinations
               (standard-method)
               (standard-reader-method) (standard-writer-method))
      (satiate method-specializers
               (standard-method)
               (standard-reader-method) (standard-writer-method))
      (satiate method-function
               (standard-method)
               (standard-reader-method) (standard-writer-method))
      (satiate accessor-method-slot-definition
               (standard-reader-method) (standard-writer-method))
      (satiate slot-definition-allocation
               (standard-direct-slot-definition)
               (standard-effective-slot-definition))
      (satiate slot-definition-name
               (standard-direct-slot-definition)
               (standard-effective-slot-definition))
      (satiate slot-definition-location
               (standard-direct-slot-definition)
               (standard-effective-slot-definition))
      (satiate generic-function-name
               (standard-generic-function))
      (satiate generic-function-method-combination
               (standard-generic-function))
      (satiate generic-function-lambda-list
               (standard-generic-function))
      (satiate leaf-method-p
               (standard-method)
               (standard-reader-method) (standard-writer-method))
      (satiate fast-method-function
               (standard-method)
               (standard-reader-method) (standard-writer-method)))))

(mlog "Done satiating%N")

;;; Generic functions can be called now!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Make methods real

;;; First generic function calls done here.

(defun register-method-with-specializers (method)
  (loop for spec in (method-specializers method)
        do (add-direct-method spec method)))

(defun fixup-early-methods ()
  (dolist (method-info *early-methods*)
    (dolist (method (cdr method-info))
      (register-method-with-specializers method))))

(fixup-early-methods)

(makunbound '*early-methods*)

;;; *early-methods* is used by the primitive add-method in method.lsp.
;;; Avoid defining any new methods until the new add-method is installed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Redefine ENSURE-GENERIC-FUNCTION

;;; Uses generic functions properly now.
;;; DEFMETHOD and INSTALL-METHOD and stuff call ensure-generic-function,
;;; so after this they will do generic function calls.

(defun ensure-generic-function (name &rest args &key &allow-other-keys)
  (mlog "ensure-generic-function  name -> %s  args -> %s %N" name args)
  (mlog "(not (fboundp name)) -> %s%N" (not (fboundp name)))
  (let ((gfun (si::traced-old-definition name)))
    (cond ((not (legal-generic-function-name-p name))
	   (simple-program-error "~A is not a valid generic function name" name))
          ((not (fboundp name))
           (mlog "A gfun -> %s name -> %s  args -> %s%N" gfun name args)
           ;;           (break "About to setf (fdefinition name)")
           (mlog "#'ensure-generic-function-using-class -> %s%N" #'ensure-generic-function-using-class )
	   (setf (fdefinition name)
		 (apply #'ensure-generic-function-using-class gfun name args)))
          ((si::instancep (or gfun (setf gfun (fdefinition name))))
           (mlog "B%N")
	   (let ((new-gf (apply #'ensure-generic-function-using-class gfun name args)))
	     new-gf))
	  ((special-operator-p name)
           (mlog "C%N")
	   (simple-program-error "The special operator ~A is not a valid name for a generic function" name))
	  ((macro-function name)
           (mlog "D%N")
	   (simple-program-error
            "The symbol ~A is bound to a macro and is not a valid name for a generic function" name))
          ((not *clos-booted*)
           (mlog "E%N")
           (setf (fdefinition name)
		 (apply #'ensure-generic-function-using-class nil name args))
           (fdefinition name))
	  (t
	   (simple-program-error "The symbol ~A is bound to an ordinary function and is not a valid name for a generic function" name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Redefine things to their final form.

(defun method-p (method) (typep method 'METHOD))

(defun make-method (method-class qualifiers specializers arglist function options)
  (apply #'make-instance
	 method-class
	 :generic-function nil
	 :qualifiers qualifiers
	 :lambda-list arglist
	 :specializers specializers
	 :function function
	 :allow-other-keys t
	 options))

(defun all-keywords (l)
  (let ((all-keys '()))
    (do ((l (rest l) (cddddr l)))
	((null l)
	 all-keys)
      (push (first l) all-keys))))

(defun congruent-lambda-p (l1 l2)
  (multiple-value-bind (r1 opts1 rest1 key-flag1 keywords1 a-o-k1)
      (si::process-lambda-list l1 'FUNCTION)
    (declare (ignore a-o-k1))
    (multiple-value-bind (r2 opts2 rest2 key-flag2 keywords2 a-o-k2)
	(si::process-lambda-list l2 'FUNCTION)
      (and (= (length r2) (length r1))
           (= (length opts1) (length opts2))
           (eq (and (null rest1) (null key-flag1))
               (and (null rest2) (null key-flag2)))
           ;; All keywords mentioned in the genericf function
           ;; must be accepted by the method.
           (or (null key-flag1)
               (null key-flag2)
               a-o-k2
               (null (set-difference (all-keywords keywords1)
                                     (all-keywords keywords2))))
           t))))

;;; It's possible we could use DEFMETHOD for these.

(defun add-method (gf method)
  ;; during boot it's a structure accessor
  (declare (notinline method-qualifiers remove-method))
  ;;
  ;; 1) The method must not be already installed in another generic function.
  ;;
  (let ((other-gf (method-generic-function method)))
    (unless (or (null other-gf) (eq other-gf gf))
      (error "The method ~A belongs to the generic function ~A ~
and cannot be added to ~A." method other-gf gf)))
  ;;
  ;; 2) The method and the generic function should have congruent lambda
  ;;    lists. That is, it should accept the same number of required and
  ;;    optional arguments, and only accept keyword arguments when the generic
  ;;    function does.
  ;;
  (let ((new-lambda-list (method-lambda-list method)))
    (if (slot-boundp gf 'lambda-list)
	(let ((old-lambda-list (generic-function-lambda-list gf)))
	  (unless (congruent-lambda-p old-lambda-list new-lambda-list)
	    (error "Cannot add the method ~A to the generic function ~A because their lambda lists ~A and ~A are not congruent."
		   method gf old-lambda-list new-lambda-list)))
	(reinitialize-instance gf :lambda-list new-lambda-list)))
  ;;
  ;; 3) Finally, it is inserted in the list of methods, and the method is
  ;;    marked as belonging to a generic function.
  ;;
  (when (generic-function-methods gf)
    (let* ((method-qualifiers (method-qualifiers method)) 
	   (specializers (method-specializers method))
	   (found (find-method gf method-qualifiers specializers nil)))
      (when found
	(remove-method gf found))))
  ;;
  ;; We install the method by:
  ;;  i) Adding it to the list of methods
  (push method (generic-function-methods gf))
  (setf (method-generic-function method) gf)
  ;;  FIXME!!!  Method specializers should be implemented shouldn't they? meister
  ;;  ii) Updating the specializers list of the generic function. Notice that
  ;;  we should call add-direct-method for each specializer but specializer
  ;;  objects are not yet implemented
  #+(or)
  (dolist (spec (method-specializers method))
    (add-direct-method spec method))
  ;;  iii) Computing a new discriminating function... Well, since the core
  ;;  ECL does not need the discriminating function because we always use
  ;;  the same one, we just update the spec-how list of the generic function.
  (compute-g-f-spec-list gf)
  ;; Clasp must update the specializer-profile
  #+clasp
  (progn
    (update-specializer-profile gf (method-specializers method))
    (update-generic-function-call-history-for-add-method gf method))
  (set-funcallable-instance-function gf (compute-discriminating-function gf))
  ;;  iv) Update dependents.
  (update-dependents gf (list 'add-method method))
  ;;  v) Register with specializers
  (register-method-with-specializers method)
  gf)

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
	(delete method (generic-function-methods gf))
	(method-generic-function method) nil)
  (loop for spec in (method-specializers method)
     do (remove-direct-method spec method))
  (compute-g-f-spec-list gf)
  #+clasp
  (progn
    (compute-and-set-specializer-profile gf)
    (update-generic-function-call-history-for-remove-method gf method))
  (set-funcallable-instance-function gf (compute-discriminating-function gf))
  (update-dependents gf (list 'remove-method method))
  gf)


;;(setq cmp:*debug-compiler* t)
(function-to-method 'add-method '(gf method) '(standard-generic-function standard-method))
(function-to-method 'remove-method '(gf method) '(standard-generic-function standard-method))
(function-to-method 'find-method '(gf qualifiers specializers &optional error)
                    '(standard-generic-function t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error messages

(defgeneric no-applicable-method (gf &rest args)
  (declare (optimize (debug 3))))

;;; FIXME: use actual condition classes

(defmethod no-applicable-method (gf &rest args)
  (declare (optimize (debug 3)))
  (error "No applicable method for ~S with ~
          ~:[no arguments~;arguments of types ~:*~{~& ~A~}~]."
         (generic-function-name gf)
         (mapcar #'type-of args)))

(defmethod no-next-method (gf method &rest args)
  (declare (ignore gf))
  (error "In method ~A~%No next method given arguments ~A" method args))

(defun no-required-method (gf group-name &rest args)
  (error "No applicable methods in required group ~a for generic function ~a~@
          Given arguments: ~a"
         group-name gf args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MISCELLANY

(defmethod reader-method-class ((class std-class)
				(direct-slot direct-slot-definition)
				&rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-reader-method))

(defmethod writer-method-class ((class std-class)
				(direct-slot direct-slot-definition)
				&rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-writer-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finish initializing classes that we defined in C++ that
;;; are not in :COMMON-LISP or :SYS package
;;; so that we can use them as specializers for generic functions

(defun gather-cxx-classes ()
  (let ((additional-classes (reverse core:*all-cxx-classes*))
	classes)
    (dolist (class-symbol additional-classes)
      (unless (or (eq class-symbol 'core::model)
                  (eq class-symbol 'core::instance)
                  (assoc class-symbol +class-hierarchy+))
        (push class-symbol classes)))
    (nreverse classes)))

(defun add-cxx-class (class-symbol)
    (let* ((class (find-class class-symbol))
	   (supers-names (mapcar #'(lambda (x) (class-name x))
                                 (clos:direct-superclasses class))))
      (ensure-boot-class class-symbol :metaclass 'core:cxx-class ;; was 'builtin-class
                         :direct-superclasses supers-names)
      (finalize-inheritance class)))

(defun add-extra-classes (additional-classes)
  (dolist (class-symbol additional-classes)
    (add-cxx-class class-symbol)))

;;
;; Initialize all extra classes
;;
(add-extra-classes (gather-cxx-classes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; We define the MAKE-LOAD-FORM for source-pos-info early, so that it can be
;;; used in the expansion of the defclass below.
;;; Most MAKE-LOAD-FORMs are in print.lsp.

(defmethod make-load-form ((object core:source-pos-info) &optional environment)
  `(core:make-cxx-object 'core:source-pos-info
                         :sfi (core:decode (core:make-cxx-object 'core:source-file-info)
                                           ',(core:encode (core:source-file-info
                                                           (core:source-pos-info-file-handle object))))
                         :fp ,(core:source-pos-info-filepos object)
                         :l ,(core:source-pos-info-lineno object)
                         :c ,(core:source-pos-info-column object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEPENDENT MAINTENANCE PROTOCOL
;;;

(defmethod add-dependent ((c class) dep)
  (pushnew dep (class-dependents c)))

(defmethod add-dependent ((c generic-function) dependent)
  (pushnew dependent (generic-function-dependents c)))

(defmethod remove-dependent ((c class) dep)
  (setf (class-dependents c)
        (remove dep (class-dependents c))))

(defmethod remove-dependent ((c standard-generic-function) dep)
  (setf (generic-function-dependents c)
        (remove dep (generic-function-dependents c))))

(defmethod map-dependents ((c class) function)
  (dolist (d (class-dependents c))
    (funcall function d)))

(defmethod map-dependents ((c standard-generic-function) function)
  (dolist (d (generic-function-dependents c))
    (funcall function d)))

(defgeneric update-dependent (object dependent &rest initargs))

;; After this, update-dependents will work
(setf *clos-booted* 'map-dependents)


(defclass initargs-updater ()
  ())

(defun recursively-update-class-initargs-cache (a-class)
  ;; Bug #588: If a class is forward referenced and you define an initialize-instance
  ;; (or whatever) method on it, it got here and tried to compute valid initargs, which
  ;; involved taking the class-prototype, which couldn't be allocated of course.
  ;; There's no value in precomputing the initargs for an unfinished class, so we don't.
  (when (class-finalized-p a-class)
    (precompute-valid-initarg-keywords a-class)
    (mapc #'recursively-update-class-initargs-cache (class-direct-subclasses a-class))))

(defmethod update-dependent ((object generic-function) (dep initargs-updater)
			     &rest initargs
                             &key ((add-method added-method) nil am-p)
                               ((remove-method removed-method) nil rm-p)
                             &allow-other-keys)
  (declare (ignore initargs))
  (let ((method (cond (am-p added-method) (rm-p removed-method))))
    ;; update-dependent is also called when the gf itself is reinitialized, so make sure we actually have
    ;; a method that's added or removed
    (when method
      (let ((spec (first (method-specializers method)))) ; the class being initialized or allocated
        (when (classp spec) ; sanity check against eql specialization
          (recursively-update-class-initargs-cache spec))))))

(let ((x (make-instance 'initargs-updater)))
  (add-dependent #'shared-initialize x)
  (add-dependent #'initialize-instance x)
  (add-dependent #'allocate-instance x))

(function-to-method 'make-method-lambda
                    '(gf method lambda-form environment)
                    '(standard-generic-function standard-method t t))

(function-to-method 'compute-discriminating-function '(gf)
                    '(standard-generic-function))

(function-to-method 'print-object
                    '(object stream)
                    '(t t))
