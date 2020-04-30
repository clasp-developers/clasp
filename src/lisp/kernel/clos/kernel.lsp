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

#+(or)(eval-when (:execute)
        (setq core:*echo-repl-read* t))

(defparameter *clos-booted* nil)
(export '*clos-booted*)

;;; Returns a closure usable as a discriminating function
;;; when the generic function is in the invalidated state.
(defun invalidated-discriminating-function-closure (gf)
  (lambda (core:&va-rest args)
    (declare (core:lambda-name invalidated-discriminating-function))
    (invalidated-dispatch-function gf args)))

;;; Sets a GF's discrminating function to the "invalidated" state.
;;; In this state, the next call will compute a real discriminating function.
(defun invalidate-discriminating-function (gf)
  (set-funcallable-instance-function
   gf (invalidated-discriminating-function-closure gf)))

;;; ----------------------------------------------------------------------
;;;
;;; FIND-CLASS  naming classes.
;;;
;;;
;;; (FIND-CLASS <name>) returns the class named <name>.  setf can be used
;;; with find-class to set the class named <name>.  These are "extrinsic"
;;; names.  Neither find-class nor setf of find-class do anything with the
;;; name slot of the class, they only lookup and change the association from
;;; name to class.
;;; 
;;; This is only used during boot.
#+(or)
(eval-when (:compile-toplevel #+clasp-boot :load-toplevel)
  (defun (setf find-class) (new-value class &optional errorp env)
    (warn "Ignoring class definition for ~S" class)))

(defun (setf find-class) (new-value name &optional errorp env)
  (declare (ignore errorp env))
  (let ((old-class (find-class name nil)))
    (cond
      ((and old-class
	    (or (typep old-class 'built-in-class)
		(member name '(class built-in-class) :test #'eq)))
       (unless (eq new-value old-class)
	 (error "The class associated to the CL specifier ~S cannot be changed."
		name)))
      ((or (classp new-value) (null new-value))
       (core:setf-find-class new-value name)
       #+static-gfs
       (static-gfs:invalidate-named-constructors name))
      (t (error 'simple-type-error :datum new-value :expected-type '(or class null)
                                   :format-control "~A is not a valid class for (setf find-class)"
                                   :format-arguments (list new-value)))))
  new-value)


;;; ----------------------------------------------------------------------
;;; Methods

(defun install-method (name qualifiers specializers lambda-list fun &rest options)
  (declare (notinline ensure-generic-function))
;  (record-definition 'method `(method ,name ,@qualifiers ,specializers))
  (let* ((gf (ensure-generic-function name))
	 (method (make-method (generic-function-method-class gf)
			      qualifiers specializers lambda-list
			      fun options)))
    (add-method gf method)
    method))

;;; ----------------------------------------------------------------------
;;;                                                         early versions

;;; early version used during bootstrap
(defun ensure-generic-function (name &key (lambda-list (si::unbound) l-l-p))
  (if (and (fboundp name) (si::instancep (fdefinition name)))
      (fdefinition name)
      ;; create a fake standard-generic-function object:
      (with-early-make-funcallable-instance +standard-generic-function-slots+
	(gfun (find-class 'standard-generic-function)
	      :lambda-list lambda-list
              :method-combination nil
	      :argument-precedence-order
	      (and l-l-p (rest (si::process-lambda-list lambda-list t)))
	      :method-class (find-class 'standard-method)
	      :docstring nil
	      :methods nil
	      :a-p-o-function nil
	      :declarations nil
	      :dependents nil)
        (setf-function-name gfun name)
        (setf (generic-function-method-combination gfun)
              (find-method-combination gfun 'standard nil))
        (invalidate-discriminating-function gfun)
	(setf (fdefinition name) gfun)
	gfun)))

;;; made into generic functions later
(defun generic-function-name (generic-function)
  (core:function-name generic-function))

(defun (setf generic-function-name) (new-name gf)
  (if *clos-booted*
      (reinitialize-instance gf :name new-name)
      (setf-function-name gf new-name))
  new-name)

;;; Will be the standard method after fixup.
(defun compute-discriminating-function (generic-function)
  (invalidated-discriminating-function-closure generic-function))

;;; ----------------------------------------------------------------------
;;; COMPUTE-APPLICABLE-METHODS
;;;
;;; This part is a source of problems because we have to access slots of
;;; various objects, which could potentially lead to infinite recursion as
;;; those accessors require also some dispatch. The solution is to avoid
;;; calling then generic function that implement the accessors.
;;; This is possible because:
;;;   1. The user can only extend compute-applicable-methods if it
;;;      defines a method with a subclass of standard-generic-function
;;;   2. The user cannot extend slot-value and friends on standard-classes
;;;      due to the restriction "Any method defined by a portable program
;;;      on a specified generic function must have at least one specializer
;;;      that is neither a specified class nor an eql specializer whose
;;;      associated value is an instance of a specified class."
;;;   3. Subclasses of specified classes preserve the slot order in ECL.
;;;
(defun std-compute-applicable-methods (gf args)
  (sort-applicable-methods gf
                           (applicable-method-list gf args)
                           (mapcar #'class-of args)))

(setf (fdefinition 'compute-applicable-methods) #'std-compute-applicable-methods)

(defun applicable-method-list (gf args)
  (declare (optimize (speed 3)))
  (with-early-accessors (+standard-method-slots+
			 +standard-generic-function-slots+
			 +eql-specializer-slots+
			 +standard-class-slots+)
    (flet ((applicable-method-p (method args)
	     (loop for spec in (method-specializers method)
		for arg in args
		always (if (eql-specializer-flag spec)
			   (eql arg (eql-specializer-object spec))
			   (si::of-class-p arg spec)))))
      (loop for method in (generic-function-methods gf)
	 when (applicable-method-p method args)
	 collect method))))

#+mlog
(defun std-compute-applicable-methods-using-classes (gf classes)
  (declare (optimize (speed 3)))
  (with-early-accessors
      (+standard-method-slots+ +eql-specializer-slots+ +standard-generic-function-slots+)
    (flet ((applicable-method-p (method classes)
             (loop for spec in (method-specializers method)
                   for class in classes
                   always (cond ((eql-specializer-flag spec)
                                 ;; EQL specializer invalidate computation                       
                                 ;; we return NIL                                                
                                 (when (si::of-class-p (eql-specializer-object spec) class)
                                   (return-from std-compute-applicable-methods-using-classes
                                     (values nil nil)))
                                 nil)
                                ((si::subclassp class spec))))))
      (mlog "std-compute-applicable-methods-using-classes gf -> %s classes -> %s%N" gf classes)
      (let ((result (sort-applicable-methods
                     gf
                     (loop for method in (generic-function-methods gf)
                           when (applicable-method-p method classes)
                             collect method)
                     classes)))
        (mlog "  result -> %s%N" result)
        (values result t)))))

#-mlog
(defun std-compute-applicable-methods-using-classes (gf classes)
  (declare (optimize (speed 3)))
  (with-early-accessors (+standard-method-slots+ +eql-specializer-slots+ +standard-generic-function-slots+)
    (mlog "std-compute-applicable-methods-using-classes gf -> %s classes -> %s%N" gf classes)
    (flet ((applicable-method-p (method classes)
	     (loop for spec in (method-specializers method)
		for class in classes
		always (cond ((eql-specializer-flag spec)
			      ;; EQL specializer can invalidate computation
			      (when (si::of-class-p (eql-specializer-object spec) class)
				(return-from std-compute-applicable-methods-using-classes
				  (values nil nil)))
			      nil)
			     ((si::subclassp class spec))))))
      (values (sort-applicable-methods
	       gf
	       (loop for method in (generic-function-methods gf)
		  when (applicable-method-p method classes)
		  collect method)
	       classes)
	      t))))

(defun sort-applicable-methods (gf applicable-list args-specializers)
  (declare (optimize (safety 0) (speed 3)))
  (with-early-accessors (+standard-method-slots+ +standard-generic-function-slots+)
    (let ((f (generic-function-a-p-o-function gf)))
      ;; reorder args to match the precedence order
      (when f
        (setf args-specializers
              (funcall f (subseq args-specializers 0
                                 (length (generic-function-argument-precedence-order gf))))))
      ;; then order the list
      (do* ((scan applicable-list)
            (most-specific (first scan) (first scan))
            (ordered-list))
           ((null (cdr scan))
            (when most-specific
              ;; at least one method
              (nreverse
               (push most-specific ordered-list))))
        (dolist (meth (cdr scan))
          (when (eql (compare-methods most-specific meth args-specializers f) 2)
            (setq most-specific meth)))
        (setq scan (delete most-specific scan))
        (push most-specific ordered-list)))))

(defun compare-methods (method-1 method-2 args-specializers f)
  (with-early-accessors (+standard-method-slots+)
    (let* ((specializers-list-1 (method-specializers method-1))
	   (specializers-list-2 (method-specializers method-2)))
      (compare-specializers-lists (if f (funcall f specializers-list-1) specializers-list-1)
				  (if f (funcall f specializers-list-2) specializers-list-2)
				  args-specializers))))

(defun compare-specializers-lists (spec-list-1 spec-list-2 args-specializers)
  (when (or spec-list-1 spec-list-2)
    (ecase (compare-specializers (first spec-list-1)
				 (first spec-list-2)
				 (first args-specializers))
      (1 '1)
      (2 '2)
      (= 
       (compare-specializers-lists (cdr spec-list-1)
				   (cdr spec-list-2)
				   (cdr args-specializers)))
      ((nil)
       (error "The type specifiers ~S and ~S can not be disambiguated~
                  with respect to the argument specializer: ~S"
	      (or (car spec-list-1) t)
	      (or (car spec-list-2) t)
	      (car args-specializers)))))
  )

(defun fast-subtypep (spec1 spec2)
  ;; Specialized version of subtypep which uses the fact that spec1
  ;; and spec2 are either classes or of the form (EQL x)
  (with-early-accessors (+eql-specializer-slots+ +standard-class-slots+)
    (if (eql-specializer-flag spec1)
	(if (eql-specializer-flag spec2)
	    (eql (eql-specializer-object spec1)
		 (eql-specializer-object spec2))
	    (si::of-class-p (eql-specializer-object spec1) spec2))
	(if (eql-specializer-flag spec2)
	    ;; There is only one class with a single element, which
	    ;; is NULL = (MEMBER NIL).
	    (and (null (eql-specializer-object spec2))
		 (eq (class-name spec1) 'null))
	    (si::subclassp spec1 spec2)))))

(defun compare-specializers (spec-1 spec-2 arg-class)
  (with-early-accessors (+standard-class-slots+ +standard-class-slots+)
    (let* ((cpl (class-precedence-list arg-class)))
      (cond ((eq spec-1 spec-2) '=)
            ((fast-subtypep spec-1 spec-2) '1)
            ((fast-subtypep spec-2 spec-1) '2)
            ((eql-specializer-flag spec-1) '1) ; is this engough?
            ((eql-specializer-flag spec-2) '2) ; Beppe
            ((member spec-1 (member spec-2 cpl)) '2)
            ((member spec-2 (member spec-1 cpl)) '1)
	    ;; This will force an error in the caller
	    (t nil)))))

;;; mutates the specializer profile to account for new specializers.
(defun update-specializer-profile (specializer-profile specializers)
  (with-early-accessors (+eql-specializer-slots+)
    (loop for spec in specializers
          for i from 0
          for e = (svref specializer-profile i)
          do (setf (svref specializer-profile i)
                   (cond ((eql-specializer-flag spec)
                          (let ((o (eql-specializer-object spec)))
                            ;; Add to existing list of eql spec
                            ;; objects, or make a new one.
                            (if (consp e)
                                (cons o e)
                                (list o))))
                         ((eql spec +the-t-class+) (or e nil))
                         (t t)))))
  specializer-profile)

;;; Add one method to the specializer profile.
(defun update-gf-specializer-profile (gf specializers)
  (with-early-accessors (+standard-generic-function-slots+)
    (let* ((sv (generic-function-specializer-profile gf))
           (to-update (or sv (make-array (length specializers)
                                         :initial-element nil))))
      (update-specializer-profile to-update specializers))))

;;; Recompute the specializer profile entirely.
;;; Needed if a method has been removed.
(defun compute-gf-specializer-profile (gf)
  (with-early-accessors (+standard-generic-function-slots+
			 +standard-method-slots+)
    (setf (generic-function-specializer-profile gf)
          (let ((sp nil))
            (dolist (method (generic-function-methods gf))
              (let ((specializers (method-specializers method)))
                (when (null sp)
                  (setf sp (make-array (length specializers))))
                (update-specializer-profile sp specializers)))
            sp))))

(defun compute-a-p-o-function (gf)
  (with-early-accessors (+standard-generic-function-slots+)
    (let ((a-p-o (generic-function-argument-precedence-order gf))
          (gf-ll (generic-function-lambda-list gf)))
      (setf (generic-function-a-p-o-function gf)
            (if (consp gf-ll)
                (let ((required-arguments (rest (core:process-lambda-list gf-ll t))))
                  (if (equal a-p-o required-arguments)
                      nil
                      (coerce `(lambda (%list)
                                 (destructuring-bind ,required-arguments %list
                                   (list ,@a-p-o)))
                              'function)))
                nil)))))

;;; Will be upgraded to a method in fixup.
(defun print-object (object stream)
  (print-unreadable-object (object stream)
    ;; We don't just use :type, because that outputs an extra space.
    (let ((*package* (find-package "CL")))
      (format stream "~S"
              (class-name (si:instance-class object)))))
  object)
