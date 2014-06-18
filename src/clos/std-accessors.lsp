;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.o
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
;;; ACCESSOR / READER / WRITER METHOD CREATION
;;;
;;; The following code creates optimized and unoptimized versions of the
;;; slot accessors defined for a class. They are designed so that at least
;;; some varieties work at boot time.
;;; 

(defun safe-slot-definition-location (slotd &optional default)
  (cond ((listp slotd)
	 (error "List instead of a slot definition object"))
	(t
	 (slot-value slotd 'location))))

(defun std-class-sealed-accessors (index)
  (declare (si::c-local)
	   (fixnum index))
  (values #'(lambda (self)
              (declare (optimize (safety 0) (speed 3) (debug 0))
                       (standard-object self))
              (ensure-up-to-date-instance self)
	      (safe-instance-ref self index))
	  #'(lambda (value self)
              (declare (optimize (safety 0) (speed 3) (debug 0))
                       (standard-object self))
              (ensure-up-to-date-instance self)
	      (si:instance-set self index value))))

(defun std-class-accessors (slot-name)
  (declare (si::c-local))
  ;; The following are very slow. We do not optimize for the slot position.
  (values #'(lambda (self)
	      (slot-value self slot-name))
	  #'(lambda (value self)
	      (setf (slot-value self slot-name) value))))

(defun safe-add-method (name method)
  ;; Adds a method to a function which might have been previously defined
  ;; as non-generic, without breaking the function
  (cond ((or *clos-booted*
	     (not (fboundp name))
	     (si::instancep (fdefinition name)))
	 (add-method (ensure-generic-function name) method))
	(t
	 (let* ((alt-name '#:foo)
		(gf (ensure-generic-function alt-name)))
	   (add-method gf method)
	   (setf (generic-function-name gf) name)
	   (setf (fdefinition name) gf)
	   (fmakunbound alt-name)))))

(defun std-class-generate-accessors (standard-class &optional (optimizable t))
  ;;
  ;; The accessors are closures, which are generated every time the
  ;; slots of the class change. The accessors are safe: they check that
  ;; the slot is bound after retreiving the value, and they may take
  ;; the liberty of using SI:INSTANCE-REF because they know the class of
  ;; the instance.
  ;;
  (dolist (slotd (slot-value standard-class 'direct-slots))
    (with-slots ((name name) (allocation allocation) (location location)
		 (readers readers) (writers writers))
	slotd
      ;; When a class is of a specified class in the MOP (such as
      ;; STANDARD-CLASS), then the user may not write any method
      ;; around SLOT-VALUE-USING-CLASS. This allows us to write
      ;; optimized versions of the accessors.
      (unless (member (slot-value standard-class 'name)
		      '(standard-class
			funcallable-standard-class
			structure-class))
	(setf optimizable nil))
      (multiple-value-bind (reader writer)
	  (cond ((and optimizable
		      (eq allocation :instance)
		      ;; This is an extension by ECL in which a direct slot
		      ;; definition specifies the location of a slot. It
		      ;; only happens for sealed classes.
		      (typep location 'fixnum))
		 (std-class-sealed-accessors location))
		(t
		 (std-class-accessors name)))
	(let* ((options (list :slot-definition slotd))
	       (reader-args (list* :function reader
				   :generic-function nil
				   :qualifiers nil
				   :lambda-list '(object)
				   :specializers `(,standard-class)
				   options))
	       (reader-class (if (boundp '*early-methods*)
				 'standard-reader-method
				 (apply #'reader-method-class standard-class slotd
					reader-args)))
	       (writer-args (list* :function writer
				   :generic-function nil
				   :qualifiers nil
				   :lambda-list '(value object)
				   :specializers `(,(find-class t) ,standard-class)
				   options))
	       (writer-class (if (boundp '*early-methods*)
				 'standard-writer-method
				 (apply #'writer-method-class standard-class slotd
					writer-args))))
	  (dolist (fname readers)
	    (let ((method (make-method reader-class nil `(,standard-class) '(self)
				       (wrapped-method-function reader)
				       options)))
	      (safe-add-method fname method)
	      ;; This is redundant, but we need it at boot time because
	      ;; the early MAKE-METHOD does not use the options field.
	      (unless *clos-booted*
		(setf (slot-value method 'slot-definition) slotd))))
	  (dolist (fname writers)
	    (let ((method (make-method writer-class nil
				       `(,(find-class t) ,standard-class) '(value self)
				       (wrapped-method-function writer)
				       options)))
	      (safe-add-method fname method)
	      ;; This is redundant, but we need it at boot time because
	      ;; the early MAKE-METHOD does not use the options field.
	      (unless *clos-booted*
		(setf (slot-value method 'slot-definition) slotd)))))))))

(defun reader-closure (index)
  (declare (si::c-local))
  (lambda (object) (si::instance-ref object index)))

(defun writer-closure (index)
  (declare (si::c-local))
  (lambda (value object) (si::instance-set object index value)))

(labels ((generate-accessors (class)
	   (declare (optimize speed (safety 0)))
	   (if (and (typep class 'std-class)
		    #+(or)
		    (not (member (slot-value class 'name)
				 '(slot-definition
				   direct-slot-definition
				   effective-slot-definition
				   standard-slot-definition
				   standard-direct-slot-definition
				   standard-effective-slot-definition))))
	       (std-class-generate-accessors class t)
	       (loop for slotd in (slot-value class 'slots)
		  for index = (slot-value slotd 'location)
		  do (loop for reader in (slot-value slotd 'readers)
			do (setf (fdefinition reader) (reader-closure index)))
		  do (loop for writer in (slot-value slotd 'writers)
			do (setf (fdefinition writer) (writer-closure index)))))
	   (mapc #'generate-accessors (slot-value class 'direct-subclasses))))
  (generate-accessors +the-t-class+))
