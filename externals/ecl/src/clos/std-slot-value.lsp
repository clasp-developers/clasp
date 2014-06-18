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

(eval-when (:compile-toplevel :execute)
  (load "src:clos;hierarchy.lsp"))

;;; ----------------------------------------------------------------------
;;; SLOTS READING AND WRITING
;;;
;;; Functional and macro interface for accessing the slots of an instance.
;;; This interface is defined with specialization for classes that ECL
;;; knows of such as standard classes and funcallable standard class.
;;; This is needed to avoid circularity in compute-applicable-methods,
;;; which needs the slot values and thus cannot go through a dispatch
;;; itself.
;;;
;;; Note that using SLOT-VALUE or specialized versions of it is not
;;; wrong because the MOP enforces various restrictions on portable
;;; code:
;;;  1) Accessors must behave as SLOT-VALUE
;;;  2) In particular, any method defined by the user must be
;;;     specialized on at least one non-specified class. This means
;;;     that the user cannot change the behavoir of SLOT-VALUE for
;;;     standard classes.
;;;
;;; First of all we define WITH-SLOTS because it is going to be useful
;;; for enforcing the use of SLOT-VALUE and not of accessors
;;; throughout the bootstrap code.
;;;
(defmacro with-slots (slot-entries instance-form &body body)
  (let* ((temp (gensym))
	 (accessors
	  (do ((scan slot-entries (cdr scan))
	       (res))
	      ((null scan) (nreverse res))
	    (if (symbolp (first scan))
		(push `(,(first scan) (slot-value ,temp ',(first scan))) res)
		(push `(,(caar scan)
			 (slot-value ,temp ',(cadar scan))) res)))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))

;;;
;;; The following macro is a convenience that can be used to directly
;;; access the slots of a class based on their s-form description. It
;;; is used internally by ECL during bootstrap. Unlike WITH-SLOTS,
;;; the macros directly access the slots by index.
;;;
(eval-when (:compile-toplevel :execute)
  (defmacro with-early-accessors ((&rest slot-definitions) &rest body)
    `(macrolet
	 ,(loop for slots in slot-definitions
	     nconc (loop for (name . slotd) in (if (symbolp slots)
						   (symbol-value slots)
						   slots)
		      for index from 0
		      for accessor = (getf slotd :accessor)
		      when accessor
		      collect `(,accessor (object) `(si::instance-ref ,object ,,index))))
       ,@body)))

;;;
;;; The following macro is also used at bootstap for instantiating
;;; a class based only on the s-form description.
;;;
(eval-when (:compile-toplevel :execute)
  (defmacro with-early-make-instance (slots (object class &rest key-value-pairs)
				      &rest body)
    (when (symbolp slots)
      (setf slots (symbol-value slots)))
    `(let* ((%class ,class)
	    (,object (si::allocate-raw-instance nil %class
						,(length slots))))
       (declare (type standard-object ,object))
       ,@(flet ((initializerp (name list)
		  (not (eq (getf list name 'wrong) 'wrong))))
	       (loop for (name . slotd) in slots
		  for initarg = (getf slotd :initarg)
		  for initform = (getf slotd :initform (si::unbound))
		  for initvalue = (getf key-value-pairs initarg)
		  for index from 0
		  do (cond ((and initarg (initializerp initarg key-value-pairs))
			    (setf initform (getf key-value-pairs initarg)))
			   ((initializerp name key-value-pairs)
			    (setf initform (getf key-value-pairs name))))
		  when (si:sl-boundp initform)
		  collect `(si::instance-set ,object ,index ,initform)))
       (when %class
	 (si::instance-sig-set ,object))
       (with-early-accessors (,slots)
	 ,@body))))

;;;
;;; ECL classes store slots in a hash table for faster access. The
;;; following functions create the cache and allow us to locate the
;;; slots rapidly.
;;;
(defun std-create-slots-table (class)
  (with-slots ((all-slots slots)
	       (slot-table slot-table)
	       (location-table location-table))
      class
    (let* ((size (max 32 (* 2 (length all-slots))))
	   (table (make-hash-table :size size)))
      (dolist (slotd all-slots)
	(setf (gethash (slot-definition-name slotd) table) slotd))
      (let ((metaclass (si::instance-class class))
	    (locations nil))
	(when (or (eq metaclass (find-class 'standard-class))
		  (eq metaclass (find-class 'funcallable-standard-class))
		  (eq metaclass (find-class 'structure-class)))
	  (setf locations (make-hash-table :size size))
	  (dolist (slotd all-slots)
	    (setf (gethash (slot-definition-name slotd) locations)
		  (slot-definition-location slotd))))
	(setf slot-table table
	      location-table locations)))))

(defun find-slot-definition (class slot-name)
  (with-slots ((slots slots) (slot-table slot-table))
      class
    (if (or (eq (si:instance-class class) +the-standard-class+)
	    (eq (si:instance-class class) +the-funcallable-standard-class+))
	(gethash slot-name slot-table nil)
	(find slot-name slots :key #'slot-definition-name))))

;;;
;;; INSTANCE UPDATE PREVIOUS
;;;
(eval-when (:compile-toplevel :execute)
  (defmacro ensure-up-to-date-instance (instance)
    ;; The up-to-date status of a class is determined by
    ;; instance.sig. This slot of the C structure contains a list of
    ;; slot definitions that was used to create the instance. When the
    ;; class is updated, the list is newly created. Structures are also
    ;; "instances" but keep ECL_UNBOUND instead of the list.
    `(let* ((i ,instance)
	    (s (si::instance-sig i)))
       (declare (:read-only i s))
       (with-early-accessors (+standard-class-slots+)
	 (when (si:sl-boundp s)
	   (unless (eq s (class-slots (si::instance-class i)))
	     (update-instance i)))))))

(defun update-instance (x)
  (si::instance-sig-set x))
(declaim (notinline update-instance))

;;;
;;; STANDARD-CLASS INTERFACE
;;;
;;; Specific functions for slot reading, writing, boundness checking, etc.
;;;

(defun standard-instance-access (instance location)
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    (ensure-up-to-date-instance instance)
    (cond ((ext:fixnump location)
	   ;; local slot
	   (si:instance-ref instance (truly-the fixnum location)))
	  ((consp location)
	   ;; shared slot
	   (car location))
	  (t
	   (invalid-slot-location instance location)))))

(defun standard-instance-set (instance location val)
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    (ensure-up-to-date-instance instance)
    (cond ((ext:fixnump location)
	   ;; local slot
	   (si:instance-set instance (truly-the fixnum location) val))
	  ((consp location)
	   ;; shared slot
	   (setf (car location) val))
	  (t
	   (invalid-slot-location instance location)))
    val))

(defsetf standard-instance-access standard-instance-set)
(defsetf funcallable-standard-instance-access standard-instance-set)

(defun slot-value (self slot-name)
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    (let* ((class (class-of self))
	   (location-table (class-location-table class)))
      (if location-table
	  (let ((location (gethash slot-name location-table nil)))
	    (if location
		(let ((value (standard-instance-access self location)))
		  (if (si:sl-boundp value)
		      value
		      (values (slot-unbound class self slot-name))))
		(slot-missing class self slot-name 'SLOT-VALUE)))
	  (let ((slotd (find slot-name (class-slots class) :key #'slot-definition-name)))
	    (if slotd
		(slot-value-using-class class self slotd)
		(values (slot-missing class self slot-name 'SLOT-VALUE))))))))

(defun slot-exists-p (self slot-name)
  (and (find-slot-definition (class-of self) slot-name)
       t))

(defun slot-boundp (self slot-name)
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    (let* ((class (class-of self))
	   (location-table (class-location-table class)))
      (if location-table
	  (let ((location (gethash slot-name location-table nil)))
	    (if location
		(si:sl-boundp (standard-instance-access self location))
		(values (slot-missing class self slot-name 'SLOT-BOUNDP))))
	  (let ((slotd (find slot-name (class-slots class) :key #'slot-definition-name)))
	    (if slotd
		(slot-boundp-using-class class self slotd)
		(values (slot-missing class self slot-name 'SLOT-BOUNDP))))))))

(defun clos::slot-value-set (value self slot-name)
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    (let* ((class (class-of self))
	   (location-table (class-location-table class)))
      (if location-table
	  (let ((location (gethash slot-name location-table nil)))
	    (if location
		(setf (standard-instance-access self location) value)
		(slot-missing class self slot-name 'SETF value)))
	  (let ((slotd (find slot-name (class-slots class) :key #'slot-definition-name)))
	    (if slotd
		(setf (slot-value-using-class class self slotd) value)
		(slot-missing class self slot-name 'SETF value))))))
  value)

(setf (fdefinition '(setf slot-value)) #'clos::slot-value-set)

;;;
;;; 2) Overloadable methods on which the previous functions are based
;;;

(defun invalid-slot-location (instance location)
  (declare (si::c-local))
  (error "Invalid location ~A when accessing slot of class ~A"
	 location (class-of location)))
