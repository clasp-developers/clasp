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

#-clasp
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
            (let ((entry (first scan)))
              (ext:with-current-source-form (entry)
                (etypecase entry
                  (symbol
                   (push `(,entry (slot-value ,temp ',entry)) res))
                  ((cons symbol (cons symbol null))
                   (push `(,(first entry)
                           (slot-value ,temp ',(second entry)))
                         res))))))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))

;;;
;;; The following macro is a convenience that can be used to directly
;;; access the slots of a class based on their s-form description. It
;;; is used internally by ECL during bootstrap. Unlike WITH-SLOTS,
;;; the macros directly access the slots by index.
;;;
(defmacro with-early-accessors ((&rest slot-definitions) &rest body)
  `(macrolet
       ,(loop for slots in slot-definitions
              nconc (loop for (name . slotd) in (if (symbolp slots)
                                                    (symbol-value slots)
                                                    slots)
                          for index from 0
                          ;; KLUDGE: The early slots sometimes have both readers and
                          ;; accessors so that only one is exported. In this case the
                          ;; reader usually has the name with no % and we use that in
                          ;; the code, so we prefer the reader here.
                          for accessor = (or (getf slotd :reader) (getf slotd :accessor))
                          when accessor
                            collect `(,accessor (object)
                                       `(standard-instance-access ,object ,,index))))
     ,@body))

;;;
;;; The following macro are also used at bootstrap for instantiating
;;; a class based only on the s-form description.
;;;
(defmacro with-early-make-instance (slots (object class &rest key-value-pairs)
                                    &rest body)
  (when (symbolp slots)
    (setf slots (symbol-value slots)))
  `(let* ((%class ,class)
          (,object (core:allocate-standard-instance %class ,(length slots))))
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
     (with-early-accessors (,slots)
       ,@body)))

(defmacro with-early-make-funcallable-instance (slots (object class &rest key-value-pairs)
                                                &rest body)
  (when (symbolp slots)
    (setf slots (symbol-value slots)))
  `(let* ((%class ,class)
          ;; Identical to above macro except here. (FIXME: rewrite more nicely.)
          (,object (core:allocate-funcallable-standard-instance %class ,(length slots))))
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
     (with-early-accessors (,slots)
       ,@body)))

;;;
;;; Clasp classes store slots in a hash table for faster access. The
;;; following functions create the cache and allow us to locate the
;;; slots rapidly.
;;;
(defun std-create-slots-table (class)
  (with-slots ((all-slots slots)
	       (location-table location-table))
      class
    (let ((size (max 32 (* 2 (length all-slots))))
          (metaclass (si::instance-class class))
          (locations nil))
	(when (or (eq metaclass (find-class 'standard-class))
		  (eq metaclass (find-class 'funcallable-standard-class))
		  (eq metaclass (find-class 'structure-class)))
	  (setf locations (make-hash-table :size size))
	  (dolist (slotd all-slots)
	    (setf (gethash (slot-definition-name slotd) locations)
		  (slot-definition-location slotd))))
	(setf location-table locations))))

;;;
;;; STANDARD-CLASS INTERFACE
;;;
;;; Specific functions for slot reading, writing, boundness checking, etc.
;;;

;;; MOP specifies that consequences are undefined if the slot is unbound when this function
;;; is called. We presume that that means for the reader, not the writer.
;;; If the reader is so called, in Clasp, it returns the slot-unbound marker. Users should
;;; not deal with that, but we can.

(defun standard-instance-access (instance location)
  (core:rack-ref (core:instance-rack instance) location))

(defun (setf standard-instance-access) (val instance location)
  (setf (core:rack-ref (core:instance-rack instance) location) val))

#+threads
(mp::define-simple-cas-expander clos:standard-instance-access core::instance-cas
  (instance location)
  "The requirements of the normal STANDARD-INSTANCE-ACCESS writer
must be met, including that the slot has allocation :instance, and is
bound before the operation.
If there is a CHANGE-CLASS concurrent with this operation the
consequences are not defined.")

;;; On Clasp, funcallable instances and regular instances store
;;; their slots identically, at the moment.
(defun funcallable-standard-instance-access (instance location)
  (core:rack-ref (core:instance-rack instance) location))

(defun (setf funcallable-standard-instance-access) (val instance location)
  (setf (core:rack-ref (core:instance-rack instance) location) val))

;;; This works on both class locations (conses) and instance ones.
(defun standard-location-access (instance location)
  (if (core:fixnump location)
      (core:rack-ref (core:instance-rack instance) location)
      (car location)))

(defun (setf standard-location-access) (val instance location)
  (if (core:fixnump location)
      (setf (core:rack-ref (core:instance-rack instance) location) val)
      (setf (car location) val)))

(defun slot-value (self slot-name)
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    (let* ((class (class-of self))
	   (location-table (class-location-table class)))
      (if location-table
	  (let ((location (gethash slot-name location-table nil)))
	    (if location
		(let ((value (standard-location-access self location)))
		  (if (si:sl-boundp value)
		      value
		      (values (slot-unbound class self slot-name))))
		(slot-missing class self slot-name 'SLOT-VALUE)))
	  (let ((slotd
                  ;;; with-early-accessors defines local macros, not functions,
                  ;;; so we can't do this.
                  ;;; The fully correct thing to do would be having it define
                  ;;; local functions which are then inlined.
                  ;;; Anyway, so we loop more manually instead.
                  #+(or) (find slot-name (class-slots class) :key #'slot-definition-name)
                  (loop for prospect in (class-slots class)
                        for prospect-name = (slot-definition-name prospect)
                        when (eql slot-name prospect-name)
                          return prospect)))
	    (if slotd
		(slot-value-using-class class self slotd)
		(values (slot-missing class self slot-name 'SLOT-VALUE))))))))

(defun slot-exists-p (self slot-name)
  (with-slots ((slots slots) (location-table location-table))
      (class-of self)
    (if location-table ; only for direct instances of standard-class, etc
        (gethash slot-name location-table nil)
        (find slot-name slots :key #'slot-definition-name))))

(defun slot-boundp (self slot-name)
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    (let* ((class (class-of self))
	   (location-table (class-location-table class)))
      (if location-table
	  (let ((location (gethash slot-name location-table nil)))
	    (if location
		(si:sl-boundp (standard-location-access self location))
		(values (slot-missing class self slot-name 'SLOT-BOUNDP))))
	  (let ((slotd
                  #+(or) (find slot-name (class-slots class) :key #'slot-definition-name)
                  ;; Can't break this out into a function because again, local macro.
                  ;; FIXME
                  (loop for prospect in (class-slots class)
                        for prospect-name = (slot-definition-name prospect)
                        when (eql slot-name prospect-name)
                          return prospect)))
	    (if slotd
		(slot-boundp-using-class class self slotd)
		(values (slot-missing class self slot-name 'SLOT-BOUNDP))))))))

(defun slot-makunbound (self slot-name)
  (with-early-accessors (+standard-class-slots+
                         +slot-definition-slots+)
    (let* ((class (class-of self))
	   (location-table (class-location-table class)))
      (if location-table
	  (let ((location (gethash slot-name location-table nil)))
	    (if location
		(setf (standard-location-access self location) (si:unbound))
		(slot-missing class self slot-name 'slot-makunbound)))
	  (let ((slotd
                  #+(or) (find slot-name (class-slots class) :key #'slot-definition-name)
                  ;; Can't break this out into a function because again, local macro.
                  ;; FIXME
                  (loop for prospect in (class-slots class)
                        for prospect-name = (slot-definition-name prospect)
                        when (eql slot-name prospect-name)
                          return prospect)))
	    (if slotd
		(slot-makunbound-using-class class self slotd)
		(slot-missing class self slot-name 'SLOT-BOUNDP))))))
  self)

;;; 7.7.12 slot-missing
;;; If slot-missing returns, its values will be treated as follows:
;;;     If the operation is setf or slot-makunbound, any values will be ignored by the caller.
;;;     -> returning value after the calls to slot-missing
(defun (setf slot-value) (value self slot-name)
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    (let* ((class (class-of self))
	   (location-table (class-location-table class)))
      (if location-table
	  (let ((location (gethash slot-name location-table nil)))
	    (if location
                (setf (standard-location-access self location) value)
                (slot-missing class self slot-name 'SETF value)))
	  (let ((slotd
                 #+(or) (find slot-name (class-slots class) :key #'slot-definition-name)
                 (loop for prospect in (class-slots class)
                       for prospect-name = (slot-definition-name prospect)
                       when (eql slot-name prospect-name)
                         return prospect)))
	    (if slotd
		(setf (slot-value-using-class class self slotd) value)
		(slot-missing class self slot-name 'SETF value))))))
  value)

;;; FIXME: (cas slot-value) would be a better name.
#+threads
(defun cas-slot-value (old new object slot-name)
  (let* ((class (class-of object))
         (location-table (class-location-table class)))
    (if location-table
        (let ((location (gethash slot-name location-table)))
          (if location
              (core::instance-cas object location old new)
              (slot-missing class object slot-name
                            'cas (list old new))))
        (let ((slotd (find slot-name (clos:class-slots class)
                           :key #'clos:slot-definition-name)))
          (if slotd
              (cas-slot-value-using-class old new class object slotd)
              (slot-missing class object slot-name
                            'cas (list old new)))))))

#+threads
(mp::define-simple-cas-expander slot-value cas-slot-value (object slot-name)
  "See SLOT-VALUE-USING-CLASS documentation for constraints.
If no slot with the given SLOT-NAME exists, SLOT-MISSING will be called,
with operation = mp:cas, and new-value a list of OLD and NEW.
If SLOT-MISSING returns, its primary value is returned.")
