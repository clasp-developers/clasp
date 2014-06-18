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

(defconstant +builtin-classes-pre-array+
  (make-array (1+ #.(length +builtin-classes-list+))))

;;; ----------------------------------------------------------------------
;;; Building the classes T, CLASS, STANDARD-OBJECT and STANDARD-CLASS.
;;;
;;; We cannot use the functions CREATE-STANDARD-CLASS and others because SLOTS,
;;; DIRECT-SLOTS, etc are empty and therefore SLOT-VALUE does not work.

(defun make-empty-standard-class (name &key (metaclass 'standard-class)
				  direct-superclasses direct-slots index)
  (declare (optimize speed (safety 0)))
  (let* ((the-metaclass (and metaclass (gethash metaclass si::*class-name-hash-table*)))
	 (class (or (gethash name si::*class-name-hash-table*)
		    (si:allocate-raw-instance nil the-metaclass
					      #.(length +standard-class-slots+)))))
    (with-early-accessors (+standard-class-slots+)
      (when (eq name 'standard-class)
	(defconstant +the-standard-class+ class)
	(si:instance-class-set class class))
      (setf (class-id                  class) name
	    (class-direct-subclasses   class) nil
	    (class-direct-default-initargs class) nil
	    (class-default-initargs    class) nil
	    (class-finalized-p         class) t
	    (eql-specializer-flag      class) nil
	    (specializer-direct-methods class) nil
	    (specializer-direct-generic-functions class) nil
	    (gethash name si::*class-name-hash-table*) class
	    (class-sealedp             class) nil
	    (class-dependents          class) nil
	    (class-valid-initargs      class) nil
	    )
      (add-slots class direct-slots)
      (let ((superclasses (loop for name in direct-superclasses
			     for parent = (find-class name)
			     do (push class (class-direct-subclasses parent))
			     collect parent)))
	(setf (class-direct-superclasses class) superclasses
	      (class-precedence-list class)
	      (compute-clos-class-precedence-list class superclasses)))
      (when index
	(setf (aref +builtin-classes-pre-array+ index) class))
      class)))

(defun remove-accessors (slotds)
  (declare (optimize speed (safety 0)))
  (loop for i in slotds
     for j = (copy-list i)
     do (remf (cdr j) :accessor)
     collect j))

(defun add-slots (class slots)
  (declare (si::c-local)
	   (optimize speed (safety 0)))
  ;; It does not matter that we pass NIL instead of a class object,
  ;; because CANONICAL-SLOT-TO-DIRECT-SLOT will make simple slots.
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    (let* ((table (make-hash-table :size (if slots 24 0)))
	   (location-table (make-hash-table :size (if slots 24 0)))
	   (slots (parse-slots slots))
	   (direct-slots (loop for slotd in slots
			    collect (apply #'make-simple-slotd
				     (find-class 'standard-direct-slot-definition)
				     slotd)))
	   (effective-slots (loop for i from 0
			       for slotd in slots
			       for name = (getf slotd :name)
			       for s = (apply #'make-simple-slotd
					(find-class 'standard-effective-slot-definition)
					slotd)
			       do (setf (slot-definition-location s) i
					(gethash name location-table) i
					(gethash name table) s)
			       collect s)))
      (setf (class-slots class) effective-slots
	    (class-direct-slots class) direct-slots
	    (class-size class) (length slots)
	    (slot-table class) table
	    (class-location-table class) location-table))))

;; 1) Create the classes
;;
;; Notice that, due to circularity in the definition, STANDARD-CLASS has
;; itself as metaclass. MAKE-EMPTY-STANDARD-CLASS takes care of that.
;;
(let* ((class-hierarchy '#.+class-hierarchy+))
  (let ((all-classes (loop for c in class-hierarchy
			for class = (apply #'make-empty-standard-class c)
			collect class)))
    (defconstant +the-t-class+ (find-class 't nil))
    (defconstant +the-class+ (find-class 'class nil))
    (defconstant +the-std-class+ (find-class 'std-class nil))
    (defconstant +the-funcallable-standard-class+
      (find-class 'funcallable-standard-class nil))
    ;;
    ;; 2) Class T had its metaclass wrong. Fix it.
    ;;
    (si:instance-class-set (find-class 't) (find-class 'built-in-class))
    ;;
    ;; 3) Finalize
    ;;
    (mapc #'si::instance-sig-set all-classes)
    ;;
    ;; 4) This is needed for further optimization
    ;;
    (setf (slot-value (find-class 'method-combination) 'sealedp) t)
    ;;
    ;; 5) This is needed so that slot-definition objects are not marked
    ;;    obsolete and need to be updated
    ;;
    (with-early-accessors (+standard-class-slots+)
      (loop for c in all-classes
	 do (loop for s in (class-direct-slots c)
	       do (si::instance-sig-set s))
	 do (loop for s in (class-slots c)
	       do (si::instance-sig-set s))))
    ))
