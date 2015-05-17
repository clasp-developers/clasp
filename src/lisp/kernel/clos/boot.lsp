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
#+clasp(defvar +the-standard-class+)




(defun make-empty-standard-class (name &key (metaclass 'standard-class)
					 direct-superclasses direct-slots index)
  (declare (optimize speed (safety 0)))
  (let* ((the-metaclass (and metaclass (gethash metaclass si::*class-name-hash-table*)))
	 (class (or (let ((existing-class (gethash name si::*class-name-hash-table*)))
		      (if existing-class
			  existing-class
			  nil))
		    #+clasp
		    (core:allocate-raw-class nil the-metaclass #.(length +standard-class-slots+) name)
		    #-clasp
		    (si:allocate-raw-instance nil the-metaclass
					      #.(length +standard-class-slots+)))))
    (with-early-accessors (+standard-class-slots+)
      (when (eq name 'standard-class)
	#+ecl(defconstant +the-standard-class+ class)
	#+clasp(setq +the-standard-class+ class)
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
	    (class-valid-initargs      class) nil)
      (add-slots class direct-slots)
      (let ((superclasses (loop for name in direct-superclasses
			     for parent = (find-class name)
			     do (push class (class-direct-subclasses parent))
			     collect parent)))
	(setf (class-direct-superclasses class) superclasses)
	;; In clasp each class contains a default allocator functor
	;; that is used to allocate instances of this class
	;; If a superclass is derived from a C++ adaptor class
	;; then we must inherit its allocator
	;; This means that a class can only ever
	;; inherit from one C++ adaptor class
	#+clasp(sys:inherit-default-allocator class superclasses)
	(let ((cpl (compute-clos-class-precedence-list class superclasses)))
	  (setf (class-precedence-list class) cpl)))
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
	   (slots (let ((ps (parse-slots slots)))
		    #+compare(print "MLOG Done parse-slots")
		    ps))
	   (direct-slots (progn
			   #+compare(print (list "MLOG slots --> " slots))
			   (loop for slotd in slots
			    collect (apply #'make-simple-slotd
				     (find-class 'standard-direct-slot-definition)
				     slotd)))
	     )
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
	    (slot-table class) table)
      #+compare(print (list "MLOG Adding slots location-table --> " location-table ))
      (setf (class-location-table class) location-table)
      #+compare(print (list "MLOG Just added slots location-table --> " (class-location-table class)))
      )))

(defmacro clos-boot-log (msg &rest args)
  `(bformat t ,msg ,@args))


;; 1) Create the classes
;;
;; Notice that, due to circularity in the definition, STANDARD-CLASS has
;; itself as metaclass. MAKE-EMPTY-STANDARD-CLASS takes care of that.
;;
#+clasp
(progn
    (defvar +the-t-class+)
    (defvar +the-class+)
    (defvar +the-std-class+)
    (defvar +the-funcallable-standard-class+))


;;;
;;; make-empty-standard-class compiles a lot of code using EVAL
;;;
;;; This is being run every time Clasp starts up!
;;; We have to figure out how ECL avoids this.
;;;
(let* ((class-hierarchy '#.+class-hierarchy+))
  (let ((all-classes (loop for c in class-hierarchy
			for class = (progn
				      (apply #'make-empty-standard-class c))
			collect class)))
    #+ecl
    (progn
      (defconstant +the-t-class+ (find-class 't nil))
      (defconstant +the-class+ (find-class 'class nil))
      (defconstant +the-std-class+ (find-class 'std-class nil))
      (defconstant +the-funcallable-standard-class+
	(find-class 'funcallable-standard-class nil)))
    #+clasp
    (progn
      (setq +the-t-class+ (find-class 't nil))
      (setq +the-class+ (find-class 'class nil))
      (setq +the-std-class+ (find-class 'std-class nil))
      (setq +the-funcallable-standard-class+
	    (find-class 'funcallable-standard-class nil)))
    ;;
    ;; 2) Class T had its metaclass wrong. Fix it.
    ;;
    #+compare    (print (list "MLOG STAGE 2 - BRCL skips this"))
    #-clasp
    (si:instance-class-set (find-class 't) (find-class 'built-in-class))
    ;;
    ;; 3) Finalize
    ;;
    #+compare(print (list "MLOG STAGE 3"))
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
