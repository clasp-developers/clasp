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
  #+compare(print (list "MLOG make-empty-standard-class +++++++++ ENTERED make-empty-standard-class name-> " name metaclass ))
  (let* ((the-metaclass (and metaclass (gethash metaclass si::*class-name-hash-table*)))
	 (class (or (let ((existing-class (gethash name si::*class-name-hash-table*)))
		      (if existing-class
			  (progn
			    #+compare (print (list "MLOG make-empty-standard-class got existing class -->" #+brcl existing-class))
			    existing-class)
			  nil))
		    #+brcl
		    (core:allocate-raw-class nil the-metaclass #.(length +standard-class-slots+) name)
		    #-brcl
		    (si:allocate-raw-instance nil the-metaclass
					      #.(length +standard-class-slots+)))))
    (with-early-accessors (+standard-class-slots+)
      #+compare(print (list "MLOG make-empty-standard-class +++++++++ STARTING +++++++ name --> " name))
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
      #+compare(print (list "MLOG add-slots--> " name "  direct-slots--> " direct-slots ))
      (add-slots class direct-slots)
      #+compare(print "MLOG ---- Done add-slots")
      (let ((superclasses (loop for name in direct-superclasses
			     for parent = (find-class name)
			     do (push class (class-direct-subclasses parent))
			     collect parent)))
	#+compare(print "MLOG About to assign superclasses")
	(setf (class-direct-superclasses class) superclasses)
          ;; In brcl each class contains a default allocator functor
          ;; that is used to allocate instances of this class
          ;; If a superclass is derived from a C++ adaptor class
          ;; then we must inherit its allocator
          ;; This means that only any class can only ever inherit from one C++ adaptor class
          #+brcl(sys:inherit-default-allocator class superclasses)
	#+compare(print "MLOG About to compute-clos-class-precedence-list")
	(let ((cpl (compute-clos-class-precedence-list class superclasses)))
	  #+compare(print (list "MLOG Finished compute-clos-class-precedence-list - saving it" "computed for: " #+brcl class " cpl: " #+brcl cpl ))
	  (setf (class-precedence-list class) cpl)
	  #+compare(print (list "MLOG Checking set class-precedence-list class: " #+brcl class " cpl: " #+brcl (class-precedence-list class)))
	  )
	)
      
      (when index
	(setf (aref +builtin-classes-pre-array+ index) class))
      #+compare(print (list "MLOG make-empty-standard-class +++++++++ LEAVING +++++++ name --> " name))
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
  #+compare(print "MLOG entered add-slots")
  #+compare(progn
	     (clos-log "-------- add-slots for class: %s\n" class )
	     (clos-log "             slots: %s\n" slots))
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    #+compare(print "MLOG entered with-early-accessors body")
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

#+compare(print "MLOG ----- Stage#1   creating the clases")
(let* ((class-hierarchy '#.+class-hierarchy+))
  (let ((all-classes (loop for c in class-hierarchy
			for class = (apply #'make-empty-standard-class c)
			do (progn #+compare(print (list "MLOG Class--> " c)))
			collect class)))
    (defconstant +the-t-class+ (find-class 't nil))
    (defconstant +the-class+ (find-class 'class nil))
    (defconstant +the-std-class+ (find-class 'std-class nil))
    (defconstant +the-funcallable-standard-class+
      (find-class 'funcallable-standard-class nil))
    ;;
    ;; 2) Class T had its metaclass wrong. Fix it.
    ;;
#+compare    (print (list "MLOG STAGE 2 - BRCL skips this"))
#-brcl
    (si:instance-class-set (find-class 't) (find-class 'built-in-class))
    ;;
    ;; 3) Finalize
    ;;
#+compare(print (list "MLOG STAGE 3"))
    (mapc #'si::instance-sig-set all-classes)
    ;;
    ;; 4) This is needed for further optimization
    ;;
    #+compare(print (list "MLOG STAGE 4"))
    (setf (slot-value (find-class 'method-combination) 'sealedp) t)
    ;;
    ;; 5) This is needed so that slot-definition objects are not marked
    ;;    obsolete and need to be updated
    ;;
    #+compare(print (list "MLOG STAGE 5"))
    (with-early-accessors (+standard-class-slots+)
      (loop for c in all-classes
	 do (loop for s in (class-direct-slots c)
	       do (si::instance-sig-set s))
	 do (loop for s in (class-slots c)
	       do (si::instance-sig-set s))))
    ))


#+compare(print "MLOG    quitting at end of boot.lsp")
