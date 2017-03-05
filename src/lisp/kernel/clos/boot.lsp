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

#+(or)(defmacro debug-boot (msg &rest args)
  `(format t ,msg ,@args))
(defmacro debug-boot (msg &rest args))


(defun ensure-boot-class (name &key (metaclass 'standard-class)
                                 direct-superclasses direct-slots index)
  #+(or)(format t "ensure-boot-class for ~s metaclass: ~s direct-superclasses: ~s :direct-slots ~s :index ~s~%"
                name metaclass direct-superclasses direct-slots index)
  (let* ((the-metaclass (the class (gethash metaclass si::*class-name-hash-table*)))
         (class (or (gethash name si::*class-name-hash-table*)
                    #+clasp
                    (core:allocate-raw-class nil the-metaclass #.(length +standard-class-slots+) name)
                    #-clasp
                    (si:allocate-raw-instance nil the-metaclass #.(length +standard-class-slots+)))))
    (debug-boot "About to with-early-accessors -> macroexpand = ~a~%" (macroexpand '(with-early-accessors (+standard-class-slots+) (setf (class-id                  class) name))))
    (with-early-accessors (+standard-class-slots+)
      (let ((existing-slots (class-slots class)))
        (when (and (typep existing-slots 'list)
                   (not (= (length existing-slots)
                           (length direct-slots)))
                   (not (zerop (length existing-slots))))
          (error "~S was called on the already instantiated class ~S, but with ~S slots while it already has ~S slots."
                 'ensure-boot-class name (length direct-slots) (length existing-slots))))
      (when (eq name 'standard-class)
	#+ecl(defconstant +the-standard-class+ class)
	#+clasp(setq +the-standard-class+ class)
	(si:instance-class-set class class))
      (debug-boot "  (get-setf-expansion '(class-id class) ENV) -> ~a~%" (macrolet ((hack (form &environment e) `',(multiple-value-list (get-setf-expansion form e)))) (hack '(class-id class))))
      (setf (class-id                  class) name)
      (debug-boot "    (class-id class) -> ~a    name -> ~a~%" (class-id class) name)
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

(defun add-slots (class slots)
  (declare (si::c-local)
	   (optimize speed (safety 0)))
  ;; It does not matter that we pass NIL instead of a class object,
  ;; because CANONICAL-SLOT-TO-DIRECT-SLOT will make simple slots.
  (with-early-accessors (+standard-class-slots+
			 +slot-definition-slots+)
    (let* ((table (make-hash-table :size (if slots 24 0)))
	   (location-table (make-hash-table :size (if slots 24 0)))
           (direct-slot-class (find-class 'standard-direct-slot-definition nil))
	   (direct-slots (loop for slotd in slots
                               collect (apply #'make-simple-slotd direct-slot-class slotd)))
           (effective-slot-class (find-class 'standard-effective-slot-definition nil))
	   (effective-slots (loop for i from 0
			       for slotd in slots
			       for name = (getf slotd :name)
			       for s = (apply #'make-simple-slotd effective-slot-class slotd)
			       do (setf (slot-definition-location s) i
					(gethash name location-table) i
					(gethash name table) s)
			       collect s)))
      (setf (class-slots class) effective-slots
	    (class-direct-slots class) direct-slots
	    (class-size class) (length slots)
	    (slot-table class) table)
      (setf (class-location-table class) location-table))))


;; Create the classes
;;
;; Notice that, due to circularity in the definition, STANDARD-CLASS has
;; itself as metaclass. ENSURE-BOOT-CLASS takes care of that.
;;
#+clasp
(progn
    (defvar +the-t-class+)
    (defvar +the-class+)
    (defvar +the-std-class+)
    (defvar +the-funcallable-standard-class+))

(defmacro dbg-boot (fmt &rest fmt-args)
  nil)

#+(or)(defmacro dbg-boot (fmt &rest fmt-args)
  `(bformat t ,fmt ,@fmt-args))
(defmacro boot-hierarchy ()
  `(progn
     ,@(loop for (class . options) in +class-hierarchy+
             for direct-slots = (getf options :direct-slots)
             collect
             (if direct-slots
                 `(apply #'ensure-boot-class ',class
                         :direct-slots ,(parse-slots direct-slots)
                         ',(let ((copy (copy-list options)))
                             (remf copy :direct-slots)
                             copy))
                 `(apply #'ensure-boot-class ',class ',options)))))

(boot-hierarchy)
(dbg-boot "About to start block\n")

#+ecl
(progn
  (defconstant +the-t-class+ (find-class 't nil))
  (defconstant +the-class+ (find-class 'class nil))
  (defconstant +the-std-class+ (find-class 'std-class nil))
  (defconstant +the-funcallable-standard-class+
    (find-class 'funcallable-standard-class nil)))
#+clasp
(progn
  (dbg-boot "About to setq stuff\n")
  (setq +the-t-class+ (find-class 't nil))
  (setq +the-class+ (find-class 'class nil))
  (setq +the-std-class+ (find-class 'std-class nil))
  (setq +the-funcallable-standard-class+
        (find-class 'funcallable-standard-class nil)))
;;
;; Class T had its metaclass wrong. Fix it.
;;
#-clasp
(si:instance-class-set (find-class 't) (find-class 'built-in-class))
;;
;; Finalize
;;
;;
;; This is needed for further optimization
;;
(dbg-boot "About to set slot-value for method-combination\n")
(setf (slot-value (find-class 'method-combination) 'sealedp) t)
;; This is needed so that slot-definition objects are not marked
;; obsolete and need to be updated
(let ()
 (with-early-accessors (+standard-class-slots+)
   (loop for (class-name) in +class-hierarchy+
         for class = (find-class class-name)
         do (loop for s in (class-slots class)
                  do (si::instance-sig-set s))
            (loop for s in (class-direct-slots class)
                  do (si::instance-sig-set s)))))
