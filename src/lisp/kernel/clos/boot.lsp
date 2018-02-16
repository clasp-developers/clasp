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

#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *echo-repl-read* t))

(defconstant +builtin-classes-pre-array+
  (make-array (1+ #.(length +builtin-classes-list+))))

;;; ----------------------------------------------------------------------
;;; Building the classes T, CLASS, STANDARD-OBJECT and STANDARD-CLASS.
;;;
;;; We cannot use the functions CREATE-STANDARD-CLASS and others because SLOTS,
;;; DIRECT-SLOTS, etc are empty and therefore SLOT-VALUE does not work.

#+(or)
(defmacro debug-boot (msg &rest args)
  `(format t ,msg ,@args))
(defmacro debug-boot (msg &rest args))


(defun ensure-boot-class (name &key (metaclass 'standard-class)
                                 direct-superclasses direct-slots index
                                 creates-classes)
  (debug-boot "!!ensure-boot-class for ~s metaclass: ~s direct-superclasses: ~s :direct-slots ~s :index ~s~%"
              name metaclass direct-superclasses direct-slots index)
  (let* ((the-metaclass (progn
                          (debug-boot "    About to do the~%")
                          (the class
                               #+clasp(find-class metaclass nil)
                               #+ecl(gethash metaclass si::*class-name-hash-table*))))
         (class (progn
                  (debug-boot "    About to allocate-raw-class~%")
                  (or #+clasp(find-class name nil)
                      #+ecl(gethash name si::*class-name-hash-table*)
                      #+clasp
                      (core:allocate-raw-class nil the-metaclass #.(length +standard-class-slots+) creates-classes)
                      #-clasp
                      (si:allocate-raw-instance nil the-metaclass #.(length +standard-class-slots+))))))
    ;;    (debug-boot "About to with-early-accessors -> macroexpand = ~a~%" (macroexpand '(with-early-accessors (+standard-class-slots+) (setf (class-id                  class) name))))
    (debug-boot "    About to with-early-accessors~%")
    (with-early-accessors (+standard-class-slots+)
      (let ((existing-slots (class-slots class)))
        (when (and (typep existing-slots 'list)
                   (not (= (length existing-slots)
                           (length direct-slots)))
                   (not (zerop (length existing-slots))))
          (error "~S was called on the already instantiated class ~S, but with ~S slots while it already has ~S slots."
                 'ensure-boot-class name (length direct-slots) (length existing-slots))))
      #+ecl(when (eq name 'standard-class)
             (defconstant +the-standard-class+ class)
             (si:instance-class-set class class))
      ;;      (debug-boot "  (get-setf-expansion '(class-id class) ENV) -> ~a~%" (macrolet ((hack (form &environment e) `',(multiple-value-list (get-setf-expansion form e)))) (hack '(class-id class))))
      (setf (class-id                  class) name)
      (debug-boot "    (class-id class) -> ~a    name -> ~a~%" (class-id class) name)
      (setf (class-id                  class) name
            (class-direct-subclasses   class) nil
            (class-direct-default-initargs class) nil
            (class-default-initargs    class) nil
            (class-finalized-p         class) t
            (eql-specializer-flag      class) nil
            (specializer-direct-methods class) nil
            (specializer-direct-generic-functions class) nil)
      (debug-boot "    About to setf class name -> ~a  class -> ~a~%" name class)
      #+clasp(core:set-class class name)
      #+ecl(setf (gethash name si::*class-name-hash-table*) class)
      (debug-boot "    Done setf class name -> ~a  class -> ~a~%" name class)
      (setf
       (class-sealedp             class) nil
       (class-dependents          class) nil)
      (debug-boot "      About to add-slots~%")
      (add-slots class direct-slots)
      (debug-boot "      About to get superclasses~%")
      (let ((superclasses (loop for name in direct-superclasses
                             for parent = (find-class name)
                             do (push class (class-direct-subclasses parent))
                             collect parent)))
        (debug-boot "      Collected superclasses~%")
        (setf (class-direct-superclasses class) superclasses)
        ;; In clasp each class contains a default allocator functor
        ;; that is used to allocate instances of this class
        ;; If a superclass is derived from a C++ adaptor class
        ;; then we must inherit its allocator
        ;; This means that a class can only ever
        ;; inherit from one C++ adaptor class
        #+clasp(setf (creator class) (sys:compute-instance-creator class the-metaclass superclasses))
        (debug-boot "      compute-clos-class-precedence-list  class->~a   superclasses->~a~%" class superclasses)
        (let ((cpl (compute-clos-class-precedence-list class superclasses)))
          (setf (class-precedence-list class) cpl)))
      (debug-boot "      maybe add index~%")
      (when index
        (setf (aref +builtin-classes-pre-array+ index) class))
      class)))

(defun add-slots (class slots)
  (declare (optimize speed (safety 0)))
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

#++
(defmacro dbg-boot (fmt &rest fmt-args)
  `(bformat t ,fmt ,@fmt-args))


(defmacro boot-hierarchy ()
  `(progn
     ,@(loop for (class . options) in +class-hierarchy+
          for direct-slots = (getf options :direct-slots)
;;;          do (core:bformat t "boot-hierarchy  class->%s\n" class)
          collect
            (if direct-slots
                `(apply #'ensure-boot-class ',class
                        :direct-slots ,(parse-slots direct-slots)
                        ',(let ((copy (copy-list options)))
                               (remf copy :direct-slots)
                               copy))
                `(apply #'ensure-boot-class ',class ',options)))))

(boot-hierarchy)

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
