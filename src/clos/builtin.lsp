;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

;;; ----------------------------------------------------------------------
;;; Methods

;;; ======================================================================
;;; Built-in classes
;;; ----------------------------------------------------------------------

(defmethod make-instance ((class built-in-class) &rest initargs)
  (declare (ignore initargs))
  (error "The built-in class (~A) cannot be instantiated" class))

;;;
;;; At this point we can activate the vector of builtin classes, which
;;; is used by class-of and other functions.
;;;
(si::*make-constant '+builtin-classes+ +builtin-classes-pre-array+)

(defmethod ensure-class-using-class ((class null) name &rest rest)
  (declare (ignore class))
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
    (declare (ignore direct-superclasses))
    (setf class (apply #'make-instance metaclass :name name options))
    (when name
      (si:create-type-name name)
      (setf (find-class name) class))))

(defmethod change-class ((instance t) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class))
  class)

(defmethod make-instance ((class-name symbol) &rest initargs)
  (apply #'make-instance (find-class class-name) initargs))

(defmethod slot-makunbound-using-class ((class built-in-class) self slotd)
  (declare (ignore class self slotd))
  (error "SLOT-MAKUNBOUND-USING-CLASS cannot be applied on built-in objects"))

(defmethod slot-boundp-using-class ((class built-in-class) self slotd)
  (declare (ignore class self slotd))
  (error "SLOT-BOUNDP-USING-CLASS cannot be applied on built-in objects"))

(defmethod slot-value-using-class ((class built-in-class) self slotd)
  (declare (ignore class self slotd))
  (error "SLOT-VALUE-USING-CLASS cannot be applied on built-in objects"))

(defmethod (setf slot-value-using-class) (val (class built-in-class) self slotd)
  (declare (ignore class self slotd val))
  (error "SLOT-VALUE-USING-CLASS cannot be applied on built-in objects"))

(defmethod slot-exists-p-using-class ((class built-in-class) self slotd)
  (declare (ignore class self slotd))
  nil)

;;; ======================================================================
;;; STRUCTURES
;;;

;;; structure-classes cannot be instantiated
(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (apply #'si::make-structure class
	 (make-list (class-size class) :initial-element (si::unbound))))

(defmethod finalize-inheritance ((class structure-class))
  (call-next-method)
  (dolist (slot (class-slots class))
    (unless (eq :INSTANCE (slot-definition-allocation slot))
      (error "The structure class ~S can't have shared slots" (class-name class)))))

(defmethod make-load-form ((object structure-object) &optional environment)
  (make-load-form-saving-slots object :key environment))

(defmethod print-object ((obj structure-object) stream)
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class)))
    (declare (:read-only class))
    (when (and slotds
               ;; *p-readably* effectively disables *p-level*
	       (not *print-readably*)
	       *print-level*
	       (zerop *print-level*))
      (write-string "#" stream)
      (return-from print-object obj))
    (write-string "#S(" stream)
    (prin1 (class-name class) stream)
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i))
	 (limit (or *print-length* most-positive-fixnum))
	 (sv))
	((null scan))
      (declare (fixnum i))
      (when (>= i limit)
	(write-string " ..." stream)
	(return))
      (setq sv (si:instance-ref obj i))
      (write-string " :" stream)
      (prin1 (slot-definition-name (car scan)) stream)
      (write-string " " stream)
      (prin1 sv stream))
    (write-string ")" stream)
    obj))
