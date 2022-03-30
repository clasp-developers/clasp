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

;;; ======================================================================
;;; Built-in classes
;;; ----------------------------------------------------------------------

(defmethod make-instance ((class built-in-class) &rest initargs)
  (declare (ignore initargs))
  (error "The built-in class (~A) cannot be instantiated" class))

(defmethod allocate-instance ((class built-in-class) &rest initargs)
  (declare (ignore initargs))
  (error "The built-in class (~A) cannot be instantiated" class))

(defmethod ensure-class-using-class ((class null) name core:&va-rest rest)
  (clos::gf-log "In ensure-class-using-class (class null)%N")
  (clos::gf-log "     class -> %s%N" name)
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
    (declare (ignore direct-superclasses))
    (setf class (apply #'make-instance metaclass :name name options))
    (when name
      (si:create-type-name name)
      (setf (find-class name) class))))

(defmethod change-class ((instance t) (new-class symbol) core:&va-rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class))
  class)

(defmethod make-instance ((class-name symbol) core:&va-rest initargs)
  (apply #'make-instance (find-class class-name) initargs))

(defmethod slot-makunbound-using-class ((class built-in-class) self slotd)
  (declare (ignore self slotd))
  (error "SLOT-MAKUNBOUND-USING-CLASS cannot be applied on built-in object ~a of class ~a" class (class-of class)))

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

#+threads
(defmethod cas-slot-value-using-class
    (old new (class built-in-class) object slotd)
  (declare (ignore old new object slotd))
  (error "Cannot modify slots of object with built-in-class"))

;;; ======================================================================
;;; STRUCTURES
;;;
;;; As an extension, we allow the use of MAKE-INSTANCE, as well as SLOT-VALUE
;;; and sundry, on structure objects and classes.
;;; However, at least for now we do not go through SHARED-INITIALIZE or
;;; INITIALIZE-INSTANCE when using constructors instead, so specializing those
;;; on structure classes has undefined behavior.
;;; Also note that we don't define whether uninitialized slots are bound, or
;;; what they are bound to if they are bound.
;;; Most of the methods in standard.lsp work fine for structures and don't need
;;; to be specialized here.

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (core:allocate-raw-instance class (make-rack-for-class class)))

;;; The slot methods do need to be specialized. FIXME: Could possibly be
;;; cleaned up by making structure-class a subclass of std-class, but with
;;; an improved structure runtime we'd probably need to do something special
;;; here regardless.
(defmethod slot-value-using-class ((class structure-class) self slotd)
  (let* ((location (slot-definition-location slotd))
	 (value (standard-instance-access self location)))
    (if (si:sl-boundp value)
	value
	(values (slot-unbound class self (slot-definition-name slotd))))))

(defmethod slot-boundp-using-class ((class structure-class) self slotd)
  (declare (ignore class))
  (si:sl-boundp (standard-instance-access self
                                          (slot-definition-location slotd))))

(defmethod (setf slot-value-using-class) (val (class structure-class)
                                          self slotd)
  (declare (ignore class))
  (setf (standard-instance-access self (slot-definition-location slotd)) val))

(defmethod slot-makunbound-using-class ((class structure-class) instance slotd)
  (declare (ignore class))
  (setf (standard-instance-access instance (slot-definition-location slotd))
        (si:unbound))
  instance)

#+threads
(defmethod cas-slot-value-using-class
    (old new (class structure-class) object
     (slotd standard-effective-slot-definition))
  (let ((loc (slot-definition-location slotd)))
    (mp:cas (standard-instance-access object loc) old new)))

#+threads
(defmethod atomic-slot-value-using-class
    ((class structure-class) object (slotd standard-effective-slot-definition))
  (let* ((loc (slot-definition-location slotd))
         (v (mp:atomic (standard-instance-access object loc))))
    (if (si:sl-boundp v)
        v
        (values (slot-unbound class object (slot-definition-name slotd))))))

#+threads
(defmethod (setf atomic-slot-value-using-class)
    (new-value (class structure-class) object
     (slotd standard-effective-slot-definition))
  (let ((loc (slot-definition-location slotd)))
    (setf (mp:atomic (standard-instance-access object loc)) new-value)))

(defmethod finalize-inheritance ((class structure-class))
  (call-next-method)
  (dolist (slot (class-slots class))
    (unless (eq :INSTANCE (slot-definition-allocation slot))
      (error "The structure class ~S can't have shared slots" (class-name class)))))

(defun copy-structure (structure)
  ;; This could be done slightly faster by making copy-structure generic,
  ;; and having defstruct define a copy-structure method that works without a loop
  ;; or checking the size.
  (let* ((class (class-of structure))
         (copy (allocate-instance class))
         (size (class-size class)))
    (loop for i below size
          do (si:instance-set copy i (si:instance-ref structure i)))
    copy))
