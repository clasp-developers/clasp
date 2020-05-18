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

(defmethod ensure-class-using-class ((class null) name core:&va-rest rest)
  (clos::gf-log "In ensure-class-using-class (class null)%N")
  (clos::gf-log "     class -> %s%N" name)
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
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
  (error "Cannot modify slots of object with built-in-class"))

;;; ======================================================================
;;; STRUCTURES
;;;

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (core:allocate-new-instance class (class-size class)))

;;; structure-classes cannot be instantiated (but could be, as an extension)
(defmethod make-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (error "The structure-class (~A) cannot be instantiated" class))

(export 'make-instance)

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
