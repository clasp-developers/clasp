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

(defmethod ensure-class-using-class ((class null) name &rest rest)
  (clos::gf-log "In ensure-class-using-class (class null)\n")
  (clos::gf-log "     class -> %s\n" name)
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
    (setf class (apply #'make-instance metaclass :name name options))
    (invalidate-generic-functions-with-class-selector class)
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

;;; ======================================================================
;;; STRUCTURES
;;;

;;; structure-classes cannot be instantiated
(defmethod make-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (error "The structure-class (~A) cannot be instantiated" class))

(export 'make-instance)

(defmethod finalize-inheritance ((class structure-class))
  (call-next-method)
  (dolist (slot (class-slots class))
    (unless (eq :INSTANCE (slot-definition-allocation slot))
      (error "The structure class ~S can't have shared slots" (class-name class)))))

(defmethod print-object ((obj structure-object) stream)
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class)))
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
      ;; fix bug where symbols like :FOO::BAR are printed
      (write-string " " stream)
      (let ((kw (intern (symbol-name (slot-definition-name (car scan)))
                        (load-time-value (find-package "KEYWORD")))))
        (prin1 kw stream))
      (write-string " " stream)
      (prin1 sv stream))
    (write-string ")" stream)
    obj))


