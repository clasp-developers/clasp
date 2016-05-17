;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

(defconstant +initform-unsupplied+ '+initform-unsupplied+)

;;; ----------------------------------------------------------------------
;;; SLOT descriptors
;;;

(defun make-simple-slotd (class
			  &key name (initform +initform-unsupplied+) initfunction
			  (type 'T) (allocation :instance)
			  initargs readers writers documentation location)
  (when (and (eq allocation :class)
	     (functionp initfunction))
    (setf initfunction (constantly (funcall initfunction))))
  (with-early-make-instance +slot-definition-slots+
    (slotd class
	   :name name
	   :initform initform
	   :initfunction (if (listp initfunction) (eval initfunction) initfunction)
	   :type type
	   :allocation allocation
	   :initargs initargs
	   :readers readers
	   :writers writers
	   :documentation documentation
	   :location location)
    slotd))

(defun freeze-class-slot-initfunction (slotd)
  (when (eq (getf slotd :allocation) :class)
    (let ((initfunc (getf slotd :initfunction)))
      (when initfunc
        (setf (getf slotd :initfunction)
              (constantly (funcall initfunc))))))
  slotd)

(defun canonical-slot-to-direct-slot (class slotd)
  ;; Class slot init functions must be called right away
  (setf slotd (freeze-class-slot-initfunction slotd))
  (if (find-class 'slot-definition nil)
      (apply #'make-instance
	     (apply #'direct-slot-definition-class class
		    (freeze-class-slot-initfunction slotd))
	     slotd)
      (apply #'make-simple-slotd class slotd)))

(defun direct-slot-to-canonical-slot (slotd)
  (list . #.(loop for (name . rest) in +slot-definition-slots+
	       collect (getf rest :initarg)
	       collect `(,(getf rest :accessor) slotd))))

;;; ----------------------------------------------------------------------
;;;
;;; (PARSE-SLOTS slot-definition-form) => slot-definition-object
;;;
;;; This routine is the one responsible for parsing the definition of
;;; a slot in DEFCLASS.
;;;

(defun make-function-initform (form)
  ;; INITFORM is a form that is to be evaluated at runtime. If it is a
  ;; constant value, we output simply a quoted form. If it is not,
  ;; we output a function that can be invoked at runtime to retrieve
  ;; the value.
  ;;
  ;; Output => (FUNCTION (LAMBDA () form))
  ;;        => (QUOTE ...)
  ;;
  (if (constantp form)
      `(constantly ,form)
      `#'(lambda () ,form)))

(defun parse-slot (slot &optional (full nil))
  (declare (si::c-local))
  (if (symbolp slot)
      (list* :name slot
	     (when full (list :initform '+INITFORM-UNSUPPLIED+ :initfunction nil
			      :initargs nil :readers nil :writers nil
			      :allocation :instance :documentation nil
			      :type 'T)))
      (do* ((output (parse-slot (first slot) full))
	    (options (rest slot))
	    (value nil)
	    (extra nil))
	   ((null options)
	    (nconc output extra))
	(let ((option (pop options)))
	  (when (endp options)
	    (si::simple-program-error
	     "In the slot description ~S,~%the option ~S is missing an argument"
	     slot option))
	  (let ((value (pop options)))
	    (when (and (member option '(:allocation :initform :type :documentation))
		       (getf options option))
	      (si::simple-program-error
	       "In the slot description ~S,~%the option ~S is duplicated"
	       slot option))
	    (case option
	      (:initarg    (push value (getf output :initargs)))
	      (:initform   (setf (getf output :initform) value
				 (getf output :initfunction)
				 (make-function-initform value)))
	      (:accessor   (push value (getf output :readers))
			   (push `(setf ,value) (getf output :writers)))
	      (:reader     (push value (getf output :readers)))
	      (:writer     (push value (getf output :writers)))
	      (:allocation (setf (getf output :allocation) value))
	      (:type       (setf (getf output :type) value))
	      (:documentation  (push value (getf output :documentation)))
	      (otherwise   (if (or (getf extra option)
				   (getf options option))
			       (push value (getf extra option))
			       (setf (getf extra option) value)))))))))

(defun parse-slots (slots)
  (do ((scan slots (cdr scan))
       (collect))
      ((null scan) (nreverse collect))
    (let* ((slotd (parse-slot (first scan)))
	   (name (getf slotd :name)))
      (dolist (other-slotd collect)
	(when (eq name (getf other-slotd :name))
	  (si::simple-program-error
	   "A definition for the slot ~S appeared twice in a DEFCLASS form"
	   name)))
      (push slotd collect))))

;;; ----------------------------------------------------------------------
