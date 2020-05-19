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
	   :initfunction initfunction
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

;;; ----------------------------------------------------------------------
;;;
;;; (PARSE-SLOTS slot-definition-form) => slot-definition-object
;;;
;;; This routine is the one responsible for parsing the definition of
;;; a slot in DEFCLASS.
;;;

(defun parse-slot (slot)
  (if (symbolp slot)
      `(list :name ',slot)
      (do* (output
	    (options (rest slot))
	    (value nil)
	    (extra nil)
            (initfunction))
	   ((null options)
            (let ((result (nconc output extra)))
              (if initfunction
                  `(list* :name ',(first slot) :initfunction ,initfunction ',result)
                  `(list* :name ',(first slot) ',result))))
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
                                 initfunction
				 `(lambda () ,value)))
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
      ((null scan)
       `(list ,@(nreverse collect)))
    (let* ((slotd (parse-slot (first scan)))
	   (name (getf (cdr slotd) :name)))
      (dolist (other-slotd collect)
        ;;; name might be (quote <name>) so test with eq or eql does not work 
	(when (equal name (getf (cdr other-slotd) :name))
	  (si::simple-program-error
	   "A definition for the slot ~S appeared twice in a DEFCLASS form"
	   name)))
      (push slotd collect))))

;;; ----------------------------------------------------------------------
