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

;;; ----------------------------------------------------------------------
;;; INTERACTIVE NAVIGATION THROUGH OBJECTS
;;;

(defmethod select-clos-N ((instance standard-object))
  (let* ((class (si:instance-class instance))
	 (local-slotds (class-local-slots class))
	 (class-slotds (class-class-slots class)))
        (if local-slotds
	    (progn
	      (si::inspect-indent)
	      (format t "The local slots are:~%")
	      (incf si::*inspect-level*)
	      (dolist (slotd local-slotds)
		(si::inspect-indent-1)
		(format t "name : ~S" (clos::slot-definition-name slotd))
		(if (slot-boundp instance (clos::slot-definition-name slotd))
		    (si::inspect-recursively "value:"
		       (slot-value instance (clos::slot-definition-name slotd))
		       (slot-value instance (clos::slot-definition-name slotd)))
		    (si::inspect-print "value: Unbound"
		       nil
		       (slot-value instance (clos::slot-definition-name slotd)))))
	      (decf si::*inspect-level*))
	    (progn
	      (si::inspect-indent)
	      (format t "It has no local slots.~%")))
	(if class-slotds
	    (progn
	      (si::inspect-indent)
	      (format t "The class slots are:~%")
	      (incf si::*inspect-level*)
	      (dolist (slotd class-slotds)
		(si::inspect-indent-1)
		(format t "name : ~S" (clos::slot-definition-name slotd))
		(if (slot-boundp instance (clos::slot-definition-name slotd))
		    (si::inspect-recursively "value:"
		       (slot-value instance (clos::slot-definition-name slotd))
		       (slot-value instance (clos::slot-definition-name slotd)))
		    (si::inspect-print "value: Unbound"
		       nil
		       (slot-value instance (clos::slot-definition-name slotd)))))
	      (decf si::*inspect-level*))
	    (progn
	      (si::inspect-indent)
	      (format t "It has no class slots.~%")))))

(defun select-clos-N-inner-class (instance)
  (let* ((class (si:instance-class instance))
	 (local-slotds (class-local-slots class)))
        (if local-slotds
	    (progn
	      (si::inspect-indent)
	      (format t "The (local) slots are:~%")
	      (incf si::*inspect-level*)
	      (dolist (slotd local-slotds)
		(si::inspect-indent-1)
		(format t "name : ~S" (clos::slot-definition-name slotd))
		(if (slot-boundp instance (clos::slot-definition-name slotd))
		    (si::inspect-recursively "value:"
		       (slot-value instance (clos::slot-definition-name slotd))
;		       (slot-value instance (clos::slot-definition-name slotd))
		       )
		    (si::inspect-print "value: Unbound"
		       nil
;		       (slot-value instance (clos::slot-definition-name slotd))
		       )))
	      (decf si::*inspect-level*))
	    (progn
	      (si::inspect-indent)
	      (format t "It has no (local) slots.~%")))))

(defmethod select-clos-N ((instance std-class))
  (select-clos-N-inner-class instance))

(defmethod select-clos-N ((instance t))
  (select-clos-N-inner-class instance))

(defmethod select-clos-L ((instance standard-object))
  (let* ((class (si:instance-class instance))
	 (local-slotds (class-local-slots class))
	 (class-slotds (class-class-slots class)))
        (terpri)
	(if local-slotds
	    (progn
	      (format t "The names of the local slots are:~%")
	      (dolist (slotd local-slotds)
		(format t "  ~S~%" (clos::slot-definition-name slotd))))
	    (progn
	      (format t "It has no local slots.~%")))
	(terpri)
	(if class-slotds
	    (progn
	      (format t "The names of the class slots are:~%")
	      (dolist (slotd class-slotds)
		(format t "  ~S~%" (clos::slot-definition-name slotd))))
	    (progn
	      (format t "It has no class slots.~%")))
	(terpri)))

(defun select-clos-L-inner-class (instance)
  (let* ((class (si:instance-class instance))
	 (local-slotds (class-local-slots class)))
        (terpri)
	(if local-slotds
	    (progn
	      (format t "The names of the (local) slots are:~%")
	      (dolist (slotd local-slotds)
		      (format t "  ~S~%" (clos::slot-definition-name slotd))))
	    (progn
	      (format t "It has no (local) slots.~%")))
	(terpri)))

(defmethod select-clos-L ((instance std-class))
  (select-clos-L-inner-class instance))

(defmethod select-clos-L ((instance t))
  (select-clos-L-inner-class instance))

(defmethod select-clos-J ((instance standard-object))
  (let* ((class (si:instance-class instance))
	 (local-slotds (class-local-slots class))
	 (class-slotds (class-class-slots class))
	 (slotd (car (member (prog1
			       (read-preserving-whitespace *query-io*)
			       (si::inspect-read-line))
			     (append local-slotds class-slotds)
			     :key #'clos::slot-definition-name
			     :test #'eq))))
        (if slotd
	    (progn
	      (incf si::*inspect-level*)
	      (si::inspect-indent-1)
	      (format t "name : ~S" (clos::slot-definition-name slotd))
	      (if (slot-boundp instance (clos::slot-definition-name slotd))
		  (si::inspect-recursively "value:"
		     (slot-value instance (clos::slot-definition-name slotd))
		     (slot-value instance (clos::slot-definition-name slotd)))
		  (si::inspect-print "value: Unbound"
		     nil
		     (slot-value instance (clos::slot-definition-name slotd))))
	      (decf si::*inspect-level*))
	    (progn
	      (terpri)
	      (format t "~S is not a slot of the instance." (slot-definition-name slotd))
	      (terpri)
	      (terpri)))))

(defun select-clos-J-inner-class (instance)
  (let* ((class (si:instance-class instance))
	 (local-slotds (class-local-slots class))
	 (slotd (car (member (prog1
			       (read-preserving-whitespace *query-io*)
			       (si::inspect-read-line))
			     local-slotds
			     :key #'clos::slot-definition-name
			     :test #'eq))))
        (if slotd
	    (progn
	      (incf si::*inspect-level*)
	      (si::inspect-indent-1)
	      (format t "name : ~S" (clos::slot-definition-name slotd))
	      (if (slot-boundp instance (clos::slot-definition-name slotd))
		  (si::inspect-recursively "value:"
		     (slot-value instance (clos::slot-definition-name slotd))
;		     (slot-value instance (clos::slot-definition-name slotd))
		     )
		  (si::inspect-print "value: Unbound"
		     nil
;		     (slot-value instance (clos::slot-definition-name slotd))
		     ))
	      (decf si::*inspect-level*))
	    (progn
	      (terpri)
	      (format t "~S is not a slot of the instance." (slot-definition-name slotd))
	      (terpri)
	      (terpri)))))

(defmethod select-clos-J ((instance std-class))
  (select-clos-J-inner-class instance))

(defmethod select-clos-J ((instance t))
  (select-clos-J-inner-class instance))

(defun select-clos-? ()
  (terpri)
  (format t
	  "Inspect commands for clos instances:~%~
n (or N or Newline):  inspects all slots of the class (recursively).~%~
s (or S):             skips the field.~%~
p (or P):             pretty-prints the field.~%~
a (or A):             aborts the inspection of the rest of the fields.~%~
e (or E) form:        evaluates and prints the form.~%~
l (or L):             show the names of all slots.~%~
j (or J) slot-name:   inspect the slot with the name requested.~%~
q (or Q):             quits the inspection.~%~
?:                    prints this.~%~%"
	  ))

(defun class-local-slots (class)
  (remove :class (class-slots class) :key #'clos::slot-definition-allocation))

(defun class-class-slots (class)
  (remove :instance (class-slots class) :key #'clos::slot-definition-allocation))

(defmethod inspect-obj ((instance standard-object))
  (unless (let ((metaclass (si:instance-class (si:instance-class instance))))
            (or (eq metaclass (find-class 'STANDARD-CLASS))
                (eq metaclass (find-class 'FUNCALLABLE-STANDARD-CLASS))))
          (terpri)
          (format t "No applicable method CLOS::INSPECT-OBJ for an instance~%")
          (format t "of class ~S" (si:instance-class instance))
          (throw 'SI::ABORT-INSPECT nil))
  (decf si::*inspect-level*)
  (let* ((class (si:instance-class instance))
	 (local-slotds (class-local-slots class))
	 (class-slotds (class-class-slots class)))
    (declare (type class class))
    (loop
      (format t "~S - clos object:" instance)
      (incf si::*inspect-level*)
      (si::inspect-indent)
      (format t "- it is an instance of class named ~S,"
	      (class-name class))
      (si::inspect-indent)
      (format t "- it has ~A local slots and ~A class slots: "
	      (length local-slotds) (length class-slotds))
      (force-output)
      (case (do ((char (read-char *query-io*) (read-char *query-io*)))
		((and (char/= char #\Space) (char/= #\Tab)) char))
	    ((#\Newline #\Return)
	     (select-clos-N instance)
	     (return nil))
	    ((#\n #\N)
	     (si::inspect-read-line)
	     (select-clos-N instance)
	     (return nil))
	    ((#\s #\S)
	     (si::inspect-read-line)
	     (return nil))
	    ((#\p #\P)
	     (si::inspect-read-line)
	     (si::select-P instance))
	    ((#\a #\A)
	     (si::inspect-read-line)
	     (throw 'SI::ABORT-INSPECT nil))
	    ((#\e #\E)
	     (si::select-E))
	    ((#\q #\Q)
	     (si::inspect-read-line)
	     (throw 'SI::QUIT-INSPECT nil))
	    ((#\l #\L)
	     (si::inspect-read-line)
	     (select-clos-L instance))
	    ((#\j #\J)
	     (select-clos-J instance))
	    ((#\?)
	     (si::inspect-read-line)
	     (select-clos-?))
	    (t
	     (si::inspect-read-line)))
      (decf si::*inspect-level*)
      (si::inspect-indent)))
  (incf si::*inspect-level*))

(defun inspect-obj-inner-class (instance)
  (decf si::*inspect-level*)
  (let* ((class (si:instance-class instance))
	 (local-slotds (class-local-slots class)))
    (declare (type class class))
    (loop
      (format t "~S - clos object:" instance)
      (incf si::*inspect-level*)
      (si::inspect-indent)
      (format t "- it is an instance of class named ~S,"
	      (class-name class))
      (si::inspect-indent)
      (format t "- it has ~A local slots: " (length local-slotds))
      (force-output)
      (case (do ((char (read-char *query-io*) (read-char *query-io*)))
		((and (char/= char #\Space) (char/= #\Tab)) char))
	    ((#\Newline #\Return)
	     (select-clos-N instance)
	     (return nil))
	    ((#\n #\N)
	     (si::inspect-read-line)
	     (select-clos-N instance)
	     (return nil))
	    ((#\s #\S)
	     (si::inspect-read-line)
	     (return nil))
	    ((#\p #\P)
	     (si::inspect-read-line)
	     (si::select-P instance))
	    ((#\a #\A)
	     (si::inspect-read-line)
	     (throw 'SI::ABORT-INSPECT nil))
	    ((#\e #\E)
	     (si::select-E))
	    ((#\q #\Q)
	     (si::inspect-read-line)
	     (throw 'SI::QUIT-INSPECT nil))
	    ((#\l #\L)
	     (si::inspect-read-line)
	     (select-clos-L instance))
	    ((#\j #\J)
	     (select-clos-J instance))
	    ((#\?)
	     (si::inspect-read-line)
	     (select-clos-?))
	    (t
	     (si::inspect-read-line)))
      (decf si::*inspect-level*)
      (si::inspect-indent)))
  (incf si::*inspect-level*))

(defmethod inspect-obj ((instance std-class))
  (inspect-obj-inner-class instance))

(defmethod inspect-obj ((instance t))
  (inspect-obj-inner-class instance))

;;; -------------------------------------------------------------------------
;;;
;;; Documentation
;;;

(core:defconstant-equal +valid-documentation-types+
    '(compiler-macro function method-combination setf structure
      t type variable))

#+clasp(fmakunbound 'documentation)
#+clasp(fmakunbound '(setf documentation))

(defgeneric documentation (object doc-type))
(defgeneric (setf documentation) (new-value object doc-type))

(defmethod documentation ((object symbol) doc-type)
  (when (member doc-type +valid-documentation-types+)
    (case doc-type
      (type
       (let ((c (find-class object nil)))
	 (if c
	     (documentation c t)
	     (si::get-documentation object doc-type))))
      (function
       (or (si::get-documentation object doc-type)
	   (and (fboundp object)
		(documentation (or (macro-function object)
				   (fdefinition object))
			       doc-type))))
      (otherwise
       (si::get-documentation object doc-type)))))

(defmethod (setf documentation) (new-value (object symbol) doc-type)
  (when (member doc-type +valid-documentation-types+)
    (case doc-type
      (type
       (let ((c (find-class object nil)))
	 (if c
	     (progn
	       (si::set-documentation object 'type nil)
	       (si::set-documentation object 'structure nil)
	       (setf (documentation c t) new-value))
	     (si::set-documentation object doc-type new-value))))
      (function
       (if (fboundp object)
	   (let ((c (or (macro-function object) (fdefinition object))))
	     (si::set-documentation c 'function nil)
	     (setf (documentation c 'function) new-value))
	   (si::set-documentation object doc-type new-value)))
      (otherwise
       (si::set-documentation object doc-type new-value))))
  new-value)

(defmethod documentation ((object package) doc-type)
  (when (member doc-type '(t package))
    (core:package-documentation object)))

(defmethod (setf documentation) (new-value (object package) doc-type)
  (when (member doc-type '(t package))
    (setf (core:package-documentation object) new-value)))

(defmethod documentation ((object class) doc-type)
  (when (and (member doc-type '(t type)) (slot-boundp object 'docstring))
    (slot-value object 'docstring)))

(defmethod (setf documentation) (new-value (object class) doc-type)
  (when (member doc-type '(t type))
    (setf (slot-value object 'docstring) new-value)))

(defmethod documentation ((object structure-class) doc-type)
  (when (member doc-type '(t type))
    (si::get-documentation (class-name object) 'structure)))

(defmethod (setf documentation) (new-value (object structure-class) doc-type)
  (when (member doc-type '(t type))
    (setf (documentation (class-name object) 'structure) new-value)))

(defmethod documentation ((object list) doc-type)
  (when (member doc-type '(function compiler-macro))
    (si::get-documentation object doc-type)))

(defmethod (setf documentation) (new-value (object list) doc-type)
  (when (member doc-type '(function compiler-macro))
    (si::set-documentation object doc-type new-value)))

(defmethod documentation ((object standard-generic-function) doc-type)
  (when (member doc-type '(t function))
    (slot-value object 'docstring)))

(defmethod (setf documentation) (new-value (object standard-generic-function) doc-type)
  (when (member doc-type '(t function))
    (setf (slot-value object 'docstring) new-value)))

(defmethod documentation ((object standard-method) doc-type)
  (when (member doc-type '(t function))
    (slot-value object 'docstring)))

(defmethod (setf documentation) (new-value (object standard-method) doc-type)
  (when (member doc-type '(t function))
    (setf (slot-value object 'docstring) new-value)))

(defmethod documentation ((object function) doc-type)
  (when (member doc-type '(t function))
    (si::get-documentation object doc-type)))

(defmethod (setf documentation) (new-value (object function) doc-type)
  (when (member doc-type '(t function))
    (si::set-documentation object doc-type new-value)))

(defmethod documentation ((object slot-definition) doc-type)
  (when (member doc-type '(t function))
    (slot-value object 'docstring)))

(defmethod (setf documentation) (new-value (object slot-definition) doc-type)
  (when (member doc-type '(t function))
    (setf (slot-value object 'docstring) new-value)))

