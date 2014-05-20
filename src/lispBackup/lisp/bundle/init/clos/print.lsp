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
;;; Load forms
;;;
;;; ECL extends the ANSI specification by allowing to use
;;; MAKE-LOAD-FORM on almost any kind of lisp object.
;;;

(defun make-load-form-saving-slots (object &key slot-names environment)
  (declare (ignore environment))
  (do* ((class (class-of object))
	(initialization (list 'progn))
	(slots (class-slots class) (cdr slots)))
      ((endp slots)
       (values `(allocate-instance ,class) (nreverse initialization)))
    (let* ((slot (first slots))
	   (slot-name (slot-definition-name slot)))
      (when (or (and (null slot-names)
		     (eq (slot-definition-allocation slot) :instance))
		(member slot-name slot-names))
	(push (if (slot-boundp object slot-name)
		  `(setf (slot-value ,object ',slot-name)
			 ',(slot-value object slot-name))
		  `(slot-makunbound ,object ',slot-name))
	      initialization)))))

(defun need-to-make-load-form-p (object env)
  "Return T if the object cannot be externalized using the lisp
printer and we should rather use MAKE-LOAD-FORM."
  (declare (ignore env))
  (let ((*load-form-cache* nil))
    (declare (special *load-form-cache*))
    (labels ((recursive-test (object)
	       (loop
		;; For simple, atomic objects we just return NIL. There is no need to
		;; call MAKE-LOAD-FORM on them
		(when (typep object '(or character number symbol pathname string bit-vector))
		  (return nil))
		;; For complex objects we set up a cache and run through the
		;; objects content looking for data that might require
		;; MAKE-LOAD-FORM to be externalized.  The cache is used to
		;; solve the problem of circularity and of EQ references.
		(unless *load-form-cache*
		  (setf *load-form-cache* (make-hash-table :size 128 :test #'eq)))
		(when (gethash object *load-form-cache*)
		  (return nil))
		(setf (gethash object *load-form-cache*) t)
		(cond ((arrayp object)
		       (unless (subtypep (array-element-type object) '(or character number))
			 (dotimes (i (array-total-size object))
			   (recursive-test (row-major-aref object i))))
		       (return nil))
		      ((consp object)
		       (recursive-test (car object))
		       (setf object (rest object)))
                      ((compiled-function-p object)
                       (multiple-value-bind (lex code data name)
                           (si::bc-split object)
                         (when (or (null data)
                                   (null code)
                                   (recursive-test lex)
                                   (recursive-test code)
                                   (recursive-test name))
                           (throw 'need-to-make-load-form t))
                         (setf object data)))
		      (t
		       (throw 'need-to-make-load-form t))))))
      (catch 'need-to-make-load-form
	(recursive-test object)
	nil))))

(defmethod make-load-form ((object t) &optional env)
  (flet ((maybe-quote (object)
	   (if (or (consp object) (symbolp object))
	       (list 'quote object)
	       object)))
    (unless (need-to-make-load-form-p object env)
      (return-from make-load-form (maybe-quote object)))
    (typecase object
      (compiled-function
       (multiple-value-bind (lex code data name)
           (si::bc-split object)
         (unless code
           (error "Cannot externalize object ~a" object))
         (values `(si::bc-join ,(make-load-form lex env)
                               ',code ; An specialized array, no load form
                               ,(make-load-form data env)
                               ,(make-load-form name env)))))
      (array
       (let ((init-forms '()))
	 (values `(make-array ',(array-dimensions object)
		   :element-type ',(array-element-type object)
		   :adjustable ',(adjustable-array-p object)
		   :initial-contents
		   ',(loop for i from 0 below (array-total-size object)
			   collect (let ((x (row-major-aref object i)))
				     (if (need-to-make-load-form-p x env)
					 (progn (push `(setf (row-major-aref ,object ,i) ',x)
						      init-forms)
						0)
					 x))))
		 (and init-forms `(progn ,@init-forms)))))
      (cons
       (values `(cons ,(maybe-quote (car object)) nil)
	       (and (rest object) `(rplacd ,(maybe-quote object)
					   ,(maybe-quote (cdr object))))))
      (hash-table
       (let* ((content (ext:hash-table-content object))
	      (make-form `(make-hash-table
			   :size ,(hash-table-size object)
			   :rehash-size ,(hash-table-rehash-size object)
			   :rehash-threshold ,(hash-table-rehash-threshold object)
			   :test ',(hash-table-test object))))
	 (if (need-to-make-load-form-p content env)
	     (values
	      make-form
	      `(dolist (i ',(loop for key being each hash-key in object
			       using (hash-value obj)
			       collect (cons key obj)))
		 (setf (gethash (car i) ,object) (cdr i))))
	     (values
	      `(ext:hash-table-fill ,make-form ',content)
	      nil))))
      (t
       (no-make-load-form object)))))

(defmethod make-load-form ((object standard-object) &optional environment)
  (no-make-load-form object))

(defmethod make-load-form ((object structure-object) &optional environment)
  (no-make-load-form object))

(defmethod make-load-form ((object condition) &optional environment)
  (no-make-load-form object))

(defun no-make-load-form (object)
  (declare (si::c-local))
  (error "No adequate specialization of MAKE-LOAD-FORM for an object of type"
	 (type-of object)))

(defmethod make-load-form ((class class) &optional environment)
  (declare (ignore environment))
  (let ((name (class-name class)))
    (if (and name (eq (find-class name) class))
	`(find-class ',name)
	(error "Cannot externalize anonymous class ~A" class))))

(defmethod make-load-form ((package package) &optional environment)
  (declare (ignore environment))
  `(find-package ,(package-name package)))

;;; ----------------------------------------------------------------------
;;; Printing
;;; ----------------------------------------------------------------------

(defmethod print-object ((instance t) stream)
  (print-unreadable-object (instance stream)
    (let ((*package* (find-package "CL")))
      (format stream "a ~S"
	      (class-name (si:instance-class instance)))))
  instance)

(defmethod print-object ((class class) stream)
  (print-unreadable-object (class stream)
    (let ((*package* (find-package "CL")))
      (format stream "The ~S ~S"
	      (class-name (si:instance-class class)) (class-name class))))
  class)

(defmethod print-object ((gf standard-generic-function) stream)
  (print-unreadable-object (gf stream :type t)
    (prin1 (generic-function-name gf) stream))
  gf)

(defmethod print-object ((m standard-method) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~A ~A"
	    (let ((gf (method-generic-function m)))
	      (if gf
		  (generic-function-name gf)
		  'UNNAMED))
	    (method-specializers m)))
  m)

(defun ext::float-nan-string (x)
  (when *print-readably*
    (error 'print-not-readable :object x))
  (cdr (assoc (type-of x)
              '((single-float . "#<single-float quiet NaN>")
                (double-float . "#<double-float quiet NaN>")
                (long-float . "#<long-float quiet NaN>")
                (short-float . "#<short-float quiet NaN>")))))

(defun ext::float-infinity-string (x)
  (when (and *print-readably* (null *read-eval*))
    (error 'print-not-readable :object x))
  (let* ((negative-infinities '((single-float .
                                 "#.ext::single-float-negative-infinity")
                                (double-float .
                                 "#.ext::double-float-negative-infinity")
                                (long-float .
                                 "#.ext::long-float-negative-infinity")
                                (short-float .
                                 "#.ext::short-float-negative-infinity")))
         (positive-infinities '((single-float .
                                 "#.ext::single-float-positive-infinity")
                                (double-float .
                                 "#.ext::double-float-positive-infinity")
                                (long-float .
                                 "#.ext::long-float-positive-infinity")
                                (short-float .
                                 "#.ext::short-float-positive-infinity")))
         (record (assoc (type-of x)
                        (if (plusp x) positive-infinities negative-infinities))))
    (unless record
      (error "Not an infinity"))
    (cdr record)))

;;; ----------------------------------------------------------------------
;;; Describe
;;; ----------------------------------------------------------------------

(defmethod describe-object ((obj t) (stream t))
  (let* ((class (class-of obj))
	 (slotds (class-slots class)))
    (format stream "~%~A is an instance of class ~A"
	    obj (class-name class))
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i))
	 (sv))
	((null scan))
	(declare (fixnum i))
	(setq sv (si:instance-ref obj i))
	(print (slot-definition-name (car scan)) stream) (princ ":	" stream)
	(if (si:sl-boundp sv)
	    (prin1 sv stream)
	  (prin1 "Unbound" stream))))
  obj)

(defmethod describe-object ((obj class) (stream t))
  (let* ((class  (si:instance-class obj))
	 (slotds (class-slots class)))
    (format stream "~%~A is an instance of class ~A"
	    obj (class-name class))
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i))
	 (sv))
	((null scan))
	(declare (fixnum i))
	(print (slot-definition-name (car scan)) stream) (princ ":	" stream)
	(case (slot-definition-name (car scan))
	      ((superiors inferiors)
	       (princ "(" stream)
	       (do* ((scan (si:instance-ref obj i) (cdr scan))
		     (e (car scan) (car scan)))
		    ((null scan))
		    (prin1 (class-name e) stream)
		    (when (cdr scan) (princ " " stream)))
	       (princ ")" stream))
	      (otherwise 
	       (setq sv (si:instance-ref obj i))
	       (if (si:sl-boundp sv)
		   (prin1 sv stream)
		 (prin1 "Unbound" stream))))))
  obj)

;;; ----------------------------------------------------------------------
