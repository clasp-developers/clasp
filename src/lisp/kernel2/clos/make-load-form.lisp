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
;;; Clasp extends the ANSI specification by allowing to use
;;; MAKE-LOAD-FORM on almost any kind of lisp object.
;;; But it doesn't necessarily use those methods in the compiler. see cmpliteral.lisp
;;;

(defun make-load-form-saving-slots (object &key slot-names environment)
  ;; The ALLOCATE-INSTANCE form here is treated magically by the file
  ;; compiler; see cmp/cmpliteral.lisp ALLOCATE-INSTANCE-FORM-P
  (declare (ignore environment))
  (do* ((class (class-of object))
	(initialization (list object 'load-instance))
	(slots (class-slots class) (cdr slots)))
      ((endp slots)
       (values `(allocate-instance ,class) (nreverse initialization)))
    (let* ((slot (first slots))
	   (slot-name (slot-definition-name slot)))
      (when (or (and (null slot-names)
		     (eq (slot-definition-allocation slot) :instance))
		(member slot-name slot-names))
        (when (slot-boundp object slot-name)
          (push `',slot-name initialization)
          (push `',(slot-value object slot-name) initialization))))))

;;; This function basically exists so that cmpliteral can handle
;;; make-load-form-saving-slots forms without compiling them recursively.
;;; We used to use a progn of setf slot-values, but that's more complex.
(defun load-instance (instance &rest slot-names-values)
  (loop for (name value) on slot-names-values by #'cddr
        do (setf (slot-value instance name) value))
  (values))

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
		      (t
		       (throw 'need-to-make-load-form t))))))
      (catch 'need-to-make-load-form
	(recursive-test object)
	nil))))

(defgeneric make-load-form (object &optional env))

(defmethod make-load-form ((object t) &optional env)
  (flet ((maybe-quote (object)
	   (if (or (consp object) (symbolp object))
	       (list 'quote object)
	       object)))
    (unless (need-to-make-load-form-p object env)
      (return-from make-load-form (maybe-quote object)))
    (typecase object
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
      (t
       (no-make-load-form object)))))

(defmethod make-load-form ((object standard-object) &optional environment)
  (declare (ignore environment))
  (no-make-load-form object))

(defmethod make-load-form ((object structure-object) &optional environment)
  (declare (ignore environment))
  (no-make-load-form object))

(defmethod make-load-form ((object condition) &optional environment)
  (declare (ignore environment))
  (no-make-load-form object))

(defun no-make-load-form (object)
  #+(or)(declare (optimize (debug 3)))
  (error "No adequate specialization of MAKE-LOAD-FORM for an object of type ~a"
	 (type-of object)))

(defmethod make-load-form ((class class) &optional environment)
  ;; The find-class form here is treated magically by the file compiler-
  ;; see cmp/cmpliteral.lisp FIND-CLASS-FORM-P
  (declare (ignore environment))
  (let ((name (class-name class)))
    (if (and name (eq (find-class name) class))
	`(find-class ',name)
	(error "Cannot externalize anonymous class ~A" class))))

(defmethod make-load-form ((package package) &optional environment)
  (declare (ignore environment))
  `(find-package ,(package-name package)))

;;; Extension. (Allowed per CLHS 3.2.4.3.)
;;; This is required for a lot of satiation.lisp to function.
(defmethod make-load-form ((method method) &optional environment)
  (declare (ignore environment))
  ;; FIXME: Should spruce up cmpliteral so it doesn't compile calls with
  ;; all constant arguments.
  `(load-method
    ',(generic-function-name (method-generic-function method))
    ',(method-qualifiers method)
    ',(method-specializers method)))

;;; Also an extension, to support the above.
(defmethod make-load-form ((spec eql-specializer) &optional environment)
  (declare (ignore environment))
  `(intern-eql-specializer ',(eql-specializer-object spec)))

(defun class-slotd-form (slot-name class &optional earlyp)
  (let ((form
          `(or (find ',slot-name (class-slots ,class) :key #'slot-definition-name)
               (error "Probably a BUG: slot ~a in ~a stopped existing between compile and load"
                      ',slot-name ,class))))
    (if earlyp
        `(with-early-accessors (+standard-class-slots+ +slot-definition-slots+)
           (flet ((slot-definition-name (sd) (slot-definition-name sd))) ; macro, so.
             ,form))
        form)))

(defmethod make-load-form ((method effective-reader-method)
                           &optional environment)
  (declare (ignore environment))
  (let ((orig (effective-accessor-method-original method)))
    `(,(if (eq (class-of orig) (find-class 'standard-reader-method))
           'early-intern-effective-reader
           'intern-effective-reader)
      ',orig
      ',(effective-accessor-method-location method))))

(defmethod make-load-form ((method effective-writer-method)
                           &optional environment)
  (declare (ignore environment))
  (let ((orig (effective-accessor-method-original method)))
    `(,(if (eq (class-of orig) (find-class 'standard-writer-method))
           'early-intern-effective-writer
           'intern-effective-writer)
      ',orig
      ',(effective-accessor-method-location method))))

(defmethod make-load-form ((object core:file-scope) &optional env)
  (declare (ignore env))
  (values
   `(core:make-cxx-object ,(find-class 'core:file-scope))
   `(core:decode
     ,object
     ',(core:encode object))))

(defmethod make-load-form ((object core:source-pos-info) &optional environment)
  (declare (ignore environment))
  (values
   `(core:make-cxx-object ,(find-class 'core:source-pos-info)
                          :sfi ,(core:file-scope
                                 (core:source-pos-info-file-handle object))
                          :fp ,(core:source-pos-info-filepos object)
                          :l ,(core:source-pos-info-lineno object)
                          :c ,(core:source-pos-info-column object))
   `(core:setf-source-pos-info-extra
     ',object
     ',(core:source-pos-info-inlined-at object)
     ',(core:source-pos-info-function-scope object))))
