;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPSTRUCT. STRUCTURE related optimizations.

;;;;  Copyright (c) 2008. Juan Jose Garcia-Ripol
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;;;
;;; GET-SLOT-TYPE
;;;
;;; Given a structure type and a slot index, infer the type of the output.
;;;
(defun get-slot-type (name index)
  ;; default is t
  (or (third (nth index (get-sysprop name 'SYS::STRUCTURE-SLOT-DESCRIPTIONS))) 'T))

;;;
;;; STRUCTURE SLOT READING
;;;
;;; By looking at the name of a function we may infer whether it is a
;;; reader for a structure slot. If this is the case and the policy
;;; allows us, we will inline the slot access and infer the type of
;;; the output.
;;;

(defun maybe-optimize-structure-access (fname args)
  (let* ((slot-description (get-sysprop fname 'SYS::STRUCTURE-ACCESS)))
    (when (and slot-description
	       (inline-possible fname)
	       (policy-inline-slot-access-p))
      ;(format t "~%;;; Optimizing structure accessor ~A" fname)
      (let (structure-type slot-index)
	(unless (and (consp slot-description)
		     (setf structure-type (car slot-description)
			   slot-index (cdr slot-description))
		     (typep slot-index 'fixnum))
	  (cmpwarn "Unable to inline access to structure slot ~A because index is corrupt: ~A"
		   fname slot-index)
	  (return-from maybe-optimize-structure-access nil))
	(unless (= (length args) 1)
	  (cmpwarn "Too many arguments for structure slot accessor ~A" fname)
	  (return-from maybe-optimize-structure-access nil))
	(setf args (first args))
	(cond
	  ((eq structure-type 'list)
	   `(elt ,args ,slot-index))
	  ((eq structure-type 'vector)
	   `(svref ,args ,slot-index))
	  ((consp structure-type)
	   `(aref (the ,structure-type ,args) ,slot-index))
	  (t
           `(,args ',structure-type ,slot-index)))))))

(define-compiler-macro si::structure-ref (&whole whole object structure-name index
					  &environment env)
  (if (and (policy-inline-slot-access env)
	   (constantp structure-name env)
	   (constantp index env))
      (let* ((index (ext:constant-form-value index env))
	     (aux (gensym))
	     (form `(ffi:c-inline (,aux ,index) (:object :fixnum) :object
				  "(#0)->instance.slots[#1]"
				  :one-liner t)))
	(unless (policy-assume-no-errors env)
	  (setf form
		(let ((structure-name (ext:constant-form-value structure-name env)))
		  `(ext:compiler-typecase ,aux
		     (,structure-name ,form)
		     (t (ffi:c-inline (,aux ,structure-name ,index)
				      (:object :object :fixnum)
				      :object
				      "ecl_structure_ref(#0,#1,#2)"
				      :one-liner t))))))
	`(let ((,aux ,object))
	   (declare (:read-only ,aux))
	   ,form))
      whole))

(define-compiler-macro si::structure-set (&whole whole object structure-name index value
					  &environment env)
  (if (and (policy-inline-slot-access env)
	   (constantp structure-name env)
	   (constantp index env))
      (let* ((index (ext:constant-form-value index env))
	     (aux (gensym))
	     (form `(ffi:c-inline (,aux ,index ,value) (:object :fixnum :object) :object
				  "(#0)->instance.slots[#1]=#2"
				  :one-liner t)))
	(unless (policy-assume-no-errors env)
	  (let ((structure-name (ext:constant-form-value structure-name env)))
	    (setf form
		  `(ext:compiler-typecase
		    ,aux
		    (,structure-name ,form)
		    (t (ffi:c-inline (,aux ',structure-name ,index ,value)
				     (:object :object :fixnum :object)
				     :object
				     "ecl_structure_set(#0,#1,#2,#3)"
				     :one-liner t))))))
	`(let ((,aux ,object))
	   (declare (:read-only ,aux))
	   ,form))
      whole))
