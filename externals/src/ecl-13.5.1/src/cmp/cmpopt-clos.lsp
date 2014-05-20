;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPOPT-CLOS. Optimization of CLOS related operations

;;;;  Copyright (c) 201. Juan Jose Garcia-Ripol
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun clos-compiler-macro-expand (fname args)
  (when (and (si::valid-function-name-p fname)
	     (fboundp fname))
    (let ((function (fdefinition fname)))
      (when (typep function 'generic-function)
	(generic-function-macro-expand function (list* fname args))))))

(defmethod generic-function-macro-expand ((g standard-generic-function) whole)
  (let* ((output (optimizable-slot-accessor g whole))
	 (success (and output t)))
    (values output success)))

(defun optimizable-slot-reader (method whole)
  (when (typep method 'clos:standard-reader-method)
    (let ((class (first (clos:method-specializers method))))
      (when (clos::class-sealedp class)
	(let* ((slotd (clos:accessor-method-slot-definition method))
	       (location (clos:slot-definition-location slotd)))
	  (let ((object (gentemp)))
	    (cmpnote "Inlining read access to slot ~a from class ~a"
		     (clos:slot-definition-name slotd)
		     (class-name class))
	    #+(or)
	    `(let ((,object ,(second whole)))
	       (locally (declare (notinline ,(first whole)))
		 (if (typep ,object ',(class-name class))
		     (si::instance-ref ,object ,location)
		     (,(first whole) ,object))))
	    ;(format t "~&;;; Inlining accessor ~a" (first whole))
	    `(let ((,object ,(second whole)))
	       (optional-type-check ,object ',class)
	       (locally (declare (optimize speed (safety 0)))
		 (si::instance-ref ,object ,location)))))))))

(defun optimizable-slot-writer (method whole)
  (when (typep method 'clos:standard-writer-method)
    (let ((class (second (clos:method-specializers method))))
      (when (clos::class-sealedp class)
	(let* ((slotd (clos:accessor-method-slot-definition method))
	       (location (clos:slot-definition-location slotd)))
	  (let* ((object (gentemp))
		 (value (gentemp)))
	    (cmpnote "Inlining write access to slot ~a from class ~a"
		     (clos:slot-definition-name slotd)
		     (class-name class))
	    #+(or)
	    `(let ((,value ,(second whole))
		   (,object ,(third whole)))
	       (locally (declare (notinline ,(first whole)))
		 (if (typep ,object ',(class-name class))
		     (si::instance-set ,object ,location ,value)
		     (funcall #',(first whole) ,value ,object))))
	    ;(format t "~&;;; Inlining accessor ~a" (first whole))
	    `(let ((,value ,(second whole))
		   (,object ,(third whole)))
	       (optional-type-check ,object ',class)
	       (locally (declare (optimize speed (safety 0)))
		 (si::instance-set ,object ,location ,value)))))))))

(defun optimizable-slot-accessor (g whole)
  (and (policy-inline-slot-access)
       (let ((methods (clos:generic-function-methods g)))
	 (and methods
	      (null (rest methods))
	      (let* ((principal (first methods)))
		(or (optimizable-slot-reader principal whole)
		    (optimizable-slot-writer principal whole)))))))
