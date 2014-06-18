;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPCLOS. CLOS related optimizations.

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
;;; GENERIC OPTIMIZATION
;;;

(defun maybe-optimize-generic-function (fname args)
  (when (fboundp fname)
    (let ((gf (fdefinition fname)))
      (when (typep gf 'standard-generic-function)
	;;(check-generic-function-args gf args)
	(when (policy-inline-slot-access)
	  (maybe-optimize-slot-accessor fname gf args))))))

;;;
;;; PRECOMPUTE APPLICABLE METHODS
;;;
;;; Computes a list of methods that would apply given what we know
;;; about their arguments. Since the types are not exact, we have to
;;; use subtypep.  We could speed this up if we could precompute the
;;; classes for the c-args.
;;;

(defun precompute-applicable-methods (methods c-args)
  (flet ((applicable-method-p (m)
	   (loop for specializer in (clos:method-specializers m)
	      for arg in c-args
	      always (let ((arg-type (c1form-type arg)))
		       (subtypep arg-type (if (consp specializer)
					      `(member ,(second specializer))
					      specializer))))))
    (delete-if-not #'applicable-method-p methods)))

;;;
;;; SLOT ACCESSORS
;;;
;;; The following functions deal with an ECL extension, which are
;;; sealed slots.  These slots have a fixed location which is
;;; inherited by subclasses. They normally appear when you add the
;;; option (:sealedp t) to a class definition.
;;;
;;; When ECL detects that you call an accessor to such a slot, it can
;;; optimize the operation, using a direct access based on the
;;; position of the slot. This optimization is only active when the
;;; safety levels are low, because it prevents you from changing the
;;; class hierarchy.
;;;

(defun find-slot-accessors (gf)
  (loop for method in (clos:generic-function-methods gf)
     with readers = '()
     with writers = '()
     with reader-class = (find-class 'clos:standard-reader-method)
     with writer-class = (find-class 'clos:standard-writer-method)
     do (let ((method-class (class-of method)))
	  (cond ((si::subclassp method-class reader-class)
		 (push method readers))
		((si::subclassp method-class writer-class)
		 (push method writers))))
     finally (return (values readers writers))))

(defun maybe-optimize-slot-accessor (fname gf args)
  (multiple-value-bind (readers writers)
      (find-slot-accessors gf)
    ;(format t "~%;;; Found ~D readers and ~D writers for ~A" (length readers) (length writers) fname)
    (cond ((and readers writers)
	   (cmpwarn "When analyzing generic function ~A found both slot reader and writer methods"
		    fname))
	  ((not (or readers writers))
	   nil)
	  ((/= (length args) (length (clos::generic-function-spec-list gf)))
	   (cmpwarn "Too many arguments for generic function ~A" fname)
	   nil)
	  (readers
	   (try-optimize-slot-reader readers args))
	  (writers
	   (try-optimize-slot-writer writers args)))))

(defun try-optimize-slot-reader (readers args)
  (let* ((object (first args))
	 (c-object (c1expr object))
	 (readers (precompute-applicable-methods readers (list c-object))))
    ;(format t "~%;;; Found ~D really applicable reader" (length readers))
    (when (= (length readers) 1)
      (let ((reader (first readers)))
	(when (typep reader 'clos:standard-reader-method)
	  (let* ((slotd (clos:accessor-method-slot-definition reader))
		 (index (clos::safe-slot-definition-location slotd)))
	    (when (si::fixnump index)
	      `(clos::safe-instance-ref ,object ,index))))))))

(defun try-optimize-slot-writer (orig-writers args)
  (let* ((c-args (mapcar #'c1expr args))
	 (writers (precompute-applicable-methods orig-writers c-args)))
    ;(format t "~%;;; Found ~D really applicable writer" (length writers))
    (when (= (length writers) 1)
      (let ((writer (first writers)))
	(when (typep writer 'clos:standard-writer-method)
	  (let* ((slotd (clos:accessor-method-slot-definition writer))
		 (index (clos::safe-slot-definition-location slotd)))
	    (when (si::fixnump index)
	      `(si::instance-set ,(second args) ,index ,(first args)))))))))

#+(or)
(progn .
  #.(loop for var in '(clos::+standard-generic-function-slots+
                       clos::+standard-method-slots+
                       clos::+standard-class-slots+
                       clos::+class-slots+)
       for slot-list = (symbol-value var)
       nconc
         (loop for i from 0
            for slot-definition in slot-list
            for accessor = (cadr (member :accessor slot-definition))
            when accessor
            collect `(define-compiler-macro ,accessor (&whole whole obj &environment env)
                       (if (policy-inline-slot-access env)
			   `(clos::safe-instance-ref ,obj ,,i)
                           whole)))))
