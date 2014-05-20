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
;;; ORDERING OF CLASSES
;;;
;;; We have two implementations of the algorithm described in Sect. 4.3.5
;;; of the Common Lisp Hyperspec. The first implementation is a literal
;;; transcription of that algorithm. The second implementation does not
;;; create the list of pairs for describing the order, it is not recursive
;;; and conses less
#+(or)
(defun compute-clos-class-precedence-list (new-class superclasses)
  (labels ((pair-list (l)
	     (if (or (null l) (endp (cdr l)))
		 nil
		 (cons (cons (first l) (second l))
		       (pair-list (rest l)))))
	   (walk-supers (parent superclasses class-list precedence-alist)
	     (let ((new-alist (pair-list (if parent
					     (list* parent superclasses)
					     superclasses))))
	       (setf precedence-alist (nconc new-alist precedence-alist)
		     class-list (union superclasses class-list)))
	     (dolist (c superclasses)
	       (multiple-value-setq (class-list precedence-alist)
		 (walk-supers c (class-direct-superclasses c) class-list precedence-alist)))
	     (values class-list precedence-alist))
	   (cycle-error (new-class)
	     (error "A cycle has been detected in the class precedence list for ~A."
		    (class-name new-class)))
	   (free-elements (class-list precedence-alist)
	     (set-difference class-list
			     (delete-duplicates (mapcar #'cdr precedence-alist))))
	   (next-element (free-list cpl)
	     (if (or (null cpl) (endp free-list) (endp (rest free-list)))
		 (first free-list)
		 (dolist (i cpl nil)
		   (dolist (j (class-direct-superclasses i))
		     (when (member j free-list)
		       (return-from next-element j)))))))
  (if (endp (rest superclasses))
      (let ((class (first superclasses)))
	(list* new-class (class-precedence-list class)))
      (multiple-value-bind (class-list precedence-alist)
	  (walk-supers nil superclasses nil nil)
	  (do ((cpl (list new-class)))
	      ((null class-list)
	       (if precedence-alist (cycle-error new-class) (nreverse cpl)))
	    (let* ((candidates (free-elements class-list precedence-alist))
		   (next (next-element candidates cpl)))
	      (unless next
		(cycle-error new-class))
	      (setf precedence-alist (delete next precedence-alist :key #'car)
		    class-list (delete next class-list)
		    cpl (cons next cpl))))))))

(defun compute-clos-class-precedence-list (new-class superclasses)
  (labels ((walk-supers (superclasses)
	     ;; Creates two lists, one with all the superclasses of a class to be created,
	     ;; and a second list with lists (c1 c2 c3 ... cn) that represent a partial
	     ;; ordering of the classes (c1 > c2), (c2 > c3), etc."
	     (let ((class-list '())
		   (precedence-lists (list superclasses)))
	       (loop (unless superclasses
		       (return (values class-list precedence-lists)))
		  (let ((next-class (pop superclasses)))
		    (unless (member next-class class-list :test 'eql)
		      (let ((more-classes (slot-value next-class 'direct-superclasses)))
			(setf class-list (list* next-class class-list)
			      precedence-lists (list* (list* next-class more-classes)
						      precedence-lists)
			      superclasses (append more-classes superclasses))))))))
	   (cycle-error (class)
	     (error "A cycle has been detected in the class precedence list for ~A."
		    (class-name class)))
	   (has-no-precedent (class precedence-lists)
	     ;; Check if CLASS is not preceded by any other class in the partial order.
	     (dolist (partial-order precedence-lists t)
	       (when (member class (rest partial-order) :test 'eql)
		 (return nil))))
	   (free-elements (class-list precedence-lists)
	     ;; Return classes that are not preceded by anyone
	     (let ((output '()))
	       (dolist (class class-list)
		 (when (has-no-precedent class precedence-lists)
		   (push class output)))
	       output))
	   (next-element (free-list cpl)
	     ;; Compute the next element that we will add to the class precedence list.
	     (if (or (null cpl) (endp free-list) (endp (rest free-list)))
		 (first free-list)
		 (dolist (i cpl nil)
		   (dolist (j (slot-value i 'direct-superclasses))
		     (when (member j free-list :test 'eql)
		       (return-from next-element j))))))
	   (delete-class (class precedence-lists)
	     (do ((l precedence-lists (rest l)))
		 ((null l)
		  (delete nil precedence-lists))
	       (let ((one-list (first l)))
		 (when (eq class (first one-list))
		   (setf (first l) (rest one-list)))))))
    (cond ((null superclasses)
	   (list new-class))
	  ((endp (rest superclasses))
	   (let ((class (first superclasses)))
	     (list* new-class (slot-value class 'precedence-list))))
	  (t
	   (multiple-value-bind (class-list precedence-lists)
	       (walk-supers superclasses)
	     (do ((cpl (list new-class)))
		 ((null class-list)
		  (if precedence-lists (cycle-error new-class) (nreverse cpl)))
	       (let* ((candidates (free-elements class-list precedence-lists))
		      (next (next-element candidates cpl)))
		 (unless next
		   (cycle-error new-class))
		 (setf precedence-lists (delete-class next precedence-lists)
		       class-list (delete next class-list)
		       cpl (cons next cpl)))))))))

;;; ----------------------------------------------------------------------
