(in-package #:cross-clasp.clasp.clos)

;;;; identical to the runtime code, except it presumes everything is a
;;;; compiler-class and not forward referenced.

(defun compute-class-precedence-list (new-class superclasses)
  (labels ((walk-supers (superclasses)
	     ;; Creates two lists, one with all the superclasses of a class to be created,
	     ;; and a second list with lists (c1 c2 c3 ... cn) that represent a partial
	     ;; ordering of the classes (c1 > c2), (c2 > c3), etc."
	     (let ((class-list '())
		   (precedence-lists (list superclasses)))
	       (loop (unless superclasses
		       (return (values class-list precedence-lists)))
                     (let ((next-class (pop superclasses)))
                       #+(or)
                       (when (forward-referenced-class-p next-class)
                         (error "Cannot compute class precedence list for forward-referenced class ~A."
                                (class-name next-class)))
                       (unless (member next-class class-list :test 'eql)
                         (let ((more-classes
                                 (mop:class-direct-superclasses next-class)))
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
		   (dolist (j (mop:class-direct-superclasses i))
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
          #+(or) ; we don't keep CPLs around, but maybe we ought to?
	  ((and (endp (rest superclasses))
             #+(or)
             (not (forward-referenced-class-p (first superclasses))))
           (list* new-class (slot-value (first superclasses) 'precedence-list)))
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
