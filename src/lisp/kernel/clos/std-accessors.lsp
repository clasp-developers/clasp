
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.o
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
;;; ACCESSOR / READER / WRITER METHOD CREATION
;;;
;;; The following code creates unoptimized versions of the
;;; slot accessors defined for a class. They are designed so that at least
;;; some varieties work at boot time, but are called in normal system operation.
;;;
;;; There is not much optimization because fastgf can usually inline accesses anyway.
;;; The functions created here by std-class-accessors are set as the method functions
;;; of standard reader and writer methods, but these functions are only used
;;; if fastgf cannot optimize the access, which will only happen if there are
;;; additional user methods. In which case we have to go slowly anyway.
;;; 

(defun std-class-accessors (slot-name)
  (values (make-%method-function-fast
           #'(lambda (self)
               (declare (core:lambda-name std-class-accessors.reader.lambda))
               (slot-value self slot-name)))
          (make-%method-function-fast
           #'(lambda (new-value self)
               (declare (core:lambda-name std-class-accessors.writer.lambda))
               (setf (slot-value self slot-name) new-value)))))

(defun safe-add-method (name method)
  ;; Adds a method to a function which might have been previously defined
  ;; as non-generic, without breaking the function
  (cond ((or *clos-booted*
	     (not (fboundp name))
	     (si::instancep (fdefinition name)))
	 (add-method (ensure-generic-function name) method))
	(t
         ;; NOTE: Using fmakunbound means there will be a problem if
         ;; NAME is ADD-METHOD or ENSURE-GENERIC-FUNCTION.
         (fmakunbound name)
         (add-method (ensure-generic-function name) method))))

(defun std-class-generate-accessors (standard-class)
  ;;
  ;; The accessors are closures, which are generated every time the
  ;; slots of the class change. The accessors are safe: they check that
  ;; the slot is bound after retreiving the value, and they may take
  ;; the liberty of using SI:INSTANCE-REF because they know the class of
  ;; the instance.
  ;;
  (dolist (slotd (slot-value standard-class 'direct-slots))
    (with-slots ((name name) (allocation allocation) (location location)
		 (readers readers) (writers writers))
	slotd
      (multiple-value-bind (reader writer) (std-class-accessors name)
	(let* ((options (list* :slot-definition slotd
                               :leaf-method-p t
                               (if (boundp '*early-methods*)
                                   nil
                                   (list
                                    :source-position (class-source-position
                                                      standard-class)))))
	       (reader-args (list* :function reader
				   :generic-function nil
				   :qualifiers nil
				   :lambda-list '(object)
				   :specializers `(,standard-class)
				   options))
	       (reader-class (if (boundp '*early-methods*)
				 'standard-reader-method
				 (apply #'reader-method-class standard-class slotd
					reader-args)))
	       (writer-args (list* :function writer
				   :generic-function nil
				   :qualifiers nil
				   :lambda-list '(value object)
				   :specializers `(,(find-class t) ,standard-class)
				   options))
	       (writer-class (if (boundp '*early-methods*)
				 'standard-writer-method
				 (apply #'writer-method-class standard-class slotd
					writer-args))))
	  (dolist (fname readers)
	    (let ((method (make-method reader-class nil `(,standard-class) '(object)
				       reader
				       options)))
	      (safe-add-method fname method)
	      ;; This is redundant, but we need it at boot time because
	      ;; the early MAKE-METHOD does not use the options field.
	      (unless *clos-booted*
		(setf (slot-value method 'slot-definition) slotd))))
	  (dolist (fname writers)
	    (let ((method (make-method writer-class nil
				       `(,(find-class t) ,standard-class) '(value object)
				       writer
				       options)))
	      (safe-add-method fname method)
	      ;; This is redundant, but we need it at boot time because
	      ;; the early MAKE-METHOD does not use the options field.
	      (unless *clos-booted*
		(setf (slot-value method 'slot-definition) slotd)))))))))

(defun reader-closure (index)
  (lambda (object)
    (declare (core:lambda-name reader-closure.lambda))
    (si::instance-ref object index)))

(defun writer-closure (index)
  (lambda (value object)
    (declare (core:lambda-name writer-closure.lambda))
    (si::instance-set object index value)))

;;; Loop through the entire class hierarchy making accessors.
;;; Some classes may be reachable from multiple superclasses, so we have to
;;; track which ones we've already generated accessors for (the early add-method
;;; never replaces old methods, even with the same specializers and qualifiers!)

(let ((seen nil))
  (labels ((generate-accessors (class)
             (cond ((find class seen) (return-from generate-accessors))
                   ((and (typep class 'std-class)
                         #+(or)(not (member (slot-value class 'name)
                                            '(slot-definition
                                              direct-slot-definition
                                              effective-slot-definition
                                              standard-slot-definition
                                              standard-direct-slot-definition
                                              standard-effective-slot-definition))))
                    (std-class-generate-accessors class))
                   ((typep class 'core:derivable-cxx-class)
                    (std-class-generate-accessors class))
                   (t
                    (loop for slotd in (slot-value class 'slots)
                          for index = (slot-value slotd 'location)
                          do (loop for reader in (slot-value slotd 'readers)
                                   do (if (fboundp reader)
                                        (setf (fdefinition reader) (reader-closure index))))
                          do (loop for writer in (slot-value slotd 'writers)
                                   do (if (fboundp writer)
                                        (setf (fdefinition writer) (writer-closure index)))))))
             (push class seen)
             (mapc #'generate-accessors (slot-value class 'direct-subclasses))))
    (generate-accessors +the-t-class+)))

;;; Readers for effective accessor methods

(macrolet ((defproxies (reader)
             `(progn
                (defmethod ,reader ((method effective-reader-method))
                  (with-early-accessors (+effective-accessor-method-slots+)
                    (,reader (effective-accessor-method-original method))))
                (defmethod ,reader ((method effective-writer-method))
                  (with-early-accessors (+effective-accessor-method-slots+)
                    (,reader (effective-accessor-method-original method))))))
           (def ()
             `(progn
                ,@(loop for (name . plist) in +standard-accessor-method-slots+
                        ;; See KLUDGE in WITH-EARLY-ACCESSORS
                        for reader = (or (getf plist :reader)
                                         (getf plist :accessor))
                        ;; effective accessors have their own function slot.
                        when (and reader (not (eq reader 'method-function)))
                          collect `(defproxies ,reader)))))
  (def))
