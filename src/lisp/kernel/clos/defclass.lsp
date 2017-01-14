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
;;; DEFCLASS

(defun parse-default-initargs (default-initargs)
  (declare (si::c-local))
  (do* ((output-list nil)
	(scan default-initargs (cddr scan))
	(already-supplied '()))
       ((endp scan) `(list ,@(nreverse output-list)))
    (when (endp (rest scan))
      (si::simple-program-error "Wrong number of elements in :DEFAULT-INITARGS option."))
    (let ((slot-name (first scan))
	  (initform (second scan)))
      (if (member slot-name already-supplied)
	  (si::simple-program-error "~S is duplicated in :DEFAULT-INITARGS form ~S"
				    slot-name default-initargs)
	  (push slot-name already-supplied))
      (push `(list ',slot-name ',initform (lambda () ,initform))
	    output-list))))

(defmacro defclass (&whole form &rest args)
  (unless (>= (length args) 3)
    (si::simple-program-error "Illegal defclass form: the class name, the superclasses and the slots should always be provided"))
  (let* ((name (pop args))
	 (superclasses (pop args))
	 (slots (pop args))
	 (options args))
    (unless (and (listp superclasses) (listp slots))
      (si::simple-program-error "Illegal defclass form: superclasses and slots should be lists"))
    (unless (and (symbolp name) (every #'symbolp superclasses))
      (si::simple-program-error "Illegal defclass form: superclasses and class name are not valid"))
    `(eval-when (compile load eval)
       ,(ext:register-with-pde
	 form
	 `(load-defclass ',name ',superclasses
                         ,(parse-slots slots)
			 ,(process-class-options options))))))

(defun process-class-options (class-args)
  (let ((options '())
	(processed-options '()))
    (dolist (option class-args)
      (let ((option-name (first option))
	    option-value)
	(if (member option-name processed-options)
	    (si:simple-program-error
	     "Option ~s for DEFCLASS specified more than once"
	     option-name)
	    (push option-name processed-options))
	(setq option-value
	      (case option-name
		((:metaclass :documentation)
		 (ext:maybe-quote (second option)))
		(:default-initargs
		 (setf option-name :direct-default-initargs)
		 (parse-default-initargs (rest option)))
		(otherwise
		 (ext:maybe-quote (rest option))))
	      options (list* (ext:maybe-quote option-name)
			     option-value options))))
    (and options `(list ,@options))))
  
(defun load-defclass (name superclasses slot-definitions options)
  (apply #'ensure-class name :direct-superclasses superclasses
                             :direct-slots slot-definitions options))

;;; ----------------------------------------------------------------------
;;; ENSURE-CLASS
;;;
(defun ensure-class (name &rest initargs)
  (apply #'ensure-class-using-class
         (let ((class (and name
                           (find-class name nil))))
           ;; Only classes which have a PROPER name are redefined. If a class
           ;; with the same name is register, but the name of the class does not
           ;; correspond to the registered name, a new class is returned.
           ;; [Hyperspec 7.7 for DEFCLASS]
           (when (and class (eq name (class-name class)))
             class))
         name initargs))

#+(or) ;#+cross
(eval-when (compile)
  (defun ensure-class (name &rest initargs)
    (warn "Ignoring definition for class ~S" name)))

