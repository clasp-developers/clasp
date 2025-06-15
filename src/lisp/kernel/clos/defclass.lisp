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
      (push `(list ',slot-name ',initform (lambda ()
                                            (declare (core:lambda-name parse-default-initargs.lambda))
                                            ,initform))
	    output-list))))

(defun gen-note-accessors (slots)
  (flet ((gen-note (name)
           `(cmp::register-global-function-def 'defmethod ',name)))
    (loop with result = nil
          for slot in slots
          when (consp slot)
            do (loop for (key value) on (rest slot) by #'cddr
                     do (case key
                          ((:reader :writer)
                           (push (gen-note value) result))
                          ((:accessor)
                           (push (gen-note value) result)
                           (push (gen-note `(setf ,value)) result))))
          finally (return result))))

(defmacro defclass (name superclasses slots &rest options)
  (let (;; Throw in source info if there is any.
        (options (if core:*current-source-pos-info*
                     (list* (cons :source-position core:*current-source-pos-info*) options)
                     options)))
    (unless (and (listp superclasses) (listp slots))
      (si::simple-program-error "Illegal defclass form: superclasses and slots should be lists"))
    (unless (and (symbolp name) (every #'symbolp superclasses))
      (si::simple-program-error "Illegal defclass form: superclasses and class name are not valid"))
    (let ((parsed-slots (parse-slots slots))
          (processed-class-options (process-class-options options)))
      `(progn
         (eval-when (:compile-toplevel)
           ,@(gen-note-accessors slots)
           (setf (core::class-info ',name) t))
         (eval-when (:load-toplevel :execute)
           (ensure-class ',name :direct-superclasses ',superclasses
                                :direct-slots ,parsed-slots
                                ,@processed-class-options))))))

(defun process-class-options (class-args)
  (let ((options '())
	(processed-options '()))
    (dolist (option class-args options)
      (unless (consp option)
        (si:simple-program-error
         "Option ~s for DEFCLASS has invalid syntax: not a cons" option))
      (let ((option-name (first option))
	    option-value)
        (unless (symbolp option-name)
          (si:simple-program-error
           "~s is not a valid DEFCLASS option: not a symbol" option-name))
	(if (member option-name processed-options)
	    (si:simple-program-error
	     "Option ~s for DEFCLASS specified more than once"
	     option-name)
	    (push option-name processed-options))
	(setq option-value
	      (case option-name
		((:metaclass :documentation)
		 (ext:maybe-quote (second option)))
;;                ((:source-position) (second option)) ; see FIXME above
		(:default-initargs
		 (setf option-name :direct-default-initargs)
		 (parse-default-initargs (rest option)))
		(otherwise
		 (ext:maybe-quote (rest option))))
	      options (list* (ext:maybe-quote option-name)
			     option-value options))))))

;;; ----------------------------------------------------------------------
;;; ENSURE-CLASS
;;;
(defun ensure-class (name &rest initargs)
  (apply #'ensure-class-using-class
         (let ((class (and name
                           (find-class name nil))))
           ;; Only classes which have a PROPER name are redefined. If a class
           ;; with the same name is registered, but the name of the class does not
           ;; correspond to the registered name, a new class is returned.
           ;; [Hyperspec 7.7 for DEFCLASS]
           (when (and class (eq name (class-name class)))
             class))
         name initargs))

#+(or) ;#+cross
(eval-when (compile)
  (defun ensure-class (name &rest initargs)
    (warn "Ignoring definition for class ~S" name)))

