;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 2009, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;
;;;; CMPTYPE-PROP -- Type propagation basic routines and database
;;;;

(in-package #-new-cmp "COMPILER" #+new-cmp "C-TYPES")

(defun infer-arg-and-return-types (fname forms &optional (env *cmp-env*))
  (let ((found (gethash fname *p0-dispatch-table*))
        arg-types
        (return-type '(VALUES &REST T)))
    (cond (found
           (multiple-value-setq (arg-types return-type)
             (apply found fname (mapcar #'location-primary-type forms))))
          ((multiple-value-setq (arg-types found)
             (get-arg-types fname env))
           (setf return-type (or (get-return-type fname) return-type))))
    (values arg-types return-type found)))

(defun enforce-types (fname arg-types arguments)
  (do* ((types arg-types (rest types))
        (args arguments (rest args))
        (i 1 (1+ i))
        (in-optionals nil))
       ((endp types)
        (when types
          (cmpwarn "Too many arguments passed to ~A" fname)))
    (let ((expected-type (first types)))
      (cond ((member expected-type '(* &rest &key &allow-other-keys) :test #'eq)
             (return))
            ((eq expected-type '&optional)
             (when in-optionals
               (cmpwarn "Syntax error in type proclamation for function ~A.~&~A"
                        fname arg-types))
             (setf in-optionals t
                   types (rest types)))
            ((endp args)
             (unless in-optionals
               (cmpwarn "Too few arguments for proclaimed function ~A" fname))
             (return))
            (t
             (let* ((value (first args))
                    (actual-type (location-primary-type value))
                    (intersection (type-and actual-type expected-type)))
               (unless intersection
                 (cmpwarn-style "The argument ~d of function ~a has type~&~4T~A~&instead of expected~&~4T~A"
                          i fname actual-type expected-type))
               #-new-cmp
               (when (zerop (cmp-env-optimization 'safety))
                 (setf (c1form-type value) intersection))))))))

(defun propagate-types (fname forms)
  (multiple-value-bind (arg-types return-type found)
      (infer-arg-and-return-types fname forms)
    (when found
      (enforce-types fname arg-types forms))
    return-type))

(defmacro def-type-propagator (fname lambda-list &body body)
  (unless (member '&rest lambda-list)
    (let ((var (gensym)))
      (setf lambda-list (append lambda-list (list '&rest var))
            body (list* `(declare (ignorable ,var)) body))))
  `(setf (gethash ',fname *p0-dispatch-table*)
         #'(lambda ,lambda-list (declare (ignorable ,(first lambda-list))) ,@body)))

(defun copy-type-propagator (orig dest-list)
  (loop with function = (gethash orig *p0-dispatch-table*)
     for name in dest-list
     when function
     do (setf (gethash name *p0-dispatch-table*) function)))


