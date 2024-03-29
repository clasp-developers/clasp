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

;;;----------------------------------------------------------------------
;;; Method
;;; ----------------------------------------------------------------------

(defmethod function-keywords ((method standard-method))
  (values (method-keywords method) (method-allows-other-keys-p method)))

(defmethod shared-initialize :before
    ((method standard-method) slot-names &rest initargs
     &key (specializers nil spec-supplied-p)
       (lambda-list nil lambda-supplied-p))
  (declare (ignore initargs))
  (when slot-names
    (unless spec-supplied-p
      (error "Specializer list not supplied in method initialization"))
    (unless lambda-supplied-p
      (error "Lambda list not supplied in method initialization"))
    (unless (= (first (si::process-lambda-list lambda-list 'method))
	       (length specializers))
      (error "The list of specializers does not match the number of required arguments in the lambda list ~A"
	     lambda-list)))
  (when spec-supplied-p
    (loop for s in specializers
       unless (typep s 'specializer)
       do (error "Object ~A is not a valid specializer" s))))

(defmethod shared-initialize :after
    ((method standard-method) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (setf (values (method-keywords method) (method-allows-other-keys-p method))
        (compute-method-keywords (method-lambda-list method))))

#+threads
(defparameter *eql-specializer-lock* (mp:make-lock :name 'eql-specializer))
;; We don't want this hash-table erased when we reload during bootstrap, so defvar instead of parameter.
(defvar *eql-specializer-hash*
  (make-hash-table :size 128 :test #'eql))

#+threads
(defun intern-eql-specializer (object)
  (let ((table *eql-specializer-hash*))
    (flet ((get-it ()
             (or (gethash object table nil)
                 (setf (gethash object table)
                       ;; See note on initargs-updater in fixup.lisp.
                       (with-early-make-instance +eql-specializer-slots+
                         (e (find-class 'eql-specializer) :object object)
                         e)))))
      #+threads (mp:with-lock (*eql-specializer-lock*) (get-it))
      #-threads (get-it))))

(defmethod add-direct-method ((spec specializer) (method method))
  (pushnew method (%specializer-direct-methods spec))
  (values))

(defmethod remove-direct-method ((spec specializer) (method method))
  (setf (%specializer-direct-methods spec)
        (delete method (specializer-direct-methods spec)))
  (values))

#+threads
(defmethod remove-direct-method ((spec eql-specializer) (method method))
  (mp:with-lock (*eql-specializer-lock*)
    (call-next-method)
    (unless (specializer-direct-methods spec)
      (remhash spec *eql-specializer-hash*)))
  (values))

#-threads
(defmethod remove-direct-method ((spec eql-specializer) (method method))
    (call-next-method)
    (unless (specializer-direct-methods spec)
      (remhash spec *eql-specializer-hash*))
  (values))

(defmethod specializer-direct-generic-functions ((specializer specializer))
  (loop with result = nil
        for method in (specializer-direct-methods specializer)
        for gf = (method-generic-function method)
        do (pushnew gf result :test #'eq)
        finally (return result)))
