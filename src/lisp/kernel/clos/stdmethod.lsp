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

(defmethod shared-initialize ((method standard-method) slot-names &rest initargs
			      &key (specializers nil spec-supplied-p)
			      (lambda-list nil lambda-supplied-p)
			      generic-function)
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
       do (error "Object ~A is not a valid specializer" s)))
  (setf method
        (call-next-method)
	(values (method-keywords method) (method-allows-other-keys-p method))
        (compute-method-keywords (method-lambda-list method)))
  method)

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
                       ;; KLUDGE to avoid compiler macro, see fixup.lsp
                       (apply #'make-instance 'eql-specializer :object object nil)
                       #+(or)
                       (make-instance 'eql-specializer :object object)))))
      #+threads (mp:with-lock (*eql-specializer-lock*) (get-it))
      #-threads (get-it))))

(defmethod add-direct-method ((spec specializer) (method method))
  (pushnew method (specializer-direct-methods spec))
  (let ((gf (method-generic-function method)))
    (pushnew gf (specializer-direct-generic-functions spec)))
  (values))

(defmethod remove-direct-method ((spec specializer) (method method))
  (let* ((gf (method-generic-function method))
	 (methods (delete method (specializer-direct-methods spec))))
    (setf (specializer-direct-methods spec) methods)
    (unless (find gf methods :key #'method-generic-function)
      (setf (specializer-direct-generic-functions spec)
	    (delete gf (specializer-direct-generic-functions spec))))
    (values)))

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

