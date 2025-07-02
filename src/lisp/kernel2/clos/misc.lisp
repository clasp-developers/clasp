(in-package "CLOS")

;;; find-method-combination

(defun make-method-combination (name compiler options)
  (early-make-instance method-combination
                       :name name
                       :compiler compiler
                       :options options))

(defgeneric find-method-combination (generic-function
                                     method-combination-type-name
                                     method-combination-options))

(defmethod find-method-combination ((gf standard-generic-function)
                                    name options)
  (declare (ignore gf))
  (make-method-combination name
			   (or (search-method-combination name)
                               (error "~A does not name a method combination"
                                      name))
			   options))

;;; no-etc-method

(defgeneric no-applicable-method (generic-function &rest arguments))

(defmethod no-applicable-method (gf &rest args)
  (error 'no-applicable-method-error :generic-function gf :arguments args))

;;; FIXME: See method.lisp: This is not actually used normally.
(defgeneric no-next-method (generic-function method &rest arguments))

(defmethod no-next-method ((gf standard-generic-function) (method standard-method)
                           &rest args)
  (declare (ignore gf))
  (error "In method ~A~%No next method given arguments ~A" method args))

;;; and a few other method combination things

(defun method-combination-error (format-control &rest args)
  ;; FIXME! We should emit a more detailed error!
  (error "Method-combination error:~%~S"
	 (apply #'format nil format-control args)))

(defun invalid-method-error (method format-control &rest args)
  (error "Invalid method error for ~A~%~S"
	 method
	 (apply #'format nil format-control args)))

;;; generic-function-name

(defgeneric generic-function-name (generic-function))
(defmethod generic-function-name ((gf standard-generic-function))
  (core:function-name gf))
