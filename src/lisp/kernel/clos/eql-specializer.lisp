(in-package #:clos)

(defclass eql-specializer (specializer)
  ((object :initarg :object :reader eql-specializer-object)))

(defvar *eql-specializer-lock* (mp:make-lock :name 'eql-specializer))

(defvar *eql-specializer-hash*
  (make-hash-table :size 128 :test #'eql))

(defun intern-eql-specializer (object)
  (mp:with-lock (*eql-specializer-lock*)
    (or (gethash object *eql-specializer-hash*)
      (setf (gethash object *eql-specializer-hash*)
            (early-make-instance eql-specializer :object object)))))

;;; FIXME: Move?
(defgeneric add-direct-method (specializer method))
(defgeneric remove-direct-method (specializer method))
(defgeneric specializer-direct-generic-functions (specializer))

(defmethod add-direct-method ((spec specializer) (method method))
  (pushnew method (%specializer-direct-methods spec))
  (values))

(defmethod remove-direct-method ((spec specializer) (method method))
  (setf (%specializer-direct-methods spec)
        (delete method (specializer-direct-methods spec)))
  (values))

(defmethod add-direct-method ((spec specializer) (method method))
  (pushnew method (%specializer-direct-methods spec))
  (values))

(defmethod remove-direct-method ((spec eql-specializer) (method method))
  (mp:with-lock (*eql-specializer-lock*)
    (call-next-method)
    (unless (specializer-direct-methods spec)
      (remhash spec *eql-specializer-hash*)))
  (values))

(defmethod specializer-direct-generic-functions ((specializer specializer))
  (loop with result = nil
        for method in (specializer-direct-methods specializer)
        for gf = (method-generic-function method)
        do (pushnew gf result :test #'eq)
        finally (return result)))
