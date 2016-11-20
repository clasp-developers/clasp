
(eval-when (:compile-toplevel :load-toplevel)
  (defclass foo ()
    ((thing :initarg :thing :accessor thing)))

  (defmethod make-load-form ((self foo) &optional environment)
    (values
     `(make-instance ',(class-of self))
     `(setf (thing ,self) ,self)))

  (defparameter *foo* (make-instance 'foo))
  (setf (thing *foo*) *foo*))

(defun get-foo ()
  #.*foo*)
