
(defclass foo () ((fooa :initarg :fooa :accessor fooa)))

(defparameter *a* (make-instance 'foo :fooa 1234))

(defun accessn (n o) (dotimes (i n) (fooa o) (fooa o) (fooa o) (fooa o) (fooa o)))
