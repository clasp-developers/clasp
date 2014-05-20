(defparameter *a* (make-array '(2 3 4)))
(dotimes (i (array-total-size *a*))
  (setf-row-major-aref *a* i i))
(print *a*)
