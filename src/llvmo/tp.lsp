
(defparameter *a* nil)

(defun p (n)
  (push n *a*))

(let ((*a* nil))
  (p 1)
  (p 2)
  (print *a*))
