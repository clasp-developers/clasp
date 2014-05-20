

(defun make-closure (x)
  (lambda () (print x)))


(defparameter *closures* nil)

(defparameter *values* '(0 1 2 3 4 5))

(dotimes (idx 3)
  (push (make-closure idx) *closures*))

#|
(loop for index from 0 to 3
     for idx = (nth index *values*)
     do (push (make-closure idx) *closures*))
|#


(dolist (l *closures*)
  (funcall l))
