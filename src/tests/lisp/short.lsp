
(defun a (x y) (+ x y))

(defun m (x y) (* x y))

(defun s (x) (sqrt x))

(format t "Pythag = ~a~%" (s (a (m 2 2) (m 3 3))))


