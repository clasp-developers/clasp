(defun a (&key y)
       (print y))

(defun foo (&key (x 1) (y 2) (z 3))
  (list x y z))
