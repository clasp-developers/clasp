(defun return-one ()
  :return-one)

(multiple-value-bind (x y)
    (return-one)
  (print (list x y)))
