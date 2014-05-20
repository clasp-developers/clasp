

(print (let ((a 1)) (funcall #'(lambda (&aux (y a)) y))))
