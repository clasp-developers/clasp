
(load-boot :cmp :interp t)
(defun a (x &optional y &key z) (list x y z))

(compile 'a)
