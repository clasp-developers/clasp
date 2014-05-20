
(defun z (u v)
  (print "z1")
  (error "Test")
  (print "z2"))

(defun y (m)
  (print "y1")
  (z m 2)
  (print "y2"))

(defun x ()
  (print "x1")
  (let ((k 10))
    (y 1))
  (print "x2"))
