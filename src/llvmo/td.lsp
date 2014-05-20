
(defmacro dumpenv ( &environment env )
  (print env)
  nil)

(let ((z 1))
  (labels ((testy (y) (print y))
	   (testx (x) (print x)))
    (dumpenv)))
