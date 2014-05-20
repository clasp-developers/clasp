(let ((a (block a
	   (block b
	     (print "top a")
	     (print "a2")
	     (funcall #'(lambda () (bformat t "In the lambda\n")
				(return-from a "Hi there")))
	     (print "a3")
	     )
	   )))
  (bformat t "returned --> %s\n" a))
