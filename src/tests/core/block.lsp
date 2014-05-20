(test (not
       (block c
	 (block b
	   (block a
	     (return-from a nil)
	     )
	   )
	 )))

(test (eq 'test
	  (block c
	    (block b
	      (let ((val "Hi there"))
		(block a
		  (apply #'(lambda ()
			     (print val)
			     (return-from b 'test)) nil)
		  (print "Dont reach here"))
		(print "end of b")))
	    (print "end of c"))))


#|(progn
  (block vvv
    (block www
      (block xxx
	(print "Hello")
	(apply #'(lambda (x)
		   (print "About to return-from www")
		   (return-from www 'test)) '(1))
;;	(return-from www 'test)
	(print "End of block xxx"))
      (print "End of block www"))
    (print "end of block vvv"))
  (print "done"))
|#
