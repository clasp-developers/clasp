(let ((reqs '(1 2 3 4 5)))
  (BLOCK NIL
    (co:dbgi 1)
    (print 1)
    (LET ((cur (CDR REQS)))
      (co:dbgi 2)
      (print 2)
      (TAGBODY
	 (co:dbgi 3)
	 (print 3)
       start
	 (co:dbgi 4)
	 (print 4)
	 (if (ENDP cur)
	     nil
	     (progn
	       (co:dbgi 5)
	       (print 5)
	       (LET* ((V (CAR cur)))
		 (co:dbgi 6)
		 (print 6)
		 (SETQ cur (CDR cur))
		 (TAGBODY
		    (co:dbgi 7)
		    (print 7)
		    ))
	       (co:dbgi 8)
	       (print 8)
	       (GO start)
	       )
	     )
	 ))
    NIL)
  )
