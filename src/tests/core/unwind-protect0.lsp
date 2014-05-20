(progn
  (block a
    (print "top a")
    (unwind-protect
	 (block b
	   (print "top b")
	   (return-from a 'nil)
	   (print "NEVER HIT"))
      (print "unwind")
      )
    (print "Bottom a")
    )
  (print "done"))
