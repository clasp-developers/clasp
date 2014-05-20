
(let ((ll '(1 2 3 4 5)))
  (dolist (num ll)

    ( MULTIPLE-VALUE-CALL
	( FUNCTION ( LAMBDA ( &OPTIONAL (A  ) (B  ) &REST rest-var )
		     (BFORMAT T "num[%d]  a,b = %d,%d\n" NUM A B  ) ) )
      (LET ((X (+ 10 NUM ))
	    (Y (+ 20 NUM )))
	(BFORMAT T "Called with num[%d] x[%d] y[%d]\n" NUM X Y  )
	(VALUES X Y  )  )
      )
    )
  )


#|
(let ((ll '(1 2 3 4 5)))
  (dolist (num ll)
    (multiple-value-bind (a b)
	(let ((x (+ 10 num))
	      (y (+ 20 num)))
	  (bformat t "Called with num[%d] x[%d] y[%d]\n" num x y)
	  (values x y))
      (bformat t "num[%d]  a,b = %d,%d\n" num a b))))
|#



(bformat t "expanded macro = \n%s\n"
	 (macroexpand '(multiple-value-bind (a b)
			(let ((x (+ 10 num))
			      (y (+ 20 num)))
			  (bformat t "Called with num[%d] x[%d] y[%d]\n" num x y)
			  (values x y))
			(bformat t "num[%d]  a,b = %d,%d\n" num a b))))


