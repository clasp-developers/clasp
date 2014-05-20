
(progn
  (block xxx
    (block yyy
      (apply (function (lambda (x)
	       (print "In lambda")
	       (return-from xxx)
	       (print "XXX Not reached"))) '(1))
      (print "XXX Not reached")
      )
    (print "XXX not reached")
    )
  (print "done"))

#|
(macroexpand '(multiple-value-bind (x y) (values 1 2) (return-from xxxx) (print "Nope")))
|#
#|
( FUNCTION 
  ( LAMBDA-BLOCK PUSH ( G52719 ENV &AUX (G52720 ( CDR ( THE CONS G52719 ) )  )
			       (ITEM ( PROGN
				       (IF ( NULL G52720 )
					   ( DM-TOO-FEW-ARGUMENTS G52719 ) )
				       ( PROG1
					   ( CAR ( THE CONS G52720 ) )
					 ( SETQ G52720 ( CDR ( THE CONS G52720 ) ) ) ) ) )
			       (PLACE ( PROGN
					( IF ( NULL G52720 ) ( DM-TOO-FEW-ARGUMENTS G52719 ) )
					( PROG1
					    ( CAR ( THE CONS G52720 ) )
					  ( SETQ G52720 ( CDR ( THE CONS G52720 ) ) ) ) )  )
			       )
		 ( DECLARE ( NOTINLINE MAPCAR ) )
		 ( IF G52720 ( DM-TOO-MANY-ARGUMENTS G52719 ) )
		 ( MULTIPLE-VALUE-BIND
		       ( VARS VALS STORES STORE-FORM ACCESS-FORM )
		     ( GET-SETF-EXPANSION PLACE ENV )
		   ( WHEN ( TRIVIAL-SETF-FORM PLACE VARS STORES STORE-FORM ACCESS-FORM )
		     ( RETURN-FROM PUSH ( BACKQUOTE
					  ( SETQ ( COMMA PLACE ) ( CONS ( COMMA ITEM ) ( COMMA PLACE ) ) ) ) ) )
		   ( UNLESS ( CONSTANTP ITEM )
		     ( SETQ VALS ( CONS ITEM VALS ) ITEM ( GENSYM "pushval" ) VARS ( CONS ITEM VARS ) ) )
 ) ) )
|#
