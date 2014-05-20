 (restart-case
     (handler-bind ((error #'(lambda (c)
                             (declare (ignore condition))
                             (invoke-restart 'my-restart 7))))
       (error "Foo."))
   (my-restart (&optional v) v))



(handler-case (error "Bomb") (error (c) (print (list "Error " c))))


( BLOCK #:G2859
  ( LET ( ( #:G2860 nil ) )
    ( DECLARE ( IGNORABLE #:G2860 ) )
    ( TAGBODY
       ( HANDLER-BIND
	   ((ERROR
	     ( FUNCTION ( LAMBDA ( TEMP )
			  ( DECLARE ( IGNORABLE TEMP ) )
			  ( SETQ #:G2860 TEMP )
			  ( GO TAG ) ) )
	      )  )
	 ( RETURN-FROM #:G2859 ( ERROR "Bomb" ) )
	 )
       TAG
       ( RETURN-FROM #:G2859 ( LET ( ( C #:G2860 ) )
			       ( PRINT ( LIST "Error " C ) )
			       )
		     )
       )
    )
  )
