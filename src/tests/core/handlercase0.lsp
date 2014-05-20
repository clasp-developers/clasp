;; (handler-case (error "Bomb") (error (c) (print (list "Error " c))))


( BLOCK btag
  ( LET ( ( xxx nil ) )
    ( DECLARE ( IGNORABLE xxx ) )
    ( TAGBODY
       ( LET ( ( *HANDLER-CLUSTERS* ( CONS ( LIST ( CONS ( QUOTE ERROR ) ( FUNCTION ( LAMBDA ( TEMP )
										      ( DECLARE ( IGNORABLE TEMP ) )
										      ( SETQ XXX TEMP )
										      ( GO TAG ) ) ) ) )
					   *HANDLER-CLUSTERS* ) ) )
	 ( RETURN-FROM BTAG ( ERROR "Bomb" ) ) )
     TAG
       ( RETURN-FROM btag ( LET ( ( C xxx ) )
			    ( PRINT ( LIST "Error " C ) )
			    )
		     )
       )
    )
  )


