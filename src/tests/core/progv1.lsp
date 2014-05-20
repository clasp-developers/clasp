( LET ( ( xxx ( EQUAL ( LET ( ( *X* 100 ) )
			( PROGV ( QUOTE ( *X* ) )
			    ( QUOTE ( 4 ) )
			  ( LIST *X* ( SYMBOL-VALUE ( QUOTE *X* ) ) ) ) ) ( QUOTE ( 100 4 ) ) ) ) )
  ( IF xxx
       ( IF nil ( BFORMAT T "PASSED %s\n" nil ) nil )
       ( PROGN
	 ( BFORMAT T "FAILED " )
	 ( IF nil ( BFORMAT T "%s" nil ) ) ( BFORMAT T " -> %s\n" ( QUOTE ( EQUAL ( LET ( ( *X* 100 ) ) ( PROGV ( QUOTE ( *X* ) ) ( QUOTE ( 4 ) ) ( LIST *X* ( SYMBOL-VALUE ( QUOTE *X* ) ) ) ) ) ( QUOTE ( 100 4 ) ) ) ) ) )
       )
  xxx )
