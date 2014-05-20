 ( LET* (( PARAM-AND-OFFSET '( 6 . #\.) )
	 ( OFFSET ( CAR PARAM-AND-OFFSET ) )
	 ( PARAM ( CDR PARAM-AND-OFFSET ) ) )
   ( print (list "MEISTER param-and-offset: " PARAM-AND-OFFSET ))
   ( CASE PARAM ( :ARG ( OR 'next-arg 'arg-space ) )
	  ( :REMAINING ( LENGTH ARGS ) )
	  ( ( nil ) 'nil-space  )
	  ( T PARAM ) ) )

(if (member #\. '(nil)) 'res-true 'res-false)

'(nil)
nil


(case #\.
  (:arg (or 'next-arg #\space))
  (:remaining (length args))
  ( ( nil ) 'nil-space)
(print "Hello") 
