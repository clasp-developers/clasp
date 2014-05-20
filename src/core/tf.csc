
(labels ( ( test () 
		 (println "test function") )
	  ( test2 () 
		  (println "test 2") 
		  (test) ) )
  (test2) )
