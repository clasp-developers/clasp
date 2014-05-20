
(defMacro ad ( str &rest body) 
    (print (% "str = %s" str ))
    (print (% "body = %s" (repr body) ))
    `(progn ,@body))


(ad "test" (ad (print "hi") (print "there")))
