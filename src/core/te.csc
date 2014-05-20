
defun b ()
    "b function"
    raise (LispError "testing raise")


defun a ()
    "a function"
    b




; a


;handlerCase
;    progn
;	println "About to raise error"
;	raise (LispError "raised in handlerCase")
;    LispError (se)
;	progn
;	    println [ "Caught LispError in inner handlerCase: %s" % [ se message ] ]
;	    raise (LispError "raise in handler")



handlerCase
    handlerCase
	progn
	    println "About to raise error"
	    raise (LispError "raised in handlerCase")
	LispError (se)
	    progn
		println [ "Caught LispError in inner handlerCase: %s" % [ se message ] ]
		raise (FileNotFoundException "raise in handler")
    FileNotFoundException (se)
        println "Caught FileNotFoundException in outer handlerCase"
    LispError (se)
        println [ "Caught LispError in outer handlerCase: %s" % [ se message ] ]

println "Done"
