


defMacro zz ( a b ) 
    println [ "Macro zz started" ]
    return `(progn 
    		(println ["evaluated args = %s %s" % (repr (quote ,a)) (repr (quote ,b)) ])
    		(println ["evaluated args = %s %s" % (repr ,a) (repr ,b) ])
	    )

println [ "Defining globals" ]
global red "Red"
global blue "Blue"

println [ "invoking macro"]
(zz red blue)
