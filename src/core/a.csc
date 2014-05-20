defClass Z () ()
    method dump ( self z )
        println [ "Z:dump z = %s" % (repr z) ]
	println [ "__methodClass__ = %s" % (className sys::__methodClass__) ]

defClass A () ( _a )
    method dump ( self z )
        println [ "A:dump z = %s" % (repr z) ]
	println [ "__methodClass__ = %s" % (className sys::__methodClass__) ]

defClass B A ( _b )
    method dump ( self z )
        println [ "B:dump z = %s" % (repr z) ]
	println [ "__methodClass__ = %s" % (className sys::__methodClass__) ]
	evaluateAncestorMethod (baseClass sys::__methodClass__) dump self 100

defClass C B ( _c )
    method dump ( self z )
        println [ "C:dump z = %s" % (repr z) ]
	println [ "__methodClass__ = %s" % (className sys::__methodClass__) ]
	evaluateAncestorMethod (baseClass sys::__methodClass__) dump self 12312


println "Describing A"
describe A
println "Describing B"
describe B
setq c (C)


dump c 1
