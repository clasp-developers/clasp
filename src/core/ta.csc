defClass A () ( _a )
    method __init__ (self a1 )
        setq [ self slot _a ] a1 
	println [ "=========    In A.__init__ _a = %s" % (repr [self slot _a]) ]

    method dump (self)
        println [ "A.dump slot _a = %s" % (repr [self slot _a]) ]

defClass B A ( _b )
    method __init__ ( self z )
	callAncestorMethod
        setq [ self slot _b ] z
	println [ "=======  In B.__init__ _b to %d" % z ]
    method setb (self y)
        setq [ self slot _b ] y
    method dump ( self )
        callAncestorMethod
        println [ "dump>>self slot _b = %d" % [ self slot _b ] ]
	callAncestorMethod

describe B


println ""
println ""
println "     Creating zz"
local zz (B 1)

println "---------------------------"
dump zz
[zz setb 2 ]
println "After setb to 2"
println "---------------------------"
dump zz


