
defClass A () ()
    method a (self)
        println "a"

;
; Make sure the B is a subclass of A
;
defClass B A (_bval)
    method __init__ (self)
        setq _bval "initialValue"
    method b (self)
        println "b"
	setq _bval 1

defClass C B ()
    method c (self)
        println "c"

println "---------    Describing class A"
describe A
println "-------------------------------------------------------"
println "---------    Describing class B"
describe B
println "-------------------------------------------------------"
println "---------    Describing class C"
describe C

println "Creating b"

local b (C)

println "Class of b"
describe (class b)



println "calling [b b]"
[b b]
println "calling [b a]"
[b a]
[b c]


println "Defining class M"
defClass M Atom ()
    method aaa (self)
        println "aaa"



describe M
local m (M)
[m aaa ]
[m setCharge 1.0 ]
println [ "m charge = %lf" % [ m getCharge ] ]

saveArchive m "_m.cxml"

saveArchive b "_b.cxml"


local lm (loadArchive "_m.cxml")
local lb (loadArchive "_b.cxml")
saveArchive lm "_lm.cxml"
saveArchive lb "_lb.cxbl"
