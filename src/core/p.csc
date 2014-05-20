usePackage "sys"
defClass A (:a :b)
    method test ( self ) 
        println "Entered a"


defClass B (:c :d)


defClass C ( )

defClass D ( )


# println [ "Package sys symbols = %s" % ( repr (allSymbols (getPackage "sys") ) ) ]

setq a Matter

println [ "Matter isAssignableTo Object = %d" % (isAssignableTo Matter Object ) ]





println "Hi"
println "This is a test"
println "Ending now"
(println "In parenthesis")

setq t1 1
println [ "t1=%d" % t1 ]
dumpClasses

setq g (A)
describe A
# [ g :set_a 20 ]

