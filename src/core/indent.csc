[ x := 10 ]

defFunction dec ( )
    [ x := [ x - 1 ] ]
    println [ "dec x = %d" % x ]
    return [ x > 0 ] 

while ( dec )

println [ "x = %d" % x ]
println "Unbracketed"
(println "This is bracketed")
[ x = 0 ]
if [ x < 1 ]
    block
        println "x less than 1"
    block
        println "x greater than 1"
cond
    [ x == 0 ]
        println "x == 0"
    [ x == 1 ]
        println "x == 1"
    [ x == 2 ]
        println "x == 2"
