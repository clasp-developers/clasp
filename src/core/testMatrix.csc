setq s "1.0  2.0  0.096299  4.338652
-0.777515 0.272796 -0.566615 1.556894 
0.492305 -0.296584 -0.818336 -1.641319 
0 0 0 1"

setq times 3000000
println [ "Parsing: \n%s" % s ]
setq m (OMatrix)
[m setFromStringFast s]

println "Press enter to run setFromStringFast: "
setq tb [ (PosixTime) now ]
foreach x (Range 0 times)
    [m setFromStringFast s]
setq te [ (PosixTime) now ]
setq td [ te sub tb ]
println [ "Time for setFromStringFast: %s" % [ td toSimpleString ] ]

println "Press enter to run asStringFast: "
setq tb [ (PosixTime) now ]
foreach x (Range 0 times)
    setq g [m asStringFast]
setq te [ (PosixTime) now ]
setq td [ te sub tb ]
println [ "Time for asStringFast: %s" % [ td toSimpleString ] ]

getline "Press enter to run setFromString: "
setq tb [ (PosixTime) now ]
foreach x (Range 0 times)
    [m setFromString s]
setq te [ (PosixTime) now ]
setq td [ te sub tb ]
println [ "Time for setFromString: %s" % [ td toSimpleString ] ]

getline "Press enter to run asString: "
setq tb [ (PosixTime) now ]
foreach x (Range 0 times)
    setq g [m asString]
setq te [ (PosixTime) now ]
setq td [ te sub tb ]
println [ "Time for asString: %s" % [ td toSimpleString ] ]


println [ "[m asStringFast] = %s" % [ m asStringFast ] ]
