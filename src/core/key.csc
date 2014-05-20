[defFunction pr [ a b:(3 + 5) c:"carg" ] [block
    [println "In pr" ]
    [println ( "    a = %s" % [repr a ] ) ]
    [println ( "    b = %s" % [repr b ] ) ]
    [println ( "    c = %s" % [repr c ] ) ]
] ]

[pr 1 ]
[pr 1 b:1234 ]
[pr 1 b:1234 c: 5678 ]
[pr 1 b:1234 d:234234 ]
