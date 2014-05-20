[defFunction AA [ a b ] [block
    [println ( "a = %d" % a ) ]
    [println ( "b = %d" % b ) ]
    [return 999 ]
] ]

( z = [: ( "AA" asSymbol ) ] )
( zz = ( z extend [: 1 2 ] ) )
[println ( "length(zz) = %d" % [length zz ] ) ]
[println ( "zz = %s" % [repr zz ] ) ]

[println ( "length(z) = %d" % [length z ] ) ]
[println ( "z = %s" % [repr z ] ) ]

[println ( "eval(zz) = %s" % [repr [ eval zz ] ] ) ]
