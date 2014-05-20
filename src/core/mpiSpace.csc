
[defClass Rectangle [ _width _height ] ]
[defMethod init Rectangle [ self w h ] [block
    ( _width = w )
    ( _height = h )
] ]
[defMethod repr Rectangle [self] [return ("Rectangle(w:%lf h:%lf)" % _width _height ) ] ]
[defMethod area Rectangle [self] [ return ( _width * _height ) ] ]

( mpi := [mpiCommWorld] )
( rank := [ GetRank mpi ] )
( size := ( [ GetSize mpi ] - 1 ) )

[if [ mpiEnabled ] [block 
    [println [format "Process with MPI rank %d started" rank ] ]
    [if ( rank == 0 ) [block
        # Manager process
    ( cnt := 0 )
    [println ("I'm the manager, waiting for %d objects" % size )]
    [while (cnt < size)
        [block
        ( msg := [Recv mpi MPI::ANY_SOURCE MPI::ANY_TAG ] )
        ( source := [ GetSource mpi ] )
        [println ( "Master received message from source: %d" % source ) ]
        [println ( "      Received Rectangle: %s" % ( msg repr ) ) ]
        (cnt := (cnt + 1))
        ]
    ]
    [println "Master done" ]
    ] [ block
    [println "Seeding random number generator" ]
    [ seedRandomNumberGenerators [ mpiRank ] ]
    ( rect := [new Rectangle] )
    ( rect init ([randomNumber01] * 100.0) ([randomNumber01] * 100.0))
    [println ( "Client rank: %d Rectangle: %s" % [mpiRank] (rect repr) ) ]
    [Send mpi rect 0 0]
    ] ]
] [block
    [println "Not running mpi" ]
]
]

