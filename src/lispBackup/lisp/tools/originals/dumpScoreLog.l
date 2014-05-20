
setq fileName [ l:*ARGS* @ 1 ]
setq outFileName [ l:*ARGS* @ 2 ]
println [ "Loading %s" % fileName ]
setq hits (loadArchive fileName debug: 1)
setq scoreLogs [ [ hits getData ] get "scoreLoggerList" ]

setq fout (FileOutStream :fileName outFileName)
setq idx 0
foreach sl [ scoreLogs asCons ]
    foreach entry sl
        setq temp [ entry @ 0 ]
	foreach score [ entry @ 1 ]
	    setq accepted [ score @ 1 ]
	    when accepted 
		[ fout writeln [ "%d %lf" %  idx [ score @ 0 ] ] ]
		setq idx [ idx + 1 ]
[ fout close ]
