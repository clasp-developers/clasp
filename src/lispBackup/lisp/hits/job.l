

defun createJobName ( hitsStem hitIdx )
    let fileNamePrefix [ "%s%03d" % hitsStem hitIdx ]
    return fileNamePrefix
;
;convert
;    let oneJob ( new OneJob )
;    [ oneJob read jobName ]
;to
;    let oneJob ( readJobOrError jobName )

defun readJobOrNil( fileNamePrefix )
    let fileName[ "%s.cxml" % [ ( Path path: fileNamePrefix ) stem ] ]
    let filePath (Path path: fileName)
    ifFalse [ filePath exists ]
        println [ "readJobOrNil: job(%s) not found" % fileName ]
        return ()
    let oneJob ( load fileName )
    return oneJob

defun readJobOrError( fileNamePrefix )
    let job ( readJobOrNil fileNamePrefix )
    ifTrue [ job isNil ]
        println [ "Job %s does not exist" % fileNamePrefix ]
	exit 1
    return job


defClass OneJob () ( _BuilderDatabase _ConformationExplorer _FileNamePrefix )

defMethod setFromDictionary OneJob ( self dict )

defMethod setBuilderDatabase OneJob ( self bdb )
    [ [ self slot _BuilderDatabase ] = bdb ]

defMethod setConformationExplorer OneJob ( self ce )
    [ [ self slot _ConformationExplorer ] = ce ]

defMethod getConformationExplorer OneJob ( self )
    return [ self slot _ConformationExplorer ] 

defMethod setFileNamePrefix OneJob ( self fn )
    [ [ self slot _FileNamePrefix ] = (stem (Path path: fn) ) ]

defMethod getFileNamePrefix OneJob ( self )
    return [ self slot _FileNamePrefix ]


defMethod write OneJob ( self )
;    [ dict = ( new ObjectDictionary ) ]
;    [ dict put "builderDatabase" _BuilderDatabase ]
;    [ dict put "conformationExplorer" [ self slot _ConformationExplorer ] ]
    save self [ self getFileName ]

defMethod getFileName OneJob ( self )
    return [ "%s.cxml" % [ slot self _FileNamePrefix ] ]


