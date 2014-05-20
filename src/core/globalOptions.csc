
global *globalOptions* (ObjectDictionary)

defun setGlobalOption ( optionName optionValue )
    println [ "Setting global option(%s) to value(%s)" % (repr optionName) (repr optionValue) ]
    [ *globalOptions* put optionName optionValue ]

defun setGlobalOptionDefaultValue ( optionName optionValue )
    println [ "Setting default value of global option(%s) to value(%s)" % (repr optionName) (repr optionValue) ]
    ifTrue [ *globalOptions* contains optionName ]
        return
    println [ "Setting global option default value (%s) = %s" % optionName optionValue ]
    [ *globalOptions* put optionName optionValue  ]

defun getGlobalOption ( optionName )
    return [ *globalOptions* get optionName ]


