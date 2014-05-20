
println [ "isTopLevelScript value = %s" % (repr (isTopLevelScript) ) ]
ifFalse (isTopLevelScript)
    println [ "isTopLevelScript evaluates to false" ]
ifTrue (isTopLevelScript)
    println [ "isTopLevelScript evaluates to true" ]
