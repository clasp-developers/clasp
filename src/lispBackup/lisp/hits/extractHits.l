println [ "Starting" ]

let kargs [ *ARGS* asKeyedArguments ]

let hitIndices [ [ kargs positionalArgumentsAsCons ] cdr ]
let hitsFileName [ kargs getAndRemove "hits" ]
let outputTemplate [ kargs getAndRemove "template" ]

let hits (load hitsFileName)

let builderScorer [ hits getBuilderScorer ]
foreach hitIndexString hitIndices
    let hitIndex [ hitIndexString asInt ]
    let hit [ hits getHit hitIndex ]
    println [ "Got hit;%d" % hitIndex ]
    let builderState [ hit getBuilderState ]
    let db [ *DATABASE* ]
    let contender (Contender hit: hit)
    let outputFileName [ "%s%03d.cxml" % outputTemplate hitIndex ]
    saveWithBuilderDatabase contender outputFileName
println "Done"
