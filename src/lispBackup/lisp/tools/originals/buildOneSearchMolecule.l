;
;
; Usage:
;	cando searchSimulatedAnnealing.l [ self slot _search ].cxml [ --test true ]
;


require "CANDO/tools/packagedSearch.l"
require "CANDO/tools/processFarm.l"

println [ "*ARGS* = %s" % (repr l:*ARGS* ) ]
setq kargs [ l:*ARGS* asKeyedArguments ]

println [ "kargs = %s" % (repr kargs) ]
when [ [ kargs numberOfPositionalArguments ] != 2 ]
    println "You must provide the filename of the search"
    exit 1
setq fileName [ kargs getPositionalArgument 1 ]
setq testOnly [ kargs getBoolAndRemoveOrDefault "test" false ]


;
; Set (testOnly) to true if testing - runs a lot faster
;

println [ "Testing = %d" % testOnly ]


setq search (loadArchive fileName)
setq builderScorer [ search getBuilderScorer ]
setq masterHitList [ search getHitList ]


setq mols [ [ builderScorer getBuilder ] buildFirstMoleculeForEachOligomer ]
println [ "Built molecules = %s" % ( repr mols ) ]
saveArchive mols "_mols.cxml"
exit
;
; The number of jobs to accumulate before we save a snapshot
; of how the search is going
;
global *accumulatedJobsBeforeSave* 25


;debugLogOn
;println "Loading database"
;database "tdatabase.cxml" verbosity: 1
;println "Loaded database"
log "Starting everything"

describeDatabase




println [ "Creating HitList" ]
setq hits [ masterHitList createEmptyCopy ]
[hits setOptionalBuilderDatabase m:*DATABASE* ]


;
; Search every sequence and every conformation of every sequence
if testOnly
  then
    setq confs 1000
    setq maxJobs 10
    global *saOptions* (list
		:UseRandomConformations true
		:NumberOfConformationsPerSequence confs
		:NumberOfTemperatureEstimationSteps 10
		:NumberOfTemperatureSteps 2
		:NumberOfStepsPerTemperature 10 )
  else
    setq confs 1000
    setq maxJobs 200
    global *saOptions* ( list
		:UseRandomConformations true
		:NumberOfConformationsPerSequence confs
		:NumberOfTemperatureSteps 100
		:NumberOfStepsPerTemperature 100 )



defun workerProcessOneJob ( )
    setq hits [ masterHitList createEmptyCopy ]
    [ hits setOptionalBuilderDatabase () ]
    println [ "*saOptions* = %s" % (repr *saOptions* ) ]
    setq search (SimulatedAnnealingSearch 
    			:builderScorer builderScorer
			:hitList hits 
			:searchType SimulatedAnnealingSearchType.sequenceWithBestConformation
			:options *saOptions* )
    [ search setupScoreLogger ]
    println [ "search configuration = %s" % [ search description ] ]
    [ search run ]
    setq scoreLogger [ search getScoreLogger ]
    [ [ hits getData ] put "scoreLogger" scoreLogger ]
    return hits





defClass MyManager ProcessFarm ( _HitList _saveCounter )

    method __init__ ( self jobs hitList )
        callAncestorMethod self jobs
;	evaluateAncestorMethod (baseClass l:__methodClass__) __init__ self jobs
	setq [ self slot _HitList ] hitList
	[ [ hitList getData ] put "scoreLoggerList" (ScoreLoggerList) ]
	setq [ self slot _saveCounter ] 1

    method processData ( self process )
        setq hits [ process getData ]
        [ [ self slot _HitList ] addAllHits hits ]
	setq fileName [ "taccumulated%03d.cxml" % [ self slot _saveCounter ] ]
	setq [ self slot _saveCounter ] [ [ self slot _saveCounter ] + 1 ]
	println [ "Saving accumulated hits to %s" % fileName ]
	setq scoreLoggerList [ [ [ self slot _HitList ] getData ] get "scoreLoggerList" ]
	setq scoreLogger [ [ hits getData ] get "scoreLogger" ]
	[ scoreLoggerList append scoreLogger ]
	save [ self slot _HitList ] fileName




defClass MyWorker ParallelFarmWorker ()

    method processOneJob ( self data )
        setq hits (workerProcessOneJob)
	return hits







;
;debugLogOn
;println ["mpiRank = %d" % (mpiRank) ]
;if [ (mpiRank) == 0 ]
;  then
;    setq manager (MyManager jobs hits)
;    [ manager run ]
;  else
;    setq worker (MyWorker)
;    [ worker run ]



setq jobs [ (Range 0 maxJobs) asCons ]
setq manager (MyManager jobs hits )
setq worker (MyWorker)

processFarm.mainLoop :manager manager :worker worker


