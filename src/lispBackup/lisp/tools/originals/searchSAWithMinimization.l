;
;
; Usage:
;	cando searchSimulatedAnnealing.l [ self slot _search ].cxml [ --test true ] 
;		[ --searchType (sequenceWithBestConformation|bestConformationOfOneSequence) ]
;

require "CANDO/tools/packagedSearch.l"
require "CANDO/tools/processFarm.l"

;
; The number of jobs to accumulate before we save a snapshot
; of how the search is going
;
global *accumulatedJobsBeforeSave* 25


global *gaff* (loadArchive [ "%s/gaff.cxml" % (databaseDir) ] )

defun minimizeHitsIntoConformationExplorer ( builderScorer hits conformationExplorer )
    setq newConformationExplorer [ hits convertToConformationExplorer ]
    raise (LispError "Working")

defun createEmptyResultsTable ()
    local table (Table)
    [ table appendField :builderState BuilderState ]
    [ table appendField :score Real ]
    [ table appendField :builtMolecule Molecule ]
    [ table appendField :minimizedMolecule Molecule ]
    return table

defun fillColumnsFromHitList (hits table)
    foreach hit [hits entries]
        local builderState [ hit getBuilderState ]
	local builderScorer [ hit recreateBuilderScorer ]
	local scorerState [ hit getScorerState ]
	local score [ hit getScore ]
	local mol [ builderScorer getBuiltMoleculeWithCoordinates scorerState ]
	[ table appendWrite (list :builderState builderState :score score :builtMolecule mol ) ]

defun minimizeConformationsInTable ( table )
    local startMol ()
    local minimizedMol ()
    local minimizer ()
    foreach entry [table asCons]
        setq startMol [ entry read :builtMolecule ]
	setq minimizeMol [ startMol copy ]
	setq minimizer (Minimizer :forceField *gaff* :matter minimizeMol )
	[ minimizer enablePrintIntermediateResults ]
	[ minimizer minimize ]
	[ entry write :minimizedMolecule minimizeMol ]

defun mergeTablesAndEliminateIdenticalMinimizedConformations (accumulatedResults localResults)
    [ accumulatedResults merge localResults ]
    println "Here I should eliminate identical minimized conformations" 
    ; Eliminate identical minimized conformations


defun workerProcessOneJob ( fileName )
    local search (loadArchive fileName)
    local builderScorer [ search getBuilderScorer ]
    local hits [ search getHitList ]
    local searchType [ search getSearchType ]
    local accumulatedResults [ search loadIntermediateResults ]
    when [ accumulatedResults isNil ]
        setq accumulatedResults (createEmptyResultsTable)
    println [ "*saOptions* = %s" % (repr *saOptions* ) ]
    setq sa (SimulatedAnnealingSearch 
    			:builderScorer builderScorer
			:hitList hits 
			:searchType searchType
			:options *saOptions* )
    [ sa setupScoreLogger ]
    println [ "search configuration = %s" % [ sa description ] ]
    [ sa run ]
    local scoreLogger [ sa getScoreLogger ]
    [ [ hits getData ] put "scoreLogger" scoreLogger ]
    local localResults (createEmptyResultsTable)
    fillColumnsFromHitList hits localResults
    minimizeConformationsInTable localResults
    mergeTablesAndEliminateIdenticalMinimizedConformations accumulatedResults localResults
    [ search saveIntermediateResults accumulatedResults ]
    return ()




defClass MyManager ProcessFarm ( _saveCounter )
    method __init__ ( self jobs )
    	callAncestorMethod self jobs
;	evaluateAncestorMethod (baseClass l:__methodClass__) __init__ self jobs
;	[ [ hitList getData ] put "scoreLoggerList" (ScoreLoggerList) ]
;	setq [ self slot _saveCounter ] 1

    method processData ( self process )
        ()
;        setq hits [ process getData ]
;	setq fileName [ "_accumulated%03d.cxml" % [ self slot _saveCounter ] ]
;	setq [ self slot _saveCounter ] [ [ self slot _saveCounter ] + 1 ]
;	println [ "Saving accumulated hits to %s" % fileName ]
;	setq scoreLoggerList [ [ [ self slot _HitList ] getData ] get "scoreLoggerList" ]
;	setq scoreLogger [ [ hits getData ] get "scoreLogger" ]
;	[ scoreLoggerList append scoreLogger ]
;	saveArchive [ self slot _HitList ] fileName




defClass MyWorker ParallelFarmWorker ()

    method processOneJob ( self fileName )
        workerProcessOneJob fileName







println [ "*ARGS* = %s" % (repr l:*ARGS* ) ]
global jobsFileName [ l:*ARGS* cadr ]
global testOnly [ l:*ARGS* lookupDefault :test false ]

println [ "jobsFileName = %s" % jobsFileName ]
println [ "testOnly = %s" % (repr testOnly ) ]
;
; Search every sequence and every conformation of every sequence
if testOnly
  then
    println [ "Running in TEST mode" ]
    setq confs 1000
    setq maxJobs 10
    global *saOptions* (list
		:UseRandomConformations true
		:NumberOfConformationsPerSequence confs
		:NumberOfTemperatureEstimationSteps 10
		:NumberOfTemperatureSteps 2
		:NumberOfStepsPerTemperature 10 )
  else
    println [ "Running in normal mode" ]
    setq confs 10000
    setq maxJobs 200
    global *saOptions* ( list
		:UseRandomConformations true
		:NumberOfConformationsPerSequence confs
		:NumberOfTemperatureSteps 100
		:NumberOfStepsPerTemperature 100 )
;
; Set (testOnly) to true if testing - runs a lot faster
;
println [ "mpiRank = %d" % (mpiRank) ]
if [ (mpiRank) == 0 ]
  then
    println [ "Starting manager" ]
    local jobsList (loadArchive jobsFileName)
    local jobs [jobsList asCons]
    local manager (MyManager jobs)
    local worker (MyWorker)
    processFarm.mainLoop :manager manager :worker worker
  else
    println [ "Starting worker" ]
    local worker (MyWorker)
    processFarm.mainLoop :manager () :worker worker

