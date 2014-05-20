					; fixed code
					;
					;
					; Usage:
					;	cando searchSimulatedAnnealing.l [ self slot _search ].cxml [ --test true ] 
					;		[ --searchType (sequenceWithBestConformation|bestConformationOfOneSequence) ]
					;
(include "CANDO/tools/packagedSearch.l" )
(include "CANDO/tools/processFarm.l" )
(println (% "*ARGS* = %s" (repr l:*args* )))
(defvar *kargs* (asKeyedArguments l:*args* ))
(when (!= (numberOfPositionalArguments *kargs* )2 )
  (println "You must provide the filename of the search" )
  (exit 1 ))
(setq fileName (getPositionalArgument *kargs* 1 ))
(setq testOnly (getBoolAndRemoveOrDefault kargs "test" false ))	;
					; Set (testOnly) to true if testing - runs a lot faster
					;

(println (% "Testing = %d" testOnly ))
(defvar *search* (loadArchive fileName ))
(defvar *alchemistScorer* (getAlchemistScorer *search* ))
(defvar *masterHitList* (getHitList *search* ))
(defvar *searchType* (getSearchType *search* ))	;
					; The number of jobs to accumulate before we save a snapshot
					; of how the search is going
					;

(defvar *accumulatedJobsBeforeSave* 25 ) ;debugLogOn
					;println "Loading database"
					;database "tdatabase.cxml" verbosity: 1
					;println "Loaded database"

(log "Starting everything" )
(describeDatabase ) ;setq *alchemistScorer* (load "t*alchemistScorer*.cxml")

(println  "Creating HitList" )
(setOptionalCandoDatabase hits m:*DATABASE* )	;
					; Search every sequence and every conformation of every sequence

(defvar *maxJobs* 200)
(defvar *saOptions '( :UseRandomConformations true
		      :NumberOfConformationsPerSequence 1000
		      :NumberOfTemperatureEstimationSteps 10
		      :NumberOfTemperatureSteps 100
		      :NumberOfStepsPerTemperature 100 ))
(when testOnly 
  (setq *saOptions* '( :UseRandomConformations true
		       :NumberOfConformationsPerSequence 1000
		       :NumberOfTemperatureEstimationSteps 10
		       :NumberOfTemperatureSteps 2
		       :NumberOfStepsPerTemperature 10 ))
  (setq *maxJobs* 10))


(defun workerProcessOneJob ()
  (let ((hits (createEmptyCopy *masterHitList* ))
	search scoreLogger)
    (setOptionalCandoDatabase hits ())
    (println (% "*saOptions* = %s" (repr *saOptions* )))
    (setq search (SimulatedAnnealingSearch :alchemistScorer *alchemistScorer* :hitList hits :searchType *searchType* :options *saOptions* ))
    (setupScoreLogger search )
    (println (% "search configuration = %s" (description search )))
    (run search )
    (setq scoreLogger (getScoreLogger search ))
    (put (getData hits )"scoreLogger" scoreLogger )
    hits )


(defClass MyManager ProcessFarm (_HitList _saveCounter )
  (method __init__ (self jobs hitList )
	  (callAncestorMethod self jobs ) ;	evaluateAncestorMethod (baseClass l:__methodClass__) __init__ self jobs
	  (setq (slot self _HitList )hitList )
	  (put (getData hitList )"scoreLoggerList" (ScoreLoggerList ))
	  (setq (slot self _saveCounter )1 ))

  (method processData (self process )
	  (let ((hits (getData process))
		(fileName (% "_accumulated%03d.cxml" (slot self _saveCounter)))
		scoreLoggerList scoreLogger )
	    (addAllHits (slot self _HitList ) hits )
	    (setq (slot self _saveCounter ) (+ (slot self _saveCounter ) 1 ))
	    (println (% "Saving accumulated hits to %s" fileName ))
	    (setq scoreLoggerList (get (getData (slot self _HitList ))"scoreLoggerList" ))
	    (setq scoreLogger (get (getData hits )"scoreLogger" ))
	    (append scoreLoggerList scoreLogger )
	    (saveArchive (slot self _HitList ) fileName )))



(defClass MyWorker ParallelFarmWorker ()
  (method processOneJob (self data )
	  (let ((hits (workerProcessOneJob)))
	    hits))


(let* ((jobs (asCons (Range 0 *maxJobs* )))
       (hits (createEmptyCopy *masterHitList* ))
       (manager (MyManager jobs hits))
       (worker (MyWorker)))
  (processFarm.mainLoop :manager manager :worker worker ))
