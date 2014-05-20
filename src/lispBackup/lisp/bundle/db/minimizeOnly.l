					; fixed code
(include "CANDO/db/manageAdvance.l" )
(include "CANDO/db/stochastic.l" )
(include "CANDO/db/startMinimize.l" )	;
					; The states object is a list of lists
					; each inner list defines the currentState - commandToAdvanceFromState
					;

(defvar *advanceStates* (list (list "start" "startMinimize" )))
(defvar *startMinOptions* ())
(defvar *stochasticSearchOptions* (list (list "maxRestartFails" 60 )))
(defun advanceState (fileName forceField &key (keepEverything false ))
  (let (cmd trainer state cmd )
    (setq cmd "start" )
    (print (% "advanceState keepEverything = %d" keepEverything ))
    (print (% "Reading trainer: %s " fileName ))
    (setq trainer (readTrainer fileName ))
    (while (notNil cmd)
      (setq state (getState (getHeader trainer )))
      (print (% "    trainer: %s   :State %s" fileName state ))
      (setq cmd (lookup *advanceStates* state ))
      (if (notNil cmd)
	  (then 
	   (ifTrue (== cmd "startMinimize" )
		   (print (% "Beginning -- startMinimize on trainer[%s]" fileName ))
		   (startMinimize trainer forceField *startMinOptions* )
		   (print (% "Done -- startMinimize on trainer[%s]" fileName )))
	   (ifTrue (== cmd "stochasticSearch" )
		   (print (% "Beginning -- stochasticSearch on trainer[%s]" fileName ))
		   (stochasticSearch trainer forceField *stochasticSearchOptions* :keepEverything keepEverything )
		   (print (% "Done -- stochasticSearch on trainer[%s]" fileName ))))
	(else 
	 (print (% "    DONE!  In state: %s" state )))))))


(defun startMinimize (trainer forceField options )
  (let (min )
    (print (% "Starting minimize of trainer: %s" (asString (getPath (getHeader trainer )))))
    (setq min (StartMinimize ))
    (initTrainerAndForceField min trainer forceField options )
    (run min )))


(defun stochasticSearch (trainer forceField options &key (keepEverything false ))
  (let (search )
    (print (% "Starting stochastic search of trainer: %s keepEverything = %d"
	      (asString (getPath (getHeader trainer ))) keepEverything ))
    (setq search (StochasticSearch ))
    (init search forceField trainer options )
    (setKeepEverything search keepEverything )
    (run search )))


(defClass AdvanceTrainer_Worker MpiWorker ())
(defMethod processJob AdvanceTrainer_Worker (self job )
  (print (% "Process %2d> AdvanceTrainer_Worker process job for: %s" (mpiRank )job ))
  (advanceState job forceField )	;
					; If MPI is being used then set up a manager and Workers and
					; distribute the jobs to the workers
					;
  )



(defun advanceTrainers (jobs &key (keepEverything false ))
  (let (forceField jobPath trainerDir )
    (print (% "Starting advanceTrainers  keepEverything = %d" keepEverything ))
    (setq forceField (loadArchive (% "%s/gaff.cxml" (databaseDir ))))
    (setq jobPath (new Path ))
    (setq trainerDir (getGlobalOptions "trainerDirectory" ))
    (setPath jobPath trainerDir )      ;    setq jobs ( cdr *ARGS* )
					;    setq jobs (directory jobPath )

    (print (% "jobs = %s" (repr jobs )))
    (if (mpiEnabled )
	(then 
	 (print "MPI is enabled, running as multiple processes" )
	 (log "MPI is enabled, running as multiple processes" )
	 (distributeJobs MpiManager AdvanceTrainer_Worker jobs )
	 (print "Finished distributed jobs" ))
      (else				; Single process 
       (print "MPI is disabled, running as single process" )
       (print (% "  Running for jobs: %s" (repr jobs )))
       (foreach job jobs 
		(advanceState job forceField :keepEverything keepEverything ))))
    (print "Done" )))