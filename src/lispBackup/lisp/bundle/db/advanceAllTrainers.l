					; fixed code
(include "CANDO/db/manageAdvance.l" )
(include "CANDO/db/stochastic.l" )
(include "CANDO/db/startMinimize.l" )	;
					; The states object is a list of lists
					; each inner list defines the currentState - commandToAdvanceFromState
					;
(defvar *forceField* (loadArchive (% "%s/gaff.cxml" (databaseDir ))))
(defvar *advanceStates*
  (ObjectDictionary :entries (list
			      (list "bad" "abort" )
			      (list "start" "startMinimize" )
			      (list "min" "stochasticSearch" )
			      (list "searching" "stochasticSearch" )
			      (list "searched" "stochasticSearch" )
			      (list "searchFailed" "startMinimize" ))))
(defvar *startMinOptions* ())
(defun advanceState (fileName forceField &key (keepEverything false ))
  (let (trainer done state cmd maxRestartFails head )
    (print (% "advanceState keepEverything = %d" keepEverything ))
    (print (% "Reading trainer: %s " fileName ))
    (setq trainer (readTrainer fileName ))
    (setq done false )
    (while (not done )
      (setq state (getState (getHeader trainer )))
      (print (% "    trainer: %s   State: %s" fileName state ))
      (setq cmd (get *advanceStates* state ))
      (cond 
       ((== cmd "abort" )		;E-A
	(print  "The trainer has the state :bad: so I'm done with it - moving on" )
	(setq done true ))		;E-B

       ((== cmd "startMinimize" )	;E-A
	(print (% "Beginning -- startMinimize on trainer[%s]" fileName ))
	(startMinimize trainer forceField *startMinOptions* )
	(print (% "Done -- startMinimize on trainer[%s]" fileName )))	;E-B

       ((== cmd "stochasticSearch" )	;E-A
	(setq maxRestartFails (getGlobalOption 'maxRestartFails ))
	(setq head (getHeader trainer ))
	(print (% "setMaxConformationalSearchFailsRequired to %d" maxRestartFails ))
	(setMaxConformationalSearchFailsRequired head maxRestartFails )
	(if (conformationalSearchesRequired head )
	    (then 
	     (setState head "searching" ))
	  (else 
	   (setState head "searched" )
	   (chem:write trainer )
	   (setq done true )))
	(when (not done )
	  (print (% "Running -- stochasticSearch on trainer[%s]" fileName ))
	  (let ((stochasticSearchOptions (list )))
	    (stochasticSearch trainer forceField :options stochasticSearchOptions :keepEverything keepEverything )))
	(setq done true )
	(print (% "Done -- stochasticSearch on trainer[%s]" fileName ))) ;E-B

       (true 
	(print (% "    Illegal cmd  state[%s] cmd[%s]" state (repr cmd )))
	(setq done true ))))))





(defun startMinimize (trainer forceField options )
  (let (min )
    (print (% "Starting minimize of trainer: %s" (asString (getPath (getHeader trainer )))))
    (setq min (StartMinimize ))
    (initTrainerAndForceField min trainer forceField options )
    (run min )))




(defun stochasticSearch (trainer forceField &key (options (list ()))(keepEverything false ))
  (let (search )
    (print (% "Starting stochastic search of trainer: %s keepEverything = %d"
	      (asString (getPath (getHeader trainer ))) keepEverything ))
    (setq search (StochasticSearch forceField trainer :options options ))
    (setKeepEverything search keepEverything )
    (run search )))



(defClass AdvanceTrainer_Worker MpiWorker ()
  (method processJob (self job forceField )
	  (print (% "Process %2d> AdvanceTrainer_Worker process job for: %s" (mpiRank )job ))
	  (advanceState job forceField ) ;
					; If MPI is being used then set up a manager and Workers and
					; distribute the jobs to the workers
					;
	  ))
(defun advanceTrainers (unsortedJobs &key (keepEverything false ))
  (let ((jobs (reverse (sorted unsortedJobs ))))
    (print (% "Starting advanceTrainers  keepEverything = %d" keepEverything ))
    (print  "Jobs list -------------------" )
    (foreach jj jobs 
	     (print (% "one job = %s" jj )))
    (print  "Jobs list done^^^^^^^^^^^^^^^^" )
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
		(print (% "Job: %s" job )) ;	    advanceState job forceField :keepEverything keepEverything
		(handlerCase
		 (advanceState job *forceField* :keepEverything keepEverything )
		 (CandoException (se )
				 (progn (print "AN EXCEPTION WAS THROWN when processing the job - caught in advanceAllTrainers" )
					(print (getStackTraceDump se))
					(print (% "%s" (message se )))
					(print "   - continuing to next job" )))))))
    (print "All jobs are done" )))