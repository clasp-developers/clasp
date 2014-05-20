; fixed code
(require "CANDO/db/manageAdvance.l"   )

(require "CANDO/db/stochastic.l"   )

(require "CANDO/db/startMinimize.l"   )

(global *advanceStates*  (ObjectDictionary  :entries  (list  (list  "bad"  "abort"   )(list  "start"  "startMinimize"   )(list  "min"  "stochasticSearch"   )(list  "searching"  "stochasticSearch"   )(list  "searched"  "stochasticSearch"   )(list  "searchFailed"  "startMinimize"   ) ) ) )

(global *startMinOptions*  () )

(defun advanceState  (fileName  forceField  &key  (keepEverything  false   ) ) ;END1
(let (trainer  done  state  cmd  maxRestartFails  head   ) ;END1
(println (%   "advanceState keepEverything = %d"   keepEverything   ) )

(println (%   "Reading trainer: %s "   fileName   ) )

(setq trainer  (readTrainer  fileName   ) )

(setq done  false   )

(while (not  done   ) ;END1
(setq state  (getState   (getHeader   trainer  )) )

(println (%   "    trainer: %s   State: %s"   fileName  state   ) )

(setq cmd  (get   *advanceStates*   state   ) )

(cond ;END1
(==   cmd   "abort"   )
(println  "The trainer has the state :bad: so I'm done with it - moving on"    )

(setq done  true   )


(==   cmd   "startMinimize"   )
(println (%   "Beginning -- startMinimize on trainer[%s]"   fileName   ) )

(startMinimize trainer  forceField  *startMinOptions*   )

(println (%   "Done -- startMinimize on trainer[%s]"   fileName   ) )


(==   cmd   "stochasticSearch"   )
(setq maxRestartFails  (getGlobalOption  "maxRestartFails"   ) )

(setq head  (getHeader   trainer  ) )

(println (%   "setMaxConformationalSearchFailsRequired to %d"   maxRestartFails   ) )

(setMaxConformationalSearchFailsRequired   head   maxRestartFails   )

(if (conformationalSearchesRequired   head  ) ;END1
(then ;END1
(setState   head   "searching"   )
)

(else ;END1
(setState   head   "searched"   )

(write   trainer  )

(setq done  true   )
)
)

(when (not  done   ) ;END1
(println (%   "Running -- stochasticSearch on trainer[%s]"   fileName   ) )

(let ((stochasticSearchOptions  (list   ) ) ) ;END1
(stochasticSearch trainer  forceField  :options  stochasticSearchOptions  :keepEverything  keepEverything   )
)
)

(setq done  true   )

(println (%   "Done -- stochasticSearch on trainer[%s]"   fileName   ) )


(true ;END1
(println (%   "    Illegal cmd  state[%s] cmd[%s]"   state  (repr  cmd   ) ) )

(setq done  true   )
)
)
)
)
)

(defun startMinimize  (trainer  forceField  options   ) ;END1
(let (min   ) ;END1
(println (%   "Starting minimize of trainer: %s"   (getFileName   (getHeader   trainer  )) ) )

(=   min   (StartMinimize   ) )

(initTrainerAndForceField   min   trainer  forceField  options   )

(run   min  )
)
)

(defun stochasticSearch  (trainer  forceField  &key  (options  (list  () ) )(keepEverything  false   ) ) ;END1
(let (search   ) ;END1
(println (%   "Starting stochastic search of trainer: %s keepEverything = %d"   (getFileName   (getHeader   trainer  ))keepEverything   ) )

(=   search   (StochasticSearch  forceField  trainer  :options  options   ) )

(setKeepEverything   search   keepEverything   )

(run   search  )
)
)

(defClass AdvanceTrainer_Worker  MpiWorker  () ;END1
(method processJob  (self  job  forceField   ) ;END1
(println (%   "Process %2d> AdvanceTrainer_Worker process job for: %s"   (mpiRank   )job   ) )

(advanceState job  forceField   )
)
)

(defun advanceTrainers  (unsortedJobs  &key  (keepEverything  false   ) ) ;END1
(let (forceField  jobs   ) ;END1
(println (%   "Starting advanceTrainers  keepEverything = %d"   keepEverything   ) )

(setq forceField  (loadArchive  (%   "%s/gaff.cxml"   (databaseDir   ) ) ) )

(global *forceField*  forceField   )

(setq jobs  (reverse   (sorted   unsortedJobs  )) )

(println  "Jobs list -------------------"    )

(foreach jj  jobs   ;END1
(println (%   "one job = %s"   jj   ) )
)

(println  "Jobs list done^^^^^^^^^^^^^^^^"    )

(if (mpiEnabled   ) ;END1
(then ;END1
(println "MPI is enabled, running as multiple processes"   )

(log "MPI is enabled, running as multiple processes"   )

(distributeJobs MpiManager  AdvanceTrainer_Worker  jobs   )

(println "Finished distributed jobs"   )
)

(else ;END1
(println "MPI is disabled, running as single process"   )

(println (%   "  Running for jobs: %s"   (repr  jobs   ) ) )

(foreach job  jobs   ;END1
(println (%   "Job: %s"   job   ) )

(handlerCase  (advanceState  job  forceField  :keepEverything  keepEverything   )(CandoException  (se   )(progn  (println  "AN EXCEPTION WAS THROWN when processing the job"   )(println  (%   "%s"   (message   se  ) ) )(println  "   - continuing to next job"   ) ) ) )
)
)
)

(println "All jobs are done"   )
)
)
