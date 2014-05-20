; fixed code
(require "CANDO/db/manageAdvance.l"   )

(require "CANDO/db/stochastic.l"   )

(require "CANDO/db/startMinimize.l"   )

(set *advanceStates*  (list  (list  "start"  "startMinimize"   ) ) )

(global *startMinOptions*  () )

(global *stochasticSearchOptions*  (list  (list  "maxRestartFails"  60   ) ) )

(defun advanceState  (fileName  forceField  &  (keepEverything  false   ) ) ;END1
(let (cmd  trainer  state  cmd   ) ;END1
(setq cmd  "start"   )

(println (%   "advanceState keepEverything = %d"   keepEverything   ) )

(println (%   "Reading trainer: %s "   fileName   ) )

(setq trainer  (readTrainer  fileName   ) )

(while (!=   cmd   () ) ;END1
(=   state   (getState   (getHeader   trainer  )) )

(println  (%   "    trainer: %s   :State %s"   fileName  state   ) )

(=   cmd   (lookup   *advanceStates*   state   ) )

(if (!=   cmd   () ) ;END1
(then ;END1
(ifTrue (==   cmd   "startMinimize"   ) ;END1
(println (%   "Beginning -- startMinimize on trainer[%s]"   fileName   ) )

(startMinimize trainer  forceField  *startMinOptions*   )

(println (%   "Done -- startMinimize on trainer[%s]"   fileName   ) )
)

(ifTrue (==   cmd   "stochasticSearch"   ) ;END1
(println (%   "Beginning -- stochasticSearch on trainer[%s]"   fileName   ) )

(stochasticSearch trainer  forceField  *stochasticSearchOptions*  :keepEverything  keepEverything   )

(println (%   "Done -- stochasticSearch on trainer[%s]"   fileName   ) )
)
)

(else ;END1
(println (%   "    DONE!  In state: %s"   state   ) )
)
)
)
)
)

(defun startMinimize  (trainer  forceField  options   ) ;END1
(let (min   ) ;END1
(println (%   "Starting minimize of trainer: %s"   (getFileName   (getHeader   trainer  )) ) )

(=   min   (new  StartMinimize   ) )

(initTrainerAndForceField   min   trainer  forceField  options   )

(run   min  )
)
)

(defun stochasticSearch  (trainer  forceField  options  &key  (keepEverything  false   ) ) ;END1
(let (search   ) ;END1
(println (%   "Starting stochastic search of trainer: %s keepEverything = %d"   (getFileName   (getHeader   trainer  ))keepEverything   ) )

(=   search   (StochasticSearch   ) )

(init   search   forceField  trainer  options   )

(setKeepEverything   search   keepEverything   )

(run   search  )
)
)

(defClass AdvanceTrainer_Worker  MpiWorker  () )

(defMethod processJob  AdvanceTrainer_Worker  (self  job   ) ;END1
(println (%   "Process %2d> AdvanceTrainer_Worker process job for: %s"   (mpiRank   )job   ) )

(advanceState job  forceField   )
)

(defun advanceTrainers  (jobs  &key  (keepEverything  false   ) ) ;END1
(let (forceField  jobPath  trainerDir   ) ;END1
(println (%   "Starting advanceTrainers  keepEverything = %d"   keepEverything   ) )

(setq forceField  (loadArchive  (%   "%s/gaff.cxml"   (databaseDir   ) ) ) )

(setq jobPath  (new  Path   ) )

(setq trainerDir  (getGlobalOptions  "trainerDirectory"   ) )

(setPath   jobPath   trainerDir   )

(println (%   "jobs = %s"   (repr  jobs   ) ) )

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
(advanceState job  forceField  :keepEverything  keepEverything   )
)
)
)

(println "Done"   )
)
)
