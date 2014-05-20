; fixed code
(require "CANDO/db/globalOptions.l"   )

(require "CANDO/db/organizer.l"   )

(require "CANDO/db/jobSetup.l"   )

(println "Initialized"   )

(defun status  (dbName   ) ;END1
(let (databaseName  stateCounters  total  count  total   ) ;END1
(setq databaseName  dbName   )

(println (%   "Loading database: %s"   databaseName   ) )

(database databaseName   )

(setq stateCounters  (summarizeTrainers  :databaseFileName  databaseName   ) )

(println (%   "%10s %6s"   "STATE"  "COUNT"   ) )

(setq total  0   )

(foreach state  (entries   stateCounters  ) ;END1
(setq count  (value   state  ) )

(setq total  (+   total   count   ) )

(println (%   "%10s %6d"   (key   state  )count   ) )
)

(println (%   "Total %6d states"   total   ) )
)
)

(println "About to check status"   )

(status (seedDatabaseName   ) )
