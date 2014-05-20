; fixed code
(require "CANDO/db/advanceAllTrainers.l"   )

(require "CANDO/db/jobSetup.l"   )

(setGlobalOption "maxRestartFails"  (getGlobalOption  "quick_maxRestartFails"   ) )

(println (%   "Set maxRestartFails to %d"   (getGlobalOption  "maxRestartFails"   ) ) )

(database (seedDatabaseName   ) )

(let ((jobs  (gatherTrainers  l:*ARGS*   ) ) ) ;END1
(advanceTrainers jobs   )
)
