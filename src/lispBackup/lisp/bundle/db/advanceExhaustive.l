					; fixed code
(include "CANDO/db/advanceAllTrainers.l" )
(include "CANDO/db/jobSetup.l" )
(setGlobalOption 'maxRestartFails (getGlobalOption 'exhaustive_maxRestartFails ))
(println (% "Set maxRestartFails to %d" (getGlobalOption 'maxRestartFails )))
(database (seedDatabaseName ))
(let ((jobs (gatherTrainers *ARGS* )))
  (advanceTrainers jobs ))