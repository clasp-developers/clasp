					; fixed code
(include "CANDO/db/advanceAllTrainers.l" )
(include "CANDO/db/jobSetup.l" )
(setGlobalOption 'maxRestartFails (getGlobalOption 'quick_maxRestartFails ))
(print (% "Set maxRestartFails to %d" (getGlobalOption 'maxRestartFails )))
(database (seedDatabaseName ))
(let ((jobs (gatherTrainers *ARGS* )))
  (advanceTrainers jobs ))