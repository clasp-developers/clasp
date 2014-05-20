
(include "CANDO/charges/mopacInterface.l")

(when (isTopLevelScript)
  (progn
    (print "Resetting mopac files")
    (include "CANDO/db/jobSetup.l")
    (database (seedDatabaseName) true)
    (let ((jobs (gatherTrainers *ARGS*)))
      (for j jobs
	   (let ((trainer (readTrainer j)))
	     (mopac-reset trainer))
	   )
      )))

