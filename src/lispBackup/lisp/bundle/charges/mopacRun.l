(include "CANDO/charges/mopacInterface.l")





(when (isTopLevelScript)
  (progn
    (print "Extracting mopac files")
    (include "CANDO/db/jobSetup.l")
    (database (seedDatabaseName) true)
    (let ((jobs (gatherTrainers *ARGS*)))
      (for j jobs
	   (let ((trainer (readTrainer j)))
	     (print (% "Checking to see if we need to run mopac for trainer: %s" (asString (getPath (getHeader trainer)))))
	     (run-mopac-for-trainer trainer))
	   )
      )))

