(include "CANDO/charges/mopacInterface.l")

(print "Creating mopac files")
(include "CANDO/db/jobSetup.l")
(database (seedDatabaseName) true)
(let ((jobs (gatherTrainers *ARGS*))
      (allRun (ObjList)))
  (for j jobs
       (let ((trainer (readTrainer j)))
	 (create-mopac-interface-for-trainer trainer
					     :mopacCalculation "am1 mmok" )))
  )
