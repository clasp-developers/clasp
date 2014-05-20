
(require "CANDO/db/jobSetup.l")
(require "CANDO/db/trainer.l")
(require "CANDO/db/organizer.l")
(require "CANDO/db/dynamics/dynamics.l")
(require "CANDO/db/dynamics/openmm.l")



(defun process-dynamics-for-trainer (&key trainer)
  (let* ((dynamics (trainer-dynamics trainer))
	 (clusters (dynamics-clusters-as-cons dynamics))
	 (oligomer (trainer-oligomer trainer))
	 )
    (dolist (cluster clusters)
      
      )
    )
  )


(defun process-all-dynamics (args)
  (database (seedDatabaseName) true)
  (let ((gaff (loadArchive (% "%s/gaff.cxml" (databaseDir))))
	(jobs (gatherTrainers args)))
    (dolist (job jobs)
      (prognLog (% "Job = %s" (asString job))
		(let ((trainer (readTrainer job)))
		  (process-dynamics-for-trainer :trainer trainer )
		  (chem:write trainer)
		  )
		))
    )
  )




(process-all-dynamics *ARGS*)