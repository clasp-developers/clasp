
(require "CANDO/db/jobSetup.l")
(require "CANDO/db/trainer.l")
(require "CANDO/db/organizer.l")
(require "CANDO/db/dynamics/dynamics.l")
(require "CANDO/db/dynamics/openmm.l")

(defun setup-dynamics-for-trainer (&key trainer forceField)
  (prognLog "setup-dynamics-for-trainer"
	    (let* ((dynamics (trainer-dynamics trainer))
		   (clusters (dynamics-clusters-as-cons dynamics)))
	      (dolist (cluster clusters)
		(let* ((aggregate (cluster-aggregate cluster))
		       (systemPath (cluster-system-path cluster))
		       (structurePath (cluster-structure-path cluster))
		       (checkpointPath (cluster-checkpoint-path cluster))
		       (coordinatePath (cluster-coords-path cluster))
		       )
		  (assignTypes forceField aggregate)
		  (saveArchive aggregate structurePath)
		  (multiple-value-bind
			(system positions)
		      (setup-system-and-positions :matter aggregate :forceField forceField)
		    (print (% "Saving system to: %s" (asString systemPath)))
		    (save-system system systemPath)
		    (let* ((context (setup-context :system system
						   :integrator (default-integrator)
						   :platform (default-platform)
						   :positions positions
						   ))
			   (integrator (omm:getIntegrator context)))
		      (print "Starting dynamics ------------- 100 steps" )
		      (omm:step integrator 100)
		      (print "       ------- saving checkpoint --------" )
		      (save-checkpoint :cluster cluster
				       :context context
				       :filePos 0)
		      (let ((newFilePos (save-coordinates :cluster cluster
							  :context context
							  :time 0
							  :filePos 0)))
			(print (% "Wrote to coordinates file newpos=%d" newFilePos)))
		      )
		    )
		  )
		))))



(defun setup-all-dynamics (args)
  (database (seedDatabaseName) true)
  (let ((gaff (loadArchive (% "%s/gaff.cxml" (databaseDir))))
	(jobs (gatherTrainers args)))
    (dolist (job jobs)
      (prognLog (% "Job = %s" (asString job))
		(let ((trainer (readTrainer job)))
		  (setup-dynamics-for-trainer :trainer trainer :forceField gaff )
		  (chem:write trainer)
		  )
		))
    )
  )




(setup-all-dynamics *ARGS*)