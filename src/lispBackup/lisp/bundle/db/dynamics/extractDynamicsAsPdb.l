
(require "CANDO/db/jobSetup.l")
(require "CANDO/db/trainer.l")
(require "CANDO/db/organizer.l")
(require "CANDO/db/dynamics/dynamics.l")
(require "CANDO/db/dynamics/openmm.l")




(defun extract-frame-write-to-pdb-writers (&key cluster structure coordinates pdbWriters (frameNumber 0))
  "Write coordinates into the structure and then for every
molecule write it out into a different pdbWriter"
  (let ((ci 0))
    (for atom (atoms structure)
      (chem:setPosition atom (getElement coordinates ci))
      (setq ci (+ 1 ci))))
  (let ((mi 0)
	(molecules (molecules structure)))
    (dotimes (mi (length molecules))
      (writeModel (get pdbWriters mi) (nth mi molecules) frameNumber)))
  )



(defun extract-dynamics-for-trainer (&key trainer)
  (let* ((dynamics (trainer-dynamics trainer))
	 (clusters (dynamics-clusters-as-cons dynamics))
	 (oligomer (trainer-oligomer trainer))
	 )
    (dolist (cluster clusters)
      (let* ((structurePath (cluster-structure-path cluster))
	     (structure (loadArchive (asString structurePath)))
	     (frameCoordinates (CoordinateArray))
	     (coordinatesPath (cluster-coords-path cluster))
	     (coordinatesFileIn (FileInStream (asString coordinatesPath)))
	     (pdbWriters (let ((writers (ObjList)))
			   (dotimes (i (length (cluster-conformation-indices cluster)))
			     (let ((writerFilePath (append (copyPath (cluster-path cluster)) (% "dynamics%03d.pdb" i)))
				   (pdbWriter (PdbWriter)))
			       (chem:open pdbWriter (asString writerFilePath))
			       (append writers pdbWriter))
			     )
			   writers
			   ))
	     (frameNumber 0)
	     )
	(unwind-protect
	     (progn
	       (while true
		 (print (% "Reading frame %d" frameNumber ))
		 (setq frameNumber (+ 1 frameNumber))
		 (read-from-stream frameCoordinates coordinatesFileIn)
		 (multiplyByScalar frameCoordinates 10.0) ;; Convert nanometers to angstroms
		 (extract-frame-write-to-pdb-writers :cluster cluster
						     :structure structure
						     :coordinates frameCoordinates
						     :pdbWriters pdbWriters
						     :frameNumber frameNumber )
		 )
	       )
	  (progn
	    (print "Closing all PdbWriters")
	    (dotimes (i (size pdbWriters)) (chem:close (get pdbWriters i)))
	    (print "---> Closed all PdbWriters")
	    )
	  )
	)
      )
    )
  )


(defun extract-all-dynamics (args)
  (database (seedDatabaseName) true)
  (let ((gaff (loadArchive (% "%s/gaff.cxml" (databaseDir))))
	(jobs (gatherTrainers args)))
    (dolist (job jobs)
      (prognLog (% "Job = %s" (asString job))
		(let ((trainer (readTrainer job)))
		  (extract-dynamics-for-trainer :trainer trainer )
		  (chem:write trainer)
		  )
		))
    )
  )




(extract-all-dynamics *ARGS*)