
(require "CANDO/db/jobSetup.l")
(require "CANDO/db/trainer.l")
(require "CANDO/db/organizer.l")
(require "CANDO/db/dynamics/dynamics.l")



(defparameter *molecules-in-a-row* 2)
(defparameter *molecules-per-cluster* (* *molecules-in-a-row*
					 *molecules-in-a-row*
					 *molecules-in-a-row* ))
(defparameter *molecule-separation* 50.0 )


(defun split-list (cur num)
  (if (<= (length cur) num)
      (Cons cur ())
      (let (clusterList)
	(dotimes (i num)
	  (setq clusterList (Cons (car cur) clusterList))
	  (setq cur (cdr cur)))
	(Cons clusterList cur)
	)))



(defun cluster-anchors ( &key numClusters separation )
  "Return a list of 3 element lists that represent the x,y,z anchors
of each member of the cluster" 
  (if (eql numClusters 1)
      '( (0.0 0.0 0.0) )
      (let* ((num (floor (+ (expt (* 1.0 numClusters) 0.33333333333333) 1.0)))
	     (corner (- (* (- num 1) (/ separation 2.0))))
	     centers)
	(dotimes (ix num)
	  (dotimes (iy num)
	    (dotimes (iz num)
	      (setq centers (Cons (list (+ (* ix separation) corner)
					(+ (* iy separation) corner)
					(+ (* iz separation) corner)) centers))
	      )))
	centers)))
	      



(defun break-list-into-clusters (entries clusterSize)
  "Return a list of lists of entries that contain a maximum of *molecules-per-cluster*
entries."
  (let (clusteredEntries)
    (while entries
      (let* ((splitEntries (split-list entries clusterSize))
	     (oneClusterEntries (car splitEntries)))
	(setq clusteredEntries (Cons oneClusterEntries clusteredEntries))
	(setq entries (cdr splitEntries))))
    clusteredEntries))






(defun create-new-clusters-for-entry-ids (&key conformationExplorer entries dynamics trainer forceField)
  (prognLog (% "in create-new-clusters-for-entry-ids - there are %d entries" (length entries))
	    (let ((clusteredEntries (break-list-into-clusters entries *molecules-per-cluster*))
		  (anchors (cluster-anchors :numClusters *molecules-per-cluster*
					    :separation *molecule-separation*)))
	      (dolist (clusterEntries clusteredEntries)
		(log (% "Creating a new cluster"))
		(let ((cluster (Cluster :conformationExplorer conformationExplorer
					:entries clusterEntries
					:anchors anchors
					:dynamics dynamics
					:trainer trainer)))
		  (dynamics-add-cluster dynamics cluster)
		  (cluster-write cluster :forceField forceField)
		  (log (% "Setup a cluster with %d entries" (length clusterEntries)))
		  )
		))
	    ))




		   ;; (if (trainer-dynamics trainer)
		   ;;     (trainer-dynamics trainer)
		   ;;     (trainer-dynamics trainer (Dynamics))))


(defun setup-dynamics-clusters-for-trainer (&key trainer forceField reset)
  (prognLog "setup-dynamics-clusters-for-trainer"
	    (print "!!!!! CURRENTLY Dynamics objects are being initialized everytime setupDynamics is run!!!!!")
	    (let* ((dynamics 
		    (if (or reset (not (trainer-dynamics trainer)))
		        (trainer-dynamics trainer (Dynamics))
		        (trainer-dynamics trainer)
			))
		   (existingClusters (dynamics-clusters-as-cons dynamics))
		   (entryIdsInExistingClusters (let ((ids (ObjectSet)))
						 (dolist (cluster existingClusters)
						   (addObjectsInCons ids (cluster-conformation-indices cluster)))
						 ids))
		   (conformationExplorer (getConformationExplorer trainer))
		   (molecule (getMatter conformationExplorer))
		   (entries (entriesAsCons conformationExplorer))
		   (entriesNotInClusters (let (notInClusters)
					   (dolist (entry entries)
					     (let ((entryId (getUniqueEntryIndex entry)))
					       (unless (contains entryIdsInExistingClusters entryId)
						 (setq notInClusters (Cons entry notInClusters)))))
					   notInClusters))
		   )
	      (log (% "There are %d existing clusters and %d entries in the ConformationExplorer"
		      (length existingClusters)
		      (length entries)
		      ))
	      (create-new-clusters-for-entry-ids :entries entries
						 :conformationExplorer conformationExplorer
						 :dynamics dynamics
						 :trainer trainer
						 :forceField forceField)
	      (trainer-dynamics trainer dynamics)
	      )
	    (chem:write trainer)
	    )
  )
	

(defun setup-all-clusters (args)
  "Setup clusters for trainers, do not overwrite existing clusters"
  (database (seedDatabaseName) true)
  (let ((gaff (loadArchive (% "%s/gaff.cxml" (databaseDir))))
	(jobs (gatherTrainers args)))
    (dolist (job jobs)
      (prognLog (% "Job = %s" (asString job))
		(let ((trainer (readTrainer job)))
		  (setup-dynamics-clusters-for-trainer :trainer trainer :forceField gaff )
		  (chem:write trainer)
		  )
		))
    )
  )


(defun reset-all-clusters (args)
  "Rebuild all of the clusters for the trainers"
  (database (seedDatabaseName) true)
  (let ((gaff (loadArchive (% "%s/gaff.cxml" (databaseDir))))
	(jobs (gatherTrainers args)))
    (dolist (job jobs)
      (prognLog (% "Job = %s" (asString job))
		(let ((trainer (readTrainer job)))
		  (setup-dynamics-clusters-for-trainer :trainer trainer :forceField gaff :reset true )
		  (chem:write trainer)
		  )
		))
    )
  )



(setup-all-clusters *ARGS*)