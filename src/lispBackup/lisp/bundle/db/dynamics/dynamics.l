
(require "CANDO/db/stochastic.l")
(require "CANDO/db/dynamics/openmm.l")

(defstruct dynamics
  (clusters (ObjList))
  )
	   

(defMethod dynamics-clusters-as-cons dynamics (self)
  (asCons (dynamics-clusters self)))


(defMethod dynamics-add-cluster dynamics (self cluster)
  (setf (dynamics-clusters self) (append (dynamics-clusters self) cluster))
)


(defMethod dynamics-next-cluster-path dynamics (self trainer)
  (let ((path (copyPath (getPath (getHeader trainer)))))
    (append path (% "dynamics%03d" (size (slot self clusters))))))







;;
;; Cluster stages
;; :dynamicsDone


(defstruct cluster
  (dynamics-stage :start)
  conformation-indices
  aggregate
  path
  )


(defun make-cluster-from-conformationExplorer (&key conformationExplorer entries anchors dynamics trainer)
  (prognLog
   "Creating cluster"
   (let ((entry entries conformationIndices))
     (cluster-aggregate self (Aggregate))
     (dotimes (i (length entries))
       (let* ((entry (car entries))
	      (anchor (car anchors))
	      (entryStage (getEntryStage entry *StochasticSearch.StartStageName*))
	      (molecule (getMatter conformationExplorer)))
	 (setq conformationIndices (Cons (getUniqueEntryIndex entry) conformationIndices))
	 (writeCoordinatesToMatter entryStage molecule)
	 (let* ((molCopy (copy molecule))
		(anchorAtom (atomWithAtomId molCopy (AtomId :molecule 0 :residue 0 :atom 0)))
		(anchorAtomPos (chem::getPosition anchorAtom))
		(anchorPos (OVector3 (car anchor) (cadr anchor) (caddr anchor)))
		(offset (sub anchorPos anchorAtomPos))
		(restraintAnchor (RestraintAnchor)))
	   (log (% "Molecule anchor atom started at: %s" anchorAtomPos ))
	   (log (% "Should be anchored at: %s" anchorPos))
	   (log (% "Offset: %s" offset ))
	   (translateAllAtoms molCopy offset)
	   (setAnchorPos restraintAnchor anchorPos)
	   (setWeight restraintAnchor 10.0)
	   (setAtom restraintAnchor anchorAtom)
	   (matter-add-restraint anchorAtom restraintAnchor)
	   (addMatter (cluster-aggregate self) molCopy)
	   )
	 )
       (setq entries (cdr entries))
       (setq anchors (cdr anchors))
       )
     (make-cluster :path (dynamics-next-cluster-path dynamics trainer)
			    :conformationIndices conformationIndices
			    :aggregate cluster-aggregate )
     )
   )
  )



(defMethod cluster-set-stage (cluster stageVal)
  (unless (or
	   (== stageVal :not-set-up)
	   (== stageVal :running)
	   (== stageVal :done))
    (error (LispError (% "Illegal cluster stage[%s]" stageVal))))
  (setf (cluster-dynamics-stage cluster stageVal)))


(defMethod cluster-system-path cluster (self)
  "Return the path of the serialized system for this cluster"
  (let ((path (cluster-path self)))
    (append (copyPath path) "system.xml")))


(defMethod cluster-structure-path cluster (self)
  "Return the path of the structure for this cluster"
  (let ((path (cluster-path self)))
    (append (copyPath path) "structure.cxml")))

(defMethod cluster-checkpoint-path cluster (self)
  "Return the path of the checkpoint for this cluster"
  (let ((path (cluster-path self)))
    (append (copyPath path) "checkpoint.cxml")))

(defMethod cluster-coords-path cluster (self)
  "Return the path of the dynamics coordinates for this cluster"
  (let ((path (cluster-path self)))
    (append (copyPath path) "coords.dat")))




(defMethod cluster-write cluster (self &key forceField)
  (prognLog "cluster-write"
	    (log (% "I am creating a directory for the cluster at: %s" (cluster-path self)))
	    (createDirectory (cluster-path self))
	    )
  )

;; (defMethod cluster-generate-openmm-system (self forceField 

