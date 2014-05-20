

(defClass DynamicsCheckpoint ()
  (positions
   velocities
   time
   filePos)
  )

(defMethod __init__ DynamicsCheckpoint (self &key cluster context time (filePos 0))
  (let ((state (omm:getState context (logior omm:Positions omm:Velocities) false)))
    (let ((positions (omm:getPositions state))
	  (velocities (omm:getVelocities state)))
      (setq (slot self positions) positions)
      (setq (slot self velocities) velocities)
      (setq (slot self time) time)
      (setq (slot self filePos) filePos)
      )
    )
  )

(defMethod checkpoint-positions DynamicsCheckpoint (self) (slot self positions))
(defMethod checkpoint-velocities DynamicsCheckpoint (self) (slot self velocities))
(defMethod checkpoint-velocities DynamicsCheckpoint (self) (slot self velocities))
(defMethod checkpoint-time DynamicsCheckpoint (self) (slot self time))
(defMethod checkpoint-filepos DynamicsCheckpoint (self) (slot self filePos))


(defun save-checkpoint (&key cluster context (time 0) (filePos 0))
  (let ((checkpoint (DynamicsCheckpoint :cluster cluster :context context
				    :time time :filePos filePos))
	(savePath (cluster-checkpoint-path cluster)))
    (saveArchive checkpoint savePath)
    )
  )


(defun load-checkpoint (&key cluster)
  (loadArchive (asString (cluster-checkpoint-path cluster))))




(defun save-coordinates (&key cluster context (time 0) filePos)
  "Save the coordinates in the coordinates file and return the position in
the file where the next coordinates will be written"
  (let ((fout (FileOutStream (asString (cluster-coords-path cluster))))
	(state (omm:getState context omm:Positions false))
	(returnPos 0)
	)
    (let ((positions (omm:getPositions state)))
      (seekp fout filePos)
      (write-to-stream positions (% "time[%d]" time) fout))
    (setq returnPos (tellp fout))
    (close fout)
    returnPos
    )
  )
	


(defun restore-context (&key cluster)
  "Restore the context for a dynamics run and return it and the checkpoint"
  (let ((systemPath (cluster-system-path cluster))
	(checkpointPath (cluster-checkpoint-path cluster)))
    (let ((system (omm:system-deserialize systemPath))
	  (checkpoint (load-checkpoint :cluster cluster)))
      (let ((context (setup-context :system system
				    :integrator (default-integrator)
				    :platform (default-platform)
				    :positions (checkpoint-positions checkpoint)
				    :velocities (checkpoint-velocities checkpoint)
				    :time (checkpoint-time checkpoint))))
	(values context checkpoint))
      )
    )
  )




(defun setup-system-and-positions (&key matter forceField (verbose false))
  "Setup the system and the coordinates for the system.
return -> (values system coordinates) "
  (let* ((sys (omm:System))
	 (positions (units:CoordinateArray () :nanometers))
	 (nonbond (omm:NonbondedForce sys)))
    (for a (atoms matter)
      (when verbose (print (% "Adding atom[%s]" (chem:atomName a))))
      (omm:addCandoAtom sys a)
      (units:coordinate-array-append positions (units:Xyz (chem:getPosition a) :angstroms))
      (omm:addNonbondedAtom nonbond a forceField))
    (let* ((bonds (allBondsAsCons matter true))
	   (stretchForce (omm::HarmonicBondForce sys)))
      (for b bonds
	   (when verbose (print (% "Adding bond[%s]" (repr b))))
	   (omm:addCandoBond stretchForce b forceField)))
    (let* ((angles (allAnglesAsCons matter true))
	   (angleForce (omm::HarmonicAngleForce sys)))
      (for ang angles
	   (when verbose (print (% "Adding angle[%s]" (repr ang))))
	   (omm:addCandoAngle angleForce ang forceField)))
    (let* ((ptors (allProperTorsionsAsCons matter true))
	   (ptorForce (omm::PeriodicTorsionForce sys)))
      (for ang ptors
	   (when (omm:addCandoProperTorsions ptorForce ang forceField)
	     (when verbose (print (% "Added ptor[%s]" (repr ang)))))))
    (let* ((itors (allImproperTorsionsAsCons matter true))
	   (itorForce (omm::PeriodicTorsionForce sys)))
      (when verbose (print (% "There are %d improper torsions" (length itors))))
      (for ang itors
	   (when (omm:addCandoImproperTorsions itorForce ang forceField)
	     (when verbose (print (% "Added itor[%s]" (repr ang)))))))
    (when verbose
      (print (% "Nonbonded force = %d" (omm:getForce sys 0)))
      (let ((numForces (omm:getNumForces sys)))
	(print (% "Number of forces added: %d" numForces))))
    (values sys positions) ))




(defun setup-context (&key system integrator platform positions velocities time (verbose false))
  (let ((context (omm:Context :system system
			      :integrator integrator
			      :platform platform)))
    (when time
      (omm::setTime context (* 1.0 time)))
    (when positions
      (omm:setPositionsFromCoordinateArray context positions))
    (when velocities
      (omm:setVelocitiesFromCoordinateArray context velocities))
    context))


  




(defvar *default-omm-platform* ())
(defun initialize-platform ()
  (omm:loadPluginsFromDirectory "/Users/meister/Development/cando/build/cando.app/Contents/Resources/lib/plugins")
  (print (% "There are %d platforms" (omm:getNumPlatforms)))
  (for pi (Range 0 (omm:getNumPlatforms))
       (let ((pl (omm:getPlatform pi)))
	 (print (% "Platform#%d name[%d]    speed[%d]" pi (omm:getName pl) (omm:getSpeed pl)))))
  (setq *default-omm-platform* (omm:getPlatform 0))
  (print (% "Using %s platform -----------" (omm:getName *default-omm-platform*)))
)

(defun default-platform ()  *default-omm-platform*)


(defun default-integrator () (omm::LangevinIntegrator :temperature 300.0))



(initialize-platform)



(defun save-system (system systemFile)
  "Save the system in a XmlSerialization file"
  (omm::system-serialize system systemFile))


(defun load-system (systemFile)
  "Load the system from an XmlSerialization file"
  (omm::system-deserialize systemFile)
  )


	







(when (isTopLevelScript)
  (defvar *gaff* (loadArchive (% "%s/gaff.cxml" (databaseDir))))
  (defvar *agg* (loadArchive "_start.cxml"))
  (assignTypes *gaff* *agg*)
  (let* ((platform *default-omm-platform*)
	 (context (setup-context :matter *agg*
				 :forceField *gaff*
				 :integrator (omm:LangevinIntegrator :temperature 300.0 )
				 :platform platform))
	 (integrator (omm:getIntegrator context))
	 (pdbWriter (PdbWriter)))
  (chem:open pdbWriter "_dynamics.pdb")
  (for m (Range 0 10)
       (writeModel pdbWriter *agg* m)
       (print (% "Running dynamics for step: %d" m))
       (omm:step integrator 10000)
       (let ((state (omm:getState context omm:Positions false )))
	 (omm:updateCandoMatterWithPositions state *agg*)
	 ))
  (print "Done")))
