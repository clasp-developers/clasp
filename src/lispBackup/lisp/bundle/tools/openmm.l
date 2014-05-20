


(defun setup-openmm-context (&key integrator platform matter forceField (verbose false))
  (let* ((sys (omm:System))
	 (coords (CoordinateArray))
	 (nonbond (omm:NonbondedForce sys)))
    (for a (atoms matter)
	 (when verbose (print (% "Adding atom[%s]" (chem:atomName a))))
	 (omm:addCandoAtom sys a)
	 (append coords (getPositionInNanometers a))
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
    (let ((context (omm:Context :system sys :integrator integrator :platform platform)))
      (omm:setPositionsFromCoordinateArray context coords)
      context)))





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



(when (isTopLevelScript)
  (defvar *gaff* (loadArchive (% "%s/gaff.cxml" (databaseDir))))
  (defvar *agg* (loadArchive "_start.cxml"))
  (assignTypes *gaff* *agg*)
  (let* ((platform *default-omm-platform*)
	 (context (setup-openmm-context :matter *agg*
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

