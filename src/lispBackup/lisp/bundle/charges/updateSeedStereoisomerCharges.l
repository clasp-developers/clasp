
(include "CANDO/charges/mopacInterface.l")
(include "CANDO/db/jobSetup.l")


(defun update-stereoisomer-charges-in-database (trainer)
  (print (% "Setting charges for trainer: %s" (asString (getPath (getHeader trainer)))))
  (let* ((focusResidue (trainer-focus-residue trainer))
	 (focusResidueCharges (trainer-focus-residue-charges trainer))
	 (focusAtoms (allAtomsAsCons focusResidue false))
	 (numberOfFocusAtoms (length focusAtoms))
	 (focusStereoisomerName (trainer-header-focus-stereoisomer-name (getHeader trainer)))
	 (focusStereoisomer (getEntity *DATABASE* focusStereoisomerName))
	 (focusConstitution (constitution focusStereoisomer))
	 (focusTopologyName (trainer-header-focus-topology-name (getHeader trainer)))
	 (focusTopology (topologyWithName focusConstitution focusTopologyName))
	 (topologyStereoisomerAtoms (lookupOrCreateStereoisomerAtoms focusTopology focusStereoisomerName))
	 (targetTotalCharge (getResidueNetCharge focusTopology))
	 (constitutionAtoms (getConstitutionAtoms focusConstitution))
	 (totalCharge 0.0))
    (for a focusAtoms
	 (let* ((atomName (atomName a))
		(constitutionAtomIndex (index constitutionAtoms atomName))
		(stereoisomerAtom (atomWithId topologyStereoisomerAtoms constitutionAtomIndex))
		(charge (lookup focusResidueCharges a))
		)
	   (setCharge stereoisomerAtom charge)
	   (setq totalCharge (+ totalCharge charge))))))





(progn
  (print "Updating seed database with am1bcc charges")
  (database (seedDatabaseName) true)
  (let ((jobs (gatherTrainers *ARGS*)))
    (for j jobs
	 (let ((trainer (readTrainer j)))
	   (update-stereoisomer-charges-in-database trainer))
	 )
    )
  (print (% "Saving database: %s" (seedDatabaseName)))
  (saveArchive *DATABASE* (seedDatabaseName)) 
  )

