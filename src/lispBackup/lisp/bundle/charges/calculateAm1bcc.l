
(include "CANDO/charges/mopacInterface.l")
(include "CANDO/charges/am1bcc.l")
(include "CANDO/db/trainer.l")





(defun gather-am1bcc-charges-for-trainer (trainer)
  "Return a map of Atom to a Cons of am1bcc charges for each entry"
  (let* ((mopacInterface (trainer-mopac-interface trainer))
	 (conformations (getConformationExplorer trainer))
	 (matter (getMatter conformations))
	 (gatheredChargeMap (ObjectMap))
	 (atomList (allAtomsAsCons matter false))
	 )
    (for entry (entriesAsCons conformations)
	 (let* ((am1Stage (if (hasEntryStageWithName entry "Am1")
			      (getEntryStage entry "Am1")
			    (raise (LispError "There must be an Am1 stage")))
			  )
		(am1Charges (lookup (getBinder am1Stage) 'am1Charges))
		(bccCorrections (trainer-bcc-corrections trainer)))
	   (for a atomList
		(let* ((am1Charge (lookup am1Charges a))
		       (bccCorrection (lookup bccCorrections a))
		       (am1bccCharge (+ am1Charge bccCorrection))
		       (chargeCons (if (contains gatheredChargeMap a)
				      (lookup gatheredChargeMap a)
				     ())))
		  (setq chargeCons (Cons am1bccCharge chargeCons))
		  (extend gatheredChargeMap a chargeCons)
		))))
    gatheredChargeMap))

(defun calculate-average-am1bcc-charges (atomToChargeConsMap)
  "Given a map of atoms to Cons of charges return a map of atoms to average charge"
  (let* ((atoms (keys-as-cons atomToChargeConsMap))
	 (numberOfCharges (length (lookup atomToChargeConsMap (car atoms))))
	 (atomToAverageCharge (ObjectMap)))
    (for a atoms
	 (let ((sum 0.0)
	       (chargeCons (lookup atomToChargeConsMap a)))
	   (for c chargeCons
		(setq sum (+ sum c)))
	   (extend atomToAverageCharge a (/ sum numberOfCharges))))
    atomToAverageCharge))




(defun calculate-stddev-am1bcc-charges (atomToChargeConsMap atomToAverageCharge)
  (let* ((atoms (keys-as-cons atomToChargeConsMap))
	 (numberOfCharges (length (lookup atomToChargeConsMap (car atoms))))
	 (atomToStdDevCharge (ObjectMap)))
    (for a atoms
	 (let ((sum 0.0)
	       (chargeCons (lookup atomToChargeConsMap a))
	       (average (lookup atomToAverageCharge a)))
	   (for c chargeCons
		(setq sum (+ sum (let ((diff (- c average)))
				   (* diff diff)))))
	   (extend atomToStdDevCharge a (sqrt (/ sum numberOfCharges)))))
    atomToStdDevCharge))



(defun calculate-focus-residue-am1bcc-charges (trainer atomToAm1bccChargeMap)
  (let* ((focusResidue (trainer-focus-residue trainer))
	 (focusAtoms (allAtomsAsCons focusResidue false))
	 (numberOfFocusAtoms (length focusAtoms))
	 (nonFocusAtoms (let* ((conformations (getConformationExplorer trainer))
			       (matter (getMatter conformations))
			       (allAtomsSet (ObjectSet (allAtomsAsCons matter false)))
			       (focusAtomSet (ObjectSet focusAtoms))
			       (unfocusedAtomSet (relativeComplement allAtomsSet focusAtomSet)))
			  (assert (== (size allAtomsSet) (+ (size focusAtomSet) (size unfocusedAtomSet)))
				  (% "Mis-match between all atoms and focus/non-focus atoms"))
			  (asCons unfocusedAtomSet)))
	 (numberOfNonFocusAtoms (length nonFocusAtoms))
	 (focusStereoisomerName (trainer-header-focus-stereoisomer-name (getHeader trainer)))
	 (focusStereoisomer (getEntity *DATABASE* focusStereoisomerName))
	 (focusConstitution (constitution focusStereoisomer))
	 (focusTopologyName (trainer-header-focus-topology-name (getHeader trainer)))
	 (focusTopology (topologyWithName focusConstitution focusTopologyName))
	 (targetTotalCharge (getResidueNetCharge focusTopology))
	 (totalCharge 0.0))
    (for a focusAtoms
	 (let ((charge (lookup atomToAm1bccChargeMap a)))
	   (setq totalCharge (+ totalCharge charge))))
    (let* ((focusResidueChargeCorrection (- targetTotalCharge totalCharge))
	   (focusCorrection (/ focusResidueChargeCorrection numberOfFocusAtoms))
	   (correctedFocusCharges (ObjectMap)))
      (print (% "Applying charge correction of %f electrons to all %d focus atoms" focusCorrection numberOfFocusAtoms ))
      (assert (< (abs focusResidueChargeCorrection) 0.2) (% "The charge correction %f is too large!!!" focusResidueChargeCorrection ))
      (for a focusAtoms
	   (let* ((charge (lookup atomToAm1bccChargeMap a))
		  (correctedCharge (+ charge focusCorrection)))
	     (extend correctedFocusCharges a correctedCharge)))
      (trainer-focus-residue-charges trainer correctedFocusCharges)
      (chem:write trainer)
      correctedFocusCharges)))









(include "CANDO/db/jobSetup.l")
(database (seedDatabaseName) true)
(let ((jobs (gatherTrainers *ARGS*)))
  (for j jobs
       (print (% "Assigning am1bcc charges for trainer: %s" j))
       (let* ((trainer (readTrainer j))
	      (structures (getConformationExplorer trainer))
	      (matter (getMatter structures))
	      (bccCorrections (calculate-am1-bcc-corrections matter)))
	 (apply-bcc-corrections-to-trainer trainer bccCorrections)
	 (let* ((chargeCons (gather-am1bcc-charges-for-trainer trainer))
		(averages (calculate-average-am1bcc-charges chargeCons))
		(stdevs (calculate-stddev-am1bcc-charges chargeCons averages))
		(totalAverage 0.0))
	   ; Apply the am1bcc charges in averages to the atoms
	   (for a (allAtomsAsCons matter false)
		(let ((charge (lookup averages a)))
		  (setCharge a charge)))
	   (chem:write trainer)
	   (for a (keys-as-cons chargeCons)
		(let ((charge (lookup averages a)))
		  (setq totalAverage (+ totalAverage charge))
		  (print (% "Atom[%s] -----------" (chem:getName a)))
		  (print (% "    all charges[%s]" (repr (lookup chargeCons a))))
		  (print (% "    average charge[%f]" charge))
		  (print (% "    stdev[%f]" (lookup stdevs a))))
		)
	   (print (% "=========== TOTAL AVERAGE CHARGE: %f" totalAverage))
	   (print "============ Looking at FocusResidue ==============")
	   (setq totalAverage 0.0)
	   (let* ((focusResidue (trainer-focus-residue trainer))
		  (correctedChargeMap (calculate-focus-residue-am1bcc-charges trainer averages))
		  (focusAtoms 0))
	     (for a (allAtomsAsCons focusResidue false)
		  (let ((charge (lookup correctedChargeMap a)))
		    (setq totalAverage (+ totalAverage charge))
		    (print (% "Atom[%s] -----------" (chem:getName a)))
		    (print (% "    all charges[%s]" (repr (lookup chargeCons a))))
		    (print (% "    average charge[%f]" charge))
		    (print (% "    stdev[%f]" (lookup stdevs a))))
		  (setq focusAtoms (+ focusAtoms 1))
		  )
	     (print (% "=========== FOCUS_RESIDUE ATOMS[%d] TOTAL AVERAGE CHARGE: %f" focusAtoms totalAverage))
	     )))))

