					; fixed code
(defvar *verbose1* true )		;
					; Iterates through all of the possible parts of an oligomer that
					; are connected through a single plug to the focusMonomer
					;


(defClass TopologyBranch ()(_Plug _Mates _Overflow _RepresentedEntityIndex _RepresentedEntityNameSets ))

(defMethod __init__ TopologyBranch (self aPlug )
  (setq (slot self _Plug ) aPlug )
  (setq (slot self _Mates ) ())
  (setq (slot self _Overflow ) ())
  (setq (slot self _RepresentedEntityIndex ) ())
  (setq (slot self _RepresentedEntityNameSets ) ())
  (setupTopologyBranch self ))
  
(defMethod repr TopologyBranch (self )
  (let ((s (% "Branch: plug[%s]" (chem:getName (getPlug (slot self _Plug ))))))
    (for n (asCons (slot self _RepresentedEntityNameSets ))
	 (setq s (+ s (% "\n      %s" (repr n)))))
    s ))
  
(defMethod getPlug TopologyBranch (self )
  (slot self _Plug ) )

;;
;; This is where we define the list of represented RepresentedEntityNameSets that
;; will be iterated over for this branch.
;;
(defMethod setupTopologyBranch TopologyBranch (self )
  (setq (slot self _Mates ) (matesAsCons (getPlug (slot self _Plug ))))
  (when (== (length (slot self _Mates )) 0 )
    (raise (% "There must be at least one mate for %s" (description (slot self _Plug )))))
  (print (slot self _Mates))
  (prognLog "In resetTopologyBranch - Dumping _Mates"
	    (for zz (slot self _Mates)
		 (log (% "matesAsCons[%s] class[%s]" (repr zz) (className zz )))))
  (let ((representedEntityNameSets (ObjList)))
    (for mate (slot self _Mates)
	 (let ((listOfRepresentativeEntityNameSets (minimalRepresentativeList mate )))
	   (extend representedEntityNameSets listOfRepresentativeEntityNameSets)))
    (setq (slot self _RepresentedEntityNameSets) representedEntityNameSets))
  (restartTopologyBranchCounting self)
  )

(defMethod restartTopologyBranchCounting TopologyBranch (self)
  (setq (slot self _RepresentedEntityIndex) 0 )
  (setq (slot self _Overflow) false))
  
(defMethod increment TopologyBranch (self )
  (when (slot self _Overflow ) (return-from increment ))
  (setq (slot self _RepresentedEntityIndex ) (+ (slot self _RepresentedEntityIndex )1 ))
  (when (>= (slot self _RepresentedEntityIndex ) (size (slot self _RepresentedEntityNameSets )))
	  (setq (slot self _Overflow) true )))


(defMethod getBranchMonomerName TopologyBranch (self )
  (when (>= (slot self _RepresentedEntityIndex ) (size (slot self _RepresentedEntityNameSets )))
    (raise (LispError (% "You tried to go beyond the end of the RepresentedEntityNameSets[%d]" (slot self _RepresentedEntityIndex)))))
  (log (% "(slot self _RepresentedEntityIndex) = %d" (slot self _RepresentedEntityIndex) ))
  (let* ((grp (get (slot self _RepresentedEntityNameSets ) (slot self _RepresentedEntityIndex) ))
	(representative (getRepresentative grp)))
    (log (% "group type = %s" (repr grp )))
    (log (% "group = %s" (description grp )))
    (log (% "representative = %s" representative ))
    representative ))

(defMethod getBranchRepresentedEntityNameSet TopologyBranch (self )
  (let (res )
    (ifTrue (>= (slot self _RepresentedEntityIndex )(size (slot self _RepresentedEntityNameSets )))
	    (raise "Ran out of representations" ))
    (setq res (get (slot self _RepresentedEntityNameSets )(slot self _RepresentedEntityIndex )))
    res ))
  
(defMethod overflowed TopologyBranch (self)
  (slot self _Overflow ))


;; end





;;
;; Iterates over all training oligomers centered on one stereoisomer of one topology
;;
(defClass OligomerIterator ()(_Database _Constitution _Stereoisomer _Topology _Branches _Overflow _NameIndex ))

(defMethod __init__ OligomerIterator (self db aConstitution aStereoisomer aTopology )
  (log "Initializing OligomerIterator" )
  (setq (slot self _Database )db )
  (setq (slot self _Constitution ) aConstitution )
  (setq (slot self _Stereoisomer ) aStereoisomer )
  (setq (slot self _Topology ) aTopology )
  (log (% "About to iterate over topology plugs there should be[%d]" (numberOfPlugs aTopology )))
  (setq (slot self _Branches )
	(map (lambda (plug )
	       (prognLog (% "Creating branch over plug: %s" (repr plug))
			 (TopologyBranch plug )))
	     (plugsWithMatesAsCons aTopology )))
  (log (% "There are %d branches and they are: " (length (slot self _Branches ))))
  (prognDebug 
   (for zn (slot self _Branches )
	(log (% "BRANCH  %s" (repr zn )))))
  (setq (slot self _Overflow )false ))

  
(defMethod completeBranchWithCoupling OligomerIterator (self oligomer neighborMon existingPlugName focusMon capPrefix )
  (prognLog
   (% "branch[%s-%s]|" existingPlugName (chem:getName neighborMon ))
   (let (constitution topology plug capMonomerName capMonomer capName capCoupling capPlugName capPrefixPrefix )
     (log (% "completeBranchWithCoupling>> existingPlugName=%s" existingPlugName ))
     (log (% "Complete branch for neighborMon: %s" (description neighborMon )))
     (log (% "%s: completeBranchWithCoupling" capPrefix ))
     (setq constitution (constitutionForNameOrPdb (slot self _Database )(chem:getName neighborMon )))
     (ifTrue (isNil constitution)
	     (return-from completeBranchWithCoupling false ))
     (setq topology (simplestTopologyWithPlugNamed constitution existingPlugName ))
     (ifTrue (isNil topology)
	     (log "ERROR" )
	     (log (% "%s: :ERROR Could not find topology in constitution[%s] with Plug named[%s] requested by[%s]"
		     capPrefix (constitutionName constitution )existingPlugName (chem:getName focusMon )))
	     (log "Constitution:" )
	     (log (asXmlString constitution ))
	     (return-from completeBranchWithCoupling false ))
     (for reqplug (plugsWithMatesAsCons topology )
	  (block continue
		 (setq plug (getPlug reqplug ))
		 (if (== (chem:getName plug )existingPlugName )
		     (then 
		      (log (% "%s: Ignoring existing plug[%s]" capPrefix (chem:getName plug )))
		      (return-from continue ))
		   (else 
		    (log (% "%s: capping plug[%s]" capPrefix (chem:getName plug ))) ; find the first mate of the plug
		    ))
		 (setq capMonomerName (getCap (@ (matesAsCons plug )0 )))
		 (setq capMonomer (MultiMonomer ))
		 (setq capName (asSymbol (% "%s%d" capPrefix (slot self _NameIndex ))))
		 (log (% "%s: :capName %s  :capMonomerName %s" capPrefix capName capMonomerName ))
		 (setId capMonomer capName )
		 (setq (slot self _NameIndex )(+ (slot self _NameIndex )1 ))
		 (setGroupName capMonomer capMonomerName )
		 (addMonomer oligomer capMonomer )
		 (if (getIsIn plug )
		     (then 
		      (log (% "Prior to coupling neighborMon: %s" (description neighborMon )))
		      (setq capCoupling (couple oligomer capMonomer (chem:getName plug )neighborMon ))
		      (setq capPlugName (getInMonomerPlugName capCoupling )))
		   (else 
		    (setq capCoupling (couple oligomer neighborMon (chem:getName plug )capMonomer ))
		    (setq capPlugName (getOutMonomerPlugName capCoupling )) ;
					; Recursively call completeBranchWithCoupling until everything is capped
					;
		    ))
		 (setq capPrefixPrefix (% "Cap%s" capPrefix ))
		 (assert (notNil capMonomer )"capMonomer is nil" )
		 (completeBranchWithCoupling self oligomer capMonomer capPlugName focusMon capPrefixPrefix )))
     true )))

					; Return None if no oligomer could be built
					;    otherwise return [oligomer,context,focusMonomer]
					;

(defMethod buildSingleTrainingOligomer OligomerIterator (self )
  (setq (slot self _NameIndex )1 )
  (let* ((focusMonomerName (chem:getName (slot self _Stereoisomer )))
	 (olig (Oligomer))
	 (focusMon (MultiMonomer :name focusMonomerName :comment "focus"))
	 (contextRecognized (MonomerContext )))
    (ifTrue *verbose1* 
	    (log (% "buildSingleTrainingOligomer>> focusMonomerName[%s] topology[%s]"
		    focusMonomerName (chem:getName (slot self _Topology )))))
    (setId focusMon 'FOCUS )
    (setFocusFromMonomer contextRecognized focusMon )
    (addMonomer olig focusMon )
    (for b (slot self _Branches )
	 (let* ((innerPlug (getPlug b ))
		(plug (getPlug innerPlug ))
		(neighborMon (MultiMonomer )))
	   (log (% "Attaching to focusMonomer through plug name=%s" (chem:getName plug )))
	   (setId neighborMon (asSymbol (% "neighbor%d" (slot self _NameIndex ))))
	   (setq (slot self _NameIndex )(+ (slot self _NameIndex )1 ))
	   (let ((neighborName (getBranchMonomerName b )))
	     (log (% "neighborName: %s" (repr neighborName )))
	     (when (isNil neighborName)
	       (return-from buildSingleTrainingOligomer ))
	     (setGroupName neighborMon neighborName )
	     (addMonomer olig neighborMon )
	     (let (existingPlugName)
	       (if (getIsIn plug )
		   (let ((coupling (couple olig neighborMon (chem:getName plug )focusMon )))
		     (log "Coupling -->focusMon" )
		     (setq existingPlugName (getInMonomerPlugName coupling )))
		 (let ((coupling (couple olig focusMon (chem:getName plug )neighborMon )))
		   (log "Coupling focusMon-->" )
		   (setq existingPlugName (getOutMonomerPlugName coupling ))))
	       (let* ((branchRepresentedEntityNameSet (getBranchRepresentedEntityNameSet b ))
		      (branchRepresentedEntityNameSetName (chem:getName branchRepresentedEntityNameSet ))
		      (monGroup (RepresentedEntityNameSet :name branchRepresentedEntityNameSetName
					      :optional (getIsRingClosing plug )))
		      plugName )
		 (log (% "branchRepresentedEntityNameSet=%s" (repr branchRepresentedEntityNameSet )))
		 (log (% "branchRepresentedEntityNameSetName=%s" branchRepresentedEntityNameSetName ))
		 (addMonomerNameNoChecks monGroup branchRepresentedEntityNameSetName )
		 (setq plugName (chem:getName plug ))
		 (log (% "About to add neighbor: %s %s" plugName (description monGroup )))
		 (addNeighbor contextRecognized plugName monGroup )
		 (assert (notNil neighborMon )"neighborMon is nil" )
		 (when (not (completeBranchWithCoupling self olig neighborMon existingPlugName focusMon "Cap" ))7
		       (return-from buildSingleTrainingOligomer ())))))))
    (log (% "TrainingOligomer contextRecognized is: %s" (getKey contextRecognized)))
    (let ((focusedOligomer (TrainingOligomer olig contextRecognized focusMon (slot self _Topology ))))
      (log (% "pos1 buildSingleTrainingOligomer returns %s" (repr focusedOligomer )))
      focusedOligomer )))

  
(defMethod buildTrainingOligomer OligomerIterator (self )
  (prognLog
   "Started buildTrainingOligomer"
   (let (btoi res )
     (setq btoi 0 )
     (while (not (overflowed self ))
       (log (% "starting buildTrainingOligomer stage: %d" btoi ))
       (setq res (buildSingleTrainingOligomer self ))
       (log (% "pos2 buildTrainingOligomer stage: %d res = %s" btoi (repr res )))
       (if (isNil res)
	   (then 
	    (log (% "For some reason res is nil"))
	    (increment self ))
	 (else 
	  (return-from buildTrainingOligomer res )))
       (log "incrementing btoi" )
       (setq btoi (+ btoi 1 )))
     (log (% "Dropping out the bottom with btoi=%d" btoi ))
     ())))
  
(defMethod increment OligomerIterator (self )
  (let (idx )
    (ifTrue (overflowed self )
	    (return-from increment ()))
    (setq idx 0 )
    (for br (slot self _Branches )	;removeComment
	 (increment br )
	 (ifTrue (not (overflowed br ))
		 (return-from increment ()))
	 (setq idx (+ idx 1 ))
	 (restartTopologyBranchCounting br ))
    (setq (slot self _Overflow )true )))
  
(defMethod overflowed OligomerIterator (self )
  (slot self _Overflow )  )







(defClass TrainingOligomer ()(_Oligomer _FocusContext _FocusMonomer _Molecule _FocusTopology _ArchivedTrainerHeader ))
(defMethod __init__ TrainingOligomer (self oligomer context monomer topology )
  (setq (slot self _Oligomer )oligomer )
  (setq (slot self _Molecule )())
  (setq (slot self _FocusContext )context )
  (setq (slot self _FocusMonomer )monomer )
  (setq (slot self _FocusTopology )topology )
  (setq (slot self _ArchivedTrainerHeader )()))
(defMethod setMolecule TrainingOligomer (self mol )
  (setq (slot self _Molecule )mol ))
(defMethod getMolecule TrainingOligomer (self )
  (slot self _Molecule ))

(defMethod setArchivedTrainerHeader TrainingOligomer (self header )
  (setq (slot self _ArchivedTrainerHeader )header ))
(defMethod getArchivedTrainerHeader TrainingOligomer (self )
  (slot self _ArchivedTrainerHeader )	;removeComment
  )
(defMethod getOligomer TrainingOligomer (self )
  (slot self _Oligomer )		;removeComment
  )
(defMethod getFocusContext TrainingOligomer (self )
  (slot self _FocusContext )		;removeComment
  )
(defMethod getFocusMonomer TrainingOligomer (self )
  (slot self _FocusMonomer )		;removeComment
  )
(defMethod getFocusTopology TrainingOligomer (self )
  (slot self _FocusTopology )		;removeComment
  )
(defMethod getKey TrainingOligomer (self )
  (let (context )
    (setq context (getFocusContext self ))
    (getKey context )			;
					; Check if this will generate a MonomerCoordinates
					; after everything has been built
					;
    ))
(defMethod checkExtraction TrainingOligomer (self )
  (let (agg monomerCoordinates problems sawProblem )
    (setq agg (Aggregate ))
    (addMatter agg (slot self _Molecule ))
    (setq monomerCoordinates (MonomerCoordinates :context (slot self _FocusContext ))) ; If the following call doesn't fail then extraction will work

    (log "About to call testExtraction" )
    (debugLogOn )
    (setq problems (testExtraction monomerCoordinates (slot self _FocusTopology ) agg (slot self _FocusMonomer )))
    (setq sawProblem (lookup problems :sawProblems ))
    (ifTrue sawProblem 
	    (setq problems (lookup problems :problems ))
	    (log "Problems were identified while running [monomerCoordinates testExtraction] - they are listed below" )
	    (log (str problems ))
	    (log "Exiting from checkExtraction" )
	    (raise (LispError "exiting - check log for problem" )))))
(defMethod getMonomersYouNeed TrainingOligomer (self )
  (let (monomers monomerNames monomerSet )
    (setq monomers (monomersAsCons (slot self _Oligomer )))
    (setq monomerNames (map (lambda (x )(chem:getName x ))monomers ))
    (setq monomerSet (SymbolSet ))
    (insertConsSymbols monomerSet monomerNames )
    monomerSet ))



					;
					;
					; Generate every TrainingOligomer within the seed database and return them as a list
					;
					;


(defun generateAllTrainingOligomers (db )
  (log (% "Iterating over constitutions: %s" (repr (constitutionsAsCons db ))))
  (log (% "Database = %s" (repr db )))
  (let ((trainingOligomers (ObjList)))
    (for constitution (constitutionsAsCons db )
	 (prognLog
	  (% "Iterating over constitution[%s]" (constitutionName constitution))
	  (for stereoisomer (stereoisomersAsCons constitution)
	       (prognLog
		(% "Iterating over stereoisomer[%s]" (chem:getName stereoisomer))
		(dolist (top (topologiesAsCons constitution))
		  (when (not (topology-suppress-trainers top))
		    (prognLog
		     (% "Iterating over Topology[%s]" (chem:getName top))
		     (log "About to create new OligomerIterator" )
		     (let ((oi (OligomerIterator db constitution stereoisomer top))
			   (oligIndex 0 ))
		       (while (not (overflowed oi ))
			 (prognLog
			  (% "Building oligomer %d" oligIndex )
			  (let ((trainingOligomer (buildTrainingOligomer oi )))
			    (log (% "pos4 trainingOligomer = %s" (repr trainingOligomer )))
			    (if (notNil trainingOligomer)
				(then 
				 (log (% "===== focus_Monomer[%s]  topology[%s] " (chem:getName stereoisomer) (chem:getName top)))
				 (log (% ":generateAllTrainingOligomers:%s " (repr (getKey trainingOligomer ))))
				 (let* ((olig (getOligomer trainingOligomer ))
					(mol (getMolecule olig )))
				   (setMolecule trainingOligomer mol ))
				 (append trainingOligomers trainingOligomer ))
				(else 
				 (log "No oligomer generated" )))
			    (increment oi )
			    (setq oligIndex (+ oligIndex 1 )))))))
		    ))))))
    (log (% "Returning %d trainingOligomers" (size trainingOligomers )))
    trainingOligomers ))




(when (isTopLevelScript )
  (let (test )
    (setq test (TrainingOligomer ))
    (print (% "test_TrainingOligomer = %s" (repr test )))
    (exit 1 )
    (ifTrue (!= (length *ARGS* )2 )
	    (print "You must provide a seed database file name" )
	    (exit 1 ))
    (setq seedFileName (@ *ARGS* 1 ))
    (print (% "Loading file: %s" seedFileName ))
    (database seedFileName )
    (setq trainingOligomers (generateAllTrainingOligomers *DATABASE* ))
    (print (% "Number of trainingOligomers = %d " (size trainingOligomers )))
    (for to (entries trainingOligomers )
	 (print (% "trainingOligomer = %s" (repr to )))
	 (print (% "key %s " (getKey to )))
	 (print (% "     focus context: %s" (getKey (getFocusContext to )))))))


