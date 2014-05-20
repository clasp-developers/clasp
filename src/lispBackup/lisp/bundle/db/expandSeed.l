					; fixed code
(include "CANDO/db/defineScaffolds.l" )
(include "CANDO/db/organizer.l" )
(include "CANDO/db/trainingOligomer.l" ) ;require "CANDO/db/checkMates.l"
(include "CANDO/db/trainer.l")
(include "CANDO/db/atomTree/renderAtomTree.l")
					;
					; number specifies how many trainers to create
					; if None then it creates all of them



(defun updateExistingTrainers (existingOligomers)
  (let ((fileNames (sorted (keysAsCons existingOligomers ))))
    (printPushPrefix  "UpdateExisting: " )
    (for fileName fileNames 
	 (log (% "About to read trainer[%s]" fileName ))
	 (let* ((oneExisting (get existingOligomers fileName ))
		(context (get oneExisting "context" ))
		(trainer (readTrainer fileName ))
		(originalContext (getContext (getHeader trainer ))))
	   (log (% "Rewriting archive trainer: %s" fileName ))
	   (log (% "    Original context: %s" (getKey originalContext )))
	   (log (% "         New context: %s" (getKey context )))
	   (if (!= (getKey originalContext ) (getKey context ))
	       (then 
		(log (% "     updating trainer in fileName: %s" fileName ))
		(log (% "%s" (sequenceAsString olig )))
		(setContext (getHeader trainer )context )
		(writeTrainer fileName trainer ))
	     (else 
	      (log (% "Context is unchanged - not updating trainer in: %s" fileName ))))))
    (printPopPrefix)))







(defun expandSeed (&key databaseName (number -1))
  (prognLog
   "expandSeed"
   (assert (notNil databaseName ) "You must provide a databaseName" )
   (let* ((stage 1 )
	  (numberLeft number)
	  (trainingOligomers (prognLog
			      "Expanding the seed, generating training oligomers"
			      (printPushPrefix "GenerateTrainers: ")
			      (let ((trainOligs (generateAllTrainingOligomers *DATABASE* )))
				(printPopPrefix)
				trainOligs))))
     (prognLog
      "Check if we are missing any monomers"
      (printPushPrefix "CheckMissing: ")
      (let* ((needSet (SymbolSet))
	     (idx 0 )
	     (trainingCons (asCons trainingOligomers ))
	     (maxIdx (length trainingCons )))
	(for f trainingCons 
	     (log (% "Training oligomer %d/%d" idx maxIdx ))
	     (log (% "f = %s" (repr f )))
	     (log (% "class(f) = %s" (className f )))
	     (let ((ms (getMonomersYouNeed f )))
	       (setq needSet (union needSet ms ))
	       (setq idx (+ idx 1 ))))
	(let ((foundError false))
	  (for f (asCons needSet )
	       (when (not (recognizesNameOrPdb *DATABASE* f ))
		 (log (% "This seed database is incomplete, TrainingOligomers need the monomer: %s" f ))
		 (setq foundError true )))
	  (when  foundError 
	    (raise "There were missing monomers in the seed database" ))))
      (printPopPrefix ))
					; Organize the existing trainers
					;

     (let ((numberOfStructures 1 )
	   (newStructuresAdded 0 ))
       (setq stage (+ stage 1))
       (printPushPrefix  "Organize: " )
       (log "Organizing all of the archived trainers" )
       (let ((organizer (assembleOrganizerFromMolTrainers :databaseFileName databaseName :organizerDirectory "./" )))
					; Flag every archived Trainer as being outside of
					; the Trainer space defined by the seed
					; In the next step the ones that are still in will
					; be added back again.
	 (log  "  About to set not part of seed database" )
	 (for org (entriesAsCons organizer )
	      (setNotPartOfSeedDatabase (cadr org )true ))
	 (log (% "    There are %d existing trainers" (numberOfTrainerHeaders organizer )))
	 (when (< (size trainingOligomers )(numberOfTrainerHeaders organizer ))
	   (log (% "WARNING: There are now fewer seed trainers[%d] than there are archived trainers[%d]"
		   (size trainingOligomers )(numberOfTrainerHeaders organizer )))
	   (log  "WARNING:   This means the seed database has contracted in size [number of trainers]." )
	   (log  "WARNING:   Processing will continue but excess archive Trainers should probably be removed" )
	   (log "" ))
	 (printPopPrefix )


	 (setq stage (+ stage 1 ))
	 (printPushPrefix  "ExamineExistingTrainers: " )
	 (log "Assigning archived Trainers to seed Trainers" )
	 (if (== (numberOfTrainerHeaders organizer ) 0)
	     (log "    Skipping stage 3, there are no archived Trainers to assign" )
	   (let ((assigned 0 ))
	     (for trainer (asCons trainingOligomers )
		  (setq context (getFocusContext trainer ))
		  (setq headerThatRecognizesASubsetOfContext (getTrainerHeaderThatRecognizesSubsetOfContext organizer context ))
		  (when (!= headerThatRecognizesASubsetOfContext ())
		    (setNotPartOfSeedDatabase headerThatRecognizesASubsetOfContext false )
		    (setArchivedTrainerHeader trainer headerThatRecognizesASubsetOfContext )
		    (setq assigned (+ assigned 1 ))))
	     (log (% "   There are %d archived trainers assigned to seed Trainers" assigned ))
	     (when (< assigned (numberOfTrainerHeaders organizer ))
	       (log (% "WARNING: Only %d of the %d total archived trainers were assigned to seed trainers" assigned (numberOfTrainerHeaders organizer )))
	       (log  "WARNING:   This means the seed database has contracted in size [number of trainers]." )
	       (log  "WARNING:   Processing will continue but excess archive Trainers should probably be removed" )
	       (log "" ))))
	 (printPopPrefix )



	 (setq stage (+ stage 1 ))
	 (let ((newOligomers ())
	       (existingOligomers (ObjectDictionary )))
	   (prognLog
	    "Archiving seed trainers"
	    (printPushPrefix  "ArchiveSeedTrainers: " )
	    (dolist (trainer (asCons trainingOligomers ))
		 (log (% "===========   trainer = %s" (repr trainer )))
		 (when (== numberLeft 0 )
		   (return-from () ))
		 (setq numberLeft (- numberLeft 1 ))
		 (let* ((olig (getOligomer trainer ))
;			(conformation (kin:Conformation :oligomers (list olig)))
			(context (getFocusContext trainer ))
			(focusMon (getFocusMonomer trainer ))
			(focusTopology (getFocusTopology trainer )))
		   (log (% "Context: %s" (getKey context )))
		   (when (getHasError olig )
		     (raise (% "ERROR: Oligomer has an error!!!: %s" (getErrorMessage olig ))))
		    
					; First check if the organizer already has a trainer with this context
					; If it does then for now do nothing other than update its database
					;

		   (log (% "trainer getArchivedTrainerHeader = %s" (repr (getArchivedTrainerHeader trainer ))))
		   (if (notNil (getArchivedTrainerHeader trainer ))
		       (let ((header (getArchivedTrainerHeader trainer )))
			 (assert (notNil header) "Shit - the header is nil - this shouldn't happen" )
			 (setq fileName (asString (getPath header )))
			 (setq oneExistingOligomer (ObjectDictionary
						    :entries (list (list "trainer" trainer )
								   (list "fileName" fileName )
								   (list "context" context ))))
			 (put existingOligomers fileName oneExistingOligomer )
			 (log (% "Postponed update of existing trainer in file: %s" fileName )))
		     (else 
					; "It's a completely new context, create a new Trainer for it"
		      (log "     The context is new, building Aggregate" )
		      (log (% "%s " (sequenceAsString olig )))
		      (let ((mol (getMolecule trainer ))
			    (focusResidue (getTemporaryResidue focusMon )))
			(log (% "FocusResidue = %s" (chem:getName focusResidue )))
			(randomizeAtomPositions mol )
			(let* ((agg (let ((a (Aggregate)))
				      (addMolecule a mol)
				      a))
			       (structures (ConformationExplorer))
			       (trainer (Trainer))
			       (trainerHeader (TrainerHeader ))
			       (index (getNextUniqueIndex organizer ))
			       (dataDir (getGlobalOption 'trainerDirectory ))
			       )
			  (setUniqueIndex trainerHeader index )
			  (setState trainerHeader "start" )
			  (setContext trainerHeader context )
			  (setTopDirectory trainerHeader dataDir )
			  (setNotPartOfSeedDatabase trainerHeader false )
			  (let ((vstr (chem:getName focusMon )))
			    (trainer-header-focus-stereoisomer-name trainerHeader vstr))
			  (let ((vstr (chem:getName focusTopology )))
			    (trainer-header-focus-topology-name trainerHeader vstr ))
			  (setHeader trainer trainerHeader )
			  (let* ((fnPrefix (getTrainerUniquePathName trainerHeader ))
				 (path (Path "." "data" fnPrefix "/")))
			    (setPath trainerHeader path)
			    (extend (getBinder structures ) 'pathNamePrefix fnPrefix ))
			  (log  "About to set aggregate" )
			  (setMatter structures agg )
			  (setConformationExplorer trainer structures )
			  (log (% "Focus monomer = %s" (description focusMon )))
			  (log  "About to set trainer data" )
			  (trainer-oligomer trainer olig )
			  (trainer-focus-monomer trainer focusMon)
			  (trainer-focus-residue trainer (getTemporaryResidue focusMon))
;			  (setData trainer 'Conformation conformation)
			  (let ((vstr (constitutionName (getConstitution focusMon ))))
			    (trainer-header-focus-constitution-name trainerHeader vstr ))
			  (log (% "Ensuring that directories exist[%s]" (asString (getPath trainerHeader))))
			  (ensure-directories-exist (getPath trainerHeader))
			  (let ((trainerPathName (asString (getPath trainerHeader ))))
			    (log (% "About to write trainer to: %s" trainerPathName )))
			  (log  "     writing oligomer " )
			  (log (% "%s" (sequenceAsString olig )))
			  ;;
			  ;; Write the AtomTree described by this trainer to the trainer directory
			  ;;
			  (render-atom-tree:render-for-trainer trainer)
			  ;;
			  ;; Write out the trainer
			  ;;
			  (chem:write trainer)
			  (log  "About to add trainer header" )
			  (addTrainerHeader organizer trainerHeader )
			  (setq newStructuresAdded (+ newStructuresAdded 1 ))))))
		   (setq numberOfStructures (+ numberOfStructures 1 ))))
	    (printPopPrefix ))
	   (updateExistingTrainers existingOligomers)
	   )




	 (let ((notPartOfSeed 0))
	   (setq stage (+ stage 1 ))
	   (printPushPrefix  "UpdateUnusedExisting: " )
	   (log  "Updating archived Trainers that are not part of the seed" )
	   (for entry (entriesAsCons organizer )
		(when (getNotPartOfSeedDatabase (@ entry 1 ))
		  (let* ((fileName (asString (getPath (@ entry 1 ))))
			 (trainer (readTrainer fileName ))
			 (originalContext (getContext (getHeader trainer ))))
		    (log (% "    Original context: %s" (getKey originalContext )))
		    (log (% "         New context: %s" (getKey context )))
		    (setNotPartOfSeedDatabase (getHeader trainer )true )
		    (writeTrainer fileName trainer )
		    (setq notPartOfSeed (+ notPartOfSeed 1 )))))
	   (log (% "    There were %d archived Trainers that were not part of the seed" notPartOfSeed ))
	   (log  "About to write out TrainerOrganizer" )
	   (writeTrainerOrganizer organizer )
	   (printPopPrefix )
	   )
	 (log "DONE." )			;
					; build organizer
					;
	 )))))




(when (isTopLevelScript )
  (log "Starting expandSeed" )
  (when (!= (length *ARGS* )2 )
    (print "You must provide the seed database name" )
    (exit ))
  (let ((seedDatabase (@ *ARGS* 1 )))
    (database seedDatabase )
    (expandSeed :databaseName seedDatabase :directory "./" :number ())))



