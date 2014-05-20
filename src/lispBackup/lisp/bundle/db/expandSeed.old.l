					; fixed code
(include "CANDO/db/defineScaffolds.l" )
(include "CANDO/db/organizer.l" )
(include "CANDO/db/trainingOligomer.l" ) ;require "CANDO/db/checkMates.l"
					;
					; number specifies how many trainers to create
					; if None then it creates all of them

(defun expandSeed (&key (databaseName ())(directory "./" )(number ()))
  (prognLog
   "expandSeed"
   (let (stage numberLeft db trainingOligomers needSet idx trainingCons
	       maxIdx ms foundError numberOfStructures newStructuresAdded
	       numberOfStochasticSearches assigned context
	       headerThatRecognizesASubsetOfContext trainer
	        numberLeft olig context focusMon
	       focusTopology header fileName oneExistingOligomer
	        mol agg trainerHeader index
	       dataDir vstr structures fnPrefix
	       trainerFileName newStructuresAdded
	       oneExisting originalContext organizer
	       focusResidue fileNames notPartOfSeed )
     (assert (notNil databaseName )"You must provide a databaseName" )
     (setq stage 0 )
     (setq numberLeft -1 )
     (when (!= number ())
       (setq numberLeft number ))
     (log (% "Database directory: %s" directory ))
     (log "Reading MoleculeTrainerOrganizer" )
     (setq db *DATABASE* )
     (when (== db ())
       (raise "The database is nil, you must define a CandoDatbase" ))
     (setq stage (+ stage 1 ))
     (printPushPrefix "GenerateTrainers: " )
     (prognLog "Expanding the seed, generating training oligomers"
	       (setq trainingOligomers (generateAllTrainingOligomers db ))
	       (log (% "    Generated %d TrainingOligomers" (size trainingOligomers )))
	       )
     (printPopPrefix)
     (printPushPrefix "CheckMissing: ")
     (prognLog
      "Check if we are missing any monomers"
      (setq needSet (SymbolSet))
      (setq idx 0 )
      (setq trainingCons (asCons trainingOligomers ))
      (setq maxIdx (length trainingCons ))
      (for f trainingCons 
	   (log (% "Training oligomer %d/%d" idx maxIdx ))
	   (log (% "f = %s" (repr f )))
	   (log (% "class(f) = %s" (className f )))
	   (setq ms (getMonomersYouNeed f ))
	   (setq needSet (union needSet ms ))
	   (setq idx (+ idx 1 )))
      (setq foundError false )
      (for f (asCons needSet )
	   (when (not (recognizesNameOrPdb db f ))
	     (log (% "This seed database is incomplete, TrainingOligomers need the monomer: %s" f ))
	     (setq foundError true )))
      (when  foundError 
	(raise "There were missing monomers in the seed database" )))
     (printPopPrefix )			;
					; Organize the existing trainers
					;

     (setq stage (+ stage 1 ))
     (printPushPrefix  "Organize: " )
     (log "Organizing all of the archived trainers" )
     (setq organizer (assembleOrganizerFromMolTrainers :databaseFileName databaseName :organizerDirectory "./" )) ;
					; Flag every archived Trainer as being outside of
					; the Trainer space defined by the seed
					; In the next step the ones that are still in will
					; be added back again.
					;

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
     (----------
     (setq numberOfStructures 1 )
     (setq newStructuresAdded 0 )
     (setq numberOfStochasticSearches 0 )
     (printPopPrefix )
     (setq stage (+ stage 1 ))
     (printPushPrefix  "ExamineExistingTrainers: " )
     (log "Assigning archived Trainers to seed Trainers" )
     (if (== (numberOfTrainerHeaders organizer )0 )
	 (then 
	  (log "    Skipping stage 3, there are no archived Trainers to assign" ))
       (else 
	(setq assigned 0 )
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
	(for trainer (asCons trainingOligomers )
	     (log (% "===========   trainer = %s" (repr trainer )))
	     (when (== numberLeft 0 )
	       (return-from () ))
	     (setq numberLeft (- numberLeft 1 ))
	     (setq olig (getOligomer trainer ))
	     (setq context (getFocusContext trainer ))
	     (log (% "Context: %s" (getKey context )))
	     (setq focusMon (getFocusMonomer trainer ))
	     (setq focusTopology (getFocusTopology trainer ))
	     (when (getHasError olig )
	       (raise (% "ERROR: Oligomer has an error!!!: %s" (getErrorMessage olig ))) ;
					; First check if the organizer already has a trainer with this context
					; If it does then for now do nothing other than update its database
					;
	       )
	     (log (% "trainer getArchivedTrainerHeader = %s" (repr (getArchivedTrainerHeader trainer ))))
	     (if (!= (getArchivedTrainerHeader trainer )())
		 (then 
		  (setq header (getArchivedTrainerHeader trainer ))
		  (assert (!= header ())"Shit" )
		  (setq fileName (asString (getPath header )))
		  (setq oneExistingOligomer (ObjectDictionary
					     :entries
					     (list (list "trainer" trainer )
						   (list "fileName" fileName )
						   (list "context" context ))))
		  (put existingOligomers fileName oneExistingOligomer )
		  (log (% "Postponed update of existing trainer in file: %s" fileName )))
	       (else 
					; "It's a completely new context, create a new Trainer for it"
		(log "     The context is new, building Aggregate" )
		(log (% "%s " (sequenceAsString olig )))
		(setq mol (getMolecule trainer ))
		(setq focusResidue (getTemporaryResidue focusMon ))
		(log (% "FocusResidue = %s" (chem:getName focusResidue )))
		(randomizeAtomPositions mol )
		(setq agg (Aggregate ))
		(addMolecule agg mol )
		(updateAtomIdMap agg)
		(setq trainer (Trainer ))
		(assert (!= db ())"OH OH! the CandoDatabase is UNDEFINED" ) ;
		(setq trainerHeader (TrainerHeader ))
		(setq index (getNextUniqueIndex organizer ))
		(setUniqueIndex trainerHeader index )
		(setState trainerHeader "start" )
		(setContext trainerHeader context )
		(setq dataDir (getGlobalOption 'trainerDirectory ))
		(setTopDirectory trainerHeader dataDir )
		(setNotPartOfSeedDatabase trainerHeader false )
		(setq vstr (chem:getName focusMon ))
		(setData trainerHeader 'FocusStereoisomerName vstr )
		(setq vstr (chem:getName focusTopology ))
		(setData trainerHeader 'FocusTopologyName vstr )
		(setHeader trainer trainerHeader )
		(setq structures (ConformationExplorer ))
		(setq fnPrefix (getTrainerUniquePathName trainerHeader ))
		(let ((path (Path "." "data" fnPrefix)))
		  (setPath trainerHeader path))
		(extend (getBinder structures ) 'pathNamePrefix fnPrefix )
		(log  "About to set aggregate" )
		(setMatter structures agg )
		(setConformationExplorer trainer structures )
		(log (% "Focus monomer = %s" (description focusMon )))
		(log  "About to set trainer data" )
		(setData trainer 'oligomer olig )
		(setData trainer 'FocusMonomer focusMon )
		(setData trainer 'FocusResidue (getTemporaryResidue focusMon ))
		(setq vstr (constitutionName (getConstitution focusMon )))
		(setData trainerHeader 'FocusConstitutionName vstr )
		(ensure-directories-exist (getPath trainerHeader))
		(let ((trainerPathName (asString (getPath trainerHeader ))))
		  (log (% "About to write trainer to: %s" trainerPathName )))
		(log  "     writing oligomer " )
		(log (% "%s" (sequenceAsString olig )))
		(chem:write trainer)
		(log  "About to add trainer header" )
		(addTrainerHeader organizer trainerHeader )
		(setq newStructuresAdded (+ newStructuresAdded 1 ))))
	     (setq numberOfStructures (+ numberOfStructures 1 ))))
       (printPopPrefix )
       (printPushPrefix  "UpdateExisting: " )
       (setq fileNames (sorted (keysAsCons existingOligomers )))
       (for fileName fileNames 
	    (setq oneExisting (get existingOligomers fileName ))
	    (setq context (get oneExisting "context" ))
	    (log (% "About to read trainer[%s]" fileName ))
	    (setq trainer (readTrainer fileName ))
	    (setq originalContext (getContext (getHeader trainer )))
	    (log (% "Rewriting archive trainer: %s" fileName ))
	    (log (% "    Original context: %s" (getKey originalContext )))
	    (log (% "         New context: %s" (getKey context )))
	    (if (!= (getKey originalContext )(getKey context ))
		(then 
		 (log (% "     updating trainer in fileName: %s" fileName ))
		 (log (% "%s" (sequenceAsString olig )))
		 (setContext (getHeader trainer )context )
		 (writeTrainer fileName trainer ))
	      (else 
	       (log (% "Context is unchanged - not updating trainer in: %s" fileName )))))
       (printPopPrefix )
       )
     (setq stage (+ stage 1 ))
     (printPushPrefix  "UpdateUnusedExisting: " )
     (log  "Updating archived Trainers that are not part of the seed" )
     (setq notPartOfSeed 0 )
     (for entry (entriesAsCons organizer )
	  (when (getNotPartOfSeedDatabase (@ entry 1 ))
	    (setq fileName (asString (getPath (@ entry 1 ))))
	    (setq trainer (readTrainer fileName ))
	    (setq originalContext (getContext (getHeader trainer )))
	    (log (% "    Original context: %s" (getKey originalContext )))
	    (log (% "         New context: %s" (getKey context )))
	    (setNotPartOfSeedDatabase (getHeader trainer )true )
	    (writeTrainer fileName trainer )
	    (setq notPartOfSeed (+ notPartOfSeed 1 ))))
     (log (% "    There were %d archived Trainers that were not part of the seed" notPartOfSeed ))
     (log  "About to write out TrainerOrganizer" )
     (writeTrainerOrganizer organizer ) ;    [ organizer summarize ] 

     (printPopPrefix )
     (log "DONE." )			;
					; build organizer
					;
     )))




(when (isTopLevelScript )
  (log "Starting expandSeed" )
  (when (!= (length *ARGS* )2 )
    (print "You must provide the seed database name" )
    (exit ))
  (let ((seedDatabase (@ *ARGS* 1 )))
    (database seedDatabase )
    (expandSeed :databaseName seedDatabase :directory "./" :number ())))