; fixed code
(require "CANDO/db/defineScaffolds.l"   )

(require "CANDO/db/organizer.l"   )

(require "CANDO/db/trainingOligomer.l"   )

(defun expandSeed  (&key  (databaseName  () )(directory  "./"   )(number  () ) ) ;END1
(let (stage  numberLeft  db  trainingOligomers  needSet  idx  trainingCons  maxIdx  ms  foundError  numberOfStructures  newStructuresAdded  numberOfStochasticSearches  assigned  context  headerThatRecognizesASubsetOfContext  trainer  newOligomers  numberLeft  olig  context  focusMon  focusTopology  header  fileName  oneExistingOligomer  existingOligomers  mol  agg  trainerHeader  index  dataDir  vstr  structures  fnPrefix  trainerFileName  newStructuresAdded  oneExisting  originalContext  organizer  focusResidue  fileNames  notPartOfSeed   ) ;END1
(setq stage  0   )

(assert (notNil   databaseName  )"You must provide a databaseName"   )

(=   numberLeft   -1   )

(ifTrue (!=   number   () ) ;END1
(=   numberLeft   number   )
)

(println (%   "Database directory: %s"   directory   ) )

(println "Reading MoleculeTrainerOrganizer"   )

(=   db   m:*DATABASE*   )

(ifTrue (==   db   () ) ;END1
(raise "The database is nil, you must define a BuilderDatbase"   )
)

(setq stage  (+   stage   1   ) )

(printPushPrefix  "GenerateTrainers: "    )

(println  "Expanding the seed, generating training oligomers"    )

(setq trainingOligomers  (generateAllTrainingOligomers  db   ) )

(println (%   "    Generated %d TrainingOligomers"   (size   trainingOligomers  ) ) )

(log "Check if we are missing any monomers"   )

(=   needSet   (StringSet  :entries  () ) )

(println (%   "trainingOligomers = %s"   (repr  trainingOligomers   ) ) )

(setq idx  0   )

(setq trainingCons  (asCons   trainingOligomers  ) )

(setq maxIdx  (length   trainingCons  ) )

(foreach f  trainingCons   ;END1
(println (%   "Training oligomer %d/%d"   idx  maxIdx   ) )

(println (%   "f = %s"   (repr  f   ) ) )

(println (%   "class(f) = %s"   (className  f   ) ) )

(=   ms   (getMonomersYouNeed   f  ) )

(=   needSet   (union   needSet   ms   ) )

(setq idx  (+   idx   1   ) )
)

(=   foundError   false   )

(foreach f  (asCons   needSet  ) ;END1
(ifTrue (not  (recognizesNameOrPdb   db   f   ) ) ;END1
(println (%   "This seed database is incomplete, TrainingOligomers need the monomer: %s"   f   ) )

(=   foundError   true   )
)
)

(ifTrue  foundError    ;END1
(raise "There were missing monomers in the seed database"   )
)

(printPopPrefix )

(setq stage  (+   stage   1   ) )

(printPushPrefix  "Organize: "    )

(println "Organizing all of the archived trainers"   )

(=   organizer   (assembleOrganizerFromMolTrainers  :databaseFileName  databaseName  :organizerDirectory  "./"   ) )

(println  "  About to set not part of seed database"    )

(foreach org  (entriesAsCons   organizer  ) ;END1
(setNotPartOfSeedDatabase   (cadr   org  ) true   )
)

(println (%   "    There are %d existing trainers"   (numberOfTrainerHeaders   organizer  ) ) )

(ifTrue (<   (size   trainingOligomers  ) (numberOfTrainerHeaders   organizer  ) ) ;END1
(println (%   "WARNING: There are now fewer seed trainers[%d] than there are archived trainers[%d]"   (size   trainingOligomers  )(numberOfTrainerHeaders   organizer  ) ) )

(println  "WARNING:   This means the seed database has contracted in size [number of trainers]."    )

(println  "WARNING:   Processing will continue but excess archive Trainers should probably be removed"    )

(println ""   )
)

(=   numberOfStructures   1   )

(=   newStructuresAdded   0   )

(=   numberOfStochasticSearches   0   )

(printPopPrefix )

(setq stage  (+   stage   1   ) )

(printPushPrefix  "ExamineExistingTrainers: "    )

(println "Assigning archived Trainers to seed Trainers"   )

(if (==   (numberOfTrainerHeaders   organizer  ) 0   ) ;END1
(then ;END1
(println "    Skipping stage 3, there are no archived Trainers to assign"   )
)

(else ;END1
(=   assigned   0   )

(foreach trainer  (asCons   trainingOligomers  ) ;END1
(=   context   (getFocusContext   trainer  ) )

(=   headerThatRecognizesASubsetOfContext   (getTrainerHeaderThatRecognizesSubsetOfContext   organizer   context   ) )

(ifTrue (!=   headerThatRecognizesASubsetOfContext   () ) ;END1
(setNotPartOfSeedDatabase   headerThatRecognizesASubsetOfContext   false   )

(setArchivedTrainerHeader   trainer   headerThatRecognizesASubsetOfContext   )

(=   assigned   (+   assigned   1   ) )
)
)

(println (%   "   There are %d archived trainers assigned to seed Trainers"   assigned   ) )

(ifTrue (<   assigned   (numberOfTrainerHeaders   organizer  ) ) ;END1
(println  (%   "WARNING: Only %d of the %d total archived trainers were assigned to seed trainers"   assigned  (numberOfTrainerHeaders   organizer  ) ) )

(println   "WARNING:   This means the seed database has contracted in size [number of trainers]."    )

(println   "WARNING:   Processing will continue but excess archive Trainers should probably be removed"    )

(println  ""   )
)
)
)

(printPopPrefix )

(setq stage  (+   stage   1   ) )

(printPushPrefix  "ArchiveSeedTrainers: "    )

(println  "Archiving seed trainers"    )

(=   newOligomers   () )

(setq existingOligomers  (ObjectDictionary   ) )

(foreach trainer  (asCons   trainingOligomers  ) ;END1
(println (%   "===========   trainer = %s"   (repr  trainer   ) ) )

(ifTrue (==   numberLeft   0   ) ;END1
(break )
)

(=   numberLeft   (-   numberLeft   1   ) )

(=   olig   (getOligomer   trainer  ) )

(=   context   (getFocusContext   trainer  ) )

(prognDebug ;END1
(log (%   "Context: %s"   (getKey   context  ) ) )

(println (%   "Context: %s"   (getKey   context  ) ) )
)

(=   focusMon   (getFocusMonomer   trainer  ) )

(=   focusTopology   (getFocusTopology   trainer  ) )

(ifTrue (getHasError   olig  ) ;END1
(raise (%   "ERROR: Oligomer has an error!!!: %s"   (getErrorMessage   olig  ) ) )
)

(println (%   "trainer getArchivedTrainerHeader = %s"   (repr  (getArchivedTrainerHeader   trainer  ) ) ) )

(if (!=   (getArchivedTrainerHeader   trainer  ) () ) ;END1
(then ;END1
(=   header   (getArchivedTrainerHeader   trainer  ) )

(assert (!=   header   () )"Shit"   )

(=   fileName   (getFileName   header  ) )

(setq oneExistingOligomer  (ObjectDictionary  :entries  (list  (list  "trainer"  trainer   )(list  "fileName"  fileName   )(list  "context"  context   ) ) ) )

(put   existingOligomers   fileName  oneExistingOligomer   )

(println (%   "Postponed update of existing trainer in file: %s"   fileName   ) )
)

(else ;END1
(println "     The context is new, building Aggregate"   )

(prognDebug ;END1
(println (%   "%s "   (sequenceAsString   olig  ) ) )
)

(=   mol   (getMolecule   trainer  ) )

(=   focusResidue   (getTemporaryResidue   focusMon  ) )

(println (%   "FocusResidue = %s"   (getName   focusResidue  ) ) )

(randomizeAtomPositions   mol  )

(=   agg   (Aggregate   ) )

(addMolecule   agg   mol   )

(=   trainer   (Trainer   ) )

(assert (!=   db   () )"OH OH! the BuilderDatabase is UNDEFINED"   )

(=   trainerHeader   (TrainerHeader   ) )

(=   index   (getNextUniqueIndex   organizer  ) )

(setUniqueIndex   trainerHeader   index   )

(setState   trainerHeader   "start"   )

(setContext   trainerHeader   context   )

(setq dataDir  (getGlobalOption  "trainerDirectory"   ) )

(setTopDirectory   trainerHeader   dataDir   )

(setNotPartOfSeedDatabase   trainerHeader   false   )

(=   vstr   (getName   focusMon  ) )

(setData   trainerHeader   "FocusStereoisomerName"  vstr   )

(=   vstr   (getName   focusTopology  ) )

(setData   trainerHeader   "FocusTopologyName"  vstr   )

(setHeader   trainer   trainerHeader   )

(=   structures   (ConformationExplorer   ) )

(=   fnPrefix   (getTrainerFileNamePrefix   trainerHeader  ) )

(setString   (getData   structures  ) "fileNamePrefix"  fnPrefix   )

(log  "About to set aggregate"    )

(setMatter   structures   agg   )

(setStructures   trainer   structures   )

(println (%   "Focus monomer = %s"   (description   focusMon  ) ) )

(log  "About to set trainer data"    )

(setData   trainer   "oligomer"  olig   )

(setData   trainer   "FocusMonomer"  focusMon   )

(setData   trainer   "FocusResidue"  (getTemporaryResidue   focusMon  ) )

(=   vstr   (getName   (getConstitution   focusMon  )) )

(setData   trainerHeader   "FocusConstitutionName"  vstr   )

(=   trainerFileName   (generateFileName   trainerHeader  ) )

(println (%   "About to write trainer to: %s"   trainerFileName   ) )

(ifTrue (exists   (Path  :path  trainerFileName   )) ;END1
(raise (%   "The new trainer fileName[%s] already exists!! Crashing to prevent overwriting"   trainerFileName   ) )
)

(println  "     writing oligomer "    )

(println (%   "%s"   (sequenceAsString   olig  ) ) )

(writeTrainer trainerFileName  trainer   )

(log  "About to add trainer header"    )

(addTrainerHeader   organizer   trainerHeader   )

(=   newStructuresAdded   (+   newStructuresAdded   1   ) )
)
)

(=   numberOfStructures   (+   numberOfStructures   1   ) )
)

(printPopPrefix )

(printPushPrefix  "UpdateExisting: "    )

(setq fileNames  (sorted   (keysAsCons   existingOligomers  )) )

(foreach fileName  fileNames   ;END1
(setq oneExisting  (get   existingOligomers   fileName   ) )

(setq context  (get   oneExisting   "context"   ) )

(println (%   "About to read trainer[%s]"   fileName   ) )

(=   trainer   (readTrainer  fileName   ) )

(=   originalContext   (getContext   (getHeader   trainer  )) )

(println (%   "Rewriting archive trainer: %s"   fileName   ) )

(println (%   "    Original context: %s"   (getKey   originalContext  ) ) )

(println (%   "         New context: %s"   (getKey   context  ) ) )

(if (!=   (getKey   originalContext  ) (getKey   context  ) ) ;END1
(then ;END1
(println (%   "     updating trainer in fileName: %s"   fileName   ) )

(println (%   "%s"   (sequenceAsString   olig  ) ) )

(setContext   (getHeader   trainer  ) context   )

(writeTrainer fileName  trainer   )
)

(else ;END1
(println (%   "Context is unchanged - not updating trainer in: %s"   fileName   ) )
)
)
)

(printPopPrefix )

(setq stage  (+   stage   1   ) )

(printPushPrefix  "UpdateUnusedExisting: "    )

(println  "Updating archived Trainers that are not part of the seed"    )

(=   notPartOfSeed   0   )

(foreach entry  (entriesAsCons   organizer  ) ;END1
(ifTrue (getNotPartOfSeedDatabase   (@   entry   1   )) ;END1
(=   fileName   (getFileName   (@   entry   1   )) )

(=   trainer   (readTrainer  fileName   ) )

(=   originalContext   (getContext   (getHeader   trainer  )) )

(println  (%   "    Original context: %s"   (getKey   originalContext  ) ) )

(println  (%   "         New context: %s"   (getKey   context  ) ) )

(setNotPartOfSeedDatabase   (getHeader   trainer  ) true   )

(writeTrainer  fileName  trainer   )

(=   notPartOfSeed   (+   notPartOfSeed   1   ) )
)
)

(println (%   "    There were %d archived Trainers that were not part of the seed"   notPartOfSeed   ) )

(println  "About to write out TrainerOrganizer"    )

(writeTrainerOrganizer organizer   )

(printPopPrefix )

(println "DONE."   )
)
)

(ifTrue (isTopLevelScript   ) ;END1
(println "Starting expandSeed"   )

(ifTrue (!=   (length   l:*ARGS*  ) 2   ) ;END1
(println "You must provide the seed database name"   )

(exit )
)

(let ((seedDatabase  (@   l:*ARGS*   1   ) ) ) ;END1
(database seedDatabase   )

(expandSeed :databaseName  seedDatabase  :directory  "./"  :number  () )
)
)
