; fixed code
(require "CANDO/db/jobSetup.l"   )

(defun extractMonomerCoordinates  (molTrainer   ) ;END1
(let (db  header  body  fileName  constitutionName  context  constitution  topology  contextStringSet  focusMonomer  monomerCoordinates  searchDatabase   ) ;END1
(=   db   m:*DATABASE*   )

(=   header   (getHeader   molTrainer  ) )

(=   body   molTrainer   )

(=   fileName   (getFileName   header  ) )

(println (%   "========structure: %s"   fileName   ) )

(log (%   "molTrainer====structure: %s"   fileName   ) )

(=   rawContextKey   (getContextKey   header  ) )

(log (%   "loaded contextKey=%s"   rawContextKey   ) )

(=   oligomer   (getData   body   "oligomer"   ) )

(log  "oligomer="    )

(log (%   "%s"   (asXmlString   oligomer  ) ) )

(setq constitutionName  (getData   header   "FocusConstitutionName"   ) )

(println (%   "Constitution = %s "   constitutionName   ) )

(log (%   "Constitution = %s"   constitutionName   ) )

(setq context  (getContext   header  ) )

(log (%   "%s"   (asXmlString   context  ) ) )

(setq constitution  (monomerConstitutionForName   db   constitutionName   ) )

(ifTrue (==   constitution   () ) ;END1
(raise (%   "Could not find constitution[%s] in builderDatabase!!"   constitutionName   ) )
)

(=   topology   (getTopologyForContext   constitution   context   ) )

(println (%   "Got topology: %s"   (getName   topology  ) ) )

(log (%   "Got topology: %s"   (getName   topology  ) ) )

(=   contextStringSet   (getAllSpecificKeys   context  ) )

(ifTrue (==   (size   contextStringSet  ) 0   ) ;END1
(raise "Context doesn't expand to contextKeys, IGNORING"   )
)

(=   focusMonomer   (getData   body   "FocusMonomer"   ) )

(=   monomerCoordinates   (MonomerCoordinates  :context  context   ) )

(println (%   "Setting monomerCoordinates comment: %s"   fileName   ) )

(setComment   monomerCoordinates   (%   "Extracted from: %s"   fileName   ) )

(log (%   "extractMonomerCoordinates:extract comment=|%s|"   (getComment   monomerCoordinates  ) ) )

(=   searchDatabase   (getStructures   body  ) )

(defineFromConformationExplorer   monomerCoordinates   topology  searchDatabase  focusMonomer  (maximumConformationsInEachMonomerCoordinates   ) )

(return monomerCoordinates   )
)
)

(defun assembleUnsortedDatabase  (&key  (outputFileName  () )(fileNames  () ) ) ;END1
(assert (notNil   outputFileName  )"You must provide outputFileName"   )

(assert (notNil   fileNames  )"You must provide fileNames"   )

(let ((databaseOutputFileName  outputFileName   )(bdb  m:*DATABASE*   )job  header  state  mc  added   ) ;END1
(log "About to read filenames"   )

(foreach fn  fileNames   ;END1
(setq job  (readTrainer  fn   ) )

(setq header  (getHeader   job  ) )

(setq state  (getState   header  ) )

(if (or   (==   state   "searching"   ) (==   state   "searched"   ) ) ;END1
(then ;END1
(setq mc  (extractMonomerCoordinates  job   ) )

(setq added  (addMonomerCoordinates   bdb   mc   ) )

(println (%   "File(%s) I added %d keys"   fn  added   ) )
)

(else ;END1
(println (%   "MolTrainer %s is in the wrong state[%s]"   fn  state   ) )
)
)
)

(println (%   "Writing database to[%s]"   databaseOutputFileName   ) )

(log "About to write archive"   )

(saveArchive bdb  databaseOutputFileName   )
)
)

(defun assembleFromJobs  (&key  (seedDatabaseName  () )(outputName  () )(jobs  () ) ) ;END1
(assert (notNil   seedDatabaseName  )"seedDatabaseName must be defined"   )

(assert (notNil   jobs  )"jobs must be defined"   )

(assert (notNil   outputName  )"outputName must be defined - outputDatabase name"   )

(database seedDatabaseName   )

(assembleUnsortedDatabase :outputFileName  outputName  :fileNames  jobs   )
)

(let ((jobs  (gatherTrainers  l:*ARGS*   ) )(seedName  (seedDatabaseName   ) )(outputName  (unoptimizedDatabaseName   ) ) ) ;END1
(assembleFromJobs :seedDatabaseName  seedName  :jobs  jobs  :outputName  outputName   )
)

(println "Done"   )

(println "  now run db/optimizeDatabase.l"   )
