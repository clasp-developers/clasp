; fixed code
(raise "depreciated - this function has been moved into assembleDatabase"   )

(require "CANDO/db/jobSetup.l"   )

(defun extractOneCoordinates  (molTrainer   ) ;END1
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

(println (%   "%s"   (asXmlString   context  ) ) )

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

(println (%   "extractCoordinates>>Contexts: 
%s"   (asXmlString   contextStringSet  ) ) )

(=   focusMonomer   (getData   body   "FocusMonomer"   ) )

(=   monomerCoordinates   (MonomerCoordinates  :context  context   ) )

(println (%   "Setting monomerCoordinates comment: %s"   fileName   ) )

(setComment   monomerCoordinates   (%   "Extracted from: %s"   fileName   ) )

(log (%   "extractMonomerCoordinates:extract comment=|%s|"   (getComment   monomerCoordinates  ) ) )

(=   searchDatabase   (getStructures   body  ) )

(defineFromConformationExplorer   monomerCoordinates   topology  searchDatabase  focusMonomer   )

(println (%   "Extracted coordinates for context: %s"   (getKey   context  ) ) )

(return monomerCoordinates   )
)
)

(defun extractFromTrainers  (&key  (seedFileName  () )(jobs  () ) ) ;END1
(assert (notNil   seedFileName  )"seedFileName must never be nil"   )

(assert (notNil   jobs  )"jobs must never be nil"   )

(println (%   "Seed fileName: %s"   seedFileName   ) )

(database seedFileName   )

(println (%   "jobs = %s"   (repr  jobs   ) ) )

(let (trainer  state   ) ;END1
(foreach fileName  jobs   ;END1
(setq trainer  (readTrainer  fileName   ) )

(setq state  (getState   (getHeader   trainer  )) )

(if (==   state   "searched"   ) ;END1
(then ;END1
(println (%   "Extracting coordinates for: %s"   fileName   ) )

(extractCoordinates trainer   )

(setState   (getHeader   trainer  ) "extracted"   )

(write   trainer  )
)

(else ;END1
(println (%   "Ignoring trainer[%s] it has the state[%s]"   fileName  state   ) )
)
)
)
)
)

(let ((jobs  (gatherTrainers  l:*ARGS*   ) )(seedName  (seedDatabaseName   ) ) ) ;END1
(extractFromTrainers :seedFileName  seedName  :jobs  jobs   )
)
