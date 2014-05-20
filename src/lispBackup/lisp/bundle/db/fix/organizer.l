; fixed code
(require "CANDO/db/globalOptions.l"   )

(defun assembleOrganizerFromMolTrainers  (&key  (databaseFileName  () )(organizerDirectory  "./"   ) ) ;END1
(let (stateCodes  organizer  firstTrainer  database  dataPathName  unsortedFiles  files  trainerCount  mj  header  state  stateCode  unsortedfiles  organizer   ) ;END1
(assert (notNil   databaseFileName  )"You must provide databaseFileName"   )

(=   stateCodes   (ObjectDictionary   ) )

(put   stateCodes   "start"  "."   )

(put   stateCodes   "min"  "m"   )

(put   stateCodes   "searching"  "s"   )

(put   stateCodes   "searched"  "S"   )

(put   stateCodes   "extracted"  "e"   )

(put   stateCodes   "bad"  "b"   )

(=   organizer   (TrainerOrganizer   ) )

(=   firstTrainer   true   )

(setTopDirectory   organizer   organizerDirectory   )

(setBuilderDatabaseFileName   organizer   databaseFileName   )

(=   database   m:*DATABASE*   )

(setq dataPathName  (getGlobalOption  "trainerDirectory"   ) )

(setq unsortedfiles  (directory  (Path  :path  dataPathName   )".*cxml"   ) )

(when (isNil   unsortedfiles  ) ;END1
(println "There are no existing trainers"   )

(setq unsortedfiles  (list   ) )
)

(setq files  (sorted   unsortedfiles  ) )

(println "States: start=. searched=s min=m extracted=e bad=b other=?
"   )

(println (%   "Organizing %d trainers"   (length   files  ) ) )

(=   trainerCount   0   )

(foreach file  files   ;END1
(=   mj   (readTrainer  file   ) )

(=   header   (getHeader   mj  ) )

(=   state   (getState   header  ) )

(=   stateCode   (getOrDefault   stateCodes   state  "?"   ) )

(when (==   (mod   trainerCount   50   ) 0   ) ;END1
(print (%   "
Trainer: %4d "   (+   trainerCount   1   ) ) )
)

(print (%   "%s"   stateCode   ) )

(addTrainerHeader   organizer   (getHeader   mj  ) )

(=   trainerCount   (+   trainerCount   1   ) )
)

(println ""   )

(println  "About to save trainer organizer"    )

(writeTrainerOrganizer organizer   )

(return organizer   )
)
)

(defun summarizeTrainers  (&key  (databaseFileName  () )(organizerDirectory  "./"   ) ) ;END1
(let (stateCodes  organizer  firstTrainer  database  dataPathName  unsortedfiles  files  trainerCount  totals  mjdict  state  total  stateCode   ) ;END1
(assert (notNil   databaseFileName  )"You must provide databaseFileName"   )

(=   stateCodes   (ObjectDictionary   ) )

(put   stateCodes   "start"  "."   )

(put   stateCodes   "min"  "m"   )

(put   stateCodes   "searching"  "s"   )

(put   stateCodes   "searched"  "S"   )

(put   stateCodes   "extracted"  "e"   )

(put   stateCodes   "bad"  "b"   )

(=   organizer   (TrainerOrganizer   ) )

(=   firstTrainer   true   )

(setTopDirectory   organizer   organizerDirectory   )

(setBuilderDatabaseFileName   organizer   databaseFileName   )

(=   database   m:*DATABASE*   )

(setq dataPathName  (getGlobalOption  "trainerDirectory"   ) )

(setq unsortedfiles  (directory  (Path  :path  dataPathName   )".*cxml"   ) )

(when (isNil   unsortedfiles  ) ;END1
(println "There are no existing trainers"   )

(setq unsortedfiles  (list   ) )
)

(setq files  (sorted   unsortedfiles  ) )

(println "States: start=. searching=s searched=S min=m extracted=e bad=b other=?
"   )

(println (%   "Organizing %d trainers"   (length   files  ) ) )

(=   trainerCount   0   )

(setq totals  (ObjectDictionary   ) )

(foreach file  files   ;END1
(=   mjdict   (readTrainerHeader  file   ) )

(setq state  (get   mjdict   "State"   ) )

(when (not  (contains   totals   state   ) ) ;END1
(put   totals   state  0   )
)

(setq total  (+   (get   totals   state   ) 1   ) )

(put   totals   state  total   )

(=   stateCode   (getOrDefault   stateCodes   state  "?"   ) )

(if  (==   (mod   trainerCount   50   ) 0   )(progn  (print  (%   "
Trainer: %4d "   (+   trainerCount   1   ) ) ) ) )

(print (%   "%s"   stateCode   ) )

(=   trainerCount   (+   trainerCount   1   ) )
)

(println ""   )

(return totals   )
)
)

(defun organizeTrainers  (&key  (databaseFileName  () ) ) ;END1
(let (organizer   ) ;END1
(assert (notNil   databaseFileName  )"You must provide databaseFileName"   )

(=   organizer   (assembleOrganizerFromMolTrainers  :databaseFileName  databaseFileName  :organizerDirectory  "./"   ) )

(return organizer   )
)
)

(ifTrue (isTopLevelScript   ) ;END1
(println "Running organizer.l as a top level script"   )

(database "seed.cxml"   )

(assembleOrganizerFromMolTrainers "seed.cxml"  :organizerDirectory  "./"   )
)
