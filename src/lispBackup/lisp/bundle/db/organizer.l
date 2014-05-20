					; fixed code
(include "CANDO/db/globalOptions.l" )
(defun assembleOrganizerFromMolTrainers (&key (databaseFileName ())(organizerDirectory "./" ))
  (let (stateCodes organizer firstTrainer database dataPathName unsortedFiles files trainerCount mj header state stateCode unsortedfiles organizer )
    (assert (notNil databaseFileName )"You must provide databaseFileName" )
    (setq stateCodes (ObjectDictionary ))
    (put stateCodes "start" "." )
    (put stateCodes "min" "m" )
    (put stateCodes "searching" "s" )
    (put stateCodes "searched" "S" )
    (put stateCodes "extracted" "e" )
    (put stateCodes "bad" "b" )
    (setq organizer (TrainerOrganizer ))
    (setq firstTrainer true )
    (setTopDirectory organizer organizerDirectory )
    (setCandoDatabaseFileName organizer databaseFileName )
    (setq database chem:*DATABASE* ) ;    [ organizer setCandoDatabase database ]
					;   [ database setTrackName "master"]

    (setq dataPathName (getGlobalOption 'trainerDirectory ))
    (setq unsortedfiles (directory (Path dataPathName )".*cxml" ))
    (when (isNil unsortedfiles )
      (print "There are no existing trainers" )
      (setq unsortedfiles (list )))
    (setq files (sorted unsortedfiles ))
    (print "States: start=. searched=s min=m extracted=e bad=b other=?
" )
    (print (% "Organizing %d trainers" (length files )))
    (setq trainerCount 0 )
    (foreach file files 
	     (setq mj (readTrainer file ))
	     (setq header (getHeader mj ))
	     (setq state (getState header ))
	     (setq stateCode (getOrDefault stateCodes state "?" ))
	     (when (== (mod trainerCount 50 )0 )
	       (print (% "Trainer: %4d " (+ trainerCount 1 ))))
	     (prin1 (% "%s" stateCode ))
	     (addTrainerHeader organizer (getHeader mj )) ;	redundantCandoDatabase = None

	     (setq trainerCount (+ trainerCount 1 )))
    (print "" )
    (print "About to save trainer organizer" )
    (writeTrainerOrganizer organizer )
    (print "Done with save trainer organizer" )
    organizer ))


					; Now don't read the entire trainer, just get the header
					;
(defun summarizeTrainers (&key (databaseFileName ())(organizerDirectory "./" ))
  (let (stateCodes organizer firstTrainer database dataPathName unsortedfiles files trainerCount totals mjdict state total stateCode )
    (assert (notNil databaseFileName )"You must provide databaseFileName" )
    (setq stateCodes (ObjectDictionary ))
    (put stateCodes "start" "." )
    (put stateCodes "min" "m" )
    (put stateCodes "searching" "s" )
    (put stateCodes "searched" "S" )
    (put stateCodes "extracted" "e" )
    (put stateCodes "bad" "b" )
    (setq organizer (TrainerOrganizer ))
    (setq firstTrainer true )
    (setTopDirectory organizer organizerDirectory )
    (setCandoDatabaseFileName organizer databaseFileName )
    (setq database chem:*DATABASE* ) ;   [ organizer setCandoDatabase database ]
					;   [ database setTrackName "master"]

    (setq dataPathName (getGlobalOption 'trainerDirectory ))
    (setq unsortedfiles (directory (Path dataPathName )".*cxml" ))
    (when (isNil unsortedfiles )
      (print "There are no existing trainers" )
      (setq unsortedfiles (list )))
    (setq files (sorted unsortedfiles ))
    (print "States: start=. searching=s searched=S min=m extracted=e bad=b other=?
" )
    (print (% "Organizing %d trainers" (length files )))
    (setq trainerCount 0 )
    (setq totals (ObjectDictionary ))
    (foreach file files 
	     (setq mjdict (readTrainerHeader file ))
	     (setq state (get mjdict "State" ))
	     (when (not (contains totals state ))
	       (put totals state 0 ))
	     (setq total (+ (get totals state )1 ))
	     (put totals state total )
	     (setq stateCode (getOrDefault stateCodes state "?" ))
	     (if (== (mod trainerCount 50 ) 0 )
		 (progn
		   (print(% "Trainer: %4d " (+ trainerCount 1 )))))
	     (prin1 (% "%s" stateCode ))
	     (setq trainerCount (+ trainerCount 1 )))
    (print "" )
    totals ))


(defun organizeTrainers (&key (databaseFileName ()))
  (let (organizer )
    (assert (notNil databaseFileName )"You must provide databaseFileName" )
    (setq organizer (assembleOrganizerFromMolTrainers :databaseFileName databaseFileName :organizerDirectory "./" ))
    organizer ))




(ifTrue (isTopLevelScript )
	(print "Running organizer.l as a top level script" )
	(database "seed.cxml" )
	(assembleOrganizerFromMolTrainers "seed.cxml" :organizerDirectory "./" ))
