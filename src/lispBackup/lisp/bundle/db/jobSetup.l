
(include "CANDO/db/globalOptions.l" )	;
					;
					; Configure the database construction job
					;
					;

(defun gatherTrainers (args )
  (let ((unsortedJobs )(jobs ))
    (if (> (length *ARGS* ) 1 )
	(let* ((fileNames (cdr *ARGS*))
	       (unsortedPaths (map (lambda (fn) (Path :string fn)) fileNames)))
	 (setq unsortedJobs unsortedPaths))
      (else 
       (setq unsortedJobs (directory (Path "." "data" ) "trainer.*" ))
       (print (% "Unsorted jobs: %s" (repr unsortedJobs))) ))
    (setq jobs (sorted unsortedJobs ))
    jobs ))

(defun seedDatabaseName ()
  (let (dbName sdname )
    (setq dbName (getGlobalOption 'databaseName ))
    (setq sdname (% "seed_%s.cxml" dbName ))
    sdname ))

(defun unoptimizedDatabaseName ()
  (let (dbName sdname )
    (setq dbName (getGlobalOption 'databaseName ))
    (setq sdname (% "unoptimized_%s.cxml" dbName ))
    sdname ))


(defun fullDatabaseName ()
  (let (dbName sdname )
    (setq dbName (getGlobalOption 'databaseName ))
    (setq sdname (% "database_%s.cxml" dbName ))
    sdname ))
					;
					; Here are helper functions for setting global options
					; with simple/single commands
					;

(setGlobalOptionDefaultValue 'monomerSetup "")
(setGlobalOptionDefaultValue 'quick_maxRestartFails 3 )
(setGlobalOptionDefaultValue 'short_maxRestartFails 15 )
(setGlobalOptionDefaultValue 'exhaustive_maxRestartFails 60 )
(setGlobalOptionDefaultValue 'trainerDirectory "./data/" )
(setGlobalOptionDefaultValue 'metaDatabase "fullCrosslink_meta.l" )
(setGlobalOptionDefaultValue 'fragmentFileName "full.cdxml" )
(setGlobalOptionDefaultValue 'exhaustionLevel "exhaustive" )
(setGlobalOptionDefaultValue 'databaseName "full" )
(setGlobalOptionDefaultValue 'conformationEnergyThreshold 1000.0 )
(setGlobalOptionDefaultValue 'maximumConformationsInEachMonomerCoordinates 100000 )
(defun searchExhaustionLevel (option )
  (cond 
   ((== option "quick" )		     ;E-A
    (setGlobalOption 'exhaustionLevel "quick" )) ;E-B

   ((== option "short" )		     ;E-A
    (setGlobalOption 'exhaustionLevel "short" )) ;E-B

   ((== option "exhaustive" )			  ;E-A
    (setGlobalOption 'exhaustionLevel "exhaustive" )) ;E-B

   (true 
    (print "Error in CandoDatabase: searchExhaustionLevel must be one of (quick,short,exhaustive)" )
    (exit 1 ))))
(defun quickSearchExhaustionLevel (val )
  (setGlobalOption 'quick_maxRestartFails val ))
(defun shortSearchExhaustionLevel (val )
  (setGlobalOption 'short_maxRestartFails val ))
(defun exhaustiveSearchExhaustionLevel (val )
  (setGlobalOption 'exhaustive_maxRestartFails val ))
(defun trainerDirectory (pathName )
  (setGlobalOption 'trainerDirectory pathName ))
(defun monomerSetup (pathName )
  (setGlobalOption 'monomerSetup pathName ))
(defun metaDatabase (pathName )
  (setGlobalOption 'metaDatabase pathName ))
(defun databaseName (pathName )
  (setGlobalOption 'databaseName pathName ))
(defun conformationEnergyThreshold (val )
  (setGlobalOption 'conformationEnergyThreshold val ))
(defun maximumConformationsInEachMonomerCoordinates (&key setValue )
  (let ((optionName 'maximumConformationsInEachMonomerCoordinates ))
    (when (isNil setValue )
      (return-from maximumConformationsInEachMonomerCoordinates (getGlobalOption optionName )))
    (setGlobalOption optionName setValue ) 
    ))
					;
					; Here include the database specific file
					; that must be in the database directory
					;
(include "CandoDatabase" )