; fixed code
(require "CANDO/db/globalOptions.l"   )

(defun gatherTrainers  (args   ) ;END1
(let ((unsortedJobs   )(jobs   ) ) ;END1
(if (>   (length   l:*ARGS*  ) 1   ) ;END1
(then ;END1
(setq unsortedJobs  (cdr    l:*ARGS*   ) )
)

(else ;END1
(setq unsortedJobs  (directory  (Path  :path  "./data/"   )".*cxml"   ) )
)

(return )
)

(setq jobs  (sorted   unsortedJobs  ) )

(return jobs   )
)
)

(defun seedDatabaseName  () ;END1
(let (dbName  sdname   ) ;END1
(setq dbName  (getGlobalOption  "databaseName"   ) )

(setq sdname  (%   "seed_%s.cxml"   dbName   ) )

(return sdname   )
)
)

(defun unoptimizedDatabaseName  () ;END1
(let (dbName  sdname   ) ;END1
(setq dbName  (getGlobalOption  "databaseName"   ) )

(setq sdname  (%   "unoptimized_%s.cxml"   dbName   ) )

(return sdname   )
)
)

(defun fullDatabaseName  () ;END1
(let (dbName  sdname   ) ;END1
(setq dbName  (getGlobalOption  "databaseName"   ) )

(setq sdname  (%   "database_%s.cxml"   dbName   ) )

(return sdname   )
)
)

(setGlobalOptionDefaultValue "quick_maxRestartFails"  2   )

(setGlobalOptionDefaultValue "short_maxRestartFails"  15   )

(setGlobalOptionDefaultValue "exhaustive_maxRestartFails"  60   )

(setGlobalOptionDefaultValue "trainerDirectory"  "./data/"   )

(setGlobalOptionDefaultValue "metaDatabase"  "fullCrosslink_meta.l"   )

(setGlobalOptionDefaultValue "fragmentFileName"  "full.cdxml"   )

(setGlobalOptionDefaultValue "exhaustionLevel"  "exhaustive"   )

(setGlobalOptionDefaultValue "databaseName"  "full"   )

(setGlobalOptionDefaultValue "conformationEnergyThreshold"  1000.0   )

(setGlobalOptionDefaultValue "maximumConformationsInEachMonomerCoordinates"  100000   )

(defun searchExhaustionLevel  (option   ) ;END1
(cond ;END1
(==   option   "quick"   )
(setGlobalOption "exhaustionLevel"  "quick"   )


(==   option   "short"   )
(setGlobalOption "exhaustionLevel"  "short"   )


(==   option   "exhaustive"   )
(setGlobalOption "exhaustionLevel"  "exhaustive"   )


(true ;END1
(println "Error in CandoDatabase: searchExhaustionLevel must be one of (quick,short,exhaustive)"   )

(exit 1   )
)
)
)

(defun quickSearchExhaustionLevel  (val   ) ;END1
(setGlobalOption "quick_maxRestartFails"  val   )
)

(defun shortSearchExhaustionLevel  (val   ) ;END1
(setGlobalOption "short_maxRestartFails"  val   )
)

(defun exhaustiveSearchExhaustionLevel  (val   ) ;END1
(setGlobalOption "exhaustive_maxRestartFails"  val   )
)

(defun trainerDirectory  (pathName   ) ;END1
(setGlobalOption "trainerDirectory"  pathName   )
)

(defun chemDrawFileNames  (pathNames   ) ;END1
(setGlobalOption "chemDrawFileNames"  pathNames   )
)

(defun metaDatabase  (pathName   ) ;END1
(setGlobalOption "metaDatabase"  pathName   )
)

(defun databaseName  (pathName   ) ;END1
(setGlobalOption "databaseName"  pathName   )
)

(defun conformationEnergyThreshold  (val   ) ;END1
(setGlobalOption "conformationEnergyThreshold"  val   )
)

(defun maximumConformationsInEachMonomerCoordinates  (&key  setValue   ) ;END1
(let ((optionName  "maximumConformationsInEachMonomerCoordinates"   ) ) ;END1
(when (isNil   setValue  ) ;END1
(return (getGlobalOption  optionName   ) )
)

(setGlobalOption optionName  setValue   )
)
)

(require "CandoDatabase"   )
