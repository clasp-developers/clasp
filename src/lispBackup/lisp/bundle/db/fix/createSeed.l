; fixed code
(require "CANDO/db/globalOptions.l"   )

(require "CANDO/db/jobSetup.l"   )

(global metaDatabaseScript  (getGlobalOption  "metaDatabase"   ) )

(println "Including <CANDO/db/expandSeed.l>"   )

(require "CANDO/db/expandSeed.l"   )

(global metaInclude  (%   "CANDO/db/%s"   metaDatabaseScript   ) )

(println (%   "Including <%s>"   metaInclude   ) )

(require metaInclude   )

(defvar *allFragments*  (list   ) )

(global chemDrawFileNames  (getGlobalOption  "chemDrawFileNames"   ) )

(foreach fn  chemDrawFileNames   ;END1
(let (chemdraw  fragments   ) ;END1
(println (%   "Loading ChemDraw file: %s"   fn   ) )

(setq chemdraw  (ChemDraw  :fileName  fn   ) )

(setq fragments  (getFragments  chemdraw   ) )

(setq *allFragments*  (extend  *allFragments*  fragments   ) )
)
)

(assert  (>  (length  *allFragments*   )0   )"There are not fragments!!!"   )

(global dbName  (getGlobalOption  "databaseName"   ) )

(global seedName  (%   "seed_%s.cxml"   dbName   ) )

(createMetaDatabase seedName  *allFragments*   )

(database seedName   )

(expandSeed :databaseName  seedName  :directory  "./"  :number  () )

(println (%   "Saving final database: %s"   seedName   ) )

(saveArchive m:*DATABASE*  seedName   )
