
(shadowing-import '(chem:atom chem:boundingbox))
(use-package :chem)

(include "CANDO/db/globalOptions.l" )
(include "CANDO/db/jobSetup.l" )
(include "CANDO/db/expandSeed.l" )
(include "CANDO/db/prepareSeed.l")

(prognLog
 "createSeed.l stage"
 (prepareSeed)
 (let ((seedName (seedDatabaseName)))
   (print (% "seedName(%s)" seedName ))
   (prognLog (% "Saving database to: %s" seedName )
	     (saveArchive *DATABASE* seedName ))
   (prognLog (% "Re-loading database from: %s" seedName)
	     (database seedName))
   (print "Using neighbor stereotypes to define plug mates")
   (useNeighborStereotypesToDefinePlugMates *DATABASE*)
   (print "Expanding seed - use tail on the log file to monitor progress")
   (expandSeed :databaseName seedName)
   (print (% "Saving final database to seedName(%s)" seedName ))
   (saveArchive *DATABASE* seedName )
   (print "Saved to archive")
   (saveCandoFormat *DATABASE* (% "%s.cf" seedName))
   (print "Saved to CandoFormat")
   ))
