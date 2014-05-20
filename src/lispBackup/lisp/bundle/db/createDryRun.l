(include "CANDO/db/globalOptions.l" )
(include "CANDO/db/jobSetup.l" )
(include "CANDO/db/prepareSeed.l")
(include "CANDO/db/trainingOligomer.l" ) ;require "CANDO/db/checkMates.l"



;;
;; Look for potential neighbor stereotypes that could be defined that would
;; reduce the number of training oligomers
;;
;; Any training oligomer that has a plug coming out of the focus monomer
;; that has a single mate is a plug that could potentially have a neighbor stereotype
;; defined that would reduce the number of trainers
;;
(defun lookForPotentialNeighborStereotypes (trainers)
  (let ((potentialNeighborStereotypes (Binder)))
    (for trainer (asCons trainers)
	 (let* ((context (getFocusContext trainer))
		(neighborPlugNames (getPlugNames context)))
	   (for neighborPlugName (asCons neighborPlugNames)
		(let* ((entityNames (getNeighbor context neighborPlugName))
		       (expandedEntityNames (expandedNameSet entityNames)))
		  (when (== (size expandedEntityNames) 0)
		    (raise (LispError "There has to be at least one name")))
		  (when (== (size expandedEntityNames) 1)
		    (let ((potentialStereotypeEntityNameSet
			   (if (contains potentialNeighborStereotypes neighborPlugName)
			       (lookup potentialNeighborStereotypes neighborPlugName)
			     (extend potentialNeighborStereotypes neighborPlugName (SymbolSet)))))
		      (insertSymbolSet potentialStereotypeEntityNameSet expandedEntityNames)))
		  ))
	   ))
    (print (% "============  Potential neighbor stereotypes ==========="))
    (for plugName (keysAsCons potentialNeighborStereotypes)
	 (let ((stereotype (getNeighborStereotype plugName)))
	   (when stereotype
	     (progn
	       (let* ((names (lookup potentialNeighborStereotypes plugName))
		      (expandedStereotype (expandEntityNamesToTerminals *DATABASE* stereotype))
		      (namesWithoutStereotypes (relativeComplement names expandedStereotype)))
		 (when (> (size namesWithoutStereotypes) 0)
		   (print (% "Plug name: %10s   entities: %s" plugName (asString namesWithoutStereotypes))))))
	     )
	   ))))

(prognLog
 "examineSeed.l stage"
 (prepareSeed)
 (let* ((dbName (getGlobalOption 'databaseName ))
	(seedName (% "examine_seed_%s.cxml" dbName ))	)
   (print (% "seedName(%s)" seedName ))
   (prognLog (% "Saving database to: %s" seedName )
	     (saveArchive *DATABASE* seedName ))
   (prognLog (% "Re-loading database from: %s" seedName)
	     (database seedName))
   )
 (print "----- useNeighborStereotypesToDefinePlugMates ")
 (useNeighborStereotypesToDefinePlugMates *DATABASE*)
 (print "------ generating training oligomers")
 (let ((trainingOligomers (prognLog
			   "Expanding the seed, generating training oligomers"
			   (printPushPrefix "GenerateTrainers: ")
			   (let ((trainOligs (generateAllTrainingOligomers *DATABASE* )))
			     (printPopPrefix)
			     trainOligs)))
       (idx 0))
   (for to (asCons trainingOligomers)
	(let ((context (getFocusContext to)))
	  (print (% "----------  Trainer#%3d" idx))
	  (print (% "Context: %s" (getKey context))))
	(setq idx (+ idx 1)))
   (print (% "%d training oligomers are currently being generated" (size trainingOligomers)))
   (lookForPotentialNeighborStereotypes trainingOligomers)
   )
 )

