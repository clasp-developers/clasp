
(include "CANDO/db/checkTopologies.l" )
(include "CANDO/db/checkMetaDatabase.l" )
(include "CANDO/db/buildInternalCoordinateAtomTemplateTree.l")
(include "CANDO/db/buildChiList.l")


(global *UniqueMetaId* 1 )


(defun getUniqueMetaId ()
  (let ((id (% "id%04d" *UniqueMetaId* )))
    (setq *UniqueMetaId* (+ *UniqueMetaId* 1 ))
    id ))




(defun createRepresentedEntityNameSet (&key name entityNames representativeName )
  (prognLog
   (% "Creating system group[%s]" name)
   (assert (notNil name )"You must provide name" )
   (assert (isA name Symbol) "Name must be a symbol")
   (when (recognizesEntityName *DATABASE* name)
     (log (% "*DATABASE* currently contains an entity with the name: %s" name))
     (raise (% "*DATABASE* already contains an entity with name(%s)" name )))
   (let ((group (RepresentedEntityNameSet :name name
				 :representativeName representativeName
				 :entityNames entityNames)))
     (log (% "Adding to *DATABASE* -> system monomer group: %s" name ))
     (addEntity *DATABASE* group ))
   name))



(defClass MetaConstant ()(_Value )
  (method __init__ (self val )
	  (setq (slot self _Value )val ))
  (method resolve (self fragment )
	  (slot self _Value )))



(defClass MetaTraceableObject ()(_backtrace )
  (method __init__ (self )
	  (setq (slot self _backtrace )(backtrace )))
  (method dump (self )
	  (let (serr )
	    (setq serr (StringOutStream ))
	    (for bt (slot self _backtrace )
		 (block inner
			(when (isOfClass bt Cons ) (return-from inner))
			(write serr (% "Bad object backtrace line/file = %d/%s" (lineno bt )(filename bt )))))
	    (str serr ))))


					;
					; MetaSingletonObjects represent objects that only
					; ever have one instance of a class in the fragment
					; 
(defClass MetaSingletonObject MetaTraceableObject ()
  (method __init__ (self )
	  (callAncestorMethod self ))

  (method genKey (self desc )
	  (let (key (asKeywordSymbol (% "Singleton-%s-%s" (className self) desc )))
	    key))
  )



					;
					; MetaObject(s) represent objects that are unique 

(defClass MetaObject MetaTraceableObject (_UniqueId _ResolveCallback )
  (method __init__ (self &key resolveCallback )
	  (callAncestorMethod self )
	  (setq (slot self _UniqueId )(getUniqueMetaId ))
	  (setq (slot self _ResolveCallback) resolveCallback))

  (method genKey (self desc )
	  (assert (notNil (slot self _UniqueId ))
		  (% "You must call initMetaObject for each MetaObject. This class=%s" (className self )))
	  (let ((key (% "%s-%s-%s" (symbol-name (classNameSymbol self )) (slot self _UniqueId) desc)))
	    (asKeywordSymbol key) ))
  )




;;
;; Look through all of the ChemDraw monomers and create
;; a dictionary that maps plugNames to monomerNames that cap them
;;
(defun createAMapOfAllPlugsToCaps ( chemdrawConstitutions )
  (let ((map (Binder)))
    (for cc chemdrawConstitutions
	 (when (hasProperty cc :caps)
	   (let ((plugsThatAreCappedByMonomer (getProperty cc :caps))
		 (nameTemplate (getProperty cc :nameTemplate)))
	     (assert (isA nameTemplate String) (% "The nameTemplate[%s] needs to be a String - it is a %s" nameTemplate (className nameTemplate)))
	     (for plugName plugsThatAreCappedByMonomer
		  (print (% "plugName = %s" plugName))
		  (when (contains map plugName)
		    (raise (LispError (% "You are trying to define the cap[%s] of a plug[%s] that already has one[%s]"
					 nameTemplate plugName (lookup map plugName)))))
		  (extend map plugName (asSymbol nameTemplate))))))
    map))

;;
;; Cap all of the mates using the dictionary defined in createAMapOfAllPlugsToCaps
;; that was put into the chemdraw fragment under the property :plugToCapMap
;;
(defun capAllMates (plugName mates fragment)
  (let ((plugToCapMap (getProperty fragment :plugToCapMap)))
    (for mate mates
	 (print (% "capping mate: %s" mate))
	 (let ((existingCap (getCap mate))
	       (newCap (if (contains plugToCapMap plugName)
			   (lookup plugToCapMap plugName)
			 (progn
			   (print (% "WARNING!  There is no cap defined for plug: %s" plugName))
			   (return-from ())))))
	   (when (and (not (isNil existingCap)) (not (== existingCap newCap)))
	     (raise (LispError (% "While capping mate[%s] found that there is already a cap[%s] defined for plug[%s] and its different from the one you are setting it to[%s/%s]" mate existingCap plugName newCap (className newCap)))))
	   (log (% "Capping plug[%s] with cap[%s]" plugName newCap))
	   (setCap mate newCap)))))



(defClass MetaDatabase ()  (_MetaConstitutions _GroupAdds _Recognizers _Groups ))

(defMethod __init__ MetaDatabase (self )
  (setq (slot self _Recognizers ) (Binder))
  (setq (slot self _MetaConstitutions )(Binder))
  (setq (slot self _GroupAdds )())
  (setq (slot self _Groups )()))
(defMethod addRecognizer MetaDatabase (self frameRecognizer )
  (extend (slot self _Recognizers ) (getRecognizerName frameRecognizer )frameRecognizer )
  (addFrameRecognizer *DATABASE* frameRecognizer ))
(defMethod getFrameRecognizer MetaDatabase (self recognizerName )
  (lookup (slot self _Recognizers )recognizerName ))
(defMethod hasRecognizer MetaDatabase (self name )
  (contains (slot self _Recognizers )name ))



(defMethod addGroupAdd MetaDatabase (self groupAdd )
  (setq (slot self _GroupAdds )(Cons groupAdd (slot self _GroupAdds ))))
(defMethod getMetaConstitutions MetaDatabase (self )
  (slot self _MetaConstitutions ))

(defMethod getMetaConstitution MetaDatabase (self name )
  (lookup (slot self _MetaConstitutions )name ))
(defMethod addMetaConstitution MetaDatabase (self metaConstitution )
  (let ((name (getGroup metaConstitution )))
    (log (% "Adding MetaConstitution(%s)" name ))
    (assert (not (contains (slot self _MetaConstitutions )name ))
	    (% "MetaConstitution %s has already been defined" name ))
    (extend (slot self _MetaConstitutions )name metaConstitution )))


(defMethod checkTopologiesForProblems MetaDatabase (self )
  (prognLog "Checking topologies"
	    (let ((collection (MetaOutPlugCollection)))
	      (print (dumpMethods MetaOutPlugCollection))
	      MetaDatabase (for mce (keysAsCons (slot self _MetaConstitutions ))
				(for mop (getMetaOutPlugs (lookup (slot self _MetaConstitutions) mce))
				     (addMetaOutPlug collection mop )))
	      (let ((sawProblem false)
		    (fullReport (StringOutStream)))
		(for key (keysAsCons (getMetaOutPlugsGroupedByName collection ))
		     (let ((checker (lookup (getMetaOutPlugsGroupedByName collection) key))
			   (report (StringOutStream )))
		       (ifFalse (allMetaOutPlugsAreConsistent checker report )
				(writeln fullReport  "PROBLEM!!" )
				(writeln fullReport (% "%s" (str report )))
				(setq sawProblem true )
				)))
		(ifTrue sawProblem 
			(log (% "%s" (str fullReport )))
			(log "There are problems in the database - fix them to proceed" )
			(raise (LispError (str fullReport))))))))
 
(defMethod collectGroupsWithTopologiesWithRingClosingPlugNames MetaDatabase (self plugName )
  (let ((groupNames (StringSet ))mc )
    (for mce (keysAsCons (slot self _MetaConstitutions ))
	 (setq mc (lookup (slot self _MetaConstitutions) mce ))
	 (when (hasTopologyWithRingClosingPlugNamed mc plugName )
	   (insert groupNames (getGroupName mc ))))
    groupNames ))

(defMethod collectGroupsWithTopologiesWithPlugNames MetaDatabase (self plugName )
  (let (groupNames mc )
    (setq groupNames (SymbolSet ))
    (for mce (keysAsCons (slot self _MetaConstitutions ))
	 (setq mc (lookup (slot self _MetaConstitutions) mce ))
	 (when (hasTopologyWithPlugNamed mc plugName )
	   (insert groupNames (getGroupName mc ))))
    groupNames ))


;;
;;
;; 
(defMethod defineMatesWithRepresentativesForAllPlugs MetaDatabase (self )
  (prognLog
   (% "Define Mates for all PlugsWithMates in all MetaConstitutions[%s]" (keysAsCons (slot self _MetaConstitutions)))
   (for mce (keysAsCons (slot self _MetaConstitutions ))
	(let* ((mc (lookup (slot self _MetaConstitutions) mce))
	       (metaPlugs (getMetaPlugs mc )))
	  (log (% "======== Identifying mates for all PlugWithMates in: %s" (getGroupName mc )))
	  (for mp metaPlugs 
	       (when (hasMates mp )
		 (prognLog (% "Looking at plug: %s" (getName mp))
			   (let* ((otherSidePlugName (DirectionalCoupling_otherSidePlugName (getName mp )))
				  (predictedMateGroupNames (collectGroupsWithTopologiesWithPlugNames self otherSidePlugName ))
				  (givenMateCons (getMates mp )))
			     (when (> (length givenMateCons )1 )
			       (raise "There is more than one mates, this means I have to decide which Mate handle which groups and this depends on what type of cap each mates has - deal with this if/when it comes up" ))
			     (let ((givenMate (car givenMateCons )))
			       (when (isNil givenMate )
				 (log (% "givenMate can not be nil for MetaPlug=%s" (getName mp )))
				 (dump mp )
				 (raise (LispError (% "There is no mate defined for MetaPlug[%s]" (getName mp)))))
			       (log (% "Setting mates to predicted ones: %s" (repr predictedMateGroupNames )))
			       (setGroupNames givenMate predictedMateGroupNames ))))))))))


(defMethod updateRingClosingMatesForRingClosingPlugs MetaDatabase (self )
  (let (mc metaPlugs otherSidePlugName predictedMateGroupNames
	   givenRingClosingMateCons givenRingClosingMate
	   givenRingClosingMateGroupNames )
    (for mce (keysAsCons (slot self _MetaConstitutions ))
	 (setq mc (lookup (slot self _MetaConstitutions) mce ))
	 (log (% "========= Identifying RingClosingMates for RingClosingPlugs in: %s" (getGroupName mc )))
	 (setq metaPlugs (getMetaPlugs mc ))
	 (for mp metaPlugs 
	      (when (isRingClosingPlug mp )
		(setq otherSidePlugName (getOtherSidePlugName mp ))
		(setq predictedMateGroupNames
		      (collectGroupsWithTopologiesWithRingClosingPlugNames self otherSidePlugName ))
		(setq givenRingClosingMateCons (getRingClosingMates mp ))
		(when (> (length givenRingClosingMateCons )1 )
		  (raise "There is more than one mates, this means I have to decide which Mate handle which groups and this depends on what type of cap each mates has - deal with this if/when it comes up" ))
		(setq givenRingClosingMate (car givenRingClosingMateCons ))
		(setq givenRingClosingMateGroupNames (getUnExpandedNames givenRingClosingMate ))
		(log (% "Given RingClosingMates = %s" (repr givenRingClosingMateGroupNames )))
		(log (% "Setting mates to predicted ones: %s" (repr predictedMateGroupNames )))
		(setGroupNames givenRingClosingMate predictedMateGroupNames ) ;
					; Now mateGroups should contain a list of group names that provide the other side plug name
					; Merge them into the mate list for this plug
					;
		)))))


(defMethod checkMetaDatabaseForProblems MetaDatabase (self )
  (checkAllMates self )
  (checkTopologiesForProblems self ))

(defMethod createConstitution MetaDatabase (self fragment )
  (let (group metaConstitution constitution groupAdds )
    (setq group (getProperty fragment :group ))
    (log (% "Creating Constitution for group(%s)" group ))
    (ifFalse (contains (slot self _MetaConstitutions )group )
	     (let ((ss (StringOutStream )))
	       (writeln ss (% "Creating Constitution for chemdraw constitution(%s)" (getProperty fragment :name )))
	       (writeln ss (% "   that is supposed to be in group(%1%)" group ))
	       (writeln ss (% "   but [self slot _MetaConstitutions] did not contain the name(%s)" group ))
	       (writeln ss (% "   known MetaConstitutions groups are(%s)" (allKeysAsString (slot self _MetaConstitutions ))))
	       (writeln ss (% "   define a MetaConstitution with the name(%s)" group ))
	       (raise (LispError (str ss )))))
    (setq metaConstitution (lookup (slot self _MetaConstitutions )group ))
    (prognLog "Resolving Constitution"
	      (setq constitution (resolve metaConstitution fragment )))
    (assert (notNil constitution )"constitution can not be nil" )
    (log (% "Adding constitution(%s)" (constitutionName constitution )))
    (addEntity *DATABASE* constitution )
    (setq groupAdds (getGroupAdds metaConstitution ))
    (for groupAdd groupAdds 
	 (resolve groupAdd fragment ))))



(defMethod addChemdrawFragments MetaDatabase (self chemdrawConstitutions )
  ;; When monomers are drawn in Chemdraw you can add the property :cap to
  ;; describe any Plugs that that monomer caps
  ;; Here we accumulate a map of all of the plugs that are capped and what
  ;; caps them and we put it into each monomer under the property :plugToCapMap
  ;;
  (let ((mapOfPlugsToCaps (createAMapOfAllPlugsToCaps chemdrawConstitutions)))
    (for cc chemdrawConstitutions 
	 (setProperty cc :plugToCapMap mapOfPlugsToCaps)
	 (prognLog (% "ChemdrawConstitution ----> %s" (getProperty cc :nameTemplate ))
		   (createConstitution self cc )))))



(defMethod checkChemdraw MetaDatabase (self )
  (let (representative members ss metaConstitution capName serr capConstitution
		       capMetaConstitutionName allMateNames ss alignTo plugName
		       found anchorFrame extractScaffold originAtom
		       frameRecognizer constitutionAtom
		       constitutionAtoms matches mates )
    (prognLog
     "Checking monomer groups"
     (let ((representedEntityNameSets (representedEntityNameSetsAsCons *DATABASE*)))
       (print (% "representedEntityNameSets = [%s]" representedEntityNameSets ))
       (for representedEntityNameSet representedEntityNameSets
	    (block inner
		   (log (% "Checking representedEntityNameSet(%s)" (chem:getName representedEntityNameSet )))
		   (setq representative (getRepresentative representedEntityNameSet ))
		   (setq members (getMonomerNames representedEntityNameSet ))
		   (when (isNil representative )
		     (log  "     no representative -> skip" )
		     (return-from inner))
		   (when (== (size members ) 0)
		     (log  "     no members -> skip" )
		     (return-from inner))
		   (ifFalse (contains members representative )
			    (setq ss (StringOutStream ))
			    (writeln ss (% "The RepresentedEntityNameSet(%s) has a representative(%s) that is not a member" (chem:getName representedEntityNameSet )representative ))
			    (writeln ss (% "   The members of this group are(%s)" (repr members )))
			    (log (str ss ))
			    (log  "Terminating so that you can fix it" )
			    (print (str ss))
			    (exit 1 ))))))
    (prognLog
     "Checking plug caps"
     (for mce (keysAsCons (slot self _MetaConstitutions ))
	  (setq metaConstitution (lookup (slot self _MetaConstitutions) mce ))
	  (for metaPlug (getMetaPlugs metaConstitution )
	       (block inner
		      (ifTrue (isOfClass metaPlug MetaOriginPlug )
			      (return-from inner ))
		      (for mate (getMates metaPlug )
			   (setq capName (getCap mate ))
			   (when (isNil capName)
			     (setq serr (StringOutStream))
			     (writeln serr (% "There is no cap defined for MetaConstitution(%s)    MetaPlug(%s)"
					      (getMetaConstitutionName metaConstitution )(getName metaPlug )))
			     (raise (LispError (str serr ))))
			   (ifFalse (recognizesNameOrPdb *DATABASE* capName)
				    (setq serr (StringOutStream ))
				    (writeln serr (% "A cap(%s) for a Mate was specified that does not exist in the database" capName ))
				    (writeln serr (% "MetaConstitution(%s)    MetaPlug(%s)" (getMetaConstitutionName metaConstitution )(getName metaPlug )))
				    (raise (LispError (str serr ))))
			   (setq capConstitution (constitutionForNameOrPdb *DATABASE* capName ))
			   (setq capMetaConstitutionName (getMetaConstitutionName capConstitution ))
			   (setq mates (getMates metaPlug ))
			   (for mate mates 
				(setq allMateNames (getUnExpandedNames mate))
				(ifFalse (getOverrideCapMateRequirement metaPlug)
					 (ifFalse (contains allMateNames capMetaConstitutionName)
						  (setq ss (StringOutStream))
						  (writeln ss (% "There is a problem with MetaConstitution(%s)/MetaPlug(%s)"
								 (getGroup metaConstitution )(getName metaPlug )))
						  (writeln ss (% "    it has a cap(%s) with the MetaConstitution(%s) that is not one of its mates"
								 capName capMetaConstitutionName ))
						  (writeln ss (% "    currently its mates are defined by groups that have the plug name(%s)"
								 (otherSidePlugName metaPlug )))
						  (writeln ss (% "    and they are: ( %s )" (asString allMateNames )))
						  (writeln ss (% "    Solution:  1. To MetaConstitution(%s) create a topology with the plug(%s)"
								 capMetaConstitutionName (otherSidePlugName metaPlug )))
						  (writeln ss (% "               2. To MetaConstitution/MetaPlug(%s/%s) add to the Mate the option 'overrideCapMateRequirement: true'"
								 (getGroup metaConstitution ) (getName metaPlug )))
						  (log (str ss ))
						  (log  "Terminating so that you can fix it" )
						  (print (str ss))
						  (exit 1 )))))))))
    ))



;;
;;
;;
;; End of MetaDatabase
;;
;;
;;




(defClass MetaExtractIncompleteFrame MetaObject (_plugName _metaFrame _alias )
  (method __init__ (self &key plugName metaFrame alias )
	  (callAncestorMethod self )
	  (assert (notNil plugName )"plugName must be provided" )
	  (assert (notNil metaFrame )"metaFrame must be provided" )
	  (assert (notNil alias )"alias must be provided" )
	  (setq (slot self _metaFrame) metaFrame )
	  (setq (slot self _plugName) plugName )
	  (setq (slot self _alias) alias ))
  (method repr (self) (% "%s[%s]" (className self) (repr (slot self _metaFrame))))
  (method getMetaFrameName (self ) 
	  (let ((result (getName (slot self _metaFrame)))) ; I changed this from (slot self _metaFrame)
	    (prognLog (% "In MetaExtractIncompleteFrame::getMetaFrameName returning[%s]" result)
		      result))) 

  (method resolve (self fragment )
	  (let (frame key value )
	    (setq frame (resolve (slot self _metaFrame )fragment ))
	    (setq key (genKey self (getName frame )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq value (ExtractIncompleteFrame :alias (slot self _alias ):frame frame :plugName (slot self _plugName )))
	    (setProperty fragment key value ))))





(defClass MetaExtractCompleteFrame MetaObject (_metaFrame )
  (method __init__ (self &key metaFrame )
	  (callAncestorMethod self )
	  (setq (slot self _metaFrame )metaFrame ))
  (method getMetaFrameName (self )
	  (prognLog "In MetaExtractCompleteFrame::getMetaFrameName"
		    (getName (slot self _metaFrame ))))
  (method repr (self) (% "%s[%s]" (className self) (getMetaFrameName self)))
  (method resolve (self fragment )
	  (let (frame key value )
	    (setq frame (resolve (slot self _metaFrame )fragment ))
	    (setq key (genKey self (getName frame )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq value (ExtractCompleteFrame :frame frame ))
	    (setProperty fragment key value ))))


(defClass MetaExtractFrameFinisher MetaObject (_alias _plugName )
  (method __init__ (self &key alias plugName )
	  (callAncestorMethod self )
	  (assert (notNil alias )"alias must be provided" )
	  (assert (notNil plugName )"plugName must be provided" )
	  (setq (slot self _alias )alias )
	  (setq (slot self _plugName )(DirectionalCoupling_inPlugName (DirectionalCoupling_couplingName plugName ))))
  (method getAlias (self )
	  (slot self _alias ))

  (method getPlugName (self )
	  (slot self _plugName ))

  (method resolve (self fragment )
	  (let ( key value )
	    (setq key (genKey self (slot self _plugName )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq value (ExtractFrameFinisher :alias (slot self _alias ):plugName (slot self _plugName )))
	    (setProperty fragment key value ))))



(defClass MetaAnchor MetaObject (_plugName )
  (method __init__ (self pn )
	  (callAncestorMethod self )
	  (setq (slot self _plugName )pn ))
  (method resolve (self fragment )
	  (let ( key value )
	    (setq key (genKey self "scaffold" ))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq value (AnchorOnOtherSideOfPlug :plugName (slot self _plugName )))
	    (setProperty fragment key value ))))



(defClass MetaExtractScaffold MetaObject (_anchor _topologyName _metaExtractCompleteFrames _metaExtractIncompleteFrames _metaExtractFrameFinishers _metaInPlug )
  (method __init__ (self metaInPlug )
	  (callAncestorMethod self )
	  (setq (slot self _metaInPlug )metaInPlug )
	  (setq (slot self _anchor )())
	  (setq (slot self _metaExtractCompleteFrames )())
	  (setq (slot self _metaExtractIncompleteFrames )())
	  (setq (slot self _metaExtractFrameFinishers )())
	  (createMetaAnchor self ))


  (method describe (self)
	  (let ((d (description self)))
	    (log d)))

  (method description (self )
	  (let (ss )
	    (setq ss (StringOutStream ))
	    (writeln ss  "MetaExtractScaffold description follows" )
	    (writeln ss  "MetaExtractCompleteFrames" )
	    (for one (slot self _metaExtractCompleteFrames )
		 (writeln ss (% "     complete MetaFrame(%s)" (getMetaFrameName one ))))
	    (writeln ss  "MetaExtractIncompleteFrames" )
	    (for one (slot self _metaExtractIncompleteFrames )
		 (writeln ss (% "   incomplete MetaFrame(%s)" (getMetaFrameName one ))))
	    (str ss )))


  (method createMetaAnchor (self )
	  (cond 
	   ((isOfClass (slot self _metaInPlug )MetaOriginPlug )	;E-A
	    (setq (slot self _anchor )(MetaAnchorOrigin (getMetaOriginFrame (slot self _metaInPlug ))))
	    (return-from createMetaAnchor ))
	   ((isOfClass (slot self _metaInPlug )MetaInPlug ) ;E-A
	    (setq (slot self _anchor )(MetaAnchorOnOtherSideOfPlug :plugName (getName (slot self _metaInPlug ))))
	    (return-from createMetaAnchor ))
	   (true 
	    (raise (% "Illegal type in metaInPlug = %s" (className (slot self _metaInPlug )))) 
	    )))


					;
					; Make sure that all of the Frames exported by plugs of the MetaTopology
					; are either ExtractCompleteFrames or ExtractIncompleteFrames in the
					; MetaExtractScaffold
					;
  (method check (self metaTopology )
	  (prognLog
	   "Checking its MetaExtractScaffold for consistency"
	   (describe self)
	   (for metaPlug (getMetaPlugs metaTopology )
		(prognLog
		 (% "Checking if %s exports a MetaFrame" (repr metaPlug))
		 (if (exportsMetaFrame metaPlug )
		     (let* ((foundExtractFrame 0)
			    (exportedMetaFrame (getExportMetaFrame metaPlug))
			    (exportedName (getName exportedMetaFrame)))
		       (log (% "The %s is supposed to export a MetaFrame named[%s]" (repr metaPlug) exportedName))
		       (prognLog
			(% "Looking through complete frames for one named[%s]" exportedName )
			(for metaExtractCompleteFrame (slot self _metaExtractCompleteFrames )
			     (log (% "This Scaffold has a complete Frame named[%s]" (getMetaFrameName metaExtractCompleteFrame)))
			     (if (== (getMetaFrameName metaExtractCompleteFrame ) exportedName )
				 (then
				  (log "AND IT IS THE ONE WE ARE LOOKING FOR")
				  (setq foundExtractFrame (+ foundExtractFrame 1 )))
				 (else
				  (log "But its not the one we are looking for")
				  ))))
		       (prognLog
			(% "Looking through incomplete frames for one named[%s]" exportedName)
			(for metaExtractIncompleteFrame (slot self _metaExtractIncompleteFrames )
			     (log (% "This Scaffold has an incomplete Frame named[%s]" (getMetaFrameName metaExtractIncompleteFrame)))
			     (if (== (getMetaFrameName metaExtractIncompleteFrame ) exportedName )
				 (then
				  (log "AND IT IS THE ONE WE ARE LOOKING FOR")
				  (setq foundExtractFrame (+ foundExtractFrame 1 )))
				 (else
				  (log "but its not the one we are looking for")
				  ))))
		       (cond 
			 ((== foundExtractFrame 0 )
			  (setq ss (StringOutStream ))
			  (log "I was not able to find any frame to export!!!!")
			  (log (% "ERROR: The MetaConstitution/MetaTopology(%s/%s) has a plug(%s)"
				  (getGroup (getMetaConstitution metaTopology ))
				  (getName metaTopology )
				  (getName metaPlug )))
			  (log (% "ERROR:     that exports the MetaFrame(%s) but its not part of the MetaTopology's MetaExtractScaffold" exportedName ))
			  (log    "ERROR:     the MetaScaffold MetaFrames are defined in ScaffoldSpec createFrameSpecs" )
			  (describe self)
			  (raise (LispError "There was an error in the database construction - check the log file and fix it")))

			 ((> foundExtractFrame 1 )
			  (setq ss (StringOutStream ))
			  (log (% "ERROR: VERY WEIRD: The MetaConstitution/MetaTopology(%s/%s) has a plug(%s)"
				  (getGroup (getMetaConstitution metaTopology ))
				  (getName metaTopology )
				  (getName metaPlug )))
			  (log (% "ERROR:    that exports the MetaFrame(%s) but it has several Extract;Frames in the MetaTopology's MetaScaffold" exportedName ))
			  (raise (LispError "Error - check the log file for a description and fix it" )))
			 (true 
			  (log (% "I found the one MetaFrame that is exported by the plug " ))
			  )))
		     (else
		      (log "It does not")))))))

  (method addMetaExtractCompleteFrame (self extractFrame )
	  (setq (slot self _metaExtractCompleteFrames )(Cons extractFrame (slot self _metaExtractCompleteFrames ))))
  (method addMetaExtractIncompleteFrame (self extractFrame )
	  (setq (slot self _metaExtractIncompleteFrames )(Cons extractFrame (slot self _metaExtractIncompleteFrames ))))
  (method addMetaExtractFrameFinisher (self extractFrame )
	  (log "addMetaExtractFrameFinisher" )
	  (setq (slot self _metaExtractFrameFinishers ) (Cons extractFrame (slot self _metaExtractFrameFinishers ))))
  (method addMetaExtractFrame (self extractFrame )
	  (cond 
	   ((isOfClass extractFrame MetaExtractCompleteFrame ) ;E-A
	    (addMetaExtractCompleteFrame self extractFrame ))  ;E-B

	   ((isOfClass extractFrame MetaExtractIncompleteFrame ) ;E-A
	    (addMetaExtractIncompleteFrame self extractFrame ))	 ;E-B

	   ((isOfClass extractFrame MetaExtractFrameFinisher ) ;E-A
	    (addMetaExtractFrameFinisher self extractFrame ))  ;E-B

	   (true 
	    (raise (% "Illegal class(%s) for extractFrame" (className extractFrame ))))))
  (method resolve (self fragment )
	  (let ( key value metaCompleteFrames metaIncompleteFrames metaFrameFinishers anchor )
	    (setq key (genKey self "scaffold" ))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq value (ExtractScaffold ))
	    (setq metaCompleteFrames (map (lambda (f )(resolve f fragment ))(slot self _metaExtractCompleteFrames )))
	    (setExtractCompleteFramesFromCons value metaCompleteFrames )
	    (setq metaIncompleteFrames (map (lambda (f )(resolve f fragment ))(slot self _metaExtractIncompleteFrames )))
	    (setExtractIncompleteFramesFromCons value metaIncompleteFrames )
	    (setq metaFrameFinishers (map (lambda (f )(resolve f fragment ))(slot self _metaExtractFrameFinishers )))
	    (setExtractFrameFinishersFromCons value metaFrameFinishers )
	    (setq anchor (resolve (slot self _anchor )fragment ))
	    (setAnchor value anchor )
	    (setProperty fragment key value ))))



(defClass MetaAddToGroup MetaSingletonObject (_GroupName _ConstitutionName )
  (method __init__ (self &key groupName constitutionName )
	  (callAncestorMethod self )
	  (assert (notNil groupName )"groupName must be provided" )
	  (setq (slot self _GroupName )groupName )
	  (setq (slot self _ConstitutionName )constitutionName )
	  (let ((recognized (recognizesRepresentedEntityNameSet *DATABASE* (slot self _GroupName ))))
	    (assert recognized (% "There is no group named [%s]  backTrace: %s" (slot self _GroupName )(dump self )))))

  (method resolve (self fragment )
	  (let ((constitutionName (if (isNil (slot self _ConstitutionName ))
				      (resolve (MetaProperty :name )fragment )
				    (resolve (slot self _ConstitutionName ) fragment )))
		(group (getRepresentedEntityNameSet *DATABASE* (slot self _GroupName ))))
	    (assert (notNil group )(% "There is no group named [%s]" (slot self _GroupName )))
	    (log (% "Adding %s to %s" constitutionName (slot self _GroupName )))
	    (addMonomerNameNoChecks group constitutionName ) 
	    )))
  
					;
					; With cyclic amino acids that present their cyclic amino acid on the leading
					; edge it is important to separate them into their own groups with their
					; own representative.  An "S" amino acid cannot be the representative for an 
					; "R" amino acid.


(defClass MetaAddToGroupByStereoisomer MetaSingletonObject (_groupPrefix _stereoInformation _center )
  (method __init__ (self &key groupPrefix stereoInformation separateByCenter )
	  (callAncestorMethod self )
	  (assert (notNil groupPrefix ) "groupPrefix must not be nil" )
	  (assert (notNil stereoInformation ) "stereoInformation must not be nil" )
	  (assert (notNil separateByCenter ) "separateByCenter must not be nil" )
	  (assert (isA stereoInformation MetaStereoInformation) "stereoInformation must be of class: MetaStereInformation")
	  (setq (slot self _groupPrefix ) groupPrefix )
	  (setq (slot self _stereoInformation ) stereoInformation )
	  (setq (slot self _center ) separateByCenter ))

  (method resolve (self fragment )
	  (let* ((stereoInformation (resolve (slot self _stereoInformation ) fragment ))
		 (groupPrefix (slot self _groupPrefix ))
		 (stereoisomers (stereoisomersAsCons stereoInformation )))
	    (if (> (length stereoisomers ) 1 )
		 (for stereoisomer stereoisomers 
		      (let* ((configuration (getConfigurationForCenter stereoisomer (slot self _center )))
			     (monomerName (chem:getName stereoisomer ))
			     (groupName (asSymbol (% "%s-%s" groupPrefix configuration )))
			     (groupNameRecognized (recognizesEntityName *DATABASE* groupName))
			     (group (getEntity *DATABASE* (if groupNameRecognized
							      groupName
							    (createRepresentedEntityNameSet :name groupName)))))
			(log (% "Adding %s to group[%s]" monomerName groupName ))
			(addMonomerNameNoChecks group monomerName )))
	      (let* ((stereoisomer (car stereoisomers ))
		     (monomerName (getName stereoisomer ))
		     (groupName groupPrefix )
		     (group (getEntity *DATABASE* groupName ))
		     (constitutionName (getConstitutionName (slot self _stereoInformation )fragment )))
		(log (% "Adding achiral %s to group %s" constitutionName constitutionName ))
		(addMonomerNameNoChecks group constitutionName )
	       )))))




(defun pdb-name-from-template (pdbTemplate permutationIndex numberOfPermutations)
  "Return a symbol pdb-name made from the pdbTemplate and the permutationIndex.
The pdbTemplate has the form \"TY(R Q)\" if permutationIndex is 0 it returns TYR
if its 1 it returns TYQ"
  (multiple-value-bind (firstPart firstPartEnd) (read-from-string pdbTemplate)
    (multiple-value-bind (options optionEnd) (read-from-string pdbTemplate () () :start firstPartEnd)
      (assert (== (length options) numberOfPermutations)
	      (bformat nil "For pdbTemplate[%s]\nThere are %d stereochemical permutations but the pdbTemplate only has %d options: %s" pdbTemplate numberOfPermutations (length options) options ))
      (if (> (length options) 0)
	  (let* ((permutationOption (nth permutationIndex options))
		 (name (bformat nil "%s%s" (symbol-name firstPart) (symbol-name permutationOption))))
	    (intern name))
	  (intern firstPart)
	  )
      )
    )
  )






;; Generate a Cons of Stereoisomer entries based on a list of
;; stereocenters and the nameTemplate
;;
;; Return a list of stereoisomers given the a list of center names
;; and a nameTemplate and pdbNameTemplate
;;
(defun generateStereoisomers (centers nameTemplate pdbNameTemplate )
  (let ((numCenters (length centers ))
	(numWildCards (/ (countOccurances nameTemplate "%" ) 2 ))
	(numWildCardsInPdb (/ (countOccurances pdbNameTemplate "%" ) 2 )))
    ;; permutations permutationIndex stereoisomers
    ;; monomerName ttt pdbName permutationIndex configs stereoisomers )
    (assert (<= numWildCardsInPdb 1 )
	    (% "PdbNameTemplates can only have one wildcard - error in(%s) for monomerTemplate(%s) - counted %d wildcards"
	       pdbNameTemplate nameTemplate numWildCards ))
    (ifFalse (== numCenters numWildCards )
	     (raise (LispError (% "Mismatch between the number of chiralCenters(%s)/%d and number of wild-cards in monomer name(%s)/%d"
				  (repr centers ) numCenters nameTemplate numWildCards ))))
    (if (> numCenters 0)
					; then
	(let ((permutations (stereochemicalPermutations numCenters ))
	      (permutationIndex 0 )
	      (stereoisomers ()))
	  (for perm permutations 
	       (log (% "pdbNameTemplate=%s" pdbNameTemplate ))
	       (let* ((monomerName (asSymbol (%% nameTemplate perm )))
		      (pdbName (pdb-name-from-template pdbNameTemplate
						       permutationIndex (length permutations)))
		      (configs (StereoConfiguration_createMultiple centers perm )))
		 (assert (notNil monomerName )"The monomer name is blank!" )
		 (assert (notNil pdbName )"The pdb name is blank!" )
		 (log (% "Generating stereoisomer name(%s) pdbName(%s)" monomerName pdbName ))
		 (setq stereoisomers (Cons (Stereoisomer :name monomerName
							 :pdb pdbName
							 :configs configs ) stereoisomers ))
		 (setq permutationIndex (+ permutationIndex 1 ))
		 ))
	  stereoisomers)
					; else
      (let ((monomerName (asSymbol nameTemplate) )
	    (pdbName (asSymbol pdbNameTemplate)))
	(log (% "Generating single stereoisomer name(%s) pdbName(%s)" monomerName pdbName ))
	(list (Stereoisomer :name monomerName :pdb pdbName :configs ()))))))



(defClass MetaProperty MetaObject (_PropertyName _resolveCallback )
  (method __init__ (self propertyName &key resolveCallback )
	  (callAncestorMethod self )
	  (assert (isOfClass propertyName Symbol )(% "propertyName(%s) is not a Symbol!!" (repr propertyName )))
	  (setq (slot self _resolveCallback) resolveCallback)
	  (setq (slot self _PropertyName )propertyName ))
  (method resolve (self fragment )
	  ;;
	  ;; The key for a Property is the _PropertyName
	  ;;
	  (if (hasProperty fragment (slot self _PropertyName ))
	      (getProperty fragment (slot self _PropertyName))
	    (raise (LispError (% "Could not find the Property[%s] in the ChemDraw Fragment[%s] - either add it or fix the reference to it" (slot self _PropertyName) fragment ))))))


(defClass MetaPropertyOrDefault MetaProperty (_DefaultValue)
  (method __init__ (self propertyName defaultValue)
	  (callAncestorMethod self propertyName )
	  (assert (isOfClass propertyName Symbol )(% "propertyName(%s) is not a Symbol!!" (repr propertyName )))
	  (setq (slot self _PropertyName )propertyName ))
  (method resolve (self fragment )
	  ;;
	  ;; The key for a Property is the _PropertyName
	  ;;
	  (let* ((key (slot self _PropertyName ))
		 (value (if (hasProperty fragment key)
			    (getProperty fragment key)
			  (slot self _DefaultValue))))
	    (log (% "Returning the property[%s] value[%s]" key (repr value)))
	    value))
  )



(defClass MetaConstitutionAtomWithName MetaObject (_AtomName )
  (method __init__ (self atomName )
	  (callAncestorMethod self )
	  (setq (slot self _AtomName ) atomName ))

  (method descriptionResolved (self fragment )
	  (let (atom )
	    (setq atom (resolve (slot self _AtomName ) fragment ))
	    (% "MetaConstitutionAtomWithName(%s)" atom )))
  (method resolve (self fragment )
	  (let (atomName key value retval consAtoms )
	    (setq atomName (resolve (slot self _AtomName )fragment ))
	    (setq key (genKey self atomName ))
	    (log (% "%s" (describeProperties fragment )))
	    (log (% "Just calculated key(%s) for MetaConstitutionAtomWithName atomName(%s)" key atomName ))
	    (ifTrue (hasProperty fragment key )
		    (let ((res (getProperty fragment key )))
		      (log (% "The fragment has the variable key(%s) returning its value(%s)" key (repr res )))
		      (return-from resolve res )))
	    (setq consAtoms (resolve (MetaConstitutionAtoms) fragment ))
	    (setq value (atomWithName consAtoms atomName ))
	    (log (% "Resolving MetaConstitutionAtomWithName key(%s) atomName(%s) value(%s) class(%s)" key atomName (repr value )(className value )))
	    (setq retval (setProperty fragment key value ))
	    (log (% "Returning MetaConstitutionAtomWithName key(%s) atomName(%s) retval(%s) class(%s)" key atomName (repr retval )(className retval )))
	    (log (% "fragment class(%s) value(%s)" (className fragment )(repr fragment )))
	    (log (% "%s" (describeProperties fragment )))
	    retval )))




;;
;; This class pulls up the :constitutionAtoms property
;; that was defined by the ChemDraw-Fragment itself
;;
(defClass MetaConstitutionAtoms MetaSingletonObject ()
  (method __init__ (self )
	  (callAncestorMethod self ))
  (method resolve (self fragment )
	  (getProperty fragment :constitutionAtoms)))

;;
;; A Residue is built from a resolved ConstitutionAtoms object only for
;; pattern matching using ChemInfo
(defClass MetaBuiltResidue MetaSingletonObject ()
  (method __init__ (self )
	  (callAncestorMethod self ))
  (method resolve (self fragment )
	  (getProperty fragment :builtResidue)))



(defClass MetaRecognizedAtom MetaObject (_UniqueName _Atom _Recognizer _Tag )
  (method __init__ (self uniqueName atom recognizer tag )
	  (callAncestorMethod self)
	  (setq (slot self _UniqueName ) uniqueName )
	  (assert (notNil atom )"atom is nil" )
	  (assert (isOfClass atom MetaConstitutionAtomWithName )
		  (% "atom must be MetaConstitutionAtomWithName but its: %s" (className atom )))
	  (setq (slot self _Atom )atom )
	  (assert (isOfClass recognizer MetaFrameRecognizer )
		  (% "recognizer must be of type MetaFrameRecognizer, instead it is: %s " (className recognizer )))
	  (setq (slot self _Recognizer )recognizer )
	  (setq (slot self _Tag )tag ))
  (method resolve (self fragment )
	  (let (mykey builtResidue constitutionAtom frameRecognizer match matchAtomName atom )
	    (setq mykey (genKey self (slot self _UniqueName )))
	    (prognLog (% "resolving MetaRecognizedAtom mykey[%s]" mykey )
		      (ifTrue (hasProperty fragment mykey )
			      (return-from resolve (getProperty fragment mykey )))
		      (log (% "%s [self slot _Atom] = %s" (repr (sourceFileLine ))(repr (slot self _Atom ))))
		      (setq builtResidue (resolve (MetaBuiltResidue) fragment)) ;; Not used here but 
		      (setq constitutionAtom (resolve (slot self _Atom ) fragment ))
		      ;; We have to get an Atom from a Residue to do the pattern recognition
		      (setq atom (atomWithName builtResidue (atomName constitutionAtom)))
		      (setq frameRecognizer (resolve (slot self _Recognizer ) fragment ))
		      (log (% "MetaRecognizedAtom mykey[%s]" mykey ))
		      (if (recognizes frameRecognizer atom)
			  (then 
			   (log (% "Matched frameRecognizer[%s]" (repr frameRecognizer)))
			   (setq match (getMatch frameRecognizer ))
			   (log (% "   to the atoms: %s" (repr match)))
			   (setq matchAtomName (chem:getName (tag match (slot self _Tag ))))
			   (log (% "MetaRecognizedAtom mykey[%s]" mykey )))
			(else 
			 (raise (LispError (% "Could not find match for atom(%s) recognizer(%s) tag(%s)"
					      (descriptionResolved (slot self _Atom )fragment )
					      (getRecognizerName (slot self _Recognizer ))(slot self _Tag ))))))
		      (log (% "About to write to variable mykey[%s]" mykey ))
		      (setProperty fragment mykey matchAtomName )))))


(defClass MetaRecognizedFrame MetaObject (_Name _MetaOrigin _Recognizer _source )
  (method __init__ (self &key name origin recognizer source )
	  (callAncestorMethod self )
	  (assert (notNil origin )"origin must be defined" )
	  (setq (slot self _Name )name )
	  (setq (slot self _MetaOrigin )origin )
	  (setq (slot self _Recognizer )recognizer )
					; Use the stack trace to define the source
	  (setq (slot self _source )source ))
  (method repr (self) (% "%s[%s]:%s" (className self) (slot self _Name) (repr (slot self _Recognizer)) ))
  (method getName (self ) (slot self _Name))

  (method getRecognizer (self ) (slot self _Recognizer ))

  (method resolve (self fragment )
	  (let ( key origin z )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq origin (resolve (slot self _MetaOrigin ) fragment ))
	    (assert (notNil origin ) (% "MetaRecognizedFrame origin atom is undefined define at: %s" (repr (slot self _source ))))
	    (setq z (RecognizedFrame :name (slot self _Name )
				     :origin origin
				     :recognizerName (getRecognizerName (slot self _Recognizer ))))
	    (setProperty fragment key z ))))



(defClass MetaExplicitFrame MetaObject (_Name _MetaOrigin _xAtom _xyAtom )
  (method __init__ (self &key name origin xAtom xyAtom )
	  (callAncestorMethod self )
	  (setq (slot self _Name )name )
	  (setq (slot self _MetaOrigin )origin )
	  (setq (slot self _xAtom )xAtom )
	  (setq (slot self _xyAtom )xyAtom ))


  (method resolve (self fragment )
	  (let ( key origin xAtom xyAtom z )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq origin (resolve (slot self _MetaOrigin )fragment ))
	    (setq xAtom (resolve (slot self _xAtom )fragment ))
	    (setq xyAtom (resolve (slot self _xyAtom )fragment ))
	    (setq z (ExplicitFrame :name (slot self _Name ):origin origin :xAtom xAtom :xyAtom xyAtom ))
	    (setProperty fragment key z ))))




(defClass MetaOriginFrame MetaObject (_Name _MetaOrigin )
  (method __init__ (self &key name origin )
	  (callAncestorMethod self )
	  (setq (slot self _Name )name )
	  (setq (slot self _MetaOrigin )origin ))
  (method getName (self )
	  (slot self _Name ))
  (method resolve (self fragment )
	  (let ( key origin z )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq origin (resolve (slot self _MetaOrigin )fragment ))
	    (setq z (OriginFrame :name (slot self _Name ):origin origin ))
	    (setProperty fragment key z ))))




(defClass MetaTwoAtomFrame MetaObject (_Name _MetaTwoAtom _xAtom )
  (method __init__ (self &key name origin xAtom )
	  (callAncestorMethod self )
	  (setq (slot self _Name )name )
	  (setq (slot self _MetaTwoAtom )origin )
	  (setq (slot self _xAtom )xAtom ))
  (method getName (self )
	  (slot self _Name ))
  (method resolve (self fragment )
	  (let ( key origin xAtom z )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq origin (resolve (slot self _MetaTwoAtom )fragment ))
	    (setq xAtom (resolve (slot self _xAtom )fragment ))
	    (setq z (TwoAtomFrame :name (slot self _Name ):origin origin :xAtom xAtom ))
	    (setProperty fragment key z ))))



(defClass MetaThreeAtomFrame MetaObject (_Name _MetaTwoAtom _xAtom _xyAtom )
  (method __init__ (self &key name origin xAtom xyAtom )
	  (assert (notNil origin )"origin must be defined" )
	  (assert (notNil xAtom )"xAtom must be defined" )
	  (assert (notNil xyAtom )"xyAtom must be defined" )
	  (callAncestorMethod self )
	  (setq (slot self _Name )name )
	  (setq (slot self _MetaTwoAtom )origin )
	  (setq (slot self _xAtom )xAtom )
	  (setq (slot self _xyAtom )xyAtom ))
  (method getName (self )
	  (slot self _Name ))
  (method resolve (self fragment )
	  (let ( key origin xAtom xyAtom z )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq origin (resolve (slot self _MetaTwoAtom )fragment ))
	    (setq xAtom (resolve (slot self _xAtom )fragment ))
	    (setq xyAtom (resolve (slot self _xyAtom )fragment ))
	    (setq z (ExplicitFrame :name (slot self _Name ):origin origin :xAtom xAtom :xyAtom xyAtom ))
	    (setProperty fragment key z ))))



					;    z initMetaStereoInformation stereoisomers restraints

(defClass MetaStereoInformation MetaObject (_ConstitutionName _ChiralCenters _MonomerName _PdbName _Restraints )
  (method __init__ (self constitutionName chiralCenters monomerName pdbName restraints )
	  (callAncestorMethod self )
	  (setq (slot self _ConstitutionName )constitutionName )
	  (setq (slot self _ChiralCenters )chiralCenters )
	  (setq (slot self _MonomerName )monomerName )
	  (setq (slot self _PdbName )pdbName )
	  (setq (slot self _Restraints )restraints ))

  (method resolve (self fragment )
	  (let* ((monomerName (resolve (slot self _MonomerName ) fragment ))
		 (key (genKey self monomerName )))
	    (when (hasProperty fragment key )
	      (return-from resolve (getProperty fragment key )))
	    (let* ((chiralCenters (resolve (slot self _ChiralCenters ) fragment ))
		   (pdbName (resolve (slot self _PdbName ) fragment ))
		   (restraints (resolve (slot self _Restraints ) fragment ))
		   (stereoisomers (generateStereoisomers chiralCenters monomerName pdbName ))
		   (value (StereoInformation :stereoisomers stereoisomers :restraints restraints )))
	      (setProperty fragment key value ))))

  (method getConstitutionName (self fragment )
	  (resolve (slot self _ConstitutionName )fragment ))

  (method getMonomerNamesByStereoCenter (self fragment )
	  (raise "monomerTemplate below isn't defined - what should this method do?????" )
	  (setq monomerTemplate (resolve (slot self _MonomerName )fragment ))))


(defun MetaStereoInformation_standard ()
  (let ((name (MetaProperty :name ))
	(chiralCenters (MetaPropertyOrDefault :chiralCenters ()))
	(monomerName (MetaProperty :nameTemplate ))
	(pdbName (MetaProperty :pdbTemplate ))
	(restraints (MetaPropertyOrDefault :restraints ())))
    (MetaStereoInformation name chiralCenters monomerName pdbName restraints )))


(defClass MetaAnchorOrigin MetaObject (_frame )
  (method __init__ (self frame )
	  (callAncestorMethod self )
	  (setq (slot self _frame )frame ))
  (method resolve (self fragment )
	  (let ( key frame value )
	    (setq key (genKey self "origin" ))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq frame (resolve (slot self _frame )fragment ))
	    (setq value (AnchorOrigin :originFrame frame ))
	    (setProperty fragment key value ))))



(defClass MetaAnchorOnOtherSideOfPlug MetaObject (_PlugName )
  (method __init__ (self &key plugName )
	  (callAncestorMethod self )
	  (setq (slot self _PlugName )plugName ))
  (method resolve (self fragment )
	  (let ( key value )
	    (setq key (genKey self (slot self _PlugName )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq value (AnchorOnOtherSideOfPlug :plugName (slot self _PlugName )))
	    (setProperty fragment key value ))))


(defClass MetaPlug MetaObject ()
  (method __init__ (self )	  (callAncestorMethod self ))
  (method isRingClosingPlug (self )	  false )
  (method repr (self) (% "MetaPlug-subclass[%s] should implement repr" (className self)))
  )


(defClass MetaMatedPlug MetaPlug (_Mates _OverrideCapMateRequirement )
  (method __init__ (self &key (overrideCapMateRequirement false) )
	  (callAncestorMethod self )
	  (setq (slot self _OverrideCapMateRequirement) overrideCapMateRequirement))
  (method getOverrideCapMateRequirement (self) (slot self _OverrideCapMateRequirement))
  (method hasMates (self )	  true )
  (method getMates (self )	  (slot self _Mates ) )
  (method getAllMates (self )
	  (let (result oneResult )
	    (setq result (StringSet ))
	    (for mate (slot self _Mates ) ;removeComment
		 (setq oneResult (getUnExpandedNames mate ))
		 (insertStringSet result oneResult ))
	    (asCons result )))
  (method otherSidePlugName (self )
	  (DirectionalCoupling_otherSidePlugName (slot self _Name ))))

(defClass MetaUnMatedPlug MetaPlug ()
  (method __init__ (self )
	  (callAncestorMethod self ))
  (method hasMates (self )
	  false )
  (method getMates (self )
	  ()))



(defClass MetaFrameRecognizer MetaSingletonObject (_recognizerName )
  (method __init__ (self recognizerName )
	  (setq (slot self _recognizerName )recognizerName ))
  (method repr (self) (% "%s[%s]" (className self) (slot self _recognizerName)))
  (method getRecognizerName (self )
	  (slot self _recognizerName ))

					;
					; Pull the FrameRecognizer out of the *metaDatabase* dynamic variable 
					;
  (method resolve (self fragment )
	  (let ( key value )
	    (setq key (genKey self (slot self _recognizerName )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq value (getFrameRecognizer *metaDatabase* (slot self _recognizerName )))
	    (setProperty fragment key value ))))


(defClass MetaRingClosingPlug MetaMatedPlug (_Name _otherSidePlugName _Bond0 _Bond1
						   _metaConstitution _ringClosingMates )
  (method __init__ (self &key name bond0 bond1
			 otherSidePlugName mates exportFrame (overrideCapMateRequirement false)
			 (framesCompletedByCoupling () framesCompletedByCouplingSensor )ringClosingMates )
	  (let (pluggedFrameFinishers rcms )
	    (assert (notNil otherSidePlugName )"Must provide otherSidePlugName" )
	    (callAncestorMethod self )
	    (setq (slot self _Name )name )
	    (setq (slot self _Bond0 )bond0 )
	    (setq (slot self _Bond1 )bond1 )
	    (setq (slot self _Mates )mates )
	    (setq (slot self _metaConstitution )())
	    (setq (slot self _otherSidePlugName )otherSidePlugName )
	    (setq rcms ringClosingMates )
	    (when (isNil rcms )
	      (log  "Creating one default RingClosingMate" )
	      (setq rcms (list (RingClosingMate ))))
	    (setq (slot self _ringClosingMates )rcms )))
  (method repr (self) (% "MetaRingClosingPlug[%s]" (slot self _Name)))
  (method isRingClosingPlug (self )	  true )
  (method getOtherSidePlugName (self )	  (slot self _otherSidePlugName ))
  (method isOutPlug (self )  true )
  (method getRingClosingMates (self )  (slot self _ringClosingMates ))
  (method setMetaConstitution (self top )	  (setq (slot self _metaConstitution )top ))
  (method description (self )
	  (let (out names )
	    (setq out (StringOutStream ))
	    (writeln out (% "MetaConstitution/MetaRingClosingPlug(%s/%s) " (getGroup (slot self _metaConstitution ))(slot self _Name )))
	    (str out )))
  (method matches (self other )
	  (let (selfNames otherNames res )
	    (setq selfNames (completeFrameFinisherAliasesAsStringSet self ))
	    (setq otherNames (completeFrameFinisherAliasesAsStringSet other ))
	    (log (% "RingClosing %s comparing (%s) == (%s) " (repr (sourceFileLine ))(asString selfNames )(asString otherNames )))
	    (log (% "selfNames isOfClass StringSet = %d" (isOfClass selfNames StringSet )))
	    (log (% "otherNames isOfClass StringSet = %d" (isOfClass otherNames StringSet )))
	    (setq res (equal selfNames otherNames ))
	    (log (% "      match == %d" res ))
	    res ))
  (method otherSidePlugName (self )
	  (DirectionalCoupling_otherSidePlugName (slot self _Name )))
  (method getName (self )
	  (slot self _Name ) )
  (method resolve (self fragment )
	  (let ( key name bond0 bond1 mates ringClosingMates exportFrame framesCompletedByCoupling value )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq name (slot self _Name ) )
	    (setq bond0 (resolve (slot self _Bond0 )fragment ))
	    (setq bond1 "" )
	    (ifTrue (notNil (slot self _Bond1 ))
		    (setq bond1 (resolve (slot self _Bond1 )fragment )))
	    (setq mates (slot self _Mates ))
	    (capAllMates name mates fragment)
	    (setq ringClosingMates (slot self _ringClosingMates ))
	    (setq value (RingClosingPlug :name name
					 :bond0 bond0
					 :bond1 bond1
					 :mates mates :ringClosingMates ringClosingMates ))
	    (setProperty fragment key value ))))



(defClass MetaOutPlug MetaMatedPlug (_Name _Bond0 _Bond1 _metaConstitution _StubPivotAtom )
  (method __init__ (self &key name bond0 bond1 stubPivotAtom (overrideCapMateRequirement false) )
	  (callAncestorMethod self :overrideCapMateRequirement overrideCapMateRequirement)
	  (ifFalse (or (isOfClass bond0 MetaRecognizedAtom )(isOfClass bond0 MetaProperty ))
		   (raise (% "bond0 must resolve to an atom name its current class is: %s" (className bond0 ))))
	  (assert (not (DirectionalCoupling_isInPlugName name ))(% "OutPlug name(%s) is invalid" name ))
	  (setq (slot self _Name )name )
	  (setq (slot self _Bond0 ) bond0 )
	  (setq (slot self _Bond1 ) bond1 )
	  (setq (slot self _StubPivotAtom) stubPivotAtom)
	  (setq (slot self _Mates) (list (Mate)))
	  (setq (slot self _metaConstitution ) () ))

  (method repr (self) (% "%s[%s]" (className self) (slot self _Name)))


  (method numberOfBonds (self)
	  "Return the number of bonds this MetaOutPlug makes"
	  (cond
	   ((not (== (slot self _Bond1) "")) 2 )
	   (true 1)))

  (method isOutPlug (self )
	  true )
  (method setMetaConstitution (self top )
	  (setq (slot self _metaConstitution )top ))
  (method description (self )
	  (let (out names )
	    (setq out (StringOutStream ))
	    (writeln out (% "MetaConstitution/MetaOutPlug(%s/%s) " (getGroup (slot self _metaConstitution ))(slot self _Name )))
	    (str out )))
  (method matches (self other )
	  (let ( res )
	    (log (% "%s comparing (%s) == (%s) " (repr (sourceFileLine ))(asString selfNames )(asString otherNames )))
	    (log (% "selfNames isOfClass StringSet = %d" (isOfClass selfNames StringSet )))
	    (log (% "otherNames isOfClass StringSet = %d" (isOfClass otherNames StringSet )))
	    (setq res (equal selfNames otherNames ))
	    (log (% "      match == %d" res ))
	    res ))
  (method getName (self )
	  (slot self _Name ) )
  (method resolve (self fragment )
	  (let ( key name bond0 bond1 mates exportFrame framesCompletedByCoupling value stubPivotAtom )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq name (slot self _Name ) )
	    (setq bond0 (resolve (slot self _Bond0 )fragment ))
	    (assert (isSubClassOf (class bond0 )Text )
		    (% "[self slot _Bond0] must resolve to a subclass of Text but its a %s"
		       (className (slot self _Bond0 ))))
	    (setq bond1 "" )
	    (ifTrue (notNil (slot self _Bond1 ))
		    (setq bond1 (resolve (slot self _Bond1 )fragment )))
	    (setq stubPivotAtom (if (notNil (slot self _StubPivotAtom))
				    (resolve (slot self _StubPivotAtom) fragment )
				  ""))
	    (setq mates (slot self _Mates ) )
	    (capAllMates name mates fragment)
	    (setq value (OutPlug :name name
				 :bond0 bond0
				 :bond1 bond1
				 :mates mates
				 :stubPivotAtom stubPivotAtom ))
	    (setProperty fragment key value ))))


(defClass MetaInPlug MetaMatedPlug (_Name _Bond0 _Bond1 _metaConstitution )
  (method __init__ (self &key name bond0 bond1 (overrideCapMateRequirement false))
	  (assert (DirectionalCoupling_isInPlugName name )(% "InPlug name(%s) is invalid" name ))
	  (callAncestorMethod self :overrideCapMateRequirement overrideCapMateRequirement)
	  (setq (slot self _Name )name )
	  (setq (slot self _Bond0 )bond0 )
	  (setq (slot self _Bond1 )bond1 )
	  (setq (slot self _Mates ) (list (Mate) ) )
	  (setq (slot self _metaConstitution )()))
  (method exportsMetaFrame (self )	  false )
  (method isOutPlug (self )
	  false )
  (method setMetaConstitution (self top )
	  (setq (slot self _metaConstitution )top ))
  (method otherSidePlugName (self )
	  (DirectionalCoupling_otherSidePlugName (slot self _Name )))
  (method getName (self )
	  (slot self _Name )  )
  (method resolve (self fragment )
	  (let ( key name bond0 bond1 mates value )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq name (slot self _Name ) ;removeComment
		  )
	    (setq bond0 (resolve (slot self _Bond0 )fragment ))
	    (setq bond1 "" )
	    (ifTrue (notNil (slot self _Bond1 ))
		    (setq bond1 (resolve (slot self _Bond1 )fragment )))
	    (setq mates (slot self _Mates ))
	    (capAllMates name mates fragment)
	    (setq value (InPlug :name name :bond0 bond0 :bond1 bond1 :mates mates ))
	    (setProperty fragment key value ))))



(defClass MetaOriginPlug MetaUnMatedPlug (_Name _OriginAtom _metaConstitution )
  (method __init__ (self &key name originAtom )
	  (assert (DirectionalCoupling_isInPlugName name )(% "OriginPlug name(%s) is invalid" name ))
	  (assert (notNil originAtom ) "MetaOriginPlug must have originAtom defined" )
	  (callAncestorMethod self )
	  (setq (slot self _Name )name )
	  (setq (slot self _OriginAtom ) originAtom )
	  (setq (slot self _metaConstitution )()))
  (method repr (self) (% "%s[%s]" (className self) (slot self _Name)))
  (method isOutPlug (self ) false )
  (method getMetaOriginAtom (self ) (slot self _OriginAtom ) )
  (method setMetaConstitution (self top ) (setq (slot self _metaConstitution )top ))
  (method getName (self )
	  (slot self _Name )  )
  (method resolve (self fragment )
	  (let* ( key name originAtom value )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq name (slot self _Name ) )
	    (setq originAtom (resolve (slot self _OriginAtom ) fragment ))
	    (setq value (OriginPlug :name name :originAtom originAtom ))
	    (setProperty fragment key value ))))






(defClass MetaJumpPlug MetaUnMatedPlug (_Name _JumpAtom _metaConstitution )
  (method __init__ (self &key name jumpAtomName )
	  (assert (DirectionalCoupling_isInPlugName name )(% "JumpPlug name(%s) is invalid" name ))
	  (assert (notNil jumpAtomName ) "MetaJumpPlug must have jumpAtomName defined" )
	  (callAncestorMethod self )
	  (setq (slot self _Name )name )
	  (setq (slot self _JumpAtom ) jumpAtomName )
	  (setq (slot self _metaConstitution )()))
  (method repr (self) (% "%s[%s]" (className self) (slot self _Name)))
  (method isOutPlug (self ) false )
  (method getMetaJumpAtom (self ) (slot self _JumpAtom ) )
  (method setMetaConstitution (self top ) (setq (slot self _metaConstitution )top ))
  (method getName (self )
	  (slot self _Name )  )
  (method resolve (self fragment )
	  (let* ( key name jumpAtomName value )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq name (slot self _Name ) )
	    (setq jumpAtomName (resolve (slot self _JumpAtom ) fragment ))
	    (setq value (JumpPlug :name name :jumpAtomName jumpAtomName ))
	    (setProperty fragment key value ))))









(defClass MetaFrameFinisher MetaObject (_plugName _metaFrame )
  (method __init__ (self frame )
	  (callAncestorMethod self )
	  (setq (slot self _plugName )())
	  (setq (slot self _metaFrame )frame ))
  (method repr (self)
	  (% "plugName[%s] metaFrame[%s]" (slot self _plugName) (repr (slot self _metaFrame))))
  (method getPlugName (self )
	  (slot self _plugName )	;removeComment
	  )
  (method getMetaFrame (self )
	  (slot self _metaFrame ))
  (method generateAlias (self )
	  (let (metaRecognizer recognizer )
	    (setq metaRecognizer (getRecognizer (slot self _metaFrame )))
	    (setq recognizer (getFrameRecognizer *DATABASE* (getRecognizerName metaRecognizer )))
	    (FrameFinisher_createAlias (slot self _plugName )recognizer )))
  (method setPlugName (self plugName )
	  (setq (slot self _plugName )plugName )
	  self )
  (method createMetaExtractFrameFinisher (self )
	  (let (alias z )
	    (setq alias (generateAlias self ))
	    (assert (!= alias (slot self _plugName ))"alias cannot equal [self slot _plugNam]e" )
	    (setq z (MetaExtractFrameFinisher :alias alias :plugName (slot self _plugName )))
	    z ))
  (method createMetaExtractIncompleteFrame (self )
	  (raise "may be depreciated" )
	  (setq z (new MetaExtractIncompleteFrame ))
	  (initMetaExtractIncompleteFrame z )
	  z )
  (method resolve (self fragment )
	  (let ( key frame value )
	    (setq key (genKey self (+ "frameFinisher" (getName (slot self _metaFrame )))))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (assert (notNil (slot self _plugName ))"[self slot _plugName] must be defined" )
	    (setq frame (resolve (slot self _metaFrame )fragment ))
	    (setq value (FrameFinisher :plugName (slot self _plugName ):frame frame ))
	    (setProperty fragment key value ))))


(defClass MetaFragment MetaObject (_Name _Atoms )
  (method __init__ (self &key name atoms )
	  (callAncestorMethod self )
	  (setq (slot self _Name ) name )
	  (setq (slot self _Atoms ) atoms ))
  (method getName (self )
	  (slot self _Name )		;removeComment
	  )
  (method resolveAtoms (self fragment )
	  (let (atoms )
	    (setq atoms ())
	    (for deferAtom (slot self _Atoms )
		 (setq atoms (Cons (resolve deferAtom fragment )atoms )))
	    atoms ))
  (method resolve (self fragment )
	  (let ( key name atoms value )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq name (slot self _Name ) )
	    (setq atoms (resolveAtoms self fragment ))
	    (setq value (Fragment :name name :atoms atoms ))
	    (setProperty fragment key value ))))





(defClass MetaRemainderFragment MetaObject (_Name _constitutionAtoms _RemoveFragments )
  (method __init__ (self &key constitutionAtoms name removeFragments )
	  (callAncestorMethod self )
	  (assert (isOfClass constitutionAtoms MetaConstitutionAtoms)
		  (% "The _constitutionAtoms argument must be of type MetaConstitutionAtoms - instead it is: %s" (className constitutionAtoms)))
	  (setq (slot self _constitutionAtoms ) constitutionAtoms )
	  (setq (slot self _Name ) name )
	  (setq (slot self _RemoveFragments ) removeFragments ))
  (method resolve (self fragment )
	  (let ( key residue consAtoms name removeAtoms value )
	    (setq key (genKey self (slot self _Name )))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (setq residue (resolve (slot self _constitutionAtoms ) fragment ))
	    (assert (isOfClass residue ConstitutionAtoms )
		    (% "The (residue) variable is not a Residue - it is of class: %s  the _Residue object has class: %s"
		       (className residue )(className (slot self _Residue ))))
	    (setq consAtoms (atomNamesAsStringSet residue ))
	    (setq name (slot self _Name )  )
	    (for frag (slot self _RemoveFragments )
		 (setq removeAtoms (StringSet :entries (resolveAtoms frag fragment )))
		 (setq consAtoms (removeAll consAtoms removeAtoms )))
	    (setq value (Fragment :name name :atoms (asCons consAtoms )))
	    (setProperty fragment key value ))))




(defClass MetaExtractCoreFragment MetaObject (_Fragment )
  (method __init__ (self &key fragment (metaScaffold () metaScaffoldPassed ))
	  (callAncestorMethod self )
	  (setq (slot self _Fragment )fragment ))
  (method getName (self )
	  (getName (slot self _Fragment )))
  (method resolve (self chemdrawFragment )
	  (let ( key fragment myowner myTopology scaffold value )
	    (setq key (genKey self "core" ))
	    (ifTrue (hasProperty chemdrawFragment key )
		    (return-from resolve (getProperty chemdrawFragment key )))
	    (setq fragment (resolve (slot self _Fragment )chemdrawFragment ))
	    (log (% "evaluating MetaExtractCoreFragment source = %s fragment description = %s " (dump self )(description fragment )))
	    (setq myowner (owner self ))
	    (assert (isAssignableTo myowner MetaTopology )(% "owner must be a MetaTopology - instead it has class(%s) value: %s" (className myowner )(repr myowner )))
	    (setq myTopology myowner )
	    (setq scaffold (getMetaScaffold myTopology chemdrawFragment ))	
	    (setq value (ExtractCoreFragment :fragment fragment :scaffold scaffold ))
	    (log (% "!!!!! Creating ExtractCoreFragment for fragment(%s)" (getProperty chemdrawFragment :name )))
	    (setProperty chemdrawFragment key value )))
  )



(defClass MetaExtractFragment MetaExtractCoreFragment (_Frame )
  (method __init__ (self &key fragment frame )
	  (assert (notNil fragment )"MetaExtractFragment fragment must be provided" )
	  (assert (notNil frame )"MetaExtractFragment frame must be provided" )
	  (callAncestorMethod self :fragment fragment ) ;	[ [ self base ] __init__ :fragment fragment ]

	  (setq (slot self _Frame )frame ))
  (method getMetaFrame (self )
	  (slot self _Frame ))

  (method resolve (self chemdrawFragment )
	  (let (key fragment frame value )
	    (setq key (genKey self "ExtractFragment" ))
	    (ifTrue (hasProperty chemdrawFragment key )
		    (return-from resolve (getProperty chemdrawFragment key )))
	    (setq fragment (resolve (slot self _Fragment )chemdrawFragment ))
	    (assert (notNil fragment )"fragment is nil" )
	    (setq frame (resolve (slot self _Frame )chemdrawFragment ))
	    (log (% "evaluating MetaExtractFragment source = %s" (dump self )))
	    (setq value (ExtractFragment :fragment fragment :frame frame ))
	    (setProperty chemdrawFragment key value )))
  )


(defClass MetaTopology MetaObject
  (_Name _NetCharge _MetaConstitution _ConstitutionAtoms  _Plugs _SuppressTrainers )
  (method __init__ (self &key name suppressTrainers constitutionAtoms plugs (netCharge 0) )
	  (callAncestorMethod self )
	  (setq (slot self _Name )name )
	  (setq (slot self _ConstitutionAtoms ) constitutionAtoms )
	  (setq (slot self _Plugs )(filterOutNil plugs ))
	  (setq (slot self _MetaConstitution )())
	  (setq (slot self _NetCharge) netCharge)
	  (setq (slot self _SuppressTrainers) suppressTrainers)
	  )
  (method getMetaConstitution (self )	  (slot self _MetaConstitution ))
  (method getInPlug (self )
	  (for p (slot self _Plugs )
	       (ifTrue (isOfClass p MetaInPlug )
		       (return-from getInPlug p ))
	       (ifTrue (isOfClass p MetaOriginPlug )
		       (return-from getInPlug p )))
	  ())

  (method getInPlugWithMates (self )
	  (for p (slot self _Plugs )
	       (ifTrue (isOfClass p MetaInPlug )
		       (return-from getInPlugWithMates p )))
	  ())

  (method getOutPlugs (self )
	  (let (plugs )
	    (for p (slot self _Plugs )
		 (ifTrue (isOutPlug p )
			 (setq plugs (Cons p plugs ))))
	    plugs ))

  (method setMetaConstitution (self mc )
	  (setq (slot self _MetaConstitution )mc ))
  (method getName (self )  (slot self _Name ))
  (method desc (self) (% "%s/%s" (desc (slot self _MetaConstitution)) (getName self)))
  (method getMetaPlugs (self ) (slot self _Plugs))

  (method getMetaPlugWithName (self plugName )
	  (for p (slot self _Plugs )	;removeComment
	       (ifTrue (== (getName p )plugName )
		       (return-from getMetaPlugWithName p )))
	  () )

  (method hasInPlugNamed (self name )
	  (let ((inPlug (getInPlugWithMates self )))
	    (ifTrue (isNil inPlug )
		    (return-from hasInPlugNamed false ))
	    (== (getName inPlug )name )))

  (method resolveOnlyPlugsWithMates (self fragment )
	  (let (plugs )
	    (for p (slot self _Plugs )
		 (when (not (isOfClass p MetaOriginPlug))
		   (setq plugs (Cons (resolve p fragment )plugs ))))
	    plugs ))

  (method resolve (self fragment &key constitutionName )
	  (let ((key (genKey self (slot self _Name))))
	    (ifTrue (hasProperty fragment key)
		    (return-from resolve (getProperty fragment key)))
	    (log  "MetaTopology resolve ---------" )
	    (let* ((name (slot self _Name))
		   (constitutionAtoms (resolve (MetaConstitutionAtoms) fragment))
		   (plugs (resolveOnlyPlugsWithMates self fragment))
		   (atomTreeTemplate (buildInternalCoordinateAtomTemplateTree fragment
									      constitutionAtoms plugs
									      constitutionName (slot self _Name)))
		   (chiList (build-chi-list atomTreeTemplate constitutionAtoms fragment name))
		   )
	      (log (% " topology name = %s" name ))
	      (let ((value (Topology :name name
				     :constitutionAtoms constitutionAtoms
				     :netCharge (slot self _NetCharge)
				     :atomTreeTemplate atomTreeTemplate
				     :chiList chiList
				     :suppressTrainers (slot self _SuppressTrainers)
				     :plugs plugs )))
		(setProperty fragment key value ))))
	  )
  )

(defClass MetaConstitution MetaObject (_group _name _addToGroups _stereoInformation _plugs _topologies )
  (method __init__ (self &key group name addToGroups  stereoInformation  plugs topologies )
	  (callAncestorMethod self )
	  (assert group "You must define group")
	  (assert name "You must define name")
	  (setq (slot self _group )group )
	  (setq (slot self _name ) name )
	  (assert addToGroups (% "While defining MetaConstitution[%s] you failed to defined addToGroups" name))
	  (setq (slot self _addToGroups ) addToGroups )
	  (setq (slot self _stereoInformation ) stereoInformation )
	  (setq (slot self _plugs ) (filterOutNil plugs ))
	  (setq (slot self _topologies ) topologies )
	  (mapcar (lambda (f )(setMetaConstitution f self )) (slot self _plugs ))
	  (mapcar (lambda (tt )(setMetaConstitution tt self )) (slot self _topologies )))
  (method getMetaOutPlugs (self )
	  (let (plugs )
	    (for p (slot self _plugs )	;removeComment
		 (ifTrue (isOutPlug p )
			 (setq plugs (Cons p plugs ))))
	    plugs ))

  (method hasTopologyWithPlugNamed (self plugName )
	  (for top (slot self _topologies ) ;removeComment
	       (ifTrue (notNil (getMetaPlugWithName top plugName ))
		       (return-from hasTopologyWithPlugNamed true )))
	  false )

  (method hasTopologyWithRingClosingPlugNamed (self plugName )
	  (let (plug )
	    (for top (slot self _topologies ) ;removeComment
		 (setq plug (getMetaPlugWithName top plugName ))
		 (ifTrue (notNil plug )
			 (ifTrue (isOfClass plug MetaRingClosingPlug )
				 (return-from hasTopologyWithRingClosingPlugNamed true ))))
	    false ))
  (method getMetaPlugInATopology (self plugName )
	  (let (plug )
	    (for top (slot self _topologies ) ;removeComment
		 (setq plug (getMetaPlugWithName top ))
		 (ifTrue (notNil plug )
			 (return-from getMetaPlugInATopology plug )))
	    () ))

  (method getMetaFrameNamed (self fn )
	  (for frame (slot self _frames ) ;removeComment
	       (ifTrue (== (getName frame ) fn )
		       (return-from getMetaFrameNamed frame )))
	  (raise (LispError (% "Could not find frame named(%s)" fn ))))

  (method getMetaPlugNamed (self plugName )
	  (for plug (slot self _plugs )
	       (ifTrue (== (getName plug) plugName )
		       (return-from getMetaPlugNamed plug)))
	  () )

  (method getMetaPlugs (self ) (slot self _plugs ))

  (method desc (self) (getMetaConstitutionName self))
  (method getMetaConstitutionName (self ) (slot self _group))
  (method getGroup (self ) (slot self _group))
  (method getGroupName (self ) (slot self _group))

  (method getGroupAdds (self ) (slot self _addToGroups ))
  (method getMetaTopologies (self ) (slot self _topologies) )

  (method allTopologiesWithInPlugNamed (self name )
	  (let (tops )
	    (for top (slot self _topologies ) ;removeComment
		 (ifTrue (hasInPlugNamed top name )
			 (setq tops (Cons top tops ))))
	    tops))

  (method resolve (self fragment )
	  (let (name key constitutionAtoms stereoInformation frames plugs fragments topologies value )
	    (setq name (resolve (slot self _name ) fragment ))
	    (setq key (genKey self name ))
	    (ifTrue (hasProperty fragment key )
		    (return-from resolve (getProperty fragment key )))
	    (prognLog "Resolving ConstitutionAtoms"
		      (setq constitutionAtoms (resolve (MetaConstitutionAtoms) fragment )))
	    (prognLog "Resolving & Validating StereoInformation"
		      (setq stereoInformation (resolve (slot self _stereoInformation ) fragment ))
		      (validate stereoInformation )
		      (log (% "stereoInformation--> %s" (repr stereoInformation ))))
	    (prognLog "Resolving Plugs"
		      (setq plugs (mapcar (lambda (e )(resolve e fragment ))(slot self _plugs ))))
	    (prognLog "Resolving Topologys"
		      (setq topologies (map (lambda (e )
					      (prognLog (% "Resolving topology[%s]" (getName e))
							(resolve e fragment :constitutionName name )))
					    (slot self _topologies ))))
	    (prognLog "Creating Constitution"
		      (setq value (Constitution
				   :metaConstitution (slot self _group )
				   :name name
				   :constitutionAtoms constitutionAtoms
				   :stereoInformation stereoInformation
				   :plugs plugs
				   :topologies topologies )))
	    (setProperty fragment key value ))))

