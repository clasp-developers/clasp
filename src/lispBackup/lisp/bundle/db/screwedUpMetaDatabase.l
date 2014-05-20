					; fixed code

;;
(include "CANDO/db/checkTopologies.l" )
(include "CANDO/db/checkMetaDatabase.l" )
(global *UniqueMetaId* 1 )


(defun getUniqueMetaId ()
  (let ((id (% "id%04d" *UniqueMetaId* )))
    (setq *UniqueMetaId* (+ *UniqueMetaId* 1 ))
    id ))



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
	    (foreach bt (slot self _backtrace )
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
	  (asKeywordSymbol (% "Singleton-%s-%s" (className self) desc ))))



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
	  (let ( key )
	    (setq key (% "%s-%s-%s" (className self ) (slot self _UniqueId) desc ))
	    (asKeywordSymbol key) ))
)

(defClass MetaDatabase ()  (_MetaConstitutions _GroupAdds _Recognizers _Groups )
  (method __init__ (self )
	  (setq (slot self _Recognizers )(ObjectDictionary ))
	  (setq (slot self _MetaConstitutions )(ObjectDictionary ))
	  (setq (slot self _GroupAdds )())
	  (setq (slot self _Groups )()))
  (method addRecognizer (self frameRecognizer )
	  (put (slot self _Recognizers )(getRecognizerName frameRecognizer )frameRecognizer )
	  (addFrameRecognizer *DATABASE* frameRecognizer ))
  (method getFrameRecognizer (self recognizerName )
	  (get (slot self _Recognizers )recognizerName ))
  (method hasRecognizer (self name )
	  (contains (slot self _Recognizers )name ))
  (method createSystemGroup (self &key groupName (representativeName "" ))
	  (prognLog (% "Creating system group[%s]" groupName)
		    (let (group recognized names )
		      (assert (notNil groupName )"You must provide groupName" )
		      (setq recognized (recognizesSystemRepresentedEntityNameSet *DATABASE* groupName ))
		      (when recognized 
			(setq names (getSystemRepresentedEntityNameSetKeys *DATABASE* ))
			(log (% "*DATABASE* currently contains SystemRepresentedEntityNameSet names: %s" (repr names )))
			(raise (% "*DATABASE* already contains SystemRepresentedEntityNameSet with name(%s)" groupName )))
		      (assert (not recognized ) (% "CandoDatabase already has system monomer group(%s)" groupName ))
		      (setq group (RepresentedEntityNameSet :name groupName :representativeName representativeName ))
		      (setq (slot self _Groups )(Cons group (slot self _Groups )))
		      (log (% "Adding to *DATABASE* -> system monomer group: %s" groupName ))
		      (addSystemRepresentedEntityNameSet *DATABASE* group )
		      group )))
  (method addGroupAdd (self groupAdd )
	  (setq (slot self _GroupAdds )(Cons groupAdd (slot self _GroupAdds ))))
  (method getMetaConstitutions (self )
	  (slot self _MetaConstitutions ))

  (method getMetaConstitution (self name )
	  (get (slot self _MetaConstitutions )name ))
  (method addMetaConstitution (self metaConstitution )
	  (let (name )
	    (setq name (getGroup metaConstitution ))
	    (log (% "Adding MetaConstitution(%s)" name ))
	    (assert (not (contains (slot self _MetaConstitutions )name ))
		    (% "MetaConstitution %s has already been defined" name ))
	    (put (slot self _MetaConstitutions )name metaConstitution )))


  (method checkTopologiesForProblems (self )
	  (prognLog "Checking topologies"
		    (let ((collection MetaOutPlugCollection))
		      ;; 
		      ;; Collect together all of the MetaOutPlugs
		      ;;
		      (foreach mce (entries (slot self _MetaConstitutions ))
			       (foreach mop (getMetaOutPlugs (value mce) )
					(addMetaOutPlug collection mop )))
		      (let ((sawProblem false)
			    (fullReport (StringOutStream)))
			(foreach entry (entries (getCheckers collection ))
				 (let ((key (key entry))
				       (checker (value entry))
				       (report (StringOutStream)))
				   (ifFalse (allMetaOutPlugsAreConsistent checker report )
					    (writeln fullReport  "PROBLEM!!" )
					    (writeln fullReport (% "%s" (str report )))
					    (setq sawProblem true )
					    ))
				 (ifTrue sawProblem 
					 (log (% "%s" (str fullReport )))
					 (log "There are problems in the database - fix them to proceed" )
					 (raise (LispError (str fullReport))))))))
 
  (method collectGroupsWithTopologiesWithRingClosingPlugNames (self plugName )
	  (let ((groupNames (StringSet ))mc )
	    (foreach mce (entries (slot self _MetaConstitutions ))
		     (setq mc (value mce ))
		     (when (hasTopologyWithRingClosingPlugNamed mc plugName )
		       (insert groupNames (getGroupName mc ))))
	    groupNames ))

  (method collectGroupsWithTopologiesWithPlugNames (self plugName )
	  (let (groupNames mc )
	    (setq groupNames (StringSet ))
	    (foreach mce (entries (slot self _MetaConstitutions ))
		     (setq mc (value mce ))
		     (when (hasTopologyWithPlugNamed mc plugName )
		       (insert groupNames (getGroupName mc ))))
	    groupNames ))

  (method updateMatesForPlugs (self )
	  (let (mc metaPlugs otherSidePlugName predictedMateGroupNames givenMateCons givenMate givenMateGroupNames )
	    (foreach mce (entries (slot self _MetaConstitutions ))
		     (setq mc (value mce ))
		     (log (% "======== Identifying mates for plugs in: %s" (getGroupName mc )))
		     (setq metaPlugs (getMetaPlugs mc ))
		     (foreach mp metaPlugs 
			      (when (hasMates mp )
				(setq otherSidePlugName (DirectionalCoupling_otherSidePlugName (getName mp )))
				(setq predictedMateGroupNames (collectGroupsWithTopologiesWithPlugNames self otherSidePlugName ))
				(setq givenMateCons (getMates mp ))
				(when (> (length givenMateCons )1 )
				  (raise "There is more than one mates, this means I have to decide which Mate handle which groups and this depends on what type of cap each mates has - deal with this if/when it comes up" ))
				(setq givenMate (car givenMateCons ))
				(when (isNil givenMate )
				  (log (% "givenMate can not be nil for MetaPlug=%s" (getName mp )))
				  (dump mp )
				  (raise "Bad givenMate" ))
				(setq givenMateGroupNames (getUnExpandedNames givenMate ))
				(log (% "Given mates = %s" (repr givenMateGroupNames )))
				(log (% "Setting mates to predicted ones: %s" (repr predictedMateGroupNames )))
				(setGroupNames givenMate predictedMateGroupNames ))))))
  (method updateRingClosingMatesForRingClosingPlugs (self )
	  (let (mc metaPlugs otherSidePlugName predictedMateGroupNames
		   givenRingClosingMateCons givenRingClosingMate
		   givenRingClosingMateGroupNames )
	    (foreach mce (entries (slot self _MetaConstitutions ))
		     (setq mc (value mce ))
		     (log (% "========= Identifying RingClosingMates for RingClosingPlugs in: %s" (getGroupName mc )))
		     (setq metaPlugs (getMetaPlugs mc ))
		     (foreach mp metaPlugs 
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
  (method check (self )
	  (checkAllMates self )
	  (checkTopologiesForProblems self ))

  (method createConstitution (self fragment )
	  (let (group metaConstitution constitution groupAdds )
	    (setq group (getProperty fragment :group ))
	    (log (% "Creating Constitution for group(%s)" group ))
	    (ifFalse (contains (slot self _MetaConstitutions )group )
		     (setq ss (StringOutStream ))
		     (writeln ss (% "Creating Constitution for chemdraw constitution(%s)" (getProperty fragment :name )))
		     (writeln ss (% "   that is supposed to be in group(%1%)" group ))
		     (writeln ss (% "   but [self slot _MetaConstitutions] did not contain the name(%s)" group ))
		     (writeln ss (% "   known MetaConstitutions groups are(%s)" (allKeysAsString (slot self _MetaConstitutions ))))
		     (writeln ss (% "   define a MetaConstitution with the name(%s)" group ))
		     (raise (str ss )))
	    (setq metaConstitution (get (slot self _MetaConstitutions )group ))
	    (prognLog "Resolving Constitution"
		      (setq constitution (resolve metaConstitution fragment )))
	    (assert (notNil constitution )"constitution can not be nil" )
	    (log (% "Adding constitution(%s)" (getName constitution )))
	    (addConstitution *DATABASE* constitution )
	    (setq groupAdds (getGroupAdds metaConstitution ))
	    (foreach groupAdd groupAdds 
		     (resolve groupAdd fragment ))))



  (method addChemdrawFragments (self chemdrawConstitutions )
	  (foreach cc  chemdrawConstitutions 
		   (prognLog (% "ChemdrawConstitution ----> %s" (getProperty cc :monomerName ))
			     (createConstitution self cc ))))



  (method checkChemdraw (self )
	  (let (representative members ss metaConstitution capName serr capConstitution
			       capMetaConstitutionName allMateNames ss alignTo plugName
			       found anchorFrame extractScaffold originAtom
			       frameRecognizer constitutionAtom
			       constitutionAtoms matches mates )
	    (prognLog "Checking monomer groups"
		      (foreach representedEntityNameSet (representedEntityNameSetsAsCons *DATABASE* )
			       (block inner
				      (log (% "Checking representedEntityNameSet(%s)" (getName representedEntityNameSet )))
				      (setq representative (getRepresentative representedEntityNameSet ))
				      (setq members (getMonomerNames representedEntityNameSet ))
				      (when (== representative "" )
					(log  "     no representative -> skip" )
					(return-from inner))
				      (when (== (size members ) 0)
					(log  "     no members -> skip" )
					(return-from inner))
				      (ifFalse (contains members representative )
					       (setq ss (StringOutStream ))
					       (writeln ss (% "The RepresentedEntityNameSet(%s) has a representative(%s) that is not a member" (getName representedEntityNameSet )representative ))
					       (writeln ss (% "   The members of this group are(%s)" (repr members )))
					       (log (str ss ))
					       (log  "Terminating so that you can fix it" )
					       (exit 1 )))))
	    (prognLog  "Checking plug caps"
		       (foreach mce (entries (slot self _MetaConstitutions ))
				(setq metaConstitution (value mce ))
				(foreach metaPlug (getMetaPlugs metaConstitution )
					 (block inner
						(ifTrue (isOfClass metaPlug MetaOriginPlug )
							(return-from inner ))
						(foreach mate (getMates metaPlug )
							 (setq capName (getCap mate ))
							 (ifFalse (recognizesNameOrPdb *DATABASE* capName )
								  (setq serr (StringOutStream ))
								  (writeln serr (% "A cap(%s) for a Mate was specified that does not exist in the database" capName ))
								  (writeln serr (% "MetaConstitution(%s)    MetaPlug(%s)" (getMetaConstitutionName metaConstitution )(getName metaPlug )))
								  (raise (LispError (str serr ))))
							 (setq capConstitution (constitutionForNameOrPdb *DATABASE* capName ))
							 (setq capMetaConstitutionName (getMetaConstitutionName capConstitution ))
							 (setq mates (getMates metaPlug ))
							 (foreach mate mates 
								  (setq allMateNames (getUnExpandedNames mate ))
								  (ifFalse (getOverrideCapMateRequirement mate )
									   (ifFalse (contains allMateNames capMetaConstitutionName )
										    (setq ss (StringOutStream ))
										    (writeln ss (% "There is a problem with MetaConstitution(%s)/MetaPlug(%s)" (getGroup metaConstitution )(getName metaPlug )))
										    (writeln ss (% "    it has a cap(%s) with the MetaConstitution(%s) that is not one of its mates" capName capMetaConstitutionName ))
										    (writeln ss (% "    currently its mates are defined by groups that the plug name(%s)" (otherSidePlugName metaPlug )))
										    (writeln ss (% "    and they are: ( %s )" (repr allMateNames )))
										    (writeln ss (% "    Solution:  1. To MetaConstitution(%s) create a topology with the plug(%s)" capMetaConstitutionName (otherSidePlugName metaPlug )))
										    (writeln ss (% "               2. To MetaConstitution/MetaPlug(%s/%s) add to the Mate the option 'overrideCapMateRequirement: true'" (getGroup metaConstitution )(getName metaPlug )))
										    (log (str ss ))
										    (log  "Terminating so that you can fix it" )
										    (exit 1 )))))))))
	    (prognLog  "Checking alignTos"
		       (foreach constitution (constitutionsAsCons *DATABASE* )
				(log (% "Constitution = %s" (getName constitution )))
				(foreach topology (topologiesAsCons constitution )
					 (log (% "Topology = %s" (getName topology )))
					 (setq alignTo (getAlignTo topology ))
					 (log (% "alignTo = %s" (repr alignTo )))
					 (cond 
					  ((isOfClass alignTo AnchorOnOtherSideOfPlug ) ;E-A
					   (setq plugName (getPlugName alignTo ))
					   (setq found false )
					   (foreach onePlug (plugsWithMatesAsCons topology )
						    (when (== (chem:getName onePlug )plugName )
						      (setq found true )))
					   (when (not found )
					     (setq serr (StringOutStream ))
					     (writeln serr  "You have defined an <alignTo> that will throw an error" )
					     (writeln serr (% "It is an AnchorOnOtherSideOfPlug with plugName(%s) that doesn't match a plug in the Topology" plugName ))
					     (writeln serr (% "It is in MetaConstitition(%s)" (getMetaConstitutionName constitution )))
					     (writeln serr (% "Topology(%s)" (getName topology )))
					     (raise (str serr )))) ;E-B

					  ((isOfClass alignTo AnchorOrigin ) ;E-A
					   (setq anchorFrame (chem:getFrame alignTo ))
					   (setq extractScaffold (getExtractScaffold topology ))
					   (when (not (hasExtractCompleteFrameWithName extractScaffold (chem:getName anchorFrame )))
					     (setq serr (StringOutStream ))
					     (writeln serr  "You have defined an <alignTo> that will throw an error" )
					     (writeln serr  "It is an AnchorOrigin that isn't a CompleteFrame within the Scaffold" )
					     (writeln serr (% "The AnchorOrigin has a frame name (%s)" (chem:getName anchorFrame )))
					     (writeln serr (% "It is in MetaConstitition(%s)" (getMetaConstitutionName constitution )))
					     (writeln serr (% "Topology(%s)" (getName topology )))
					     (raise (str serr )))) ;E-B

					  ((isOfClass alignTo RecognizedFrame ) ;E-A
					   (setq originAtom (getOriginAtomName alignTo ))
					   (setq frameRecognizer (getFrameRecognizer alignTo ))
					   (setq constitutionAtoms (getConstitutionAtoms constitution ))
					   (handler-case
					    (setq constitutionAtom (atomWithName constitutionAtoms originAtom))
					    (LispError (e)
						       (progn
							 (setq serr (StringOutStream ))
							 (writeln serr  "You have defined an <alignTo> that will throw an error" )
							 (writeln serr (% "It is a RecognizedFrame that has an origin atom(%s) that is not in the ConstitutionAtoms of the Constitution" originAtom ))
							 (writeln serr (% "It is in MetaConstitition(%s)" (getMetaConstitutionName constitution )))
							 (writeln serr (% "Topology(%s)" (getName topology )))
							 (raise (str serr )))))
					   (let* ((residue (makeResidue constitutionAtoms))
						  (atom (atomWithName residue (atomName constitutionAtom))))
					     (setq matches (recognizes frameRecognizer atom)))
					   (when (not matches )
					     (setq serr (StringOutStream ))
					     (writeln serr  "You have defined an <alignTo> that will throw an error" )
					     (writeln serr (% "It is a RecognizedFrame that has an origin atom(%s) that is not recognized by the FrameRecognizer(%s)"
							      (getRecognizerName frameRecognizer )))
					     (writeln serr (% "It is in MetaConstitition(%s)" (getMetaConstitutionName constitution )))
					     (writeln serr (% "Topology(%s)" (getName topology )))
					     (raise (str serr ))))
					  (true 
					   (raise (% "Unknown class(%s) for alignTo" (className alignTo )))))))))))


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
	    (setq key (genKey self (chem:getName frame )))
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
	    (setq key (genKey self (chem:getName frame )))
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
	    (foreach one (slot self _metaExtractCompleteFrames )
		     (writeln ss (% "     complete MetaFrame(%s)" (getMetaFrameName one ))))
	    (writeln ss  "MetaExtractIncompleteFrames" )
	    (foreach one (slot self _metaExtractIncompleteFrames )
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
	  (prognLog "Checking its MetaExtractScaffold for consistency"
		    (describe self)
		    (for metaPlug (getMetaPlugs metaTopology )
			 (prognLog (% "Checking if %s exports a MetaFrame" (repr metaPlug))
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
					 (prognLog (% "Looking through incomplete frames for one named[%s]" exportedName)
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
	  (let ((recognized (recognizesSystemRepresentedEntityNameSet *DATABASE* (slot self _GroupName ))))
	    (assert recognized (% "There is no group named [%s]  backTrace: %s" (slot self _GroupName )(dump self )))))
  (method resolve (self fragment )
	  (let (constitutionName group )
	    (if (isNil (slot self _ConstitutionName ))
		(then 
		 (setq constitutionName (resolve (MetaProperty :name )fragment )))
	      (else 
	       (setq constitutionName (resolve (slot self _ConstitutionName )fragment ))))
	    (setq group (getSystemRepresentedEntityNameSet *DATABASE* (slot self _GroupName )))
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
	  (assert (notNil groupPrefix )"groupPrefix must not be nil" )
	  (assert (notNil stereoInformation )"stereoInformation must not be nil" )
	  (assert (notNil separateByCenter )"separateByCenter must not be nil" )
	  (setq (slot self _groupPrefix )groupPrefix )
	  (setq (slot self _stereoInformation )stereoInformation )
	  (setq (slot self _center )separateByCenter ))
  (method resolve (self fragment )
	  (let (stereoInformation groupPrefix stereoisomers configuration monomerName groupName group constitutionName stereoisomer )
	    (setq stereoInformation (resolve (slot self _stereoInformation )fragment ))
	    (setq groupPrefix (slot self _groupPrefix )	;removeComment
		  )
	    (setq stereoisomers (stereoisomersAsCons stereoInformation ))
	    (if (> (length stereoisomers )1 )
		(then 
		 (foreach stereoisomer stereoisomers 
			  (setq configuration (getConfigurationForCenter stereoisomer (slot self _center )))
			  (setq monomerName (chem:getName stereoisomer ))
			  (setq groupName (% "%s-%s" groupPrefix configuration ))
			  (setq group (getSystemRepresentedEntityNameSet *DATABASE* groupName ))
			  (log (% "Adding %s to group %s" monomerName groupName ))
			  (addMonomerNameNoChecks group monomerName )))
	      (else 
	       (setq stereoisomer (car stereoisomers ))
	       (setq monomerName (chem:getName stereoisomer ))
	       (setq groupName groupPrefix )
	       (setq group (getSystemRepresentedEntityNameSet *DATABASE* groupName ))
	       (setq constitutionName (getConstitutionName (slot self _stereoInformation )fragment ))
	       (log (% "Adding achiral %s to group %s" constitutionName constitutionName ))
	       (addMonomerNameNoChecks group constitutionName )	;
					; Generate a Cons of Stereoisomer entries based on a list of stereocenters and the monomerNameTemplate
					;
	       )))))
(defun generateStereoisomers (centers monomerNameTemplate pdbNameTemplate )
  (let (numCenters numWildCards permutations permutationIndex stereoisomers
		   monomerName ttt pdbName permutationIndex configs stereoisomers )
    (setq numCenters (length centers ))
    (setq numWildCards (countOccurances monomerNameTemplate "%" ))
    (assert (<= (countOccurances pdbNameTemplate "%" )1 )
	    (% "PdbNameTemplates can only have one wildcard error in(%s) for monomerTemplate(%s)" pdbNameTemplate monomerNameTemplate ))
    (ifFalse (== numCenters numWildCards )
	     (raise (% "There is a mismatch between the number of chiralCenters(%s) and the number of wild-cards in the monomer name(%s)"
		       (repr chiralCenters )monomerNameTemplate )))
    (if (> numCenters 0 )
	(then
	 (setq permutations (stereochemicalPermutations numCenters ))
	 (setq permutationIndex 0 )
	 (setq stereoisomers ())
	 (foreach perm permutations 
		  (setq monomerName (%% monomerNameTemplate perm ))
		  (log (% "pdbNameTemplate=%s" pdbNameTemplate ))
		  (setq ttt (% pdbNameTemplate "yyy" ))
		  (log (% "ttt=%s" ttt ))
		  (setq pdbName (% pdbNameTemplate (% "%d" permutationIndex )))
		  (setq permutationIndex (+ permutationIndex 1 ))
		  (setq configs (StereoConfiguration_createMultiple centers perm ))
		  (assert (!= monomerName "" )"The monomer name is blank!" )
		  (assert (!= pdbName "" )"The pdb name is blank!" )
		  (log (% "Generating stereoisomer name(%s) pdbName(%s)" monomerName pdbName ))
		  (setq stereoisomers (Cons (Stereoisomer :name monomerName :pdb pdbName :configs configs )stereoisomers ))))
      (else 
       (setq stereoisomers ())
       (setq monomerName monomerNameTemplate )
       (setq pdbName pdbNameTemplate )
       (log (% "Generating single stereoisomer name(%s) pdbName(%s)" monomerName pdbName ))
       (setq stereoisomers (list (Stereoisomer :name monomerName :pdb pdbName :configs ())))))
    stereoisomers ))



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
	    (raise (LispError (% "Could not find the Property[%s] in the ChemDraw Fragment - either add it or fix the reference to it"))))))


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
	    (let ( key chiralCenters pdbName restraints stereoisomers value monomerName )
	      (setq monomerName (resolve (slot self _MonomerName )fragment ))
	      (setq key (genKey self monomerName ))
	      (ifTrue (hasProperty fragment key )
		      (return-from resolve (getProperty fragment key )))
	      (setq chiralCenters (resolve (slot self _ChiralCenters )fragment ))
	      (setq pdbName (resolve (slot self _PdbName )fragment ))
	      (setq restraints (resolve (slot self _Restraints )fragment ))
	      (setq stereoisomers (generateStereoisomers chiralCenters monomerName pdbName ))
	      (setq value (StereoInformation :stereoisomers stereoisomers :restraints restraints ))
	      (setProperty fragment key value )))
    (method getConstitutionName (self fragment )
	    (resolve (slot self _ConstitutionName )fragment ))
    (method getMonomerNamesByStereoCenter (self fragment )
	    (raise "monomerTemplate below isn't defined - what should this method do?????" )
	    (setq monomerTemplate (resolve (slot self _MonomerName )fragment ))))
  (defun MetaStereoInformation_standard ()
    (let (name chiralCenters monomerName pdbName restraints z )
      (setq name (MetaProperty :name ))
      (setq chiralCenters (MetaPropertyOrDefault :chiralCenters ()))
      (setq monomerName (MetaProperty :monomerName ))
      (setq pdbName (MetaProperty :pdbName ))
      (setq restraints (MetaPropertyOrDefault :restraints ()))
      (setq z (MetaStereoInformation name chiralCenters monomerName pdbName restraints ))
      z ))
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


  (defClass MetaMatedPlug MetaPlug (_Mates )
    (method __init__ (self )	  (callAncestorMethod self ))
    (method hasMates (self )	  true )
    (method getMates (self )	  (slot self _Mates ) )
    (method getAllMates (self )
	    (let (result oneResult )
	      (setq result (StringSet ))
	      (foreach mate (slot self _Mates ) ;removeComment
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
						     _ExportFrame _FrameFinishers _metaConstitution _ringClosingMates )
    (method __init__ (self &key name bond0 bond1
			   otherSidePlugName mates exportFrame
			   (framesCompletedByCoupling () framesCompletedByCouplingSensor )ringClosingMates )
	    (let (pluggedFrameFinishers rcms )
	      (assert (notNil otherSidePlugName )"Must provide otherSidePlugName" )
	      (assert (notNil exportFrame )"Must provide exportFrame" )
	      (assert (!= framesCompletedByCouplingSensor false )"framesCompletedByCoupling must be provided" )
	      (callAncestorMethod self )
	      (map (lambda (f )(assert (isOfClass f MetaFrameFinisher )
				       "Every framesCompletedByCoupling: element must be a MetaFrameFinisher" ))framesCompletedByCoupling )
	      (setq pluggedFrameFinishers (map (lambda (fc )(setPlugName fc name ))framesCompletedByCoupling ))
	      (setq (slot self _Name )name )
	      (setq (slot self _Bond0 )bond0 )
	      (setq (slot self _Bond1 )bond1 )
	      (setq (slot self _Mates )mates )
	      (setq (slot self _ExportFrame )exportFrame )
	      (setq (slot self _FrameFinishers )pluggedFrameFinishers )
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
    (method exportsMetaFrame (self )  true )
    (method getRingClosingMates (self )  (slot self _ringClosingMates ))

    (method getExportMetaFrame (self )  (slot self _ExportFrame ))
    (method setMetaConstitution (self top )	  (setq (slot self _metaConstitution )top ))
    (method getMetaExportFrame (self )	  (slot self _ExportFrame ))
    (method completeFrameFinisherAliasesAsStringSet (self )
	    (let (names nameSet )
	      (setq names ())
	      (foreach cf (slot self _FrameFinishers )
		       (setq names (Cons (generateAlias cf )names )))
	      (setq nameSet (StringSet :entries names ))
	      nameSet ))
    (method description (self )
	    (let (out names )
	      (setq out (StringOutStream ))
	      (writeln out (% "MetaConstitution/MetaRingClosingPlug(%s/%s) " (getGroup (slot self _metaConstitution ))(slot self _Name )))
	      (setq names (completeFrameFinisherAliasesAsStringSet self ))
	      (writeln out (% "     frameFinisherAliases(%s)" (asString names )))
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
    (method numberOfFrameFinishers (self )
	    (length (slot self _FrameFinishers )))
    (method otherSidePlugName (self )
	    (DirectionalCoupling_otherSidePlugName (slot self _Name )))
    (method getName (self )
	    (slot self _Name ) )
    (method getMetaFrameFinishers (self )
	    (slot self _FrameFinishers ))

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
	      (setq mates (slot self _Mates ) ;removeComment
		    )
	      (setq ringClosingMates (slot self _ringClosingMates ) ;removeComment
		    )
	      (setq exportFrame (resolve (slot self _ExportFrame )fragment ))
	      (setq framesCompletedByCoupling (map (lambda (mf )(resolve mf fragment ))(slot self _FrameFinishers )))
	      (log  "FrameFinishers..." )
	      (log (% "%s frameCompletedByCoupling = %s" (repr (sourceFileLine ))(repr framesCompletedByCoupling )))
	      (setq value (RingClosingPlug :name name :bond0 bond0 :bond1 bond1 :mates mates :exportFrame exportFrame :framesCompletedByCoupling framesCompletedByCoupling :ringClosingMates ringClosingMates ))
	      (setProperty fragment key value ))))
  (defClass MetaOutPlug MetaMatedPlug (_Name _Bond0 _Bond1 _ExportFrame _FrameFinishers _metaConstitution )
    (method __init__ (self &key name bond0 bond1 mates exportFrame (framesCompletedByCoupling ()framesCompletedByCouplingSensor ))
	    (callAncestorMethod self )
	    (assert (notNil mates )"mates must be defined" )
	    (assert (notNil exportFrame )"OutPlug must have exportFrame" )
	    (assert (!= framesCompletedByCouplingSensor false )(% "You must provide framesCompletedByCoupling - its value is(%s)" (repr framesCompletedByCoupling )))
	    (ifFalse (or (isOfClass bond0 MetaRecognizedAtom )(isOfClass bond0 MetaProperty ))
		     (raise (% "bond0 must resolve to an atom name its current class is: %s" (className bond0 ))))
	    (assert (not (DirectionalCoupling_isInPlugName name ))(% "OutPlug name(%s) is invalid" name ))
	    (map (lambda (f )(assert (isOfClass f MetaFrameFinisher )"Every framesCompletedByCoupling: element must be a MetaFrameFinisher" ))framesCompletedByCoupling )
	    (let (pluggedFrameFinishers )
	      (setq pluggedFrameFinishers (map (lambda (fc )(setPlugName fc name ))framesCompletedByCoupling ))
	      (setq (slot self _Name )name )
	      (setq (slot self _Bond0 )bond0 )
	      (setq (slot self _Bond1 )bond1 )
	      (setq (slot self _Mates )mates )
	      (setq (slot self _ExportFrame )exportFrame )
	      (setq (slot self _FrameFinishers )pluggedFrameFinishers )
	      (setq (slot self _metaConstitution )())))
    (method repr (self) (% "%s[%s]" (className self) (slot self _Name)))

    (method exportsMetaFrame (self )	  true )
    (method getExportMetaFrame (self )
	    (slot self _ExportFrame ))
    (method isOutPlug (self )
	    true )
    (method setMetaConstitution (self top )
	    (setq (slot self _metaConstitution )top ))
    (method getMetaExportFrame (self )
	    (slot self _ExportFrame ))
    (method completeFrameFinisherAliasesAsStringSet (self )
	    (let (names nameSet )
	      (setq names ())
	      (foreach cf (slot self _FrameFinishers ) ;removeComment
		       (setq names (Cons (generateAlias cf )names )))
	      (setq nameSet (StringSet :entries names ))
	      nameSet ))
    (method description (self )
	    (let (out names )
	      (setq out (StringOutStream ))
	      (writeln out (% "MetaConstitution/MetaOutPlug(%s/%s) " (getGroup (slot self _metaConstitution ))(slot self _Name )))
	      (setq names (completeFrameFinisherAliasesAsStringSet self ))
	      (writeln out (% "     frameFinisherAliases(%s)" (asString names )))
	      (str out )))
    (method matches (self other )
	    (let (selfNames otherNames res )
	      (setq selfNames (completeFrameFinisherAliasesAsStringSet self ))
	      (setq otherNames (completeFrameFinisherAliasesAsStringSet other ))
	      (log (% "%s comparing (%s) == (%s) " (repr (sourceFileLine ))(asString selfNames )(asString otherNames )))
	      (log (% "selfNames isOfClass StringSet = %d" (isOfClass selfNames StringSet )))
	      (log (% "otherNames isOfClass StringSet = %d" (isOfClass otherNames StringSet )))
	      (setq res (equal selfNames otherNames ))
	      (log (% "      match == %d" res ))
	      res ))
    (method numberOfFrameFinishers (self )
	    (length (slot self _FrameFinishers )))
    (method getName (self )
	    (slot self _Name ) )
    (method getMetaFrameFinishers (self )
	    (slot self _FrameFinishers )  )
    (method resolve (self fragment )
	    (let ( key name bond0 bond1 mates exportFrame framesCompletedByCoupling value )
	      (setq key (genKey self (slot self _Name )))
	      (ifTrue (hasProperty fragment key )
		      (return-from resolve (getProperty fragment key )))
	      (setq name (slot self _Name ) ;removeComment
		    )
	      (setq bond0 (resolve (slot self _Bond0 )fragment ))
	      (assert (isSubClassOf (class bond0 )Text )(% "[self slot _Bond0] must resolve to a subclass of Text but its a %s" (className (slot self _Bond0 ))))
	      (setq bond1 "" )
	      (ifTrue (notNil (slot self _Bond1 ))
		      (setq bond1 (resolve (slot self _Bond1 )fragment )))
	      (setq mates (slot self _Mates ) ;removeComment
		    )
	      (setq exportFrame (resolve (slot self _ExportFrame )fragment ))
	      (setq framesCompletedByCoupling (map (lambda (mf )(resolve mf fragment ))(slot self _FrameFinishers )))
	      (log  "FrameFinishers..." )
	      (log (% "%s framesCompletedByCoupling = %s" (repr (sourceFileLine ))(repr framesCompletedByCoupling )))
	      (setq value (OutPlug :name name :bond0 bond0 :bond1 bond1 :mates mates :exportFrame exportFrame :framesCompletedByCoupling framesCompletedByCoupling ))
	      (setProperty fragment key value ))))
  (defClass MetaInPlug MetaMatedPlug (_Name _Bond0 _Bond1 _metaConstitution )
    (method __init__ (self &key name bond0 bond1 mates )
	    (assert (DirectionalCoupling_isInPlugName name )(% "InPlug name(%s) is invalid" name ))
	    (callAncestorMethod self )
	    (setq (slot self _Name )name )
	    (setq (slot self _Bond0 )bond0 )
	    (setq (slot self _Bond1 )bond1 )
	    (setq (slot self _Mates )mates )
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
	      (setq mates (slot self _Mates ) ;removeComment
		    )
	      (setq value (InPlug :name name :bond0 bond0 :bond1 bond1 :mates mates ))
	      (setProperty fragment key value ))))
  (defClass MetaOriginPlug MetaUnMatedPlug (_Name _OriginFrame _metaConstitution )
    (method __init__ (self &key name originFrame )
	    (assert (DirectionalCoupling_isInPlugName name )(% "OriginPlug name(%s) is invalid" name ))
	    (assert (notNil originFrame )"MetaOriginPlug must have originFrame defined" )
	    (callAncestorMethod self )
	    (setq (slot self _Name )name )
	    (setq (slot self _OriginFrame )originFrame )
	    (setq (slot self _metaConstitution )()))
    (method repr (self) (% "%s[%s]" (className self) (slot self _Name)))
    (method exportsMetaFrame (self )
	    false )
    (method isOutPlug (self )
	    false )
    (method getMetaOriginFrame (self )
	    (slot self _OriginFrame ) )
    (method setMetaConstitution (self top )
	    (setq (slot self _metaConstitution )top ))
    (method getName (self )
	    (slot self _Name )  )
    (method resolve (self fragment )
	    (let ( key name originFrame value )
	      (setq key (genKey self (slot self _Name )))
	      (ifTrue (hasProperty fragment key )
		      (return-from resolve (getProperty fragment key )))
	      (setq name (slot self _Name ) )
	      (setq originFrame (resolve (slot self _OriginFrame )fragment ))
	      (setq value (OriginPlug :name name :originFrame originFrame ))
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
	      (foreach deferAtom (slot self _Atoms )
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
	      (foreach frag (slot self _RemoveFragments )
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


  (defClass MetaTopology MetaObject (_Name _NetCharge _MetaConstitution _ConstitutionAtoms _alignTo _Plugs _CoreFragment _Fragments _ScaffoldSpec _metaScaffold )
    (method __init__ (self &key name alignTo constitutionAtoms plugs coreFragment fragments (netCharge 0) )
	    (callAncestorMethod self )
	    (setq (slot self _Name )name )
	    (setq (slot self _ConstitutionAtoms ) constitutionAtoms )
	    (setq (slot self _Plugs )(filterOutNil plugs ))
	    (setq (slot self _CoreFragment )coreFragment )
	    (setOwner coreFragment self )
	    (setq (slot self _Fragments )fragments )
	    (setq (slot self _MetaConstitution )())
	    (setq (slot self _ScaffoldSpec )())
	    (setq (slot self _metaScaffold )())
	    (setq (slot self _alignTo )alignTo )
	    (setq (slot self _NetCharge) netCharge))
    (method setMetaExtractScaffold (self s )
	    (setq (slot self _metaScaffold )s ))
    (method setScaffoldSpec (self ss )
	    (setq (slot self _ScaffoldSpec )ss ))
    (method getScaffoldSpec (self ) (slot self _ScaffoldSpec ))

    (method getMetaConstitution (self )	  (slot self _MetaConstitution ))

    (method getMetaExtractFragments (self ) (slot self _Fragments ))

    (method getInPlug (self )
	    (foreach p (slot self _Plugs )
		     (ifTrue (isOfClass p MetaInPlug )
			     (return-from getInPlug p ))
		     (ifTrue (isOfClass p MetaOriginPlug )
			     (return-from getInPlug p )))
	    ())

    (method getInPlugWithMates (self )
	    (foreach p (slot self _Plugs )
		     (ifTrue (isOfClass p MetaInPlug )
			     (return-from getInPlugWithMates p )))
	    ())

    (method getOutPlugs (self )
	    (let (plugs )
	      (foreach p (slot self _Plugs )
		       (ifTrue (isOutPlug p )
			       (setq plugs (Cons p plugs ))))
	      plugs ))

    (method setMetaConstitution (self mc )
	    (setq (slot self _MetaConstitution )mc ))
    (method getName (self )  (slot self _Name ))
    (method desc (self) (% "%s/%s" (desc (slot self _MetaConstitution)) (getName self)))
    (method getMetaPlugs (self ) (slot self _Plugs))

    (method getMetaPlugWithName (self plugName )
	    (foreach p (slot self _Plugs ) ;removeComment
		     (ifTrue (== (getName p )plugName )
			     (return-from getMetaPlugWithName p )))
	    () )

    (method hasInPlugNamed (self name )
	    (let ((inPlug (getInPlugWithMates self )))
	      (ifTrue (isNil inPlug )
		      (return-from hasInPlugNamed false ))
	      (== (getName inPlug )name )))

    (method getMetaScaffold (self fragment )
	    (resolve (slot self _metaScaffold )fragment ))

    (method resolve (self fragment )
	    (let ( key name constitutionAtoms alignTo scaffold coreFragment plugs fragments value )
	      (setq key (genKey self (slot self _Name )))
	      (ifTrue (hasProperty fragment key )
		      (return-from resolve (getProperty fragment key )))
	      (log  "MetaTopology resolve ---------" )
	      (setq name (slot self _Name ))
	      (log (% " topology name = %s" name ))
	      (setq constitutionAtoms (resolve (slot self _ConstitutionAtoms )fragment ))
	      (setq alignTo (resolve (slot self _alignTo )fragment ))
	      (setq scaffold (resolve (slot self _metaScaffold )fragment ))
	      (setq coreFragment (resolve (slot self _CoreFragment )fragment ))
	      (setExtractScaffold coreFragment scaffold )
	      (log (% "MetaTopology resolve %s coreFragment = %s  " (repr (sourceFileLine ))(description coreFragment )))
	      (setq plugs (resolveOnlyPlugsWithMates self fragment ))
	      (setq fragments (map (lambda (element )(resolve element fragment ))(slot self _Fragments )))
	      (log (% "%s fragments = %s " (repr (sourceFileLine ))(repr fragments )))
	      (setq value (Topology :name name
				    :constitutionAtoms constitutionAtoms
				    :netCharge (slot self _NetCharge)
				    :alignTo alignTo
				    :plugs plugs
				    :coreFragment coreFragment
				    :fragments fragments
				    :scaffold scaffold ))
	      (setProperty fragment key value )))
    (method resolveOnlyPlugsWithMates (self fragment )
	    (let (plugs )
	      (foreach p (slot self _Plugs )
		       (when (not (isOfClass p MetaOriginPlug))
			 (setq plugs (Cons (resolve p fragment )plugs ))))
	      plugs )))

  (defClass MetaConstitution MetaObject (_group _name _addToGroups _constitutionAtoms _stereoInformation _frames _plugs _fragments _topologies )
    (method __init__ (self &key group name addToGroups constitutionAtoms stereoInformation frames plugs fragments topologies )
	    (callAncestorMethod self )
	    (setq (slot self _group )group )
	    (setq (slot self _name )name )
	    (setq (slot self _addToGroups )addToGroups )
	    (setq (slot self _constitutionAtoms ) constitutionAtoms )
	    (setq (slot self _stereoInformation )stereoInformation )
	    (setq (slot self _frames )(filterOutNil frames ))
	    (setq (slot self _plugs )(filterOutNil plugs ))
	    (setq (slot self _fragments )fragments )
	    (setq (slot self _topologies )topologies )
	    (map (lambda (f )(setMetaConstitution f self ))(slot self _plugs ))
	    (map (lambda (t )(setMetaConstitution t self ))(slot self _topologies )))
    (method getMetaOutPlugs (self )
	    (let (plugs )
	      (foreach p (slot self _plugs ) ;removeComment
		       (ifTrue (isOutPlug p )
			       (setq plugs (Cons p plugs ))))
	      plugs ))

    (method hasTopologyWithPlugNamed (self plugName )
	    (foreach top (slot self _topologies ) ;removeComment
		     (ifTrue (notNil (getMetaPlugWithName top plugName ))
			     (return-from hasTopologyWithPlugNamed true )))
	    false )

    (method hasTopologyWithRingClosingPlugNamed (self plugName )
	    (let (plug )
	      (foreach top (slot self _topologies ) ;removeComment
		       (setq plug (getMetaPlugWithName top plugName ))
		       (ifTrue (notNil plug )
			       (ifTrue (isOfClass plug MetaRingClosingPlug )
				       (return-from hasTopologyWithRingClosingPlugNamed true ))))
	      false ))
    (method getMetaPlugInATopology (self plugName )
	    (let (plug )
	      (foreach top (slot self _topologies ) ;removeComment
		       (setq plug (getMetaPlugWithName top ))
		       (ifTrue (notNil plug )
			       (return-from getMetaPlugInATopology plug )))
	      () ))

    (method getMetaFrameNamed (self fn )
	    (foreach frame (slot self _frames ) ;removeComment
		     (ifTrue (== (getName frame ) fn )
			     (return-from getMetaFrameNamed frame )))
	    (raise (LispError (% "Could not find frame named(%s)" fn ))))

    (method getMetaPlugNamed (self plugName )
	    (foreach plug (slot self _plugs )
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
	      (foreach top (slot self _topologies ) ;removeComment
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
			(setq constitutionAtoms (resolve (slot self _constitutionAtoms )fragment )))
	      (prognLog "Resolving & Validating StereoInformation"
			(setq stereoInformation (resolve (slot self _stereoInformation ) fragment ))
			(validate stereoInformation )
			(log (% "stereoInformation--> %s" (repr stereoInformation ))))
	      (prognLog "Resolving Frames"
			(setq frames (map (lambda (e )(resolve e fragment ))(slot self _frames ))))
	      (prognLog "Resolving Plugs"
			(setq plugs (map (lambda (e )(resolve e fragment ))(slot self _plugs ))))
	      (prognLog "Resolving Fragments"
			(setq fragments (map (lambda (e )(resolve e fragment ))(slot self _fragments ))))
	      (prognLog "Resolving Topologys"
			(setq topologies (map (lambda (e )(resolve e fragment ))(slot self _topologies ))))
	      (prognLog "Creating Constitution"
			(setq value (Constitution
				     :metaConstitution (slot self _group )
				     :name name
				     :constitutionAtoms constitutionAtoms
				     :stereoInformation stereoInformation
				     :frames frames
				     :plugs plugs
				     :fragments fragments
				     :topologies topologies )))
	      (setProperty fragment key value ))))

