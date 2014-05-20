					; fixed code



(global *FrameSpec.undefined* 0 )
(global *FrameSpec.complete* 1 )
(global *FrameSpec.incomplete* 2 )
(defClass FrameSpec ()(_Frame _metaTopology _metaFrameFinisher _Type _ClassifyReason _MetaOutPlug ))
(defMethod repr FrameSpec (self)
  (% "Topology[%s]/Frame[%s]" (repr (slot self _metaTopology)) (repr (slot self _Frame))))
(defMethod initFrameSpec FrameSpec (self frame topology )
  (setq (slot self _Frame )frame )
  (setq (slot self _metaTopology )topology )
  (setq (slot self _Type )*FrameSpec.complete* )
  (setq (slot self _ClassifyReason )(StringOutStream ))
  (setq (slot self _metaFrameFinisher )())
  (setq (slot self _MetaOutPlug )()))
(defMethod getMetaOutPlug FrameSpec (self )
  (slot self _MetaOutPlug  ))

(defMethod getMetaFrameFinisher FrameSpec (self )
  (slot self _metaFrameFinisher ))

(defMethod setFrameFinisherInfo FrameSpec (self ff metaOutPlug )
  (log (% "setFrameFinisherInfo frame(%s) frameFinisher(%s) metaOutPlug(%s)"
	    (getName (slot self _Frame ))
	    (repr ff )
	    (getName metaOutPlug )))
  (setq (slot self _metaFrameFinisher )ff )
  (setq (slot self _Type )*FrameSpec.incomplete* )
  (setq (slot self _MetaOutPlug )metaOutPlug ))
(defMethod typeName FrameSpec (self type )
  (cond 
   ((== type *FrameSpec.undefined* )
    (return-from typeName "undefined" ))

   ((== type *FrameSpec.complete* )	;E-A
    (return-from typeName "complete" ))		;E-B

   ((== type *FrameSpec.incomplete* )	;E-A
    (return-from typeName "incomplete" ))		;E-B
   (true 
    (return-from typeName "UNKNOWN TYPE" )))
  "Fell through" )

(defMethod describe FrameSpec (self )
  (let ((s (StringOutStream )))
    (write s (% "FrameSpec frame(%s)" (getName (slot self _Frame ))))
    (write s (% " type(%s) " (typeName self (slot self _Type ))))
    (write s (% " has _metaFrameFinisher->(%d) " (notNil (slot self _metaFrameFinisher ))))
    (log (% "%s" (str s )))))
(defMethod isComplete FrameSpec (self )
  (== (slot self _Type )  *FrameSpec.complete* ))


(defMethod getMetaFrame FrameSpec (self )
  (slot self _Frame ))

(defMethod createMetaExtractFrame FrameSpec (self )
  (let (plugName frame alias )
    (ifTrue (isComplete self )
	    (assert (notNil (slot self _Frame ))"[self slot _self] slot _Frame] cannot be nil" )
	    (return-from createMetaExtractFrame (MetaExtractCompleteFrame :metaFrame (slot self _Frame ))))
    (assert (notNil (slot self _metaFrameFinisher ))
	    (% "Frame/Topology/Constitution(%s/%s/%s) is incomplete but it doesn't have a FrameFinisher defined!"
	       (getName (slot self _Frame ))
	       (getName (slot self _metaTopology ))
	       (getGroup (getMetaConstitution (slot self _metaTopology )))))
    (setq plugName (getPlugName (slot self _metaFrameFinisher )))
    (setq frame (getMetaFrame (slot self _metaFrameFinisher )))
    (setq alias (generateAlias (slot self _metaFrameFinisher )))
    (MetaExtractIncompleteFrame :plugName plugName :metaFrame frame :alias alias )))


(defMethod getExtractFrame FrameSpec (self )
  (raise "may be depreciated" )
  (ifTrue (isComplete self )
	  (return-from getExtractFrame (ExtractCompleteFrame :frame (slot self _Frame ))))
  (assert (notNil (slot self _metaFrameFinisher ))(% "Frame/Topology/Constitution(%s/%s/%s) is incomplete but it doesn't have a FrameFinisher defined!" (getName (slot self _Frame ))(getName (slot self _metaTopology ))(getName (getConstitution (slot self _metaTopology )))))
  (setq plugName (calculateAlias (slot self _metaFrameFinisher )))
  (setq frame (getFrame (slot self _metaFrameFinisher )))
  (ExtractIncompleteFrame :plugName plugName :frame frame ))


(defMethod classify FrameSpec (self currentMonomer preceedingMonomer )
  (raise "may be depreciated" )
  (writeln (slot self _ClassifyReason )(% "classifying FrameSpec frame = %s " (getName (slot self _Frame ))))
  (writeln (slot self _ClassifyReason )(% "currentMonomer(%s) residue(%s)" (description currentMonomer )(description (getTemporaryResidue currentMonomer ))))
  (foreach c (plugNamesAndCouplingsAsCons currentMonomer )
	   (setq plugName (@ c 0 ))
	   (setq coupling (@ c 1 ))
	   (setq neighborMonomer (getOtherSideMonomer coupling currentMonomer ))
	   (writeln (slot self _ClassifyReason )(% "--> plug(%s) Monomer(%s) Residue(%s)" plugName (description neighborMonomer )(description (getTemporaryResidue neighborMonomer )))))
  (setq boundFrame (getBoundFrame (slot self _Frame )currentMonomer ))
  (setq oAtom (get_oAtom boundFrame ))
  (writeln (slot self _ClassifyReason )(% "  boundFrame = %s" (description boundFrame )))
  (ifFalse (has_pAtom boundFrame )
	   (setq (slot self _Type )*FrameSpec.complete* )
	   (writeln (slot self _ClassifyReason )"boundFrame_no_pAtom" )
	   (return-from classify ))
  (setq pAtom (get_pAtom boundFrame ))
  (ifFalse (has_qAtom boundFrame )
	   (setq (slot self _Type )*FrameSpec.complete* )
	   (writeln (slot self _ClassifyReason )"boundFrame_no_qAtom" )
	   (return-from classify ))
  (setq qAtom (get_qAtom boundFrame ))
  (setq complete true )
  (setq cr (getTemporaryResidue currentMonomer ))
  (assert (containsAtom cr oAtom )"The origin atom must alwasy be in the current monomer" )
  (ifTrue (and (containsAtom cr pAtom )(containsAtom cr qAtom ))
	  (setq (slot self _Type )*FrameSpec.complete* )
	  (writeln (slot self _ClassifyReason )"cr_contains_pAtom_and_qAtom" )
	  (return-from classify ))
  (ifTrue (notNil preceedingMonomer )
	  (setq cp (getTemporaryResidue preceedingMonomer ))
	  (ifTrue (or (containsAtom cp pAtom )(containsAtom cp qAtom ))
		  (setq (slot self _Type )*FrameSpec.complete* )
		  (writeln (slot self _ClassifyReason )"  cp_contains_pAtom_or_qAtom" )
		  (if (containsAtom cp pAtom )
		      (then 
		       (writeln (slot self _ClassifyReason )(% "  cp Residue %s contains pAtom(%s)" (getName cp )(description pAtom ))))
		    (else 
		     (writeln (slot self _ClassifyReason )(% "  cp Residue %s contains qAtom(%s)" (getName cp )(description qAtom )))))
		  (return-from classify )))
  (setq (slot self _Type ) *FrameSpec.incomplete* )
  (writeln (slot self _ClassifyReason )"fell_through" ))


(defClass ScaffoldSpec ()  (_metaTopology _frameSpecs _metaExtractFrameFinishers _FrameFinishers ))
(defMethod __init__ ScaffoldSpec (self metaTopology )
  (setq (slot self _metaTopology )metaTopology )
  (setq (slot self _metaExtractFrameFinishers )())
  (createFrameSpecs self ))


(defMethod updateFrameSpecForMetaFrameFinisher ScaffoldSpec (self finisher metaOutPlug )
  (prognLog "updateFrameSpecForMetaFrameFinisher"
  (let (ffname foundIt frameSpecFrameName foundIt )
    (setq ffname (getName (getMetaFrame finisher )))
    (setq foundIt false )
    (foreach frameSpec (slot self _frameSpecs )	;removeComment
	     (setq frameSpecFrameName (getName (getMetaFrame frameSpec )))
	     (log (% "Checking if frameSpec frame(%s) == FrameFinisher_frame(%s) " frameSpecFrameName ffname ))
	     (ifTrue (== frameSpecFrameName ffname )
		     (setq foundIt true )
		     (setFrameFinisherInfo frameSpec finisher metaOutPlug )))
    (assert foundIt (% "I have a FrameFinisher for frame %s but I couldn't find a frameSpec for it" ffname )))))


(defMethod getExtractCoreFragment ScaffoldSpec (self )
  (let ((top (slot self _metaTopology )))
    (return-from getExtractCoreFragment (getExtractCoreFragment top ))))

(defMethod describe ScaffoldSpec (self )
  (log  "ScaffoldSpec----" )
  (foreach frameSpec (slot self _frameSpecs )
	   (describe frameSpec )))

(defMethod createFrameSpecs ScaffoldSpec (self )
  (let (metaOutPlugs frameNames metaExtractFragments metaFrame metaConstitution metaFrames fs numff )
    (setq metaOutPlugs (getOutPlugs (slot self _metaTopology )))
    (setq frameNames (StringSet )) ; pull together all the frames that are exported
    (foreach metaPlug metaOutPlugs 
	     (log (% "Getting MetaExportFrame for metaPlug name %s" (getName metaPlug )))
	     (insert frameNames (getName (getMetaExportFrame metaPlug )))
	     (foreach metaFrameFinisher (getMetaFrameFinishers metaPlug )
		      (insert frameNames (getName (getMetaFrame metaFrameFinisher )))))
    (setq metaExtractFragments (getMetaExtractFragments (slot self _metaTopology ))) 
;; pull together all of the fragments that depend on frames
    (foreach metaExtractFragment metaExtractFragments 
	     (setq metaFrame (getMetaFrame metaExtractFragment ))
	     (assert (notNil metaFrame )(% "MetaFrame for extract fragment: %s is nil " (getName metaExtractFragment )))
	     (insert frameNames (getName (getMetaFrame metaExtractFragment ))))
    (setq metaConstitution (getMetaConstitution (slot self _metaTopology )))
    (setq metaFrames (map (lambda (n )(getMetaFrameNamed metaConstitution n ))(asCons frameNames ))) 
;; Create a FrameSpec for every one of these frames
    (let ((frameSpecs))
      (foreach metaFrame metaFrames 
	       (setq fs (FrameSpec))
	       (initFrameSpec fs metaFrame (slot self _metaTopology ))
	       (setq frameSpecs (Cons fs frameSpecs)))
      (log (% "Created a list of %d frameSpecs = %s" (length frameSpecs) (repr frameSpecs)))
      (setq (slot self _frameSpecs ) frameSpecs ))
;; Go through all the FrameFinishers and use them to classify
;; the FrameSpecs as "complete" or "incomplete"
    (log (% "Defining FrameFinishers for Topology/MetaConstitution(%s/%s)" (getName (slot self _metaTopology ))(getGroup (getMetaConstitution (slot self _metaTopology )))))
    (foreach metaOutPlug metaOutPlugs 
	     (log (% "    MetaOutPlug = %s" (getName metaOutPlug )))
	     (setq numff 0 )
	     (foreach frameFinisher (getMetaFrameFinishers metaOutPlug )
		      (log (% "       adding MetaFrameFinisher(%s) to ScaffoldSpec for Topology/MetaConstition(%s/%s)" (generateAlias frameFinisher )(getName (slot self _metaTopology ))(getGroup (getMetaConstitution (slot self _metaTopology )))))
		      (updateFrameSpecForMetaFrameFinisher self frameFinisher metaOutPlug )
		      (setq numff (+ numff 1 )))
	     (ifTrue (== numff 0 )
		     (log  "        --> There were no MetaFrameFinishers" )))))
(defMethod createMetaExtractFrameFinisherIfYouDontAlreadyHaveIt ScaffoldSpec (self metaFrameFinisher )
  (let (alias metaExtractFrameFinisher )
    (setq alias (generateAlias metaFrameFinisher ))
    (log (% "      createMetaExtractFrameFinisherIfYouDontAlreadyHaveIt for alias(%s)" alias ))
    (foreach metaExtractFrameFinisher (slot self _metaExtractFrameFinishers ) ;removeComment

	     (ifTrue (== (getAlias metaExtractFrameFinisher )alias )
		     (log  "     This Topology already has it" )
		     (return-from createMetaExtractFrameFinisherIfYouDontAlreadyHaveIt )))
    (log  "       Creating MetaExtractFrameFinisher" )
    (setq metaExtractFrameFinisher (createMetaExtractFrameFinisher metaFrameFinisher ))
    (setq (slot self _metaExtractFrameFinishers )(Cons metaExtractFrameFinisher (slot self _metaExtractFrameFinishers )))))


(defMethod giveYourFrameFinishersToOutPlugMates ScaffoldSpec (self metaDatabase )
  (prognLog (% "giveYourFrameFinishersToOutPlugMates for scaffoldSpec for topology [%d]" (desc (slot self _metaTopology)))
  (let (complete incomplete frameFinisher metaOutPlug neighborInPlugName 
		 neighborMetaConstitution topologies neighborScaffoldSpec )
    (log (% "---giveYourFrameFinishersToOutPlugMates Topology/MetaConstitution(%s/%s)"
	      (getName (slot self _metaTopology ))(getGroup (getMetaConstitution (slot self _metaTopology )))))
    (setq complete 0 )
    (setq incomplete 0 )
    (foreach frameSpec (slot self _frameSpecs )	;removeComment

	     (setq frameFinisher (getMetaFrameFinisher frameSpec ))
	     (if (notNil frameFinisher )
		 (then 
		  (log  "     processing frameFinisher" )
		  (setq metaOutPlug (getMetaOutPlug frameSpec ))
		  (setq neighborInPlugName (otherSidePlugName metaOutPlug ))
		  (foreach mateName (getAllMates metaOutPlug )
			   (setq neighborMetaConstitution (getMetaConstitution metaDatabase mateName ))
			   (setq topologies (allTopologiesWithInPlugNamed neighborMetaConstitution neighborInPlugName ))
			   (foreach topology topologies 
				    (log (% "For Topology/MetaConstitution(%s/%s)" (getName topology )(getGroup neighborMetaConstitution )))
				    (setq neighborScaffoldSpec (getScaffoldSpec topology ))
				    (createMetaExtractFrameFinisherIfYouDontAlreadyHaveIt neighborScaffoldSpec frameFinisher )))
		  (setq incomplete (+ incomplete 1 ))
		  (log (% "    processed frameFinisher for frame(%s) incomplete(%d)" (getName (getMetaFrame frameSpec ))incomplete )))
	       (else 
		(setq complete (+ complete 1 ))
		(log (% "     frame(%s) there is no FrameFinisher  complete=%d" (getName (getMetaFrame frameSpec ))complete )))))
    (log (% " %d frameSpecs complete(%d) - need finishing(%d)" (length (slot self _frameSpecs ))complete incomplete )) ;
					; Methods below here may be depreciated
					;
    )))
(defMethod getExtractCompleteFrames ScaffoldSpec (self )
  (raise "may be depreciated" )
  (setq frames ())
  (foreach frameSpec (slot self _frameSpecs ) ;removeComment

	   (ifTrue (isComplete frameSpec )
		   (setq frames (Cons (getExtractFrame frameSpec )frames ))))
  frames)

(defMethod getExtractIncompleteFrames ScaffoldSpec (self )
  (raise "may be depreciated" )
  (setq frames ())
  (foreach frameSpec (slot self _frameSpecs )
	   (ifTrue (not (isComplete frameSpec ))
		   (setq frames (Cons (getExtractFrame frameSpec )frames ))))
  frames )

(defMethod getIncompleteFrameSpecs ScaffoldSpec (self )
  (raise "may be depreciated" )
  (setq frames ())
  (foreach frameSpec (slot self _frameSpecs ) ;removeComment

	   (ifTrue (not (isComplete frameSpec ))
		   (setq frames (Cons frameSpec frames ))))
  frames)
(defMethod createFinishFrames ScaffoldSpec (self )
  (raise "may be depreciated" )
  (setq (slot self _ExtractFrameFinishers )())
  (setq inCoupling (getInCoupling UFOCUSMONOMERU ))
  (ifTrue (isNil inCoupling )
	  (return-from createFinishFrames ))
  (setq preceedingMonomer (getOtherSideMonomer inCoupling UFOCUSMONOMERU ))
  (setq preceedingTopology (getTopology preceedingMonomer ))
  (assert (notNil preceedingTopology )
	  (% "The preceedingTopology for preceedingMonomer(%s) was nil!!!" (description preceedingMonomer )))
  (setq preceedingScaffoldSpec (getTemporaryObject preceedingTopology ))
  (assert (notNil preceedingScaffoldSpec )
	  (% "The preceedingScaffoldSpec for %s was nil!!!" (description preceedingTopology )))
  (setq incompleteFrameSpecs (getIncompleteFrameSpecs preceedingScaffoldSpec ))
  (setq cr (getTemporaryResidue UFOCUSMONOMERU ))
  (setq cp (getTemporaryResidue preceedingMonomer ))
  (log (% "    The preceedingScaffoldSpec has %d incomplete frameSpecs" (length incompleteFrameSpecs )))
  (foreach frameSpec incompleteFrameSpecs 
	   (log "Looking at incomplete frameSpec" )
	   (setq incompleteFrame (getFrame frameSpec ))
	   (setq boundFrame (getBoundFrame incompleteFrame preceedingMonomer ))
	   (setq pAtom (get_pAtom boundFrame ))
	   (setq qAtom (get_qAtom boundFrame ))
	   (setq mine false )
	   (ifTrue (containsAtom cr pAtom )
		   (setq mine true )
		   (setq missingAtom pAtom ))
	   (ifTrue (containsAtom cr qAtom )
		   (setq mine true )
		   (setq missingAtom qAtom ))
	   (if mine 
	       (then 
		(log (% "The incomplete frame overlaps the current residue(%s %s) so we will create a finish frame" (getName cr )(description cr )))
		(setq inCoupling (getInCoupling UFOCUSMONOMERU ))
		(setq inPlugName (getOutMonomerPlugName inCoupling ))
		(setq finishFrame (ExtractFrameFinisher :plugName inPlugName :containsAtom (getName missingAtom ):recognizer (getFrameRecognizer (getFrame frameSpec ))))
		(setq (slot self _ExtractFrameFinishers )(Cons finishFrame (slot self _ExtractFrameFinishers ))))
	     (else 
	      (log (% "The incomplete frame does not overlap the current residue(%s %s) so we will not create a finish frame" (getName cr )(description cr ))))))
  (log (% "      There are %d ExtractFrameFinishers" (length (slot self _ExtractFrameFinishers ))))
  (setq extractScaffold (getExtractScaffold (slot self _metaTopology )))
  (setExtractFrameFinishersFromCons extractScaffold (slot self _ExtractFrameFinishers )))
(defMethod getFinishFrames ScaffoldSpec (self )
  (raise "may be depreciated" )
  (slot self _FinishFrames))

(defMethod realizeMetaExtractScaffold ScaffoldSpec (self )
  (prognLog (% "realizeMetaExtractScaffold for MetaConstitution/Topology(%s/%s)" (getGroup (getMetaConstitution (slot self _metaTopology )))(getName (slot self _metaTopology )))
	    (let (inPlug metaScaffold metaExtractFrame )
	      (describe self )
	      (setq inPlug (getInPlug (slot self _metaTopology )))
	      (prognLog "Building MetaExtractScaffold using _frameSpecs and _metaExtractFrameFinishers"
			(setq metaScaffold (MetaExtractScaffold inPlug ))
			(prognLog (% "Adding frameSpecs")
				  (foreach frameSpec (slot self _frameSpecs )
					   (log (% "Adding %s" (repr metaExtractFrame)))
					   (setq metaExtractFrame (createMetaExtractFrame frameSpec ))
					   (addMetaExtractFrame metaScaffold metaExtractFrame ))
				  )
			(prognLog "adding metaExtractFrameFinishers"
				  (foreach metaExtractFrameFinisher (slot self _metaExtractFrameFinishers )
					   (log (% "Adding %s" (repr metaExtractFrameFinisher)))
					   (addMetaExtractFrame metaScaffold metaExtractFrameFinisher ))
				  )
			(setMetaExtractScaffold (slot self _metaTopology ) metaScaffold )
			(check metaScaffold (slot self _metaTopology ))
			))))

(defun createScaffoldSpec (metaTopology metaConstitution )
  (log (% "Creating MetaExtractScaffoldSpec for MetaTopology/MetaConstitution(%s/%s)" (getName metaTopology )(getGroup metaConstitution )))
  (ScaffoldSpec metaTopology))

(defun createAllMetaExtractScaffolds (metaDatabase )
  (let (scaffoldSpecs metaConstitution scaffoldSpec )
    (setq scaffoldSpecs ())
    (prognLog "Creating scaffolds"
	      (foreach entry (entries (getMetaConstitutions metaDatabase ))
		       (prognLog (% "Creating scaffold for %s" (desc (value entry)))
				 (setq metaConstitution (value entry ))
				 (foreach metaTopology (getMetaTopologies metaConstitution )
					  (prognLog (% "Topology: %s" (desc metaTopology))
						    (setq scaffoldSpec (createScaffoldSpec metaTopology metaConstitution ))
						    (setScaffoldSpec metaTopology scaffoldSpec )
						    (setq scaffoldSpecs (Cons scaffoldSpec scaffoldSpecs )))))))
    (prognLog "Giving frame finishers to OutPlugMates"
	      (foreach scaffoldSpec scaffoldSpecs 
		       (giveYourFrameFinishersToOutPlugMates scaffoldSpec metaDatabase )))
    (prognLog "realizeMetaExtractScaffolds for all scaffoldSpecs"
	      (foreach scaffoldSpec scaffoldSpecs 
		       (realizeMetaExtractScaffold scaffoldSpec )))))