; fixed code
(global *FrameSpec.undefined*  0   )

(global *FrameSpec.complete*  1   )

(global *FrameSpec.incomplete*  2   )

(defClass FrameSpec  ()(_Frame  _metaTopology  _metaFrameFinisher  _Type  _ClassifyReason  _MetaOutPlug   ) )

(defMethod initFrameSpec  FrameSpec  (self  frame  topology   ) ;END1
(setq (slot   self   _Frame   )frame   )

(setq (slot   self   _metaTopology   )topology   )

(setq (slot   self   _Type   )*FrameSpec.complete*   )

(setq (slot   self   _ClassifyReason   )(StringStream   ) )

(setq (slot   self   _metaFrameFinisher   )() )

(setq (slot   self   _MetaOutPlug   )() )
)

(defMethod getMetaOutPlug  FrameSpec  (self   ) ;END1
(return (slot   self   _MetaOutPlug   ) )
)

(defMethod getMetaFrameFinisher  FrameSpec  (self   ) ;END1
(return (slot   self   _metaFrameFinisher   ) )
)

(defMethod setFrameFinisherInfo  FrameSpec  (self  ff  metaOutPlug   ) ;END1
(println (%   "setFrameFinisherInfo frame(%s) frameFinisher(%s) metaOutPlug(%s)"   (getName   (slot   self   _Frame   ))(repr  ff   )(getName   metaOutPlug  ) ) )

(setq (slot   self   _metaFrameFinisher   )ff   )

(setq (slot   self   _Type   )*FrameSpec.incomplete*   )

(setq (slot   self   _MetaOutPlug   )metaOutPlug   )
)

(defMethod typeName  FrameSpec  (self  type   ) ;END1
(cond ;END1
(==   type   *FrameSpec.undefined*   )
(return "undefined"   )


(==   type   *FrameSpec.complete*   )
(return "complete"   )


(==   type   *FrameSpec.incomplete*   )
(return "incomplete"   )


(true ;END1
(return "UNKNOWN TYPE"   )
)
)

(return "Fell through"   )
)

(defMethod describe  FrameSpec  (self   ) ;END1
(let ((s  (StringStream   ) ) ) ;END1
(write   s   (%   "FrameSpec frame(%s)"   (getName   (slot   self   _Frame   )) ) )

(write   s   (%   " type(%s) "   (typeName   self   (slot   self   _Type   ) ) ) )

(write   s   (%   " has [self slot _metaFrameFinisher](%d) "   (notNil   (slot   self   _metaFrameFinisher   )) ) )

(println (%   "%s"   (str   s  ) ) )
)
)

(defMethod isComplete  FrameSpec  (self   ) ;END1
(return (==   (slot   self   _Type   ) *FrameSpec.complete*   ) )
)

(defMethod getMetaFrame  FrameSpec  (self   ) ;END1
(return (slot   self   _Frame   ) )
)

(defMethod createMetaExtractFrame  FrameSpec  (self   ) ;END1
(let (plugName  frame  alias   ) ;END1
(ifTrue (isComplete   self  ) ;END1
(assert (notNil   (slot   self   _Frame   ))"[self slot _self] slot _Frame] cannot be nil"   )

(return (MetaExtractCompleteFrame  :metaFrame  (slot   self   _Frame   ) ) )
)

(assert (notNil   (slot   self   _metaFrameFinisher   ))(%   "Frame/Topology/Constitution(%s/%s/%s) is incomplete but it doesn't have a FrameFinisher defined!"   (getName   (slot   self   _Frame   ))(getName   (slot   self   _metaTopology   ))(getGroup   (getMetaConstitution   (slot   self   _metaTopology   ))) ) )

(setq plugName  (getPlugName   (slot   self   _metaFrameFinisher   )) )

(setq frame  (getMetaFrame   (slot   self   _metaFrameFinisher   )) )

(setq alias  (generateAlias   (slot   self   _metaFrameFinisher   )) )

(return (MetaExtractIncompleteFrame  :plugName  plugName  :metaFrame  frame  :alias  alias   ) )
)
)

(defMethod getExtractFrame  FrameSpec  (self   ) ;END1
(raise "may be depreciated"   )

(ifTrue (isComplete   self  ) ;END1
(return (ExtractCompleteFrame  :frame  (slot   self   _Frame   ) ) )
)

(assert (notNil   (slot   self   _metaFrameFinisher   ))(%   "Frame/Topology/Constitution(%s/%s/%s) is incomplete but it doesn't have a FrameFinisher defined!"   (getName   (slot   self   _Frame   ))(getName   (slot   self   _metaTopology   ))(getName   (getConstitution   (slot   self   _metaTopology   ))) ) )

(setq plugName  (calculateAlias   (slot   self   _metaFrameFinisher   )) )

(setq frame  (getFrame   (slot   self   _metaFrameFinisher   )) )

(return (ExtractIncompleteFrame  :plugName  plugName  :frame  frame   ) )
)

(defMethod classify  FrameSpec  (self  currentMonomer  preceedingMonomer   ) ;END1
(raise "may be depreciated"   )

(writeln   (slot   self   _ClassifyReason   ) (%   "classifying FrameSpec frame = %s "   (getName   (slot   self   _Frame   )) ) )

(writeln   (slot   self   _ClassifyReason   ) (%   "currentMonomer(%s) residue(%s)"   (description   currentMonomer  )(description   (getTemporaryResidue   currentMonomer  )) ) )

(foreach c  (plugNamesAndCouplingsAsCons   currentMonomer  ) ;END1
(setq plugName  (@   c   0   ) )

(setq coupling  (@   c   1   ) )

(setq neighborMonomer  (getOtherSideMonomer   coupling   currentMonomer   ) )

(writeln   (slot   self   _ClassifyReason   ) (%   "--> plug(%s) Monomer(%s) Residue(%s)"   plugName  (description   neighborMonomer  )(description   (getTemporaryResidue   neighborMonomer  )) ) )
)

(setq boundFrame  (getBoundFrame   (slot   self   _Frame   ) currentMonomer   ) )

(setq oAtom  (get_oAtom   boundFrame  ) )

(writeln   (slot   self   _ClassifyReason   ) (%   "  boundFrame = %s"   (description   boundFrame  ) ) )

(ifFalse (has_pAtom   boundFrame  ) ;END1
(setq (slot   self   _Type   )*FrameSpec.complete*   )

(writeln   (slot   self   _ClassifyReason   ) "boundFrame_no_pAtom"   )

(return )
)

(setq pAtom  (get_pAtom   boundFrame  ) )

(ifFalse (has_qAtom   boundFrame  ) ;END1
(setq (slot   self   _Type   )*FrameSpec.complete*   )

(writeln   (slot   self   _ClassifyReason   ) "boundFrame_no_qAtom"   )

(return )
)

(setq qAtom  (get_qAtom   boundFrame  ) )

(setq complete  true   )

(setq cr  (getTemporaryResidue   currentMonomer  ) )

(assert (containsAtom   cr   oAtom   )"The origin atom must alwasy be in the current monomer"   )

(ifTrue (and   (containsAtom   cr   pAtom   ) (containsAtom   cr   qAtom   ) ) ;END1
(setq (slot   self   _Type   )*FrameSpec.complete*   )

(writeln   (slot   self   _ClassifyReason   ) "cr_contains_pAtom_and_qAtom"   )

(return )
)

(ifTrue (notNil   preceedingMonomer  ) ;END1
(setq cp  (getTemporaryResidue   preceedingMonomer  ) )

(ifTrue (or   (containsAtom   cp   pAtom   ) (containsAtom   cp   qAtom   ) ) ;END1
(setq (slot   self   _Type   )*FrameSpec.complete*   )

(writeln   (slot   self   _ClassifyReason   ) "  cp_contains_pAtom_or_qAtom"   )

(if (containsAtom   cp   pAtom   ) ;END1
(then ;END1
(writeln   (slot   self   _ClassifyReason   ) (%   "  cp Residue %s contains pAtom(%s)"   (getName   cp  )(description   pAtom  ) ) )
)

(else ;END1
(writeln   (slot   self   _ClassifyReason   ) (%   "  cp Residue %s contains qAtom(%s)"   (getName   cp  )(description   qAtom  ) ) )
)
)

(return )
)
)

(setq (slot   self   _Type   )*FrameSpec.incomplete*   )

(writeln   (slot   self   _ClassifyReason   ) "fell_through"   )
)

(defClass ScaffoldSpec  ()(_metaTopology  _frameSpecs  _metaExtractFrameFinishers  _FrameFinishers   ) )

(defMethod __init__  ScaffoldSpec  (self  metaTopology   ) ;END1
(setq (slot   self   _metaTopology   )metaTopology   )

(setq (slot   self   _metaExtractFrameFinishers   )() )

(createFrameSpecs   self  )
)

(defMethod updateFrameSpecForMetaFrameFinisher  ScaffoldSpec  (self  finisher  metaOutPlug   ) ;END1
(let (ffname  foundIt  frameSpecFrameName  foundIt   ) ;END1
(println  "updateFrameSpecForMetaFrameFinisher"    )

(setq ffname  (getName   (getMetaFrame   finisher  )) )

(setq foundIt  false   )

(foreach frameSpec  (slot   self   _frameSpecs   ) ;END1
(setq frameSpecFrameName  (getName   (getMetaFrame   frameSpec  )) )

(println (%   "Checking if frameSpec frame(%s) == FrameFinisher_frame(%s) "   frameSpecFrameName  ffname   ) )

(ifTrue (==   frameSpecFrameName   ffname   ) ;END1
(setq foundIt  true   )

(setFrameFinisherInfo   frameSpec   finisher  metaOutPlug   )
)
)

(assert foundIt  (%   "I have a FrameFinisher for frame %s but I couldn't find a frameSpec for it"   ffname   ) )
)
)

(defMethod getExtractCoreFragment  ScaffoldSpec  (self   ) ;END1
(let ((top  (slot   self   _metaTopology   ) ) ) ;END1
(return (getExtractCoreFragment   top  ) )
)
)

(defMethod describe  ScaffoldSpec  (self   ) ;END1
(println  "ScaffoldSpec----"    )

(foreach frameSpec  (slot   self   _frameSpecs   ) ;END1
(describe   frameSpec  )
)
)

(defMethod createFrameSpecs  ScaffoldSpec  (self   ) ;END1
(let (metaOutPlugs  frameNames  metaExtractFragments  metaFrame  metaConstitution  metaFrames  fs  numff   ) ;END1
(setq metaOutPlugs  (getOutPlugs   (slot   self   _metaTopology   )) )

(setq frameNames  (StringSet   ) )

(foreach metaPlug  metaOutPlugs   ;END1
(println (%   "Getting MetaExportFrame for metaPlug name %s"   (getName   metaPlug  ) ) )

(insert   frameNames   (getName   (getMetaExportFrame   metaPlug  )) )

(foreach metaFrameFinisher  (getMetaFrameFinishers   metaPlug  ) ;END1
(insert   frameNames   (getName   (getMetaFrame   metaFrameFinisher  )) )
)
)

(setq metaExtractFragments  (getMetaExtractFragments   (slot   self   _metaTopology   )) )

(foreach metaExtractFragment  metaExtractFragments   ;END1
(setq metaFrame  (getMetaFrame   metaExtractFragment  ) )

(assert (notNil   metaFrame  )(%   "MetaFrame for extract fragment: %s is nil "   (getName   metaExtractFragment  ) ) )

(insert   frameNames   (getName   (getMetaFrame   metaExtractFragment  )) )
)

(setq metaConstitution  (getMetaConstitution   (slot   self   _metaTopology   )) )

(setq metaFrames  (map  (lambda  (n   )(getMetaFrameNamed   metaConstitution   n   ) )(asCons   frameNames  ) ) )

(setq (slot   self   _frameSpecs   )() )

(foreach metaFrame  metaFrames   ;END1
(setq fs  (FrameSpec   ) )

(initFrameSpec   fs   metaFrame  (slot   self   _metaTopology   ) )

(setq (slot   self   _frameSpecs   )(Cons  fs  (slot   self   _frameSpecs   ) ) )
)

(println (%   "Defining FrameFinishers for Topology/MetaConstitution(%s/%s)"   (getName   (slot   self   _metaTopology   ))(getGroup   (getMetaConstitution   (slot   self   _metaTopology   ))) ) )

(foreach metaOutPlug  metaOutPlugs   ;END1
(println (%   "    MetaOutPlug = %s"   (getName   metaOutPlug  ) ) )

(setq numff  0   )

(foreach frameFinisher  (getMetaFrameFinishers   metaOutPlug  ) ;END1
(println (%   "       adding MetaFrameFinisher(%s) to ScaffoldSpec for Topology/MetaConstition(%s/%s)"   (generateAlias   frameFinisher  )(getName   (slot   self   _metaTopology   ))(getGroup   (getMetaConstitution   (slot   self   _metaTopology   ))) ) )

(updateFrameSpecForMetaFrameFinisher   self   frameFinisher  metaOutPlug   )

(setq numff  (+   numff   1   ) )
)

(ifTrue (==   numff   0   ) ;END1
(println  "        --> There were no MetaFrameFinishers"    )
)
)
)
)

(defMethod createMetaExtractFrameFinisherIfYouDontAlreadyHaveIt  ScaffoldSpec  (self  metaFrameFinisher   ) ;END1
(let (alias  metaExtractFrameFinisher   ) ;END1
(setq alias  (generateAlias   metaFrameFinisher  ) )

(println (%   "      createMetaExtractFrameFinisherIfYouDontAlreadyHaveIt for alias(%s)"   alias   ) )

(foreach metaExtractFrameFinisher  (slot   self   _metaExtractFrameFinishers   ) ;END1
(ifTrue (==   (getAlias   metaExtractFrameFinisher  ) alias   ) ;END1
(println  "     This Topology already has it"    )

(return )
)
)

(println  "       Creating MetaExtractFrameFinisher"    )

(setq metaExtractFrameFinisher  (createMetaExtractFrameFinisher   metaFrameFinisher  ) )

(setq (slot   self   _metaExtractFrameFinishers   )(Cons  metaExtractFrameFinisher  (slot   self   _metaExtractFrameFinishers   ) ) )
)
)

(defMethod giveYourFrameFinishersToOutPlugMates  ScaffoldSpec  (self  metaDatabase   ) ;END1
(let (complete  incomplete  frameFinisher  metaOutPlug  neighborInPlugName  neighborMetaConstitution  topologies  neighborScaffoldSpec  incomplete  complete   ) ;END1
(println (%   "---giveYourFrameFinishersToOutPlugMates Topology/MetaConstitution(%s/%s)"   (getName   (slot   self   _metaTopology   ))(getGroup   (getMetaConstitution   (slot   self   _metaTopology   ))) ) )

(setq complete  0   )

(setq incomplete  0   )

(foreach frameSpec  (slot   self   _frameSpecs   ) ;END1
(setq frameFinisher  (getMetaFrameFinisher   frameSpec  ) )

(if (notNil   frameFinisher  ) ;END1
(then ;END1
(println  "     processing frameFinisher"    )

(setq metaOutPlug  (getMetaOutPlug   frameSpec  ) )

(setq neighborInPlugName  (otherSidePlugName   metaOutPlug  ) )

(foreach mateName  (getAllMates   metaOutPlug  ) ;END1
(setq neighborMetaConstitution  (getMetaConstitution   metaDatabase   mateName   ) )

(setq topologies  (allTopologiesWithInPlugNamed   neighborMetaConstitution   neighborInPlugName   ) )

(foreach topology  topologies   ;END1
(println (%   "For Topology/MetaConstitution(%s/%s)"   (getName   topology  )(getGroup   neighborMetaConstitution  ) ) )

(setq neighborScaffoldSpec  (getScaffoldSpec   topology  ) )

(createMetaExtractFrameFinisherIfYouDontAlreadyHaveIt   neighborScaffoldSpec   frameFinisher   )
)
)

(setq incomplete  (+   incomplete   1   ) )

(println (%   "    processed frameFinisher for frame(%s) incomplete(%d)"   (getName   (getMetaFrame   frameSpec  ))incomplete   ) )
)

(else ;END1
(setq complete  (+   complete   1   ) )

(println (%   "     frame(%s) there is no FrameFinisher  complete=%d"   (getName   (getMetaFrame   frameSpec  ))complete   ) )
)
)
)

(println (%   " %d frameSpecs complete(%d) - need finishing(%d)"   (length   (slot   self   _frameSpecs   ))complete  incomplete   ) )
)
)

(defMethod getExtractCompleteFrames  ScaffoldSpec  (self   ) ;END1
(raise "may be depreciated"   )

(setq frames  () )

(foreach frameSpec  (slot   self   _frameSpecs   ) ;END1
(ifTrue (isComplete   frameSpec  ) ;END1
(setq frames  (Cons  (getExtractFrame   frameSpec  )frames   ) )
)
)

(return frames   )
)

(defMethod getExtractIncompleteFrames  ScaffoldSpec  (self   ) ;END1
(raise "may be depreciated"   )

(setq frames  () )

(foreach frameSpec  (slot   self   _frameSpecs   ) ;END1
(ifTrue (not  (isComplete   frameSpec  ) ) ;END1
(setq frames  (Cons  (getExtractFrame   frameSpec  )frames   ) )
)
)

(return frames   )
)

(defMethod getIncompleteFrameSpecs  ScaffoldSpec  (self   ) ;END1
(raise "may be depreciated"   )

(setq frames  () )

(foreach frameSpec  (slot   self   _frameSpecs   ) ;END1
(ifTrue (not  (isComplete   frameSpec  ) ) ;END1
(setq frames  (Cons  frameSpec  frames   ) )
)
)

(return frames   )
)

(defMethod createFinishFrames  ScaffoldSpec  (self   ) ;END1
(raise "may be depreciated"   )

(setq (slot   self   _ExtractFrameFinishers   )() )

(setq inCoupling  (getInCoupling   UFOCUSMONOMERU  ) )

(ifTrue (isNil   inCoupling  ) ;END1
(return )
)

(setq preceedingMonomer  (getOtherSideMonomer   inCoupling   UFOCUSMONOMERU   ) )

(setq preceedingTopology  (getTopology   preceedingMonomer  ) )

(assert (notNil   preceedingTopology  )(%   "The preceedingTopology for preceedingMonomer(%s) was nil!!!"   (description   preceedingMonomer  ) ) )

(setq preceedingScaffoldSpec  (getTemporaryObject   preceedingTopology  ) )

(assert (notNil   preceedingScaffoldSpec  )(%   "The preceedingScaffoldSpec for %s was nil!!!"   (description   preceedingTopology  ) ) )

(setq incompleteFrameSpecs  (getIncompleteFrameSpecs   preceedingScaffoldSpec  ) )

(setq cr  (getTemporaryResidue   UFOCUSMONOMERU  ) )

(setq cp  (getTemporaryResidue   preceedingMonomer  ) )

(println (%   "    The preceedingScaffoldSpec has %d incomplete frameSpecs"   (length   incompleteFrameSpecs  ) ) )

(foreach frameSpec  incompleteFrameSpecs   ;END1
(println  "Looking at incomplete frameSpec"    )

(setq incompleteFrame  (getFrame   frameSpec  ) )

(setq boundFrame  (getBoundFrame   incompleteFrame   preceedingMonomer   ) )

(setq pAtom  (get_pAtom   boundFrame  ) )

(setq qAtom  (get_qAtom   boundFrame  ) )

(setq mine  false   )

(ifTrue (containsAtom   cr   pAtom   ) ;END1
(setq mine  true   )

(setq missingAtom  pAtom   )
)

(ifTrue (containsAtom   cr   qAtom   ) ;END1
(setq mine  true   )

(setq missingAtom  qAtom   )
)

(if mine   ;END1
(then ;END1
(println (%   "The incomplete frame overlaps the current residue(%s %s) so we will create a finish frame"   (getName   cr  )(description   cr  ) ) )

(setq inCoupling  (getInCoupling   UFOCUSMONOMERU  ) )

(setq inPlugName  (getOutMonomerPlugName   inCoupling  ) )

(setq finishFrame  (ExtractFrameFinisher  :plugName  inPlugName  :containsAtom  (getName   missingAtom  ):recognizer  (getFrameRecognizer   (getFrame   frameSpec  )) ) )

(setq (slot   self   _ExtractFrameFinishers   )(Cons  finishFrame  (slot   self   _ExtractFrameFinishers   ) ) )
)

(else ;END1
(println (%   "The incomplete frame does not overlap the current residue(%s %s) so we will not create a finish frame"   (getName   cr  )(description   cr  ) ) )
)
)
)

(println (%   "      There are %d ExtractFrameFinishers"   (length   (slot   self   _ExtractFrameFinishers   )) ) )

(setq extractScaffold  (getExtractScaffold   (slot   self   _metaTopology   )) )

(setExtractFrameFinishersFromCons   extractScaffold   (slot   self   _ExtractFrameFinishers   ) )
)

(defMethod getFinishFrames  ScaffoldSpec  (self   ) ;END1
(raise "may be depreciated"   )

(return (slot   self   _FinishFrames   ) )
)

(defMethod realizeMetaExtractScaffold  ScaffoldSpec  (self   ) ;END1
(let (inPlug  metaScaffold  metaExtractFrame   ) ;END1
(println "realizeMetaExtractScaffoldvvvvvvvvvvvvvvvvvv"   )

(println (%   "Realizing ScaffoldSpec for MetaConstitution/Topology(%s/%s)"   (getGroup   (getMetaConstitution   (slot   self   _metaTopology   )))(getName   (slot   self   _metaTopology   )) ) )

(describe   self  )

(setq inPlug  (getInPlug   (slot   self   _metaTopology   )) )

(setq metaScaffold  (MetaExtractScaffold  inPlug   ) )

(foreach frameSpec  (slot   self   _frameSpecs   ) ;END1
(setq metaExtractFrame  (createMetaExtractFrame   frameSpec  ) )

(addMetaExtractFrame   metaScaffold   metaExtractFrame   )
)

(foreach metaExtractFrameFinisher  (slot   self   _metaExtractFrameFinishers   ) ;END1
(addMetaExtractFrame   metaScaffold   metaExtractFrameFinisher   )
)

(setMetaExtractScaffold   (slot   self   _metaTopology   ) metaScaffold   )

(check   metaScaffold   (slot   self   _metaTopology   ) )

(println "realizeMetaExtractScaffold^^^^^^^^^^^^^^^^"   )
)
)

(defun createScaffoldSpec  (metaTopology  metaConstitution   ) ;END1
(println (%   "Creating MetaExtractScaffoldSpec for MetaTopology/MetaConstitution(%s/%s)"   (getName   metaTopology  )(getGroup   metaConstitution  ) ) )

(let ((scaffoldSpec  (ScaffoldSpec  metaTopology   ) ) ) ;END1
(return scaffoldSpec   )
)
)

(defun createAllMetaExtractScaffolds  (metaDatabase   ) ;END1
(let (scaffoldSpecs  metaConstitution  scaffoldSpec   ) ;END1
(setq scaffoldSpecs  () )

(foreach entry  (entries   (getMetaConstitutions   metaDatabase  )) ;END1
(setq metaConstitution  (value   entry  ) )

(foreach metaTopology  (getMetaTopologies   metaConstitution  ) ;END1
(setq scaffoldSpec  (createScaffoldSpec  metaTopology  metaConstitution   ) )

(setScaffoldSpec   metaTopology   scaffoldSpec   )

(setq scaffoldSpecs  (Cons  scaffoldSpec  scaffoldSpecs   ) )
)
)

(foreach scaffoldSpec  scaffoldSpecs   ;END1
(giveYourFrameFinishersToOutPlugMates   scaffoldSpec   metaDatabase   )
)

(foreach scaffoldSpec  scaffoldSpecs   ;END1
(realizeMetaExtractScaffold   scaffoldSpec  )
)
)
)
