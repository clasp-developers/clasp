; fixed code
(require "CANDO/db/checkTopologies.l"   )

(require "CANDO/db/checkMetaDatabase.l"   )

(global *UniqueMetaId*  1   )

(defun getUniqueMetaId  () ;END1
(let ((id  (%   "id%04d"   *UniqueMetaId*   ) ) ) ;END1
(setq *UniqueMetaId*  (+   *UniqueMetaId*   1   ) )

(return id   )
)
)

(defClass MetaConstant  ()(_Value   ) ;END1
(method __init__  (self  val   ) ;END1
(setq (slot   self   _Value   )val   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(return (slot   self   _Value   ) )
)
)

(defClass MetaTraceableObject  ()(_backtrace   ) ;END1
(method __init__  (self   ) ;END1
(setq (slot   self   _backtrace   )(backtrace   ) )
)

(method dump  (self   ) ;END1
(let (serr   ) ;END1
(setq serr  (StringStream   ) )

(foreach bt  (slot   self   _backtrace   ) ;END1
(when (isOfClass   bt   Cons   ) ;END1
(continue )
)

(write   serr   (%   "Bad object backtrace line/file = %d/%s"   (lineno   bt  )(filename   bt  ) ) )
)

(return (str   serr  ) )
)
)
)

(defClass MetaSingletonObject  MetaTraceableObject  () ;END1
(method __init__  (self   ) ;END1
(callAncestorMethod self   )
)

(method key  (self  desc   ) ;END1
(let (key   ) ;END1
(setq key  (%   "Singleton-%s-%s"   (className   self  )desc   ) )

(return key   )
)
)
)

(defClass MetaObject  MetaTraceableObject  (_UniqueId   ) ;END1
(method __init__  (self   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _UniqueId   )(getUniqueMetaId   ) )
)

(method key  (self  desc   ) ;END1
(assert (notNil   (slot   self   _UniqueId   ))(%   "You must call initMetaObject for each MetaObject. This class=%s"   (className   self  ) ) )

(let (key   ) ;END1
(setq key  (%   "%s-%s-%s"   (className   self  )(repr  (slot   self   _UniqueId   ) )(repr  desc   ) ) )

(return key   )
)
)
)

(defClass MetaDatabase  ()(_MetaConstitutions  _GroupAdds  _Recognizers  _Groups   ) ;END1
(method __init__  (self   ) ;END1
(setq (slot   self   _Recognizers   )(ObjectDictionary   ) )

(setq (slot   self   _MetaConstitutions   )(ObjectDictionary   ) )

(setq (slot   self   _GroupAdds   )() )

(setq (slot   self   _Groups   )() )
)

(method addRecognizer  (self  frameRecognizer   ) ;END1
(put   (slot   self   _Recognizers   ) (getRecognizerName   frameRecognizer  )frameRecognizer   )

(addFrameRecognizer   m:*DATABASE*   frameRecognizer   )
)

(method getFrameRecognizer  (self  recognizerName   ) ;END1
(return (get   (slot   self   _Recognizers   ) recognizerName   ) )
)

(method hasRecognizer  (self  name   ) ;END1
(return (contains   (slot   self   _Recognizers   ) name   ) )
)

(method createSystemGroup  (self  &key  groupName  (representativeName  ""   ) ) ;END1
(let (group  recognized  names   ) ;END1
(assert (notNil   groupName  )"You must provide groupName"   )

(setq recognized  (recognizesSystemMonomerGroup   m:*DATABASE*   groupName   ) )

(when recognized   ;END1
(setq names  (getSystemMonomerGroupKeys  m:*DATABASE*   ) )

(println (%   "*DATABASE* currently contains SystemMonomerGroup names: %s"   (repr  names   ) ) )

(raise (%   "m:*DATABASE* already contains SystemMonomerGroup with name(%s)"   groupName   ) )
)

(assert (not  recognized   )(%   "BuilderDatabase already has system monomer group(%s)"   groupName   ) )

(setq group  (MonomerGroup  :name  groupName  :representativeName  representativeName   ) )

(setq (slot   self   _Groups   )(Cons  group  (slot   self   _Groups   ) ) )

(println (%   "Creating system monomer group: %s"   groupName   ) )

(addSystemMonomerGroup   m:*DATABASE*   group   )

(return group   )
)
)

(method addGroupAdd  (self  groupAdd   ) ;END1
(setq (slot   self   _GroupAdds   )(Cons  groupAdd  (slot   self   _GroupAdds   ) ) )
)

(method getMetaConstitutions  (self   ) ;END1
(return (slot   self   _MetaConstitutions   ) )
)

(method getMetaConstitution  (self  name   ) ;END1
(return (get   (slot   self   _MetaConstitutions   ) name   ) )
)

(method addMetaConstitution  (self  metaConstitution   ) ;END1
(let (name   ) ;END1
(setq name  (getGroup   metaConstitution  ) )

(println (%   "Adding MetaConstitution(%s)"   name   ) )

(assert (not  (contains   (slot   self   _MetaConstitutions   ) name   ) )(%   "MetaConstitution %s has already been defined"   name   ) )

(put   (slot   self   _MetaConstitutions   ) name  metaConstitution   )
)
)

(method checkTopologies  (self   ) ;END1
(let (collection  mc  sawProblem  fullReport  key  checker  report   ) ;END1
(setq collection  (MetaOutPlugCollection   ) )

(foreach mce  (entries   (slot   self   _MetaConstitutions   )) ;END1
(setq mc  (value   mce  ) )

(foreach mop  (getMetaOutPlugs   mc  ) ;END1
(addMetaOutPlug   collection   mop   )
)
)

(setq sawProblem  false   )

(setq fullReport  (StringStream   ) )

(foreach entry  (entries   (getCheckers   collection  )) ;END1
(setq key  (key   entry  ) )

(setq checker  (value   entry  ) )

(setq report  (StringStream   ) )

(ifFalse (allMetaOutPlugsAreConsistent   checker   report   ) ;END1
(writeln   fullReport    "PROBLEM!!"    )

(writeln   fullReport   (%   "%s"   (str   report  ) ) )

(setq sawProblem  true   )
)
)

(ifTrue sawProblem   ;END1
(println (%   "%s"   (str   fullReport  ) ) )

(println "There are problems in the database - fix them to proceed"   )

(exit 1   )
)
)
)

(method collectGroupsWithTopologiesWithRingClosingPlugNames  (self  plugName   ) ;END1
(let ((groupNames  (StringSet   ) )mc   ) ;END1
(foreach mce  (entries   (slot   self   _MetaConstitutions   )) ;END1
(setq mc  (value   mce  ) )

(when (hasTopologyWithRingClosingPlugNamed   mc   plugName   ) ;END1
(insert   groupNames   (getGroupName   mc  ) )
)
)

(return groupNames   )
)
)

(method collectGroupsWithTopologiesWithPlugNames  (self  plugName   ) ;END1
(let (groupNames  mc   ) ;END1
(setq groupNames  (StringSet   ) )

(foreach mce  (entries   (slot   self   _MetaConstitutions   )) ;END1
(setq mc  (value   mce  ) )

(when (hasTopologyWithPlugNamed   mc   plugName   ) ;END1
(insert   groupNames   (getGroupName   mc  ) )
)
)

(return groupNames   )
)
)

(method updateMatesForPlugs  (self   ) ;END1
(let (mc  metaPlugs  otherSidePlugName  predictedMateGroupNames  givenMateCons  givenMate  givenMateGroupNames   ) ;END1
(foreach mce  (entries   (slot   self   _MetaConstitutions   )) ;END1
(setq mc  (value   mce  ) )

(println (%   "======== Identifying mates for plugs in: %s"   (getGroupName   mc  ) ) )

(setq metaPlugs  (getMetaPlugs   mc  ) )

(foreach mp  metaPlugs   ;END1
(when (hasMates   mp  ) ;END1
(setq otherSidePlugName  (DirectionalCoupling_otherSidePlugName  (getName   mp  ) ) )

(setq predictedMateGroupNames  (collectGroupsWithTopologiesWithPlugNames   self   otherSidePlugName   ) )

(setq givenMateCons  (getMates   mp  ) )

(when (>   (length   givenMateCons  ) 1   ) ;END1
(raise "There is more than one mates, this means I have to decide which Mate handle which groups and this depends on what type of cap each mates has - deal with this if/when it comes up"   )
)

(setq givenMate  (car   givenMateCons  ) )

(when (isNil   givenMate  ) ;END1
(println (%   "givenMate can not be nil for MetaPlug=%s"   (getName   mp  ) ) )

(dump   mp  )

(raise "Bad givenMate"   )
)

(setq givenMateGroupNames  (getUnExpandedNames   givenMate  ) )

(println (%   "Given mates = %s"   (repr  givenMateGroupNames   ) ) )

(println (%   "Setting mates to predicted ones: %s"   (repr  predictedMateGroupNames   ) ) )

(setGroupNames   givenMate   predictedMateGroupNames   )
)
)
)
)
)

(method updateRingClosingMatesForRingClosingPlugs  (self   ) ;END1
(let (mc  metaPlugs  otherSidePlugName  predictedMateGroupNames  givenRingClosingMateCons  givenRingClosingMate  givenRingClosingMateGroupNames   ) ;END1
(foreach mce  (entries   (slot   self   _MetaConstitutions   )) ;END1
(setq mc  (value   mce  ) )

(println (%   "========= Identifying RingClosingMates for RingClosingPlugs in: %s"   (getGroupName   mc  ) ) )

(setq metaPlugs  (getMetaPlugs   mc  ) )

(foreach mp  metaPlugs   ;END1
(when (isRingClosingPlug   mp  ) ;END1
(setq otherSidePlugName  (getOtherSidePlugName   mp  ) )

(setq predictedMateGroupNames  (collectGroupsWithTopologiesWithRingClosingPlugNames   self   otherSidePlugName   ) )

(setq givenRingClosingMateCons  (getRingClosingMates   mp  ) )

(when (>   (length   givenRingClosingMateCons  ) 1   ) ;END1
(raise "There is more than one mates, this means I have to decide which Mate handle which groups and this depends on what type of cap each mates has - deal with this if/when it comes up"   )
)

(setq givenRingClosingMate  (car   givenRingClosingMateCons  ) )

(setq givenRingClosingMateGroupNames  (getUnExpandedNames   givenRingClosingMate  ) )

(println (%   "Given RingClosingMates = %s"   (repr  givenRingClosingMateGroupNames   ) ) )

(println (%   "Setting mates to predicted ones: %s"   (repr  predictedMateGroupNames   ) ) )

(setGroupNames   givenRingClosingMate   predictedMateGroupNames   )
)
)
)
)
)

(method check  (self   ) ;END1
(checkAllMates self   )

(checkTopologies   self  )
)

(method createConstitution  (self  fragment   ) ;END1
(let (group  metaConstitution  constitution  groupAdds   ) ;END1
(setq group  (getProperty   fragment   :group   ) )

(println (%   "Creating Constitution for group(%s)"   group   ) )

(ifFalse (contains   (slot   self   _MetaConstitutions   ) group   ) ;END1
(setq ss  (StringStream   ) )

(writeln   ss   (%   "Creating Constitution for chemdraw fragment(%s)"   (getProperty   fragment   :name   ) ) )

(writeln   ss   (%   "   that is supposed to be in group(%1%)"   group   ) )

(writeln   ss   (%   "   but [self slot _MetaConstitutions] did not contain the name(%s)"   group   ) )

(writeln   ss   (%   "   known MetaConstitutions groups are(%s)"   (allKeysAsString   (slot   self   _MetaConstitutions   )) ) )

(writeln   ss   (%   "   define a MetaConstitution with the name(%s)"   group   ) )

(raise (str   ss  ) )
)

(setq metaConstitution  (get   (slot   self   _MetaConstitutions   ) group   ) )

(setq constitution  (resolve   metaConstitution   self  fragment   ) )

(assert (notNil   constitution  )"constitution can not be nil"   )

(println (%   "Adding constitution(%s)"   (getName   constitution  ) ) )

(log (%   "Adding constitution(%s)"   (getName   constitution  ) ) )

(addConstitution   m:*DATABASE*   constitution   )

(setq groupAdds  (getGroupAdds   metaConstitution  ) )

(foreach groupAdd  groupAdds   ;END1
(resolve   groupAdd   self  fragment   )
)
)
)

(method addChemdrawFragments  (self  chemdrawFragments   ) ;END1
(foreach frag   chemdrawFragments    ;END1
(println (%   "Fragment ----> %s"   (getProperty   frag   :monomerName   ) ) )

(createConstitution   self   frag   )
)
)

(method checkChemdraw  (self   ) ;END1
(let (representative  members  ss  metaConstitution  capName  serr  capConstitution  capMetaConstitutionName  allMateNames  ss  alignTo  plugName  found  anchorFrame  extractScaffold  originAtom  frameRecognizer  residue  atom  matches  mates   ) ;END1
(println  "Checking monomer groups"    )

(foreach monomerGroup  (monomerGroupsAsCons   m:*DATABASE*  ) ;END1
(println (%   "Checking monomerGroup(%s)"   (getName   monomerGroup  ) ) )

(setq representative  (getRepresentative   monomerGroup  ) )

(setq members  (getMonomerNames   monomerGroup  ) )

(ifTrue (==   representative   ""   ) ;END1
(println  "     no representative -> skip"    )

(continue )
)

(ifTrue (==   (size   members  ) 0   ) ;END1
(println  "     no members -> skip"    )

(continue )
)

(ifFalse (contains   members   representative   ) ;END1
(setq ss  (StringStream   ) )

(writeln   ss   (%   "The MonomerGroup(%s) has a representative(%s) that is not a member"   (getName   monomerGroup  )representative   ) )

(writeln   ss   (%   "   The members of this group are(%s)"   (repr  members   ) ) )

(println (str   ss  ) )

(println  "Terminating so that you can fix it"    )

(exit 1   )
)
)

(println  "Checking plug caps"    )

(foreach mce  (entries   (slot   self   _MetaConstitutions   )) ;END1
(setq metaConstitution  (value   mce  ) )

(foreach metaPlug  (getMetaPlugs   metaConstitution  ) ;END1
(ifTrue (isOfClass   metaPlug   MetaOriginPlug   ) ;END1
(continue )
)

(foreach mate  (getMates   metaPlug  ) ;END1
(setq capName  (getCap   mate  ) )

(ifFalse (recognizesNameOrPdb   m:*DATABASE*   capName   ) ;END1
(setq serr  (StringStream   ) )

(writeln   serr   (%   "A cap(%s) for a Mate was specified that does not exist in the database"   capName   ) )

(writeln   serr   (%   "MetaConstitution(%s)    MetaPlug(%s)"   (getMetaConstitutionName   metaConstitution  )(getName   metaPlug  ) ) )

(raise (LispError  (str   serr  ) ) )
)

(setq capConstitution  (constitutionForNameOrPdb   m:*DATABASE*   capName   ) )

(setq capMetaConstitutionName  (getMetaConstitutionName   capConstitution  ) )

(setq mates  (getMates   metaPlug  ) )

(foreach mate  mates   ;END1
(setq allMateNames  (getUnExpandedNames   mate  ) )

(ifFalse (getOverrideCapMateRequirement   mate  ) ;END1
(ifFalse (contains   allMateNames   capMetaConstitutionName   ) ;END1
(setq ss  (StringStream   ) )

(writeln   ss   (%   "There is a problem with MetaConstitution(%s)/MetaPlug(%s)"   (getGroup   metaConstitution  )(getName   metaPlug  ) ) )

(writeln   ss   (%   "    it has a cap(%s) with the MetaConstitution(%s) that is not one of its mates"   capName  capMetaConstitutionName   ) )

(writeln   ss   (%   "    currently its mates are defined by groups that the plug name(%s)"   (otherSidePlugName   metaPlug  ) ) )

(writeln   ss   (%   "    and they are: ( %s )"   (repr  allMateNames   ) ) )

(writeln   ss   (%   "    Solution:  1. To MetaConstitution(%s) create a topology with the plug(%s)"   capMetaConstitutionName  (otherSidePlugName   metaPlug  ) ) )

(writeln   ss   (%   "               2. To MetaConstitution/MetaPlug(%s/%s) add to the Mate the option 'overrideCapMateRequirement: true'"   (getGroup   metaConstitution  )(getName   metaPlug  ) ) )

(println (str   ss  ) )

(println  "Terminating so that you can fix it"    )

(exit 1   )
)
)
)
)
)
)

(println  "Checking alignTos"    )

(foreach constitution  (constitutionsAsCons   m:*DATABASE*  ) ;END1
(println (%   "Constitution = %s"   (getName   constitution  ) ) )

(foreach topology  (topologiesAsCons   constitution  ) ;END1
(println (%   "Topology = %s"   (getName   topology  ) ) )

(setq alignTo  (getAlignTo   topology  ) )

(println (%   "alignTo = %s"   (repr  alignTo   ) ) )

(cond ;END1
(isOfClass   alignTo   AnchorOnOtherSideOfPlug   )
(setq plugName  (getPlugName   alignTo  ) )

(setq found  false   )

(foreach onePlug  (plugsWithMatesAsCons   topology  ) ;END1
(when (==   (getName   onePlug  ) plugName   ) ;END1
(setq found  true   )
)
)

(when (not  found   ) ;END1
(setq serr  (StringStream   ) )

(writeln   serr    "You have defined an <alignTo> that will throw an error"    )

(writeln   serr   (%   "It is an AnchorOnOtherSideOfPlug with plugName(%s) that doesn't match a plug in the Topology"   plugName   ) )

(writeln   serr   (%   "It is in MetaConstitition(%s)"   (getMetaConstitutionName   constitution  ) ) )

(writeln   serr   (%   "Topology(%s)"   (getName   topology  ) ) )

(raise (str   serr  ) )
)


(isOfClass   alignTo   AnchorOrigin   )
(setq anchorFrame  (getFrame   alignTo  ) )

(setq extractScaffold  (getExtractScaffold   topology  ) )

(when (not  (hasExtractCompleteFrameWithName   extractScaffold   (getName   anchorFrame  ) ) ) ;END1
(setq serr  (StringStream   ) )

(writeln   serr    "You have defined an <alignTo> that will throw an error"    )

(writeln   serr    "It is an AnchorOrigin that isn't a CompleteFrame within the Scaffold"    )

(writeln   serr   (%   "The AnchorOrigin has a frame name (%s)"   (getName   anchorFrame  ) ) )

(writeln   serr   (%   "It is in MetaConstitition(%s)"   (getMetaConstitutionName   constitution  ) ) )

(writeln   serr   (%   "Topology(%s)"   (getName   topology  ) ) )

(raise (str   serr  ) )
)


(isOfClass   alignTo   RecognizedFrame   )
(setq originAtom  (getOriginAtomName   alignTo  ) )

(setq frameRecognizer  (getFrameRecognizer   alignTo  ) )

(setq residue  (getResidue   constitution  ) )

(when (not  (hasAtomWithName   residue   originAtom   ) ) ;END1
(setq serr  (StringStream   ) )

(writeln   serr    "You have defined an <alignTo> that will throw an error"    )

(writeln   serr   (%   "It is a RecognizedFrame that has an origin atom(%s) that is not in the residue of the Constitution"   originAtom   ) )

(writeln   serr   (%   "It is in MetaConstitition(%s)"   (getMetaConstitutionName   constitution  ) ) )

(writeln   serr   (%   "Topology(%s)"   (getName   topology  ) ) )

(raise (str   serr  ) )
)

(setq atom  (atomWithName   residue   originAtom   ) )

(setq matches  (recognizes   frameRecognizer   atom   ) )

(when (not  matches   ) ;END1
(setq serr  (StringStream   ) )

(writeln   serr    "You have defined an <alignTo> that will throw an error"    )

(writeln   serr   (%   "It is a RecognizedFrame that has an origin atom(%s) that is not recognized by the FrameRecognizer(%s)"   (getRecognizerName   frameRecognizer  ) ) )

(writeln   serr   (%   "It is in MetaConstitition(%s)"   (getMetaConstitutionName   constitution  ) ) )

(writeln   serr   (%   "Topology(%s)"   (getName   topology  ) ) )

(raise (str   serr  ) )
)


(true ;END1
(raise (%   "Unknown class(%s) for alignTo"   (className  alignTo   ) ) )
)
)
)
)
)
)
)

(defClass MetaExtractIncompleteFrame  MetaObject  (_plugName  _metaFrame  _alias   ) ;END1
(method __init__  (self  &key  plugName  metaFrame  alias   ) ;END1
(callAncestorMethod self   )

(assert (notNil   plugName  )"plugName must be provided"   )

(assert (notNil   metaFrame  )"metaFrame must be provided"   )

(assert (notNil   alias  )"alias must be provided"   )

(setq (slot   self   _metaFrame   )metaFrame   )

(setq (slot   self   _plugName   )plugName   )

(setq (slot   self   _alias   )alias   )
)

(method getMetaFrameName  (self   ) ;END1
(return (getName   (slot   self   _metaFrame   )) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (frame  key  value   ) ;END1
(setq frame  (resolve   (slot   self   _metaFrame   ) metaDatabase  fragment   ) )

(setq key  (key   self   (getName   frame  ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq value  (ExtractIncompleteFrame  :alias  (slot   self   _alias   ):frame  frame  :plugName  (slot   self   _plugName   ) ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaExtractCompleteFrame  MetaObject  (_metaFrame   ) ;END1
(method __init__  (self  &key  metaFrame   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _metaFrame   )metaFrame   )
)

(method getMetaFrameName  (self   ) ;END1
(return (getName   (slot   self   _metaFrame   )) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (frame  key  value   ) ;END1
(setq frame  (resolve   (slot   self   _metaFrame   ) metaDatabase  fragment   ) )

(setq key  (key   self   (getName   frame  ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq value  (ExtractCompleteFrame  :frame  frame   ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaExtractFrameFinisher  MetaObject  (_alias  _plugName   ) ;END1
(method __init__  (self  &key  alias  plugName   ) ;END1
(callAncestorMethod self   )

(assert (notNil   alias  )"alias must be provided"   )

(assert (notNil   plugName  )"plugName must be provided"   )

(setq (slot   self   _alias   )alias   )

(setq (slot   self   _plugName   )(DirectionalCoupling_inPlugName  (DirectionalCoupling_couplingName  plugName   ) ) )
)

(method getAlias  (self   ) ;END1
(return (slot   self   _alias   ) )
)

(method getPlugName  (self   ) ;END1
(return (slot   self   _plugName   ) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  value   ) ;END1
(setq key  (key   self   (slot   self   _plugName   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq value  (ExtractFrameFinisher  :alias  (slot   self   _alias   ):plugName  (slot   self   _plugName   ) ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaAnchor  MetaObject  (_plugName   ) ;END1
(method __init__  (self  pn   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _plugName   )pn   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  value   ) ;END1
(setq key  (key   self   "scaffold"   ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq value  (AnchorOnOtherSideOfPlug  :plugName  (slot   self   _plugName   ) ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaExtractScaffold  MetaObject  (_anchor  _topologyName  _metaExtractCompleteFrames  _metaExtractIncompleteFrames  _metaExtractFrameFinishers  _metaInPlug   ) ;END1
(method __init__  (self  metaInPlug   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _metaInPlug   )metaInPlug   )

(setq (slot   self   _anchor   )() )

(setq (slot   self   _metaExtractCompleteFrames   )() )

(setq (slot   self   _metaExtractIncompleteFrames   )() )

(setq (slot   self   _metaExtractFrameFinishers   )() )

(createMetaAnchor   self  )
)

(method description  (self   ) ;END1
(let (ss   ) ;END1
(setq ss  (StringStream   ) )

(writeln   ss    "MetaExtractScaffold description follows"    )

(writeln   ss    "MetaExtractCompleteFrames"    )

(foreach one  (slot   self   _metaExtractCompleteFrames   ) ;END1
(writeln   ss   (%   "    MetaFrame(%s)"   (getMetaFrameName   one  ) ) )
)

(println  "MetaExtractIncompleteFrames"    )

(foreach one  (slot   self   _metaExtractIncompleteFrames   ) ;END1
(writeln   ss   (%   "    MetaFrame(%s)"   (getMetaFrameName   one  ) ) )
)

(return (str   ss  ) )
)
)

(method createMetaAnchor  (self   ) ;END1
(cond ;END1
(isOfClass   (slot   self   _metaInPlug   ) MetaOriginPlug   )
(setq (slot   self   _anchor   )(MetaAnchorOrigin  (getMetaOriginFrame   (slot   self   _metaInPlug   )) ) )

(return )


(isOfClass   (slot   self   _metaInPlug   ) MetaInPlug   )
(setq (slot   self   _anchor   )(MetaAnchorOnOtherSideOfPlug  :plugName  (getName   (slot   self   _metaInPlug   )) ) )

(return )


(true ;END1
(raise (%   "Illegal type in metaInPlug = %s"   (className   (slot   self   _metaInPlug   )) ) )
)
)
)

(method check  (self  metaTopology   ) ;END1
(let (foundExtractFrame  exportedMetaFrame  exportedName  ss   ) ;END1
(foreach metaPlug  (getMetaPlugs   metaTopology  ) ;END1
(ifTrue (exportsMetaFrame   metaPlug  ) ;END1
(setq foundExtractFrame  0   )

(setq exportedMetaFrame  (getExportMetaFrame   metaPlug  ) )

(setq exportedName  (getName   exportedMetaFrame  ) )

(foreach metaExtractCompleteFrame  (slot   self   _metaExtractCompleteFrames   ) ;END1
(ifTrue (==   (getMetaFrameName   metaExtractCompleteFrame  ) exportedName   ) ;END1
(setq foundExtractFrame  (+   foundExtractFrame   1   ) )
)
)

(foreach metaExtractIncompleteFrame  (slot   self   _metaExtractIncompleteFrames   ) ;END1
(ifTrue (==   (getMetaFrameName   metaExtractIncompleteFrame  ) exportedName   ) ;END1
(setq foundExtractFrame  (+   foundExtractFrame   1   ) )
)
)

(cond ;END1
(==   foundExtractFrame   0   )
(setq ss  (StringStream   ) )

(writeln   ss   (%   "The MetaConstitution/MetaTopology(%s/%s) has a plug(%s)"   (getGroup   (getMetaConstitution   metaTopology  ))(getName   metaTopology  )(getName   metaPlug  ) ) )

(writeln   ss   (%   "    that exports the MetaFrame(%s) but its not part of the MetaTopology's MetaExtractScaffold"   exportedName   ) )

(writeln   ss    "    the MetaScaffold MetaFrames are defined in ScaffoldSpec createFrameSpecs"    )

(writeln   ss   (%   "%s"   (description   self  ) ) )

(println (str   ss  ) )

(println  "Terminating so that you can fix it"    )

(exit 1   )


(>   foundExtractFrame   1   )
(setq ss  (StringStream   ) )

(writeln   ss   (%   "VERY WEIRD: The MetaConstitution/MetaTopology(%s/%s) has a plug(%s)"   (getGroup   (getMetaConstitution   metaTopology  ))(getName   metaTopology  )(getName   metaPlug  ) ) )

(writeln   ss   (%   "    that exports the MetaFrame(%s) but it has several Extract;Frames in the MetaTopology's MetaScaffold"   exportedName   ) )

(println (str   ss  ) )

(println  "Terminating so that you can fix it"    )

(exit 1   )


(true ;END1
()
)
)
)
)
)
)

(method addMetaExtractCompleteFrame  (self  extractFrame   ) ;END1
(setq (slot   self   _metaExtractCompleteFrames   )(Cons  extractFrame  (slot   self   _metaExtractCompleteFrames   ) ) )
)

(method addMetaExtractIncompleteFrame  (self  extractFrame   ) ;END1
(setq (slot   self   _metaExtractIncompleteFrames   )(Cons  extractFrame  (slot   self   _metaExtractIncompleteFrames   ) ) )
)

(method addMetaExtractFrameFinisher  (self  extractFrame   ) ;END1
(print "addMetaExtractFrameFinisher"   )

(setq (slot   self   _metaExtractFrameFinishers   )(Cons  extractFrame  (slot   self   _metaExtractFrameFinishers   ) ) )
)

(method addMetaExtractFrame  (self  extractFrame   ) ;END1
(cond ;END1
(isOfClass   extractFrame   MetaExtractCompleteFrame   )
(addMetaExtractCompleteFrame   self   extractFrame   )


(isOfClass   extractFrame   MetaExtractIncompleteFrame   )
(addMetaExtractIncompleteFrame   self   extractFrame   )


(isOfClass   extractFrame   MetaExtractFrameFinisher   )
(addMetaExtractFrameFinisher   self   extractFrame   )


(true ;END1
(raise (%   "Illegal class(%s) for extractFrame"   (className   extractFrame  ) ) )
)
)
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  value  metaCompleteFrames  metaIncompleteFrames  metaFrameFinishers  anchor   ) ;END1
(setq key  (key   self   "scaffold"   ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq value  (ExtractScaffold   ) )

(setq metaCompleteFrames  (map  (lambda  (f   )(resolve   f   metaDatabase  fragment   ) )(slot   self   _metaExtractCompleteFrames   ) ) )

(setExtractCompleteFramesFromCons   value   metaCompleteFrames   )

(setq metaIncompleteFrames  (map  (lambda  (f   )(resolve   f   metaDatabase  fragment   ) )(slot   self   _metaExtractIncompleteFrames   ) ) )

(setExtractIncompleteFramesFromCons   value   metaIncompleteFrames   )

(setq metaFrameFinishers  (map  (lambda  (f   )(resolve   f   metaDatabase  fragment   ) )(slot   self   _metaExtractFrameFinishers   ) ) )

(setExtractFrameFinishersFromCons   value   metaFrameFinishers   )

(setq anchor  (resolve   (slot   self   _anchor   ) metaDatabase  fragment   ) )

(setAnchor   value   anchor   )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaAddToGroup  MetaSingletonObject  (_GroupName  _ConstitutionName   ) ;END1
(method __init__  (self  &key  groupName  constitutionName   ) ;END1
(callAncestorMethod self   )

(assert (notNil   groupName  )"groupName must be provided"   )

(setq (slot   self   _GroupName   )groupName   )

(setq (slot   self   _ConstitutionName   )constitutionName   )

(let ((recognized  (recognizesSystemMonomerGroup   m:*DATABASE*   (slot   self   _GroupName   ) ) ) ) ;END1
(assert recognized  (%   "There is no group named [%s]  backTrace: %s"   (slot   self   _GroupName   )(dump   self  ) ) )
)
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (constitutionName  group   ) ;END1
(if (isNil   (slot   self   _ConstitutionName   )) ;END1
(then ;END1
(setq constitutionName  (resolve   (MetaProperty  :name   ) metaDatabase  fragment   ) )
)

(else ;END1
(setq constitutionName  (resolve   (slot   self   _ConstitutionName   ) metaDatabase  fragment   ) )
)
)

(setq group  (getSystemMonomerGroup   m:*DATABASE*   (slot   self   _GroupName   ) ) )

(assert (notNil   group  )(%   "There is no group named [%s]"   (slot   self   _GroupName   ) ) )

(println (%   "Adding %s to %s"   constitutionName  (slot   self   _GroupName   ) ) )

(addMonomerNameNoChecks   group   constitutionName   )
)
)
)

(defClass MetaAddToGroupByStereoisomer  MetaSingletonObject  (_groupPrefix  _stereoInformation  _center   ) ;END1
(method __init__  (self  &key  groupPrefix  stereoInformation  separateByCenter   ) ;END1
(callAncestorMethod self   )

(assert (notNil   groupPrefix  )"groupPrefix must not be nil"   )

(assert (notNil   stereoInformation  )"stereoInformation must not be nil"   )

(assert (notNil   separateByCenter  )"separateByCenter must not be nil"   )

(setq (slot   self   _groupPrefix   )groupPrefix   )

(setq (slot   self   _stereoInformation   )stereoInformation   )

(setq (slot   self   _center   )separateByCenter   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (stereoInformation  groupPrefix  stereoisomers  configuration  monomerName  groupName  group  constitutionName  stereoisomer   ) ;END1
(setq stereoInformation  (resolve   (slot   self   _stereoInformation   ) metaDatabase  fragment   ) )

(setq groupPrefix  (slot   self   _groupPrefix   ) )

(setq stereoisomers  (stereoisomersAsCons   stereoInformation  ) )

(if (>   (length   stereoisomers  ) 1   ) ;END1
(then ;END1
(foreach stereoisomer  stereoisomers   ;END1
(setq configuration  (getConfigurationForCenter   stereoisomer   (slot   self   _center   ) ) )

(setq monomerName  (getName   stereoisomer  ) )

(setq groupName  (%   "%s-%s"   groupPrefix  configuration   ) )

(setq group  (getSystemMonomerGroup   m:*DATABASE*   groupName   ) )

(println (%   "Adding %s to group %s"   monomerName  groupName   ) )

(addMonomerNameNoChecks   group   monomerName   )
)
)

(else ;END1
(setq stereoisomer  (car   stereoisomers  ) )

(setq monomerName  (getName   stereoisomer  ) )

(setq groupName  groupPrefix   )

(setq group  (getSystemMonomerGroup   m:*DATABASE*   groupName   ) )

(setq constitutionName  (getConstitutionName   (slot   self   _stereoInformation   ) metaDatabase  fragment   ) )

(println (%   "Adding achiral %s to group %s"   constitutionName  constitutionName   ) )

(addMonomerNameNoChecks   group   constitutionName   )
)
)
)
)
)

(defun generateStereoisomers  (centers  monomerNameTemplate  pdbNameTemplate   ) ;END1
(let (numCenters  numWildCards  permutations  permutationIndex  stereoisomers  monomerName  ttt  pdbName  permutationIndex  configs  stereoisomers   ) ;END1
(setq numCenters  (length   centers  ) )

(setq numWildCards  (countOccurances   monomerNameTemplate   "%"   ) )

(assert (<=   (countOccurances   pdbNameTemplate   "%"   ) 1   )(%   "PdbNameTemplates can only have one wildcard error in(%s) for monomerTemplate(%s)"   pdbNameTemplate  monomerNameTemplate   ) )

(ifFalse (==   numCenters   numWildCards   ) ;END1
(raise (%   "There is a mismatch between the number of chiralCenters(%s) and the number of wild-cards in the monomer name(%s)"   (repr  chiralCenters   )monomerNameTemplate   ) )
)

(if (>   numCenters   0   ) ;END1
(then ;END1
(setq permutations  (stereochemicalPermutations  numCenters   ) )

(setq permutationIndex  0   )

(setq stereoisomers  () )

(foreach perm  permutations   ;END1
(setq monomerName  (%%   monomerNameTemplate   perm   ) )

(println (%   "pbbNameTemplate=%s"   pdbNameTemplate   ) )

(setq ttt  (%   pdbNameTemplate   "yyy"   ) )

(log (%   "ttt=%s"   ttt   ) )

(setq pdbName  (%   pdbNameTemplate   (%   "%d"   permutationIndex   ) ) )

(setq permutationIndex  (+   permutationIndex   1   ) )

(setq configs  (StereoConfiguration_createMultiple  centers  perm   ) )

(assert (!=   monomerName   ""   )"The monomer name is blank!"   )

(assert (!=   pdbName   ""   )"The pdb name is blank!"   )

(println (%   "Generating stereoisomer name(%s) pdbName(%s)"   monomerName  pdbName   ) )

(setq stereoisomers  (Cons  (Stereoisomer  :name  monomerName  :pdb  pdbName  :configs  configs   )stereoisomers   ) )
)
)

(else ;END1
(setq stereoisomers  () )

(setq monomerName  monomerNameTemplate   )

(setq pdbName  pdbNameTemplate   )

(println (%   "Generating single stereoisomer name(%s) pdbName(%s)"   monomerName  pdbName   ) )

(setq stereoisomers  (list  (Stereoisomer  :name  monomerName  :pdb  pdbName  :configs  () ) ) )
)
)

(return stereoisomers   )
)
)

(defClass MetaProperty  MetaObject  (_PropertyName   ) ;END1
(method __init__  (self  propertyName   ) ;END1
(callAncestorMethod self   )

(assert (isOfClass   propertyName   Symbol   )(%   "propertyName(%s) is not a Symbol!!"   (repr  propertyName   ) ) )

(setq (slot   self   _PropertyName   )propertyName   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  value   ) ;END1
(println (%   "%s [self slot _PropertyName] = %s"   (repr  (sourceFileLine   ) )(repr  (slot   self   _PropertyName   ) ) ) )

(let (key  value   ) ;END1
(setq key  (key   self   (slot   self   _PropertyName   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq value  (getProperty   fragment   (slot   self   _PropertyName   ) ) )

(return (putVariable   fragment   key  value   ) )
)
)
)
)

(defClass MetaPropertyOrDefault  MetaProperty  (_DefaultValue   ) ;END1
(method __init__  (self  propertyName  defaultValue   ) ;END1
(callAncestorMethod self  propertyName   )

(setq (slot   self   _DefaultValue   )defaultValue   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  value   ) ;END1
(setq key  (key   self   (slot   self   _PropertyName   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq value  (getPropertyOrDefault   fragment   (slot   self   _PropertyName   )(slot   self   _DefaultValue   ) ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaResidueAtomWithName  MetaObject  (_AtomName   ) ;END1
(method __init__  (self  atomName   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _AtomName   )atomName   )
)

(method descriptionResolved  (self  metaDatabase  fragment   ) ;END1
(let (atom   ) ;END1
(setq atom  (resolve   (slot   self   _AtomName   ) metaDatabase  fragment   ) )

(return (%   "MetaResidueAtom(%s)"   atom   ) )
)
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (atomName  key  res  residue  value  retval   ) ;END1
(setq atomName  (resolve   (slot   self   _AtomName   ) metaDatabase  fragment   ) )

(setq key  (key   self   atomName   ) )

(println (%   "%s"   (describeVariables  fragment   ) ) )

(println (%   "Just calculated key(%s) for MetaResidueAtomWithName atomName(%s)"   key  atomName   ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(setq res  (getVariable   fragment   key   ) )

(println (%   "The fragment has the variable key(%s) returning its value(%s)"   key  (repr  res   ) ) )

(return res   )
)

(setq residue  (resolve   (MetaResidue   ) metaDatabase  fragment   ) )

(setq value  (atomWithName   residue   atomName   ) )

(println (%   "Resolving MetaResidueAtomWithName key(%s) atomName(%s) value(%s) class(%s)"   key  atomName  (repr  value   )(className  value   ) ) )

(setq retval  (putVariable   fragment   key  value   ) )

(println (%   "Returning MetaResidueAtomWithName key(%s) atomName(%s) retval(%s) class(%s)"   key  atomName  (repr  retval   )(className  retval   ) ) )

(println (%   "fragment class(%s) value(%s)"   (className  fragment   )(repr  fragment   ) ) )

(println (%   "%s"   (describeVariables  fragment   ) ) )

(return retval   )
)
)
)

(defClass MetaResidue  MetaSingletonObject  () ;END1
(method __init__  (self   ) ;END1
(callAncestorMethod self   )
)

(method setNetCharge  (self  charge   ) ;END1
(raise "depreciated"   )

(setq (slot   self   _NetCharge   )charge   )
)

(method atomWithName  (self  atomName   ) ;END1
(let (z   ) ;END1
(raise (LispError  "depreciated"   ) )

(setq z  (MetaResidueAtom  self  atomName   ) )

(return z   )
)
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  res  residue   ) ;END1
(setq key  (key   self   "residue"   ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(setq res  (getVariable   fragment   key   ) )

(ifFalse (isOfClass   res   Residue   ) ;END1
(println (%   "The resolved MetaResidue should have class Residue but it has class: %s"   (className  res   ) ) )

(println (%   "The fragment class is %s"   (className  fragment   ) ) )

(raise (LispError  "Problem"   ) )
)

(return res   )
)

(setq residue  (getConstitutionResidue   fragment  ) )

(setq res  (putVariable   fragment   key  residue   ) )

(assert (isOfClass   res   Residue   )(%   "The resolved MetaResidue should have class Residue but it has class: %s"   (className  res   ) ) )

(return res   )
)
)
)

(defClass MetaRecognizedAtom  MetaObject  (_UniqueName  _Atom  _Recognizer  _Tag   ) ;END1
(method __init__  (self  uniqueName  atom  recognizer  tag   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _UniqueName   )uniqueName   )

(assert (notNil   atom  )"atom is nil"   )

(assert (isOfClass   atom   MetaResidueAtomWithName   )(%   "atom must be MetaResidueAtomWithName but its: %s"   (className  atom   ) ) )

(setq (slot   self   _Atom   )atom   )

(assert (isOfClass   recognizer   MetaFrameRecognizer   )(%   "recognizer must be of type MetaFrameRecognizer, instead it is: %s "   (className   recognizer  ) ) )

(setq (slot   self   _Recognizer   )recognizer   )

(setq (slot   self   _Tag   )tag   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  atom  frameRecognizer  match  matchAtomName   ) ;END1
(setq key  (key   self   (slot   self   _UniqueName   ) ) )

(log (%   "MetaRecognizedAtom key=%s"   key   ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(log (%   "MetaRecognizedAtom key=%s"   key   ) )

(println (%   "%s [self slot _Atom] = %s"   (repr  (sourceFileLine   ) )(repr  (slot   self   _Atom   ) ) ) )

(setq atom  (resolve   (slot   self   _Atom   ) metaDatabase  fragment   ) )

(setq frameRecognizer  (resolve   (slot   self   _Recognizer   ) metaDatabase  fragment   ) )

(log (%   "MetaRecognizedAtom key=%s"   key   ) )

(if (recognizes   frameRecognizer   atom   ) ;END1
(then ;END1
(log (%   "MetaRecognizedAtom key=%s"   key   ) )

(setq match  (getMatch   frameRecognizer  ) )

(setq matchAtomName  (getName   (tag   match   (slot   self   _Tag   ) )) )

(log (%   "MetaRecognizedAtom key=%s"   key   ) )
)

(else ;END1
(raise (LispError  (%   "Could not find match for atom(%s) recognizer(%s) tag(%s)"   (descriptionResolved   (slot   self   _Atom   ) metaDatabase  fragment   )(getRecognizerName   (slot   self   _Recognizer   ))(slot   self   _Tag   ) ) ) )
)
)

(log (%   "About to write to variable key=%s"   key   ) )

(return (putVariable   fragment   key  matchAtomName   ) )
)
)
)

(defClass MetaRecognizedFrame  MetaObject  (_Name  _MetaOrigin  _Recognizer  _source   ) ;END1
(method __init__  (self  &key  name  origin  recognizer  source   ) ;END1
(callAncestorMethod self   )

(assert (notNil   origin  )"origin must be defined"   )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _MetaOrigin   )origin   )

(setq (slot   self   _Recognizer   )recognizer   )

(setq (slot   self   _source   )source   )
)

(method getName  (self   ) ;END1
(return (slot   self   _Name   ) )
)

(method getRecognizer  (self   ) ;END1
(return (slot   self   _Recognizer   ) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  origin  z   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq origin  (resolve   (slot   self   _MetaOrigin   ) metaDatabase  fragment   ) )

(assert (notNil   origin  )(%   "MetaRecognizedFrame origin atom is undefined define at: %s"   (repr  (slot   self   _source   ) ) ) )

(setq z  (RecognizedFrame  :name  (slot   self   _Name   ):origin  origin  :recognizerName  (getRecognizerName   (slot   self   _Recognizer   )) ) )

(return (putVariable   fragment   key  z   ) )
)
)
)

(defClass MetaExplicitFrame  MetaObject  (_Name  _MetaOrigin  _xAtom  _xyAtom   ) ;END1
(method __init__  (self  &key  name  origin  xAtom  xyAtom   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _MetaOrigin   )origin   )

(setq (slot   self   _xAtom   )xAtom   )

(setq (slot   self   _xyAtom   )xyAtom   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  origin  xAtom  xyAtom  z   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq origin  (resolve   (slot   self   _MetaOrigin   ) metaDatabase  fragment   ) )

(setq xAtom  (resolve   (slot   self   _xAtom   ) metaDatabase  fragment   ) )

(setq xyAtom  (resolve   (slot   self   _xyAtom   ) metaDatabase  fragment   ) )

(setq z  (ExplicitFrame  :name  (slot   self   _Name   ):origin  origin  :xAtom  xAtom  :xyAtom  xyAtom   ) )

(return (putVariable   fragment   key  z   ) )
)
)
)

(defClass MetaOriginFrame  MetaObject  (_Name  _MetaOrigin   ) ;END1
(method __init__  (self  &key  name  origin   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _MetaOrigin   )origin   )
)

(method getName  (self   ) ;END1
(return (slot   self   _Name   ) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  origin  z   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq origin  (resolve   (slot   self   _MetaOrigin   ) metaDatabase  fragment   ) )

(setq z  (OriginFrame  :name  (slot   self   _Name   ):origin  origin   ) )

(return (putVariable   fragment   key  z   ) )
)
)
)

(defClass MetaTwoAtomFrame  MetaObject  (_Name  _MetaTwoAtom  _xAtom   ) ;END1
(method __init__  (self  &key  name  origin  xAtom   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _MetaTwoAtom   )origin   )

(setq (slot   self   _xAtom   )xAtom   )
)

(method getName  (self   ) ;END1
(return (slot   self   _Name   ) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  origin  xAtom  z   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq origin  (resolve   (slot   self   _MetaTwoAtom   ) metaDatabase  fragment   ) )

(setq xAtom  (resolve   (slot   self   _xAtom   ) metaDatabase  fragment   ) )

(setq z  (TwoAtomFrame  :name  (slot   self   _Name   ):origin  origin  :xAtom  xAtom   ) )

(return (putVariable   fragment   key  z   ) )
)
)
)

(defClass MetaThreeAtomFrame  MetaObject  (_Name  _MetaTwoAtom  _xAtom  _xyAtom   ) ;END1
(method __init__  (self  &key  name  origin  xAtom  xyAtom   ) ;END1
(assert (notNil   origin  )"origin must be defined"   )

(assert (notNil   xAtom  )"xAtom must be defined"   )

(assert (notNil   xyAtom  )"xyAtom must be defined"   )

(callAncestorMethod self   )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _MetaTwoAtom   )origin   )

(setq (slot   self   _xAtom   )xAtom   )

(setq (slot   self   _xyAtom   )xyAtom   )
)

(method getName  (self   ) ;END1
(return (slot   self   _Name   ) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  origin  xAtom  xyAtom  z   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq origin  (resolve   (slot   self   _MetaTwoAtom   ) metaDatabase  fragment   ) )

(setq xAtom  (resolve   (slot   self   _xAtom   ) metaDatabase  fragment   ) )

(setq xyAtom  (resolve   (slot   self   _xyAtom   ) metaDatabase  fragment   ) )

(setq z  (ExplicitFrame  :name  (slot   self   _Name   ):origin  origin  :xAtom  xAtom  :xyAtom  xyAtom   ) )

(return (putVariable   fragment   key  z   ) )
)
)
)

(defClass MetaStereoInformation  MetaObject  (_ConstitutionName  _ChiralCenters  _MonomerName  _PdbName  _Restraints   ) ;END1
(method __init__  (self  constitutionName  chiralCenters  monomerName  pdbName  restraints   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _ConstitutionName   )constitutionName   )

(setq (slot   self   _ChiralCenters   )chiralCenters   )

(setq (slot   self   _MonomerName   )monomerName   )

(setq (slot   self   _PdbName   )pdbName   )

(setq (slot   self   _Restraints   )restraints   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  chiralCenters  pdbName  restraints  stereoisomers  value  monomerName   ) ;END1
(setq monomerName  (resolve   (slot   self   _MonomerName   ) metaDatabase  fragment   ) )

(setq key  (key   self   monomerName   ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq chiralCenters  (resolve   (slot   self   _ChiralCenters   ) metaDatabase  fragment   ) )

(setq pdbName  (resolve   (slot   self   _PdbName   ) metaDatabase  fragment   ) )

(setq restraints  (resolve   (slot   self   _Restraints   ) metaDatabase  fragment   ) )

(setq stereoisomers  (generateStereoisomers  chiralCenters  monomerName  pdbName   ) )

(setq value  (StereoInformation  :stereoisomers  stereoisomers  :restraints  restraints   ) )

(return (putVariable   fragment   key  value   ) )
)
)

(method getConstitutionName  (self  metaDatabase  fragment   ) ;END1
(return (resolve   (slot   self   _ConstitutionName   ) metaDatabase  fragment   ) )
)

(method getMonomerNamesByStereoCenter  (self  fragment   ) ;END1
(raise "monomerTemplate below isn't defined - what should this method do?????"   )

(setq monomerTemplate  (resolve   (slot   self   _MonomerName   ) metaDatabase  fragment   ) )
)
)

(defun MetaStereoInformation_standard  () ;END1
(let (name  chiralCenters  monomerName  pdbName  restraints  z   ) ;END1
(setq name  (MetaProperty  :name   ) )

(setq chiralCenters  (MetaPropertyOrDefault  :chiralCenters  () ) )

(setq monomerName  (MetaProperty  :monomerName   ) )

(setq pdbName  (MetaProperty  :pdbName   ) )

(setq restraints  (MetaPropertyOrDefault  :restraints  () ) )

(setq z  (MetaStereoInformation  name  chiralCenters  monomerName  pdbName  restraints   ) )

(return z   )
)
)

(defClass MetaAnchorOrigin  MetaObject  (_frame   ) ;END1
(method __init__  (self  frame   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _frame   )frame   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  frame  value   ) ;END1
(setq key  (key   self   "origin"   ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq frame  (resolve   (slot   self   _frame   ) metaDatabase  fragment   ) )

(setq value  (AnchorOrigin  :originFrame  frame   ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaAnchorOnOtherSideOfPlug  MetaObject  (_PlugName   ) ;END1
(method __init__  (self  &key  plugName   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _PlugName   )plugName   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  value   ) ;END1
(setq key  (key   self   (slot   self   _PlugName   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq value  (AnchorOnOtherSideOfPlug  :plugName  (slot   self   _PlugName   ) ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaPlug  MetaObject  () ;END1
(method __init__  (self   ) ;END1
(callAncestorMethod self   )
)

(method isRingClosingPlug  (self   ) ;END1
(return false   )
)
)

(defClass MetaMatedPlug  MetaPlug  (_Mates   ) ;END1
(method __init__  (self   ) ;END1
(callAncestorMethod self   )
)

(method hasMates  (self   ) ;END1
(return true   )
)

(method getMates  (self   ) ;END1
(return (slot   self   _Mates   ) )
)

(method getAllMates  (self   ) ;END1
(let (result  oneResult   ) ;END1
(setq result  (StringSet   ) )

(foreach mate  (slot   self   _Mates   ) ;END1
(setq oneResult  (getUnExpandedNames   mate  ) )

(insertStringSet   result   oneResult   )
)

(return (asCons   result  ) )
)
)

(method otherSidePlugName  (self   ) ;END1
(return (DirectionalCoupling_otherSidePlugName  (slot   self   _Name   ) ) )
)
)

(defClass MetaUnMatedPlug  MetaPlug  () ;END1
(method __init__  (self   ) ;END1
(callAncestorMethod self   )
)

(method hasMates  (self   ) ;END1
(return false   )
)

(method getMates  (self   ) ;END1
(return () )
)
)

(defClass MetaFrameRecognizer  MetaSingletonObject  (_recognizerName   ) ;END1
(method __init__  (self  recognizerName   ) ;END1
(setq (slot   self   _recognizerName   )recognizerName   )
)

(method getRecognizerName  (self   ) ;END1
(return (slot   self   _recognizerName   ) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  value   ) ;END1
(setq key  (key   self   (slot   self   _recognizerName   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq value  (getFrameRecognizer   metaDatabase   (slot   self   _recognizerName   ) ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaRingClosingPlug  MetaMatedPlug  (_Name  _otherSidePlugName  _Bond0  _Bond1  _ExportFrame  _FrameFinishers  _metaConstitution  _ringClosingMates   ) ;END1
(method __init__  (self  &key  name  bond0  bond1  otherSidePlugName  mates  exportFrame  (needsFrameFinishers  ()needsFrameFinishersSensor   )ringClosingMates   ) ;END1
(let (pluggedFrameFinishers  rcms   ) ;END1
(assert (notNil   otherSidePlugName  )"Must provide otherSidePlugName"   )

(assert (notNil   exportFrame  )"Must provide exportFrame"   )

(assert (!=   needsFrameFinishersSensor   false   )"needsFrameFinishers must be provided"   )

(callAncestorMethod self   )

(map  (lambda  (f   )(assert  (isOfClass   f   MetaFrameFinisher   )"Every needsFrameFinishers: element must be a MetaFrameFinisher"   ) )needsFrameFinishers   )

(setq pluggedFrameFinishers  (map  (lambda  (fc   )(setPlugName   fc   name   ) )needsFrameFinishers   ) )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _Bond0   )bond0   )

(setq (slot   self   _Bond1   )bond1   )

(setq (slot   self   _Mates   )mates   )

(setq (slot   self   _ExportFrame   )exportFrame   )

(setq (slot   self   _FrameFinishers   )pluggedFrameFinishers   )

(setq (slot   self   _metaConstitution   )() )

(setq (slot   self   _otherSidePlugName   )otherSidePlugName   )

(setq rcms  ringClosingMates   )

(when (isNil   rcms  ) ;END1
(println  "Creating one default RingClosingMate"    )

(setq rcms  (list  (RingClosingMate   ) ) )
)

(setq (slot   self   _ringClosingMates   )rcms   )
)
)

(method isRingClosingPlug  (self   ) ;END1
(return true   )
)

(method getOtherSidePlugName  (self   ) ;END1
(return (slot   self   _otherSidePlugName   ) )
)

(method isOutPlug  (self   ) ;END1
(return true   )
)

(method exportsMetaFrame  (self   ) ;END1
(return true   )
)

(method getRingClosingMates  (self   ) ;END1
(return (slot   self   _ringClosingMates   ) )
)

(method getExportMetaFrame  (self   ) ;END1
(return (slot   self   _ExportFrame   ) )
)

(method setMetaConstitution  (self  top   ) ;END1
(setq (slot   self   _metaConstitution   )top   )
)

(method getMetaExportFrame  (self   ) ;END1
(return (slot   self   _ExportFrame   ) )
)

(method completeFrameFinisherAliasesAsStringSet  (self   ) ;END1
(let (names  nameSet   ) ;END1
(setq names  () )

(foreach cf  (slot   self   _FrameFinishers   ) ;END1
(setq names  (Cons  (generateAlias   cf  )names   ) )
)

(setq nameSet  (StringSet  :entries  names   ) )

(return nameSet   )
)
)

(method description  (self   ) ;END1
(let (out  names   ) ;END1
(setq out  (StringStream   ) )

(writeln   out   (%   "MetaConstitution/MetaRingClosingPlug(%s/%s) "   (getGroup   (slot   self   _metaConstitution   ))(slot   self   _Name   ) ) )

(setq names  (completeFrameFinisherAliasesAsStringSet   self  ) )

(writeln   out   (%   "     frameFinisherAliases(%s)"   (asString   names  ) ) )

(return (str   out  ) )
)
)

(method matches  (self  other   ) ;END1
(let (selfNames  otherNames  res   ) ;END1
(setq selfNames  (completeFrameFinisherAliasesAsStringSet   self  ) )

(setq otherNames  (completeFrameFinisherAliasesAsStringSet   other  ) )

(println (%   "RingClosing %s comparing (%s) == (%s) "   (repr  (sourceFileLine   ) )(asString   selfNames  )(asString   otherNames  ) ) )

(println (%   "selfNames isOfClass StringSet = %d"   (isOfClass   selfNames   StringSet   ) ) )

(println (%   "otherNames isOfClass StringSet = %d"   (isOfClass   otherNames   StringSet   ) ) )

(setq res  (equal   selfNames   otherNames   ) )

(println (%   "      match == %d"   res   ) )

(return res   )
)
)

(method numberOfFrameFinishers  (self   ) ;END1
(return (length   (slot   self   _FrameFinishers   )) )
)

(method otherSidePlugName  (self   ) ;END1
(return (DirectionalCoupling_otherSidePlugName  (slot   self   _Name   ) ) )
)

(method getName  (self   ) ;END1
(return (slot   self   _Name   ) )
)

(method getMetaFrameFinishers  (self   ) ;END1
(return (slot   self   _FrameFinishers   ) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  name  bond0  bond1  mates  ringClosingMates  exportFrame  frameFinishers  value   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq name  (slot   self   _Name   ) )

(setq bond0  (resolve   (slot   self   _Bond0   ) metaDatabase  fragment   ) )

(setq bond1  ""   )

(ifTrue (notNil   (slot   self   _Bond1   )) ;END1
(setq bond1  (resolve   (slot   self   _Bond1   ) metaDatabase  fragment   ) )
)

(setq mates  (slot   self   _Mates   ) )

(setq ringClosingMates  (slot   self   _ringClosingMates   ) )

(setq exportFrame  (resolve   (slot   self   _ExportFrame   ) metaDatabase  fragment   ) )

(setq frameFinishers  (map  (lambda  (mf   )(resolve   mf   metaDatabase  fragment   ) )(slot   self   _FrameFinishers   ) ) )

(println  "FrameFinishers..."    )

(println (%   "%s frameFinishers = %s"   (repr  (sourceFileLine   ) )(repr  frameFinishers   ) ) )

(setq value  (RingClosingPlug  :name  name  :bond0  bond0  :bond1  bond1  :mates  mates  :exportFrame  exportFrame  :needsFrameFinishers  frameFinishers  :ringClosingMates  ringClosingMates   ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaOutPlug  MetaMatedPlug  (_Name  _Bond0  _Bond1  _ExportFrame  _FrameFinishers  _metaConstitution   ) ;END1
(method __init__  (self  &key  name  bond0  bond1  mates  exportFrame  (needsFrameFinishers  ()needsFrameFinishersSensor   ) ) ;END1
(callAncestorMethod self   )

(assert (notNil   mates  )"mates must be defined"   )

(assert (notNil   exportFrame  )"OutPlug must have exportFrame"   )

(assert (!=   needsFrameFinishersSensor   false   )(%   "You must provide needsFrameFinishers - its value is(%s)"   (repr  needsFrameFinishers   ) ) )

(ifFalse (or   (isOfClass   bond0   MetaRecognizedAtom   ) (isOfClass   bond0   MetaProperty   ) ) ;END1
(raise (%   "bond0 must resolve to an atom name its current class is: %s"   (className  bond0   ) ) )
)

(assert (not  (DirectionalCoupling_isInPlugName  name   ) )(%   "OutPlug name(%s) is invalid"   name   ) )

(map  (lambda  (f   )(assert  (isOfClass   f   MetaFrameFinisher   )"Every needsFrameFinishers: element must be a MetaFrameFinisher"   ) )needsFrameFinishers   )

(let (pluggedFrameFinishers   ) ;END1
(setq pluggedFrameFinishers  (map  (lambda  (fc   )(setPlugName   fc   name   ) )needsFrameFinishers   ) )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _Bond0   )bond0   )

(setq (slot   self   _Bond1   )bond1   )

(setq (slot   self   _Mates   )mates   )

(setq (slot   self   _ExportFrame   )exportFrame   )

(setq (slot   self   _FrameFinishers   )pluggedFrameFinishers   )

(setq (slot   self   _metaConstitution   )() )
)
)

(method exportsMetaFrame  (self   ) ;END1
(return true   )
)

(method getExportMetaFrame  (self   ) ;END1
(return (slot   self   _ExportFrame   ) )
)

(method isOutPlug  (self   ) ;END1
(return true   )
)

(method setMetaConstitution  (self  top   ) ;END1
(setq (slot   self   _metaConstitution   )top   )
)

(method getMetaExportFrame  (self   ) ;END1
(return (slot   self   _ExportFrame   ) )
)

(method completeFrameFinisherAliasesAsStringSet  (self   ) ;END1
(let (names  nameSet   ) ;END1
(setq names  () )

(foreach cf  (slot   self   _FrameFinishers   ) ;END1
(setq names  (Cons  (generateAlias   cf  )names   ) )
)

(setq nameSet  (StringSet  :entries  names   ) )

(return nameSet   )
)
)

(method description  (self   ) ;END1
(let (out  names   ) ;END1
(setq out  (StringStream   ) )

(writeln   out   (%   "MetaConstitution/MetaOutPlug(%s/%s) "   (getGroup   (slot   self   _metaConstitution   ))(slot   self   _Name   ) ) )

(setq names  (completeFrameFinisherAliasesAsStringSet   self  ) )

(writeln   out   (%   "     frameFinisherAliases(%s)"   (asString   names  ) ) )

(return (str   out  ) )
)
)

(method matches  (self  other   ) ;END1
(let (selfNames  otherNames  res   ) ;END1
(setq selfNames  (completeFrameFinisherAliasesAsStringSet   self  ) )

(setq otherNames  (completeFrameFinisherAliasesAsStringSet   other  ) )

(println (%   "%s comparing (%s) == (%s) "   (repr  (sourceFileLine   ) )(asString   selfNames  )(asString   otherNames  ) ) )

(println (%   "selfNames isOfClass StringSet = %d"   (isOfClass   selfNames   StringSet   ) ) )

(println (%   "otherNames isOfClass StringSet = %d"   (isOfClass   otherNames   StringSet   ) ) )

(setq res  (equal   selfNames   otherNames   ) )

(println (%   "      match == %d"   res   ) )

(return res   )
)
)

(method numberOfFrameFinishers  (self   ) ;END1
(return (length   (slot   self   _FrameFinishers   )) )
)

(method getName  (self   ) ;END1
(return (slot   self   _Name   ) )
)

(method getMetaFrameFinishers  (self   ) ;END1
(return (slot   self   _FrameFinishers   ) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  name  bond0  bond1  mates  exportFrame  frameFinishers  value   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq name  (slot   self   _Name   ) )

(setq bond0  (resolve   (slot   self   _Bond0   ) metaDatabase  fragment   ) )

(assert (isSubClassOf   (class  bond0   ) Text   )(%   "[self slot _Bond0] must resolve to a subclass of Text but its a %s"   (className  (slot   self   _Bond0   ) ) ) )

(setq bond1  ""   )

(ifTrue (notNil   (slot   self   _Bond1   )) ;END1
(setq bond1  (resolve   (slot   self   _Bond1   ) metaDatabase  fragment   ) )
)

(setq mates  (slot   self   _Mates   ) )

(setq exportFrame  (resolve   (slot   self   _ExportFrame   ) metaDatabase  fragment   ) )

(setq frameFinishers  (map  (lambda  (mf   )(resolve   mf   metaDatabase  fragment   ) )(slot   self   _FrameFinishers   ) ) )

(println  "FrameFinishers..."    )

(println (%   "%s frameFinishers = %s"   (repr  (sourceFileLine   ) )(repr  frameFinishers   ) ) )

(setq value  (OutPlug  :name  name  :bond0  bond0  :bond1  bond1  :mates  mates  :exportFrame  exportFrame  :needsFrameFinishers  frameFinishers   ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaInPlug  MetaMatedPlug  (_Name  _Bond0  _Bond1  _metaConstitution   ) ;END1
(method __init__  (self  &key  name  bond0  bond1  mates   ) ;END1
(assert (DirectionalCoupling_isInPlugName  name   )(%   "InPlug name(%s) is invalid"   name   ) )

(callAncestorMethod self   )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _Bond0   )bond0   )

(setq (slot   self   _Bond1   )bond1   )

(setq (slot   self   _Mates   )mates   )

(setq (slot   self   _metaConstitution   )() )
)

(method exportsMetaFrame  (self   ) ;END1
(return false   )
)

(method isOutPlug  (self   ) ;END1
(return false   )
)

(method setMetaConstitution  (self  top   ) ;END1
(setq (slot   self   _metaConstitution   )top   )
)

(method otherSidePlugName  (self   ) ;END1
(return (DirectionalCoupling_otherSidePlugName  (slot   self   _Name   ) ) )
)

(method getName  (self   ) ;END1
(return (slot   self   _Name   ) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  name  bond0  bond1  mates  value   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq name  (slot   self   _Name   ) )

(setq bond0  (resolve   (slot   self   _Bond0   ) metaDatabase  fragment   ) )

(setq bond1  ""   )

(ifTrue (notNil   (slot   self   _Bond1   )) ;END1
(setq bond1  (resolve   (slot   self   _Bond1   ) metaDatabase  fragment   ) )
)

(setq mates  (slot   self   _Mates   ) )

(setq value  (InPlug  :name  name  :bond0  bond0  :bond1  bond1  :mates  mates   ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaOriginPlug  MetaUnMatedPlug  (_Name  _OriginFrame  _metaConstitution   ) ;END1
(method __init__  (self  &key  name  originFrame   ) ;END1
(assert (DirectionalCoupling_isInPlugName  name   )(%   "OriginPlug name(%s) is invalid"   name   ) )

(assert (notNil   originFrame  )"MetaOriginPlug must have originFrame defined"   )

(callAncestorMethod self   )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _OriginFrame   )originFrame   )

(setq (slot   self   _metaConstitution   )() )
)

(method exportsMetaFrame  (self   ) ;END1
(return false   )
)

(method isOutPlug  (self   ) ;END1
(return false   )
)

(method getMetaOriginFrame  (self   ) ;END1
(return (slot   self   _OriginFrame   ) )
)

(method setMetaConstitution  (self  top   ) ;END1
(setq (slot   self   _metaConstitution   )top   )
)

(method getName  (self   ) ;END1
(return (slot   self   _Name   ) )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  name  originFrame  value   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq name  (slot   self   _Name   ) )

(setq originFrame  (resolve   (slot   self   _OriginFrame   ) metaDatabase  fragment   ) )

(setq value  (OriginPlug  :name  name  :originFrame  originFrame   ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaFrameFinisher  MetaObject  (_plugName  _metaFrame   ) ;END1
(method __init__  (self  frame   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _plugName   )() )

(setq (slot   self   _metaFrame   )frame   )
)

(method getPlugName  (self   ) ;END1
(return (slot   self   _plugName   ) )
)

(method getMetaFrame  (self   ) ;END1
(return (slot   self   _metaFrame   ) )
)

(method generateAlias  (self   ) ;END1
(let (metaRecognizer  recognizer   ) ;END1
(setq metaRecognizer  (getRecognizer   (slot   self   _metaFrame   )) )

(setq recognizer  (getFrameRecognizer   m:*DATABASE*   (getRecognizerName   metaRecognizer  ) ) )

(return (FrameFinisher_createAlias  (slot   self   _plugName   )recognizer   ) )
)
)

(method setPlugName  (self  plugName   ) ;END1
(setq (slot   self   _plugName   )plugName   )

(return self   )
)

(method createMetaExtractFrameFinisher  (self   ) ;END1
(let (alias  z   ) ;END1
(setq alias  (generateAlias   self  ) )

(assert (!=   alias   (slot   self   _plugName   ) )"alias cannot equal [self slot _plugNam]e"   )

(setq z  (MetaExtractFrameFinisher  :alias  alias  :plugName  (slot   self   _plugName   ) ) )

(return z   )
)
)

(method createMetaExtractIncompleteFrame  (self   ) ;END1
(raise "may be depreciated"   )

(setq z  (new  MetaExtractIncompleteFrame   ) )

(initMetaExtractIncompleteFrame   z  )

(return z   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  frame  value   ) ;END1
(setq key  (key   self   (+   "frameFinisher"   (getName   (slot   self   _metaFrame   )) ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(assert (notNil   (slot   self   _plugName   ))"[self slot _plugName] must be defined"   )

(setq frame  (resolve   (slot   self   _metaFrame   ) metaDatabase  fragment   ) )

(setq value  (FrameFinisher  :plugName  (slot   self   _plugName   ):frame  frame   ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaFragment  MetaObject  (_Name  _Atoms   ) ;END1
(method __init__  (self  &key  name  atoms   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _Atoms   )atoms   )
)

(method getName  (self   ) ;END1
(return (slot   self   _Name   ) )
)

(method resolveAtoms  (self  metaDatabase  fragment   ) ;END1
(let (atoms   ) ;END1
(setq atoms  () )

(foreach deferAtom  (slot   self   _Atoms   ) ;END1
(setq atoms  (Cons  (resolve   deferAtom   metaDatabase  fragment   )atoms   ) )
)

(return atoms   )
)
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  name  atoms  value   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq name  (slot   self   _Name   ) )

(setq atoms  (resolveAtoms   self   metaDatabase  fragment   ) )

(setq value  (Fragment  :name  name  :atoms  atoms   ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaRemainderFragment  MetaObject  (_Name  _Residue  _RemoveFragments   ) ;END1
(method __init__  (self  &key  residue  name  removeFragments   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _Residue   )residue   )

(println (%   "MetaRemainderFragment residue class=%s"   (className  residue   ) ) )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _RemoveFragments   )removeFragments   )
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  residue  residueAtoms  name  removeAtoms  residueAtoms  value   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq residue  (resolve   (slot   self   _Residue   ) metaDatabase  fragment   ) )

(assert (isOfClass   residue   Residue   )(%   "The (residue) variable is not a Residue - it is of class: %s  the _Residue object has class: %s"   (className  residue   )(className  (slot   self   _Residue   ) ) ) )

(setq residueAtoms  (getAtomNamesAsStringSet   residue  ) )

(setq name  (slot   self   _Name   ) )

(foreach frag  (slot   self   _RemoveFragments   ) ;END1
(setq removeAtoms  (StringSet  :entries  (resolveAtoms   frag   metaDatabase  fragment   ) ) )

(setq residueAtoms  (removeAll   residueAtoms   removeAtoms   ) )
)

(setq value  (Fragment  :name  name  :atoms  (asCons   residueAtoms  ) ) )

(return (putVariable   fragment   key  value   ) )
)
)
)

(defClass MetaExtractCoreFragment  MetaObject  (_Fragment   ) ;END1
(method __init__  (self  &key  fragment  (metaScaffold  ()metaScaffoldPassed   ) ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _Fragment   )fragment   )
)

(method getName  (self   ) ;END1
(return (getName   (slot   self   _Fragment   )) )
)

(method resolve  (self  metaDatabase  chemdrawFragment   ) ;END1
(let (key  fragment  myowner  myTopology  scaffold  value   ) ;END1
(setq key  (key   self   "core"   ) )

(ifTrue (hasVariable   chemdrawFragment   key   ) ;END1
(return (getVariable   chemdrawFragment   key   ) )
)

(setq fragment  (resolve   (slot   self   _Fragment   ) metaDatabase  chemdrawFragment   ) )

(log (%   "evaluating MetaExtractCoreFragment source = %s fragment description = %s "   (dump   self  )(description   fragment  ) ) )

(setq myowner  (owner   self  ) )

(assert (isAssignableTo   myowner   MetaTopology   )(%   "owner must be a MetaTopology - instead it has class(%s) value: %s"   (className  myowner   )(repr  myowner   ) ) )

(setq myTopology  myowner   )

(setq scaffold  (getMetaScaffold   myTopology   metaDatabase  chemdrawFragment   ) )

(setq value  (ExtractCoreFragment  :fragment  fragment  :scaffold  scaffold   ) )

(println (%   "!!!!! Creating ExtractCoreFragment for fragment(%s)"   (getProperty   chemdrawFragment   :name   ) ) )

(return (putVariable   chemdrawFragment   key  value   ) )
)
)
)

(defClass MetaExtractFragment  MetaExtractCoreFragment  (_Frame   ) ;END1
(method __init__  (self  &key  fragment  frame   ) ;END1
(assert (notNil   fragment  )"MetaExtractFragment fragment must be provided"   )

(assert (notNil   frame  )"MetaExtractFragment frame must be provided"   )

(callAncestorMethod self  :fragment  fragment   )

(setq (slot   self   _Frame   )frame   )
)

(method getMetaFrame  (self   ) ;END1
(return (slot   self   _Frame   ) )
)

(method resolve  (self  metaDatabase  chemdrawFragment   ) ;END1
(let (key  fragment  frame  value   ) ;END1
(setq key  (key   self   "ExtractFragment"   ) )

(ifTrue (hasVariable   chemdrawFragment   key   ) ;END1
(return (getVariable   chemdrawFragment   key   ) )
)

(setq fragment  (resolve   (slot   self   _Fragment   ) metaDatabase  chemdrawFragment   ) )

(assert (notNil   fragment  )"fragment is nil"   )

(setq frame  (resolve   (slot   self   _Frame   ) metaDatabase  chemdrawFragment   ) )

(log (%   "evaluating MetaExtractFragment source = %s"   (dump   self  ) ) )

(setq value  (ExtractFragment  :fragment  fragment  :frame  frame   ) )

(return (putVariable   chemdrawFragment   key  value   ) )
)
)
)

(defClass MetaTopology  MetaObject  (_Name  _MetaConstitution  _Residue  _alignTo  _Plugs  _CoreFragment  _Fragments  _ScaffoldSpec  _metaScaffold   ) ;END1
(method __init__  (self  &key  name  alignTo  residue  plugs  coreFragment  fragments   ) ;END1
(callAncestorMethod self   )

(setq (slot   self   _Name   )name   )

(setq (slot   self   _Residue   )residue   )

(setq (slot   self   _Plugs   )(filterOutNil   plugs  ) )

(setq (slot   self   _CoreFragment   )coreFragment   )

(setOwner   coreFragment   self   )

(setq (slot   self   _Fragments   )fragments   )

(setq (slot   self   _MetaConstitution   )() )

(setq (slot   self   _ScaffoldSpec   )() )

(setq (slot   self   _metaScaffold   )() )

(setq (slot   self   _alignTo   )alignTo   )
)

(method setMetaExtractScaffold  (self  s   ) ;END1
(setq (slot   self   _metaScaffold   )s   )
)

(method setScaffoldSpec  (self  ss   ) ;END1
(setq (slot   self   _ScaffoldSpec   )ss   )
)

(method getScaffoldSpec  (self   ) ;END1
(return (slot   self   _ScaffoldSpec   ) )
)

(method getMetaConstitution  (self   ) ;END1
(return (slot   self   _MetaConstitution   ) )
)

(method getMetaExtractFragments  (self   ) ;END1
(return (slot   self   _Fragments   ) )
)

(method getInPlug  (self   ) ;END1
(foreach p  (slot   self   _Plugs   ) ;END1
(ifTrue (isOfClass   p   MetaInPlug   ) ;END1
(return p   )
)

(ifTrue (isOfClass   p   MetaOriginPlug   ) ;END1
(return p   )
)
)

(return () )
)

(method getInPlugWithMates  (self   ) ;END1
(foreach p  (slot   self   _Plugs   ) ;END1
(ifTrue (isOfClass   p   MetaInPlug   ) ;END1
(return p   )
)
)

(return () )
)

(method getOutPlugs  (self   ) ;END1
(let (plugs   ) ;END1
(foreach p  (slot   self   _Plugs   ) ;END1
(ifTrue (isOutPlug   p  ) ;END1
(setq plugs  (Cons  p  plugs   ) )
)
)

(return plugs   )
)
)

(method setMetaConstitution  (self  mc   ) ;END1
(setq (slot   self   _MetaConstitution   )mc   )
)

(method getName  (self   ) ;END1
(return (slot   self   _Name   ) )
)

(method getMetaPlugs  (self   ) ;END1
(return (slot   self   _Plugs   ) )
)

(method getMetaPlugWithName  (self  plugName   ) ;END1
(foreach p  (slot   self   _Plugs   ) ;END1
(ifTrue (==   (getName   p  ) plugName   ) ;END1
(return p   )
)
)

(return () )
)

(method hasInPlugNamed  (self  name   ) ;END1
(let ((inPlug  (getInPlugWithMates   self  ) ) ) ;END1
(ifTrue (isNil   inPlug  ) ;END1
(return false   )
)

(return (==   (getName   inPlug  ) name   ) )
)
)

(method getMetaScaffold  (self  metaDatabase  fragment   ) ;END1
(let ((scaffold  (resolve   (slot   self   _metaScaffold   ) metaDatabase  fragment   ) ) ) ;END1
(return scaffold   )
)
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (key  name  residue  alignTo  scaffold  coreFragment  plugs  fragments  value   ) ;END1
(setq key  (key   self   (slot   self   _Name   ) ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(println  "MetaTopology resolve ---------"    )

(setq name  (slot   self   _Name   ) )

(println (%   " topology name = %s"   name   ) )

(setq residue  (resolve   (slot   self   _Residue   ) metaDatabase  fragment   ) )

(setq alignTo  (resolve   (slot   self   _alignTo   ) metaDatabase  fragment   ) )

(setq scaffold  (resolve   (slot   self   _metaScaffold   ) metaDatabase  fragment   ) )

(setq coreFragment  (resolve   (slot   self   _CoreFragment   ) metaDatabase  fragment   ) )

(setExtractScaffold   coreFragment   scaffold   )

(println (%   "MetaTopology resolve %s coreFragment = %s  "   (repr  (sourceFileLine   ) )(description   coreFragment  ) ) )

(setq plugs  (resolveOnlyPlugsWithMates   self   metaDatabase  fragment   ) )

(setq fragments  (map  (lambda  (element   )(resolve   element   metaDatabase  fragment   ) )(slot   self   _Fragments   ) ) )

(log (%   "%s fragments = %s "   (repr  (sourceFileLine   ) )(repr   fragments  ) ) )

(setq value  (Topology  :name  name  :residue  residue  :alignTo  alignTo  :plugs  plugs  :coreFragment  coreFragment  :fragments  fragments  :scaffold  scaffold   ) )

(return (putVariable   fragment   key  value   ) )
)
)

(method resolveOnlyPlugsWithMates  (self  metaDatabase  fragment   ) ;END1
(let (plugs   ) ;END1
(foreach p  (slot   self   _Plugs   ) ;END1
(ifTrue (isOfClass   p   MetaOriginPlug   ) ;END1
(continue )
)

(setq plugs  (Cons  (resolve   p   metaDatabase  fragment   )plugs   ) )
)

(return plugs   )
)
)
)

(defClass MetaConstitution  MetaObject  (_group  _name  _addToGroups  _residue  _residueNetCharge  _stereoInformation  _frames  _plugs  _fragments  _topologies   ) ;END1
(method __init__  (self  &key  group  name  addToGroups  residue  residueNetCharge  stereoInformation  frames  plugs  fragments  topologies   ) ;END1
(assert (notNil   residueNetCharge  )"You must provide residueNetCharge"   )

(callAncestorMethod self   )

(setq (slot   self   _group   )group   )

(setq (slot   self   _name   )name   )

(setq (slot   self   _addToGroups   )addToGroups   )

(setq (slot   self   _residue   )residue   )

(setq (slot   self   _residueNetCharge   )residueNetCharge   )

(setq (slot   self   _stereoInformation   )stereoInformation   )

(setq (slot   self   _frames   )(filterOutNil   frames  ) )

(setq (slot   self   _plugs   )(filterOutNil   plugs  ) )

(setq (slot   self   _fragments   )fragments   )

(setq (slot   self   _topologies   )topologies   )

(map (lambda  (f   )(setMetaConstitution   f   self   ) )(slot   self   _plugs   ) )

(map (lambda  (t   )(setMetaConstitution   t   self   ) )(slot   self   _topologies   ) )
)

(method getMetaOutPlugs  (self   ) ;END1
(let (plugs   ) ;END1
(foreach p  (slot   self   _plugs   ) ;END1
(ifTrue (isOutPlug   p  ) ;END1
(setq plugs  (Cons  p  plugs   ) )
)
)

(return plugs   )
)
)

(method hasTopologyWithPlugNamed  (self  plugName   ) ;END1
(foreach top  (slot   self   _topologies   ) ;END1
(ifTrue (notNil   (getMetaPlugWithName   top   plugName   )) ;END1
(return true   )
)
)

(return false   )
)

(method hasTopologyWithRingClosingPlugNamed  (self  plugName   ) ;END1
(let (plug   ) ;END1
(foreach top  (slot   self   _topologies   ) ;END1
(setq plug  (getMetaPlugWithName   top   plugName   ) )

(ifTrue (notNil   plug  ) ;END1
(ifTrue (isOfClass   plug   MetaRingClosingPlug   ) ;END1
(return true   )
)
)
)

(return false   )
)
)

(method getMetaPlugInATopology  (self  plugName   ) ;END1
(let (plug   ) ;END1
(foreach top  (slot   self   _topologies   ) ;END1
(setq plug  (getMetaPlugWithName   top  ) )

(ifTrue (notNil   plug  ) ;END1
(return plug   )
)
)

(return () )
)
)

(method getMetaFrameNamed  (self  fn   ) ;END1
(foreach frame  (slot   self   _frames   ) ;END1
(ifTrue (==   (getName   frame  ) fn   ) ;END1
(return frame   )
)
)

(raise (%   "Could not find frame named(%s)"   fn   ) )
)

(method getMetaPlugNamed  (self  plugName   ) ;END1
(foreach plug  (slot   self   _plugs   ) ;END1
(ifTrue (==   (getName   plug  ) plugName   ) ;END1
(return plug   )
)
)

(return () )
)

(method getMetaPlugs  (self   ) ;END1
(return (slot   self   _plugs   ) )
)

(method getMetaConstitutionName  (self   ) ;END1
(return (slot   self   _group   ) )
)

(method getGroup  (self   ) ;END1
(return (slot   self   _group   ) )
)

(method getGroupAdds  (self   ) ;END1
(return (slot   self   _addToGroups   ) )
)

(method getMetaTopologies  (self   ) ;END1
(return (slot   self   _topologies   ) )
)

(method getGroupName  (self   ) ;END1
(return (slot   self   _group   ) )
)

(method allTopologiesWithInPlugNamed  (self  name   ) ;END1
(let (tops   ) ;END1
(foreach top  (slot   self   _topologies   ) ;END1
(ifTrue (hasInPlugNamed   top   name   ) ;END1
(setq tops  (Cons  top  tops   ) )
)
)

(return tops   )
)
)

(method resolve  (self  metaDatabase  fragment   ) ;END1
(let (name  key  residue  stereoInformation  frames  plugs  fragments  topologies  value   ) ;END1
(setq name  (resolve   (slot   self   _name   ) metaDatabase  fragment   ) )

(setq key  (key   self   name   ) )

(ifTrue (hasVariable   fragment   key   ) ;END1
(return (getVariable   fragment   key   ) )
)

(setq residue  (resolve   (slot   self   _residue   ) metaDatabase  fragment   ) )

(setNetCharge   residue   (resolve   (slot   self   _residueNetCharge   ) metaDatabase  fragment   ) )

(setq stereoInformation  (resolve   (slot   self   _stereoInformation   ) metaDatabase  fragment   ) )

(validate   stereoInformation  )

(setq frames  (map  (lambda  (e   )(resolve   e   metaDatabase  fragment   ) )(slot   self   _frames   ) ) )

(setq plugs  (map  (lambda  (e   )(resolve   e   metaDatabase  fragment   ) )(slot   self   _plugs   ) ) )

(setq fragments  (map  (lambda  (e   )(resolve   e   metaDatabase  fragment   ) )(slot   self   _fragments   ) ) )

(setq topologies  (map  (lambda  (e   )(resolve   e   metaDatabase  fragment   ) )(slot   self   _topologies   ) ) )

(println (%   "stereoInformation--> %s"   (repr  stereoInformation   ) ) )

(setq value  (Constitution  :metaConstitution  (slot   self   _group   ):name  name  :residue  residue  :stereoInformation  stereoInformation  :frames  frames  :plugs  plugs  :fragments  fragments  :topologies  topologies   ) )

(return (putVariable   fragment   key  value   ) )
)
)
)
