; fixed code
(global *verbose1*  true   )

(defClass TopologyBranch  ()(_Plug  _MateIndex  _Mates  _Overflow  _MonomerNameIndex  _RepresentedMonomerGroups   ) ;END1
(method __init__  (self  aPlug   ) ;END1
(=   (slot   self   _Plug   ) aPlug   )

(=   (slot   self   _MateIndex   ) () )

(=   (slot   self   _Mates   ) () )

(=   (slot   self   _Overflow   ) () )

(=   (slot   self   _MonomerNameIndex   ) () )

(=   (slot   self   _RepresentedMonomerGroups   ) () )

(reset   self  )
)

(method repr  (self   ) ;END1
(let (s   ) ;END1
(setq s  (%   "Branch: plug[%s]
"   (getName   (getPlug   (slot   self   _Plug   ))) ) )

(foreach n  (slot   self   _RepresentedMonomerGroups   ) ;END1
(setq s  (+   s   (%   "    Group representative: %s
"   (getRepresentative   n  ) ) ) )
)

(return s   )
)
)

(method getPlug  (self   ) ;END1
(return (slot   self   _Plug   ) )
)

(method reset  (self   ) ;END1
(=   (slot   self   _MateIndex   ) 0   )

(=   (slot   self   _Mates   ) (matesAsCons   (getPlug   (slot   self   _Plug   ))) )

(ifTrue (==   (length   (slot   self   _Mates   )) 0   ) ;END1
(raise (%   "There must be at least one mate for %s"   (description   (slot   self   _Plug   )) ) )
)

(initMonomerNamesAndRecognizers   self  )

(=   (slot   self   _Overflow   ) false   )
)

(method initMonomerNamesAndRecognizers  (self   ) ;END1
(let (mate  groups   ) ;END1
(=   mate   (@   (slot   self   _Mates   ) (slot   self   _MateIndex   ) ) )

(=   groups   (expandedRepresentativeList   mate  ) )

(=   (slot   self   _MonomerNameIndex   ) 0   )

(=   (slot   self   _RepresentedMonomerGroups   ) (asCons   groups  ) )

(log (%   "[self slot _RepresentedMonomerGroups] = %s"   (repr  (slot   self   _RepresentedMonomerGroups   ) ) ) )
)
)

(method increment  (self   ) ;END1
(ifTrue  (slot   self   _Overflow   )  ;END1
(return )
)

(=   (slot   self   _MonomerNameIndex   ) (+   (slot   self   _MonomerNameIndex   ) 1   ) )

(ifTrue (>=   (slot   self   _MonomerNameIndex   ) (length   (slot   self   _RepresentedMonomerGroups   )) ) ;END1
(=   (slot   self   _MateIndex   ) (+   (slot   self   _MateIndex   ) 1   ) )

(if (>=   (slot   self   _MateIndex   ) (length   (slot   self   _Mates   )) ) ;END1
(then ;END1
(=   (slot   self   _Overflow   ) true   )
)

(else ;END1
(initMonomerNamesAndRecognizers   self  )
)
)

(=   (slot   self   _MonomerNameIndex   ) 0   )
)
)

(method getBranchMonomerName  (self   ) ;END1
(let (index  grp  representative   ) ;END1
(ifTrue (>=   (slot   self   _MonomerNameIndex   ) (length   (slot   self   _RepresentedMonomerGroups   )) ) ;END1
(return () )
)

(setq index  (slot   self   _MonomerNameIndex   ) )

(log (%   "index = %d"   index   ) )

(setq grp  (@   (slot   self   _RepresentedMonomerGroups   ) index   ) )

(log (%   "group type = %s"   (repr  grp   ) ) )

(log (%   "group = %s"   (description   grp  ) ) )

(setq representative  (getRepresentative   grp  ) )

(log (%   "representative = %s"   representative   ) )

(return representative   )
)
)

(method getBranchMonomerGroup  (self   ) ;END1
(let (res   ) ;END1
(ifTrue (>=   (slot   self   _MonomerNameIndex   ) (length   (slot   self   _RepresentedMonomerGroups   )) ) ;END1
(raise "Ran out of representations"   )
)

(setq res  (@   (slot   self   _RepresentedMonomerGroups   ) (slot   self   _MonomerNameIndex   ) ) )

(return res   )
)
)

(method overflowed  (self   ) ;END1
(return (slot   self   _Overflow   ) )
)
)

(defClass OligomerIterator  ()(_Database  _Constitution  _Stereoisomer  _Topology  _Branches  _Overflow  _NameIndex   ) ;END1
(method __init__  (self  db  aConstitution  aStereoisomer  aTopology   ) ;END1
(let (br   ) ;END1
(log  "Initializing OligomerIterator"    )

(=   (slot   self   _Database   ) db   )

(=   (slot   self   _Constitution   ) aConstitution   )

(=   (slot   self   _Stereoisomer   ) aStereoisomer   )

(=   (slot   self   _Topology   ) aTopology   )

(log (%   "About to iterate over topology plugs there should be[%d]"   (numberOfPlugs   aTopology  ) ) )

(=   (slot   self   _Branches   ) (map  (lambda  (plug   )(progn  (=   br   (TopologyBranch  plug   ) )br   ) )(plugsWithMatesAsCons   aTopology  ) ) )

(log (%   "There are %d branches and they are: "   (length   (slot   self   _Branches   )) ) )

(prognDebug ;END1
(foreach zn  (slot   self   _Branches   ) ;END1
(log (%   "BRANCH  %s"   (repr   zn  ) ) )
)
)

(setq (slot   self   _Overflow   )false   )
)
)

(method completeBranchWithCoupling  (self  oligomer  neighborMon  existingPlugName  focusMon  capPrefix   ) ;END1
(let (constitution  topology  plug  capMonomerName  capMonomer  capName  capCoupling  capPlugName  capPrefixPrefix   ) ;END1
(printPushPrefix (%   "branch[%s-%s]|"   existingPlugName  (getName   neighborMon  ) ) )

(log (%   "completeBranchWithCoupling>> existingPlugName=%s"   existingPlugName   ) )

(log (%   "Complete branch for neighborMon: %s"   (description   neighborMon  ) ) )

(println  (%   "%s: completeBranchWithCoupling"   capPrefix   ) )

(=   constitution   (constitutionForNameOrPdb   (slot   self   _Database   ) (getName   neighborMon  ) ) )

(ifTrue (==   constitution   () ) ;END1
(return false   )
)

(=   topology   (simplestTopologyWithPlugNamed   constitution   existingPlugName   ) )

(ifTrue (==   topology   () ) ;END1
(println "ERROR"   )

(println (%   "%s: :ERROR Could not find topology in constitution[%s] with Plug named[%s] requested by[%s]"   capPrefix  (getName   constitution  )existingPlugName  (getName   focusMon  ) ) )

(println "Constitution:"   )

(print (asXmlString   constitution  ) )

(printPopPrefix )

(return false   )
)

(foreach reqplug  (plugsWithMatesAsCons   topology  ) ;END1
(setq plug  (getPlug   reqplug  ) )

(if (==   (getName   plug  ) existingPlugName   ) ;END1
(then ;END1
(println (%   "%s: Ignoring existing plug[%s]"   capPrefix  (getName   plug  ) ) )

(continue )
)

(else ;END1
(println (%   "%s: capping plug[%s]"   capPrefix  (getName   plug  ) ) )
)
)

(=   capMonomerName   (getCap   (@   (matesAsCons   plug  ) 0   )) )

(=   capMonomer   (MultiMonomer   ) )

(=   capName   (%   "%s%d"   capPrefix  (slot   self   _NameIndex   ) ) )

(println (%   "%s: :capName %s  :capMonomerName %s"   capPrefix  capName  capMonomerName   ) )

(setId   capMonomer   capName   )

(setq (slot   self   _NameIndex   )(+   (slot   self   _NameIndex   ) 1   ) )

(setGroupName   capMonomer   capMonomerName   )

(addMonomer   oligomer   capMonomer   )

(if (getIsIn   plug  ) ;END1
(then ;END1
(log  (%   "Prior to coupling neighborMon: %s"   (description   neighborMon  ) ) )

(=   capCoupling   (couple   oligomer   capMonomer  (getName   plug  )neighborMon   ) )

(=   capPlugName   (getInMonomerPlugName   capCoupling  ) )
)

(else ;END1
(=   capCoupling   (couple   oligomer   neighborMon  (getName   plug  )capMonomer   ) )

(=   capPlugName   (getOutMonomerPlugName   capCoupling  ) )
)
)

(=   capPrefixPrefix   (%   "Cap%s"   capPrefix   ) )

(assert (notNil   capMonomer  )"capMonomer is nil"   )

(completeBranchWithCoupling   self   oligomer  capMonomer  capPlugName  focusMon  capPrefixPrefix   )
)

(printPopPrefix )

(return true   )
)
)

(method buildSingleTrainingOligomer  (self   ) ;END1
(let (focusMonomerName  olig  focusMon  contextRecognized  innerPlug  plug  neighborMon  neighborName  dir  coupling  existingPlugName  branchMonomerGroup  monGroup  plugName  good  focusedOligomer  branchMonomerGroupName   ) ;END1
(=   (slot   self   _NameIndex   ) 1   )

(=   focusMonomerName   (getName   (slot   self   _Stereoisomer   )) )

(ifTrue *verbose1*   ;END1
(println (%   "buildSingleTrainingOligomer>> focusMonomerName[%s] topology[%s]"   focusMonomerName  (getName   (slot   self   _Topology   )) ) )
)

(=   olig   (Oligomer   ) )

(=   focusMon   (MultiMonomer  :name  focusMonomerName  :comment  "focus"   ) )

(setId   focusMon   "focus"   )

(=   contextRecognized   (MonomerContext   ) )

(setFocusFromMonomer   contextRecognized   focusMon   )

(addMonomer   olig   focusMon   )

(foreach b  (slot   self   _Branches   ) ;END1
(setq innerPlug  (getPlug   b  ) )

(setq plug  (getPlug   innerPlug  ) )

(log (%   "Attaching to focusMonomer through plug name=%s"   (getName   plug  ) ) )

(=   neighborMon   (MultiMonomer   ) )

(setId   neighborMon   (%   "neighbor%d"   (slot   self   _NameIndex   ) ) )

(=   (slot   self   _NameIndex   ) (+   (slot   self   _NameIndex   ) 1   ) )

(=   neighborName   (getBranchMonomerName   b  ) )

(log (%   "neighborName: %s"   (repr  neighborName   ) ) )

(ifTrue (==   neighborName   () ) ;END1
(return () )
)

(setGroupName   neighborMon   neighborName   )

(addMonomer   olig   neighborMon   )

(if  (getIsIn   plug  )  ;END1
(then ;END1
(=   dir   "IN "   )

(log  "Coupling -->focusMon"   )

(=   coupling   (couple   olig   neighborMon  (getName   plug  )focusMon   ) )

(=   existingPlugName   (getInMonomerPlugName   coupling  ) )
)

(else ;END1
(=   dir   "OUT"   )

(log "Coupling focusMon-->"   )

(=   coupling   (couple   olig   focusMon  (getName   plug  )neighborMon   ) )

(=   existingPlugName   (getOutMonomerPlugName   coupling  ) )
)
)

(=   branchMonomerGroup   (getBranchMonomerGroup   b  ) )

(log (%   "branchMonomerGroup=%s"   (repr  branchMonomerGroup   ) ) )

(=   branchMonomerGroupName   (getName   branchMonomerGroup  ) )

(log (%   "branchMonomerGroupName=%s"   branchMonomerGroupName   ) )

(setq monGroup  (MonomerGroup  :name  branchMonomerGroupName  :optional  (getIsRingClosing   plug  ) ) )

(addMonomerNameNoChecks   monGroup   branchMonomerGroupName   )

(=   plugName   (getName   plug  ) )

(log (%   "About to add neighbor: %s %s"   plugName  (description  monGroup   ) ) )

(addNeighbor   contextRecognized   plugName  monGroup   )

(assert (notNil   neighborMon  )"neighborMon is nil"   )

(setq good  (completeBranchWithCoupling   self   olig  neighborMon  existingPlugName  focusMon  "Cap"   ) )

(ifTrue (not  good   ) ;END1
(return () )
)
)

(log (%   "TrainingOligomer contextRecognized is: %s"   (getKey   contextRecognized  ) ) )

(setq focusedOligomer  (TrainingOligomer  olig  contextRecognized  focusMon  (slot   self   _Topology   ) ) )

(log (%   "pos1 buildSingleTrainingOligomer returns %s"   (repr  focusedOligomer   ) ) )

(return focusedOligomer   )
)
)

(method buildTrainingOligomer  (self   ) ;END1
(let (btoi  res   ) ;END1
(println "-----------------------------    Started buildTrainingOligomer"   )

(log "-----------------------------    Started buildTrainingOligomer"   )

(setq btoi  0   )

(while (not  (overflowed   self  ) ) ;END1
(println (%   "starting buildTrainingOligomer stage: %d"   btoi   ) )

(setq res  (buildSingleTrainingOligomer   self  ) )

(println (%   "pos2 buildTrainingOligomer stage: %d res = %s"   btoi  (repr  res   ) ) )

(log (%   "pos2 buildTrainingOligomer stage: %d res = %s"   btoi  (repr  res   ) ) )

(if (==   res   () ) ;END1
(then ;END1
(increment   self  )
)

(else ;END1
(return res   )
)
)

(println  "incrementing btoi"    )

(setq btoi  (+   btoi   1   ) )
)

(log "Dropping out the bottom"   )

(return () )
)
)

(method increment  (self   ) ;END1
(let (idx   ) ;END1
(ifTrue  (overflowed   self  )  ;END1
(return () )
)

(setq idx  0   )

(foreach br  (slot   self   _Branches   ) ;END1
(increment   br  )

(ifTrue (not  (overflowed   br  ) ) ;END1
(return () )
)

(setq idx  (+   idx   1   ) )

(reset   br  )
)

(setq (slot   self   _Overflow   )true   )
)
)

(method overflowed  (self   ) ;END1
(return (slot   self   _Overflow   ) )
)
)

(defClass TrainingOligomer  ()(_Oligomer  _FocusContext  _FocusMonomer  _Molecule  _FocusTopology  _ArchivedTrainerHeader   ) ;END1
(method __init__  (self  oligomer  context  monomer  topology   ) ;END1
(=   (slot   self   _Oligomer   ) oligomer   )

(setq (slot   self   _Molecule   )() )

(=   (slot   self   _FocusContext   ) context   )

(=   (slot   self   _FocusMonomer   ) monomer   )

(=   (slot   self   _FocusTopology   ) topology   )

(=   (slot   self   _ArchivedTrainerHeader   ) () )
)

(method setMolecule  (self  mol   ) ;END1
(setq (slot   self   _Molecule   )mol   )
)

(method getMolecule  (self   ) ;END1
(return (slot   self   _Molecule   ) )
)

(method setArchivedTrainerHeader  (self  header   ) ;END1
(=   (slot   self   _ArchivedTrainerHeader   ) header   )
)

(method getArchivedTrainerHeader  (self   ) ;END1
(return (slot   self   _ArchivedTrainerHeader   ) )
)

(method getOligomer  (self   ) ;END1
(return (slot   self   _Oligomer   ) )
)

(method getFocusContext  (self   ) ;END1
(return (slot   self   _FocusContext   ) )
)

(method getFocusMonomer  (self   ) ;END1
(return (slot   self   _FocusMonomer   ) )
)

(method getFocusTopology  (self   ) ;END1
(return (slot   self   _FocusTopology   ) )
)

(method getKey  (self   ) ;END1
(let (context   ) ;END1
(setq context  (getFocusContext   self  ) )

(return (getKey   context  ) )
)
)

(method checkExtraction  (self   ) ;END1
(let (agg  monomerCoordinates  problems  sawProblem   ) ;END1
(setq agg  (Aggregate   ) )

(addMatter   agg   (slot   self   _Molecule   ) )

(setq monomerCoordinates  (MonomerCoordinates  :context  (slot   self   _FocusContext   ) ) )

(log "About to call testExtraction"   )

(debugLogOn )

(setq problems  (testExtraction   monomerCoordinates   (slot   self   _FocusTopology   )agg  (slot   self   _FocusMonomer   ) ) )

(setq sawProblem  (lookup   problems   :sawProblems   ) )

(ifTrue sawProblem   ;END1
(setq problems  (lookup   problems   :problems   ) )

(println  "Problems were identified while running [monomerCoordinates testExtraction] - they are listed below"    )

(println (str   problems  ) )

(println  "Exiting from checkExtraction"    )

(exit 1   )
)
)
)

(method getMonomersYouNeed  (self   ) ;END1
(let (monomers  monomerNames  monomerSet   ) ;END1
(=   monomers   (monomersAsCons   (slot   self   _Oligomer   )) )

(=   monomerNames   (map  (lambda  (x   )(getName   x  ) )monomers   ) )

(=   monomerSet   (StringSet   ) )

(insertConsStrings   monomerSet   monomerNames   )

(return monomerSet   )
)
)
)

(defun generateAllTrainingOligomers  (db   ) ;END1
(let (structures  trainingOligomers  oi  oligIndex  res  olig  mol  structures  oligIndex   ) ;END1
(=   structures   1   )

(=   trainingOligomers   (ObjList   ) )

(log (%   "Iterating over constitutions: %s"   (repr  (constitutionsAsCons   db  ) ) ) )

(log (%   "Database = %s"   (repr  db   ) ) )

(foreach m  (constitutionsAsCons   db  ) ;END1
(prognLog (%   "Iterating over constitution[%s]"   (getName   m  ) ) ;END1
(foreach s  (stereoisomersAsCons   m  ) ;END1
(prognLog (%   "Iterating over stereoisomer[%s]"   (getName   s  ) ) ;END1
(foreach t  (topologiesAsCons   m  ) ;END1
(prognLog (%   "Iterating over Topology[%s]"   (getName   t  ) ) ;END1
(log "About to create new OligomerIterator"   )

(=   oi   (OligomerIterator  db  m  s  t   ) )

(=   oligIndex   0   )

(while (not  (overflowed   oi  ) ) ;END1
(prognLog (%   "Building oligomer %d"   oligIndex   ) ;END1
(setq res  (buildTrainingOligomer   oi  ) )

(log (%   "pos4 res = %s"   (repr  res   ) ) )

(if (!=   res   () ) ;END1
(then ;END1
(prognDebug ;END1
(println (%   "===== focus_Monomer[%s]  topology[%s] "   (getName   s  )(getName   t  ) ) )

(println (%   ":generateAllTrainingOligomers: %s "   (repr  (getKey   res  ) ) ) )

(log (%   ":generateAllTrainingOligomers:%s "   (repr  (getKey   res  ) ) ) )
)

(setq olig  (getOligomer   res  ) )

(setq mol  (getMolecule   olig  ) )

(setMolecule   res   mol   )

(checkExtraction   res  )

(append   trainingOligomers   res   )

(setq structures  (+   structures   1   ) )
)

(else ;END1
(log "No oligomer generated"   )
)
)

(increment   oi  )

(=   oligIndex   (+   oligIndex   1   ) )
)
)
)
)
)
)
)
)

(log (%   "Returning %d trainingOligomers"   (size   trainingOligomers  ) ) )

(return trainingOligomers   )
)
)

(when (isTopLevelScript   ) ;END1
(let (test   ) ;END1
(setq test  (TrainingOligomer   ) )

(println (%   "test_TrainingOligomer = %s"   (repr  test   ) ) )

(exit 1   )

(ifTrue (!=   (length   l:*ARGS*  ) 2   ) ;END1
(println "You must provide a seed database file name"   )

(exit 1   )
)

(=   seedFileName   (@   *ARGS*   1   ) )

(println (%   "Loading file: %s"   seedFileName   ) )

(database seedFileName   )

(=   trainingOligomers   (generateAllTrainingOligomers  m:*DATABASE*   ) )

(println (%   "Number of trainingOligomers = %d "   (size   trainingOligomers  ) ) )

(foreach to  (entries  trainingOligomers   ) ;END1
(println (%   "trainingOligomer = %s"   (repr  to   ) ) )

(println (%   "key %s "   (getKey   to  ) ) )

(println (%   "     focus context: %s"   (getKey   (getFocusContext   to  )) ) )
)
)
)
