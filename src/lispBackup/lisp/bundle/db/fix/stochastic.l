; fixed code
(require "CANDO/db/geometryChecker.l"   )

(global *SaveDurationInSeconds*  (*   4   60   ) )

(global *StochasticSearch.StartStageName*  "cando_gaff"   )

(global *StochasticSearch.StartPackage*  "cando"   )

(global *StochasticSearch.StartModel*  "gaff"   )

(defClass StochasticSearch  ()(_ForceField  _MolTrainer  _FileName  _StartStageName  _StartPackage  _StartModel  _TwisterDriver  _Options  _KeepEverything   ) ;END1
(method __init__  (self  forceField  trainer  &key  (options  (list   ) ) ) ;END1
(=   (slot   self   _ForceField   ) forceField   )

(=   (slot   self   _MolTrainer   ) trainer   )

(=   (slot   self   _FileName   ) (getFileName   (getHeader   trainer  )) )

(=   (slot   self   _TwisterDriver   ) () )

(=   (slot   self   _Options   ) options   )

(=   (slot   self   _KeepEverything   ) false   )

(setupRotatableTorsions self   )

(return self   )
)

(method setKeepEverything  (self  b   ) ;END1
(setq (slot   self   _KeepEverything   )b   )
)

(method defineTwistableAtoms  (self  agg  focusResidue  focusMonomer   ) ;END1
(let (atomsAdded  boundFrame  originAtom  xAtom  xyAtom   ) ;END1
(foreach a  (atoms  agg   ) ;END1
(setTempInt   a   0   )
)

(=   atomsAdded   0   )

(foreach a  (atoms  focusResidue   ) ;END1
(ifTrue (!=   (getElement   a  ) :H   ) ;END1
(setTempInt   a   1   )

(=   atomsAdded   (+   atomsAdded   1   ) )
)
)

(ifTrue (==   atomsAdded   0   ) ;END1
(foreach a  (atoms  focusResidue   ) ;END1
(setTempInt   a   1   )

(=   atomsAdded   (+   atomsAdded   1   ) )
)
)

(println (%   "Atoms with tempInt set to 1 = %d"   atomsAdded   ) )

(log (%   "Atoms with tempInt set to 1 = %d"   atomsAdded   ) )

(ifTrue (hasInCoupling   focusMonomer  ) ;END1
(setq boundFrame  (getAtomBoundFrameForInCoupling   focusMonomer  ) )

(println (%   "boundFrame = %s"   (repr  boundFrame   ) ) )

(setq originAtom  (get_oAtom   boundFrame  ) )

(setq xAtom  (get_pAtomOrNil   boundFrame  ) )

(setq xyAtom  (get_qAtomOrNil   boundFrame  ) )

(setTempInt   originAtom   1   )

(ifTrue (notNil   xAtom  ) ;END1
(setTempInt   xAtom   1   )
)

(ifTrue (notNil   xyAtom  ) ;END1
(setTempInt   xyAtom   1   )
)

(println (%   "boundFrame atoms (%s,%s,%s)"   (description   originAtom  )(description   xAtom  )(description   xyAtom  ) ) )
)

(prognDebug ;END1
(=   atomsAdded   0   )

(foreach a  (atoms  agg   ) ;END1
(ifTrue (==   (getTempInt   a  ) 1   ) ;END1
(=   atomsAdded   (+   atomsAdded   1   ) )
)
)
)

(prognDebug ;END1
(=   atomsAdded   0   )

(foreach a  (atoms  agg   ) ;END1
(ifTrue (==   (getTempInt   a  ) 1   ) ;END1
(=   atomsAdded   (+   atomsAdded   1   ) )
)
)

(println (%   "Number of selected atoms = %d"   atomsAdded   ) )

(log (%   "Number of selected atoms = %d"   atomsAdded   ) )
)
)
)

(method bondsToBreakAndTwist  (self  agg   ) ;END1
(let (bondsToBreak  bondsToTwist  a1  a2  order  bond   ) ;END1
(=   bondsToBreak   () )

(=   bondsToTwist   () )

(foreach bondLoop  (bonds  agg   ) ;END1
(=   a1   (getAtom1   bondLoop  ) )

(=   a2   (getAtom2   bondLoop  ) )

(=   order   (getBondOrder   bondLoop  ) )

(if (not  (or   (==   (getBackSpan   a1  ) a2   ) (==   (getBackSpan   a2  ) a1   ) ) ) ;END1
(then ;END1
(prognLog "Break bond not in spanning tree"   ;END1
(println  (%   "Found bond that is not in spanning tree %s-%s"   (description   a1  )(description   a2  ) ) )

(=   bond   (list  a1  a2  order   ) )

(=   bondsToBreak   (cons  bond  bondsToBreak   ) )

(removeBondTo   a1   a2   )
)
)

(else ;END1
(prognLog "Twist bond if both atoms have tempInt == 1 "   ;END1
(ifTrue (and   (==   (getTempInt   a1  ) 1   ) (==   (getTempInt   a2  ) 1   ) ) ;END1
(ifTrue (and   (>   (numberOfBonds   a1  ) 1   ) (>   (numberOfBonds   a2  ) 1   ) ) ;END1
(ifTrue (and   (==   (getTempInt   a1  ) 1   ) (==   (getTempInt   a2  ) 1   ) ) ;END1
(prognLog "Twisting around bond"   ;END1
(=   bondsToTwist   (cons  (list  a1  a2  order   )bondsToTwist   ) )
)
)
)
)
)
)
)
)

(return (list  bondsToBreak  bondsToTwist   ) )
)
)

(method setupRotatableTorsions  (self   ) ;END1
(let (header  body  structures  agg  focusMonomer  focusResidue  firstAtom  span  atomCount  a  tempBondsToBreakAndTwist  bondsToBreak  bondsToTwist   ) ;END1
(log "StochasticSearch.setupRotatableTorsions"   )

(=   header   (getHeader   (slot   self   _MolTrainer   )) )

(=   body   (slot   self   _MolTrainer   ) )

(=   structures   (getStructures   body  ) )

(=   agg   (getMatter   structures  ) )

(=   focusMonomer   (getData   body   "FocusMonomer"   ) )

(=   focusResidue   (getData   body   "FocusResidue"   ) )

(=   firstAtom   (firstAtom   focusResidue  ) )

(=   span   (SpanningLoop   ) )

(setTop   span   firstAtom   )

(=   atomCount   0   )

(while (advanceLoopAndProcess   span  ) ;END1
(=   a   (getAtom   span  ) )

(=   atomCount   (+   atomCount   1   ) )
)

(log (%   "Number of atoms = %d"   atomCount   ) )

(defineTwistableAtoms   self   agg  focusResidue  focusMonomer   )

(clearSuperposeAtoms   structures  )

(log "Adding superpose atoms"   )

(foreach a  (atoms  agg   ) ;END1
(log (%   "Looking at atom: %s"   (description   a  ) ) )

(if (==   (getTempInt   a  ) 1   ) ;END1
(then ;END1
(addSuperposeAtom structures  a   )

(log (%   "Added atom: %s"   (description   a  ) ) )
)

(else ;END1
(log (%   "NOT adding atom: %s"   (description   a  ) ) )
)
)
)

(log "Done adding superpose atoms"   )

(log "loop through bonds"   )

(=   tempBondsToBreakAndTwist   (bondsToBreakAndTwist   self   agg   ) )

(=   bondsToBreak   (car  tempBondsToBreakAndTwist   ) )

(=   bondsToTwist   (cadr  tempBondsToBreakAndTwist   ) )

(log "creating twister driver"   )

(=   (slot   self   _TwisterDriver   ) (setupTwisterDriver   self   bondsToTwist   ) )

(log "Reforming bonds that we broke"   )

(reformBrokenBonds   self   bondsToBreak   )
)
)

(method setupTwisterDriver  (self  bondsToTwist   ) ;END1
(let (twisterDriver  twistA1  twistA2  order  a1Atoms  span  a2Atoms  twistAtoms  twistFixed  twistMovable  twister   ) ;END1
(setq twisterDriver  (TwisterDriver   ) )

(foreach bond  bondsToTwist   ;END1
(=   twistA1   (car  bond   ) )

(=   twistA2   (cadr  bond   ) )

(=   order   (caddr  bond   ) )

(log  "Breaking bond between the atoms to twist"   )

(removeBondTo   twistA1   twistA2   )

(=   a1Atoms   () )

(=   span   (SpanningLoop   ) )

(setTop   span   twistA1   )

(while (advance   span  ) ;END1
(=   a1Atoms   (cons  (getAtom   span  )a1Atoms   ) )
)

(=   a2Atoms   () )

(=   span   (SpanningLoop   ) )

(setTop   span   twistA2   )

(while (advance   span  ) ;END1
(=   a2Atoms   (cons  (getAtom   span  )a1Atoms   ) )
)

(if (<   (length   a1Atoms  ) (length   a2Atoms  ) ) ;END1
(then ;END1
(=   twistAtoms   a1Atoms   )

(=   twistFixed   twistA2   )

(=   twistMovable   twistA1   )
)

(else ;END1
(=   twistAtoms   a2Atoms   )

(=   twistFixed   twistA1   )

(=   twistMovable   twistA2   )
)
)

(bondTo   twistA1   twistA2  order   )

(=   twister   (Twister   ) )

(setFixedAndMovable   twister   twistFixed  twistMovable   )

(foreach a  twistAtoms   ;END1
(addAtom   twister   a   )
)

(addTwister   twisterDriver   twister   )
)

(return twisterDriver   )
)
)

(method reformBrokenBonds  (self  bondsBroken   ) ;END1
(let (a1  a2  order   ) ;END1
(foreach b  bondsBroken   ;END1
(=   a1   (car  b   ) )

(=   a2   (cadr  b   ) )

(=   order   (caddr  b   ) )

(log (%   "Reforming bond %s - %s"   (description   a1  )(description   a2  ) ) )

(bondTo   a1   a2  order   )
)
)
)

(method createInitializeAndAddEntry  (self  structures  minimizer  matter   ) ;END1
(let (entry  entryStage   ) ;END1
(setq entry  (createEntry   structures  ) )

(=   entryStage   (createEntryStage   entry  ) )

(setStageName   entryStage   (slot   self   _StartStageName   ) )

(setExternalInterfaceName   entryStage   (slot   self   _StartPackage   ) )

(setModel   entryStage   (slot   self   _StartModel   ) )

(setEnergyKCal   entryStage   (getTotalEnergy   (getEnergyFunction   minimizer  )) )

(setSelectedStage   entry   entryStage   )

(extractCoordinatesFromMatter   entryStage   matter   )

(setComplete   entryStage   true   )

(appendEntryStage   entry   entryStage   )

(appendEntry   structures   entry   )
)
)

(defun alignAggregate  (trainer  agg   ) ;END1
(let (focusMonomer  focusConstitution  focusTopology  alignTo  dbg  coordSys  transform   ) ;END1
(setq focusMonomer  (getData   trainer   "FocusMonomer"   ) )

(setq focusConstitution  (getConstitution   focusMonomer  ) )

(setq focusTopology  (getTopology   focusMonomer  ) )

(assert (notNil   focusTopology  )"focusTopology is nil - it should never be nil"   )

(setq alignTo  (getAlignTo   focusTopology  ) )

(when (isNil   alignTo  ) ;END1
(println (%   "ERROR: FocusMonomer(%s) "   (getName   focusMonomer  ) ) )

(println (%   "ERROR: FocusTopology(%s) "   (getName   focusTopology  ) ) )

(raise  "alignTo is nil - it should never be nil"    )
)

(setq dbg  (debugAsString   alignTo   focusMonomer   ) )

(setq coordSys  (getCoordinateSystem   alignTo   focusMonomer   ) )

(setq transform  (matrixToCanonical   coordSys  ) )

(applyTransformToAtoms   agg   transform   )
)
)

(method runOneStochasticSearch  (self  trainer  structures  energyFunction  minimizer  agg  fails  restartFails  successes  geometryChecker  problems   ) ;END1
(let (startIndex  entry  entryStage  match   ) ;END1
(ifTrue (>   (numberOfEntries   structures  ) 0   ) ;END1
(setq startIndex  (int  (*   (randomNumber01   ) (numberOfEntries   structures  ) ) ) )

(=   entry   (getEntry   structures   startIndex   ) )

(=   entryStage   (getEntryStage   entry   (slot   self   _StartStageName   ) ) )

(writeCoordinatesToMatter   entryStage   agg   )
)

(log "Perturbing conformation"   )

(perturbConformation   (slot   self   _TwisterDriver   ))

(restart   minimizer  )

(minimize   minimizer  )

(alignAggregate trainer  agg   )

(ifTrue (==   (status   minimizer  ) MinimizerStatus.minimizerError   ) ;END1
(writeln   problems   (%   "STOCHASTIC-FAIL;%3d: !!! minimizer failed"   fails   ) )

(return false   )
)

(ifFalse (hasGoodGeometry   geometryChecker   energyFunction  agg  problems  fails   ) ;END1
(return false   )
)

(setq match  (ConformationExplorerMatch   ) )

(findClosestMatchingConformation   structures   match  agg  (slot   self   _StartStageName   )0.1   )

(ifTrue (and   (getMatches   match  ) (not  (slot   self   _KeepEverything   ) ) ) ;END1
(writeln   problems   (%   "STOCHASTIC-FAIL;%3d: !!! The conformation has already been found at :index %d"   fails  (getClosestMatchIndex   match  ) ) )

(return false   )
)

(createInitializeAndAddEntry   self   structures  minimizer  agg   )

(return true   )
)
)

(method run  (self   ) ;END1
(let (structures  jobHead  geometryChecker  agg  minimizer  energyFunction  fails  maxRestartFails  successes  maxMinimizations  restartFails  startTime  timeOfLastSave  problems  res  successes  currentTime  duration  secondsLeft  cursec   ) ;END1
(setq structures  (getStructures   (slot   self   _MolTrainer   )) )

(setq jobHead  (getHeader   (slot   self   _MolTrainer   )) )

(setq geometryChecker  (GeometryChecker  (getMatter   structures  ) ) )

(setq agg  (getMatter   structures  ) )

(setq minimizer  (Minimizer  :matter  agg  :forceField  (slot   self   _ForceField   ) ) )

(=   energyFunction   (getEnergyFunction   minimizer  ) )

(setName   energyFunction   (slot   self   _FileName   ) )

(disablePrintIntermediateResults   minimizer  )

(prognDebug ;END1
(enablePrintIntermediateResults   minimizer  )
)

(=   fails   0   )

(setq maxRestartFails  (getMaxConformationalSearchFailsRequired   jobHead  ) )

(=   maxMinimizations   (lookupDefault   (slot   self   _Options   ) :maxMinimizations  10000   ) )

(=   restartFails   0   )

(println (%   "NumberOfTwisters = %d"   (numberOfTwisters   (slot   self   _TwisterDriver   )) ) )

(=   successes   (numberOfEntries   structures  ) )

(if (==   successes   0   ) ;END1
(then ;END1
(println "At start of stochastic search there are NO entries"   )
)

(else ;END1
(println (%   "At start of stochastic search there are already[%d] entries"   successes   ) )
)
)

(ifTrue (==   (numberOfTwisters   (slot   self   _TwisterDriver   )) 0   ) ;END1
(println "There are no twisters, running only 2 minimizations"   )

(=   maxMinimizations   2   )
)

(ifTrue (slot   self   _KeepEverything   ) ;END1
(println "Running in KeepEverything mode, only 50 structures will be generated"   )

(setq maxMinimizations  50   )
)

(setq startTime  (PosixTime   ) )

(setToLocalTime   startTime  )

(setq timeOfLastSave  startTime   )

(foreach i  (Range  0  maxMinimizations   ) ;END1
(setq problems  (StringStream   ) )

(setq res  (runOneStochasticSearch   self   (slot   self   _MolTrainer   )structures  energyFunction  minimizer  agg  fails  restartFails  successes  geometryChecker  problems   ) )

(if (==   res   true   ) ;END1
(then ;END1
(=   successes   (+   successes   1   ) )

(=   restartFails   0   )
)

(else ;END1
(println (%   "%s"   (str   problems  ) ) )

(=   fails   (+   fails   1   ) )

(=   restartFails   (+   restartFails   1   ) )
)
)

(updateMostConformationalSearchFailsSeen   jobHead   restartFails   )

(setq currentTime  (PosixTime   ) )

(setToLocalTime   currentTime  )

(setq duration  (sub   currentTime   timeOfLastSave   ) )

(setq secondsLeft  (-   *SaveDurationInSeconds*   (totalSeconds   duration  ) ) )

(println (%   "SEARCH: SaveTimer[%4ld] Fails[%4d] Restarts[%d/%d] Hits[%4d] :job %s"   secondsLeft  fails  restartFails  maxRestartFails  successes  (slot   self   _FileName   ) ) )

(ifTrue (<   secondsLeft   0   ) ;END1
(println  "+++++++++"    )

(println  "+++++++++ Saving intermediate results of stochastic search"    )

(println  "+++++++++ Restarting Timer"    )

(println  "+++++++++"    )

(setState   jobHead   "searching"   )

(setq cursec  (getSecondsSpentConformationalSearching   jobHead  ) )

(setSecondsSpentConformationalSearching   jobHead   (+   cursec   *SaveDurationInSeconds*   ) )

(write   (slot   self   _MolTrainer   ))

(setq timeOfLastSave  (PosixTime   ) )

(setToLocalTime   timeOfLastSave  )
)

(ifTrue (not  (conformationalSearchesRequired   jobHead  ) ) ;END1
(break )
)
)

(println (%   "End of search there are %d structures."   (numberOfEntries   structures  ) ) )

(ifTrue (==   (numberOfEntries   structures  ) 0   ) ;END1
(setState   jobHead   "searchFailed"   )

(write   (slot   self   _MolTrainer   ))

(return )
)

(log "Writing result"   )

(setq cursec  (getSecondsSpentConformationalSearching   jobHead  ) )

(setSecondsSpentConformationalSearching   jobHead   (+   cursec   (totalSeconds   duration  ) ) )

(setState   jobHead   "searched"   )

(write   (slot   self   _MolTrainer   ))
)
)

(method summary  (self   ) ;END1
(let (structures  numStructures   ) ;END1
(=   structures   (getStructures   (slot   self   _MolTrainer   )) )

(=   numStructures   (numberOfEntries   structures  ) )

(ifTrue (<   numStructures   1   ) ;END1
(return "Stochastic search error, there are no structures"   )
)

(return (%   "Stochastic Search OK - %d structures"   numStructures   ) )
)
)
)
