; fixed code
(require  "CANDO/db/amideBondTester.l"   )

(defClass StartMinimize  ()(_ForceField  _MolTrainer  _FileName  _Options  _Comment   ) )

(defMethod initTrainerAndForceField  StartMinimize  (self  trainer  forceField  options   ) ;END1
(=   (slot   self   _ForceField   ) forceField   )

(=   (slot   self   _MolTrainer   ) trainer   )

(=   (slot   self   _FileName   ) (getFileName   (getHeader   trainer  )) )

(=   (slot   self   _Comment   ) ""   )

(=   (slot   self   _Options   ) options   )

(return self   )
)

(defMethod run  StartMinimize  (self   ) ;END1
(let (stage  confLip  agg  amideTester  mol  minCount  goodConformation  min  ef  minOk  msg  beyondThreshold  newState  badAmides  newStateMessage  confLib  minimumEnergy   ) ;END1
(log (%   "Working with trainer: %s"   (slot   self   _FileName   ) ) )

(setq stage  (TrainerHistoryEntry   ) )

(start   stage   "startMinimize"   )

(=   confLib   (getStructures   (slot   self   _MolTrainer   )) )

(=   agg   (getMatter   confLib  ) )

(=   amideTester   (AmideBondTester  (getMatter   confLib  ) ) )

(log "Getting first molecule"   )

(=   mol   (firstMolecule   agg  ) )

(=   minCount   0   )

(=   goodConformation   false   )

(foreach minCount  (Range  0  10   ) ;END1
(println (%   "    try [%d]"   minCount   ) )

(randomizeAtomPositions   mol  )

(log "About to assign types"   )

(setq min  (Minimizer  :matter  agg  :forceField  (slot   self   _ForceField   ) ) )

(setq ef  (getEnergyFunction   min  ) )

(setOptions   ef   (list  :nonbondTerm  false   ) )

(minimize   min  )

(setOptions   ef   (list  :nonbondTerm  true   ) )

(minimize   min  )

(setOptions   ef   (list  :nonbondTerm  false   ) )

(minimize   min  )

(setOptions   ef   (list  :nonbondTerm  true   ) )

(log "Enabling nonbond for the last time"   )

(useDefaultSettings   min  )

(log "Starting minimize"   )

(minimize   min  )

(log "Done minimize"   )

(setq minimumEnergy  (getTotalEnergy   ef  ) )

(setq minOk  true   )

(setq msg  "ok"   )

(log "Checking for minimizer error"   )

(if (==   (status   min  ) MinimizerStatus.minimizerError   ) ;END1
(then ;END1
(println "Minimization failed"   )

(setq minOk  false   )

(setq msg  (statusMessage   min  ) )
)

(else ;END1
(println "Minimization succeeded"   )

(clearEntries   confLib  )

(setMatter   confLib   agg   )
)
)

(=   newState   "?"   )

(log "Checking for beyond threshold interactions"   )

(ifTrue  minOk    ;END1
(setq beyondThreshold  (checkForBeyondThresholdInteractions   ef  ) )

(setq goodConformation  true   )

(ifTrue (>   beyondThreshold   0   ) ;END1
(println (%   "    There are %d beyond threshold interactions"   beyondThreshold   ) )

(setComment   stage   (getMessage   ef  ) )

(println  "    Summary: "    )

(println (%   "%s"   (getMessage   ef  ) ) )

(setq goodConformation  false   )

(setq minOk  false   )
)

(setq badAmides  (countBadAmidesInMatter   amideTester   agg   ) )

(ifTrue (>   badAmides   0   ) ;END1
(println (%   "    There are %d bad amides"   badAmides   ) )

(println (%   "    %s"   (getMessages   amideTester  ) ) )

(setComment   stage   (%   "There are %d bad amide bonds [cis acyclic amides + trans cyclic amides]"   badAmides   ) )

(setq goodConformation  false   )

(setq minOk  false   )
)

(ifTrue  minOk    ;END1
(break )
)
)

(setq minCount  (+   minCount   1   ) )
)

(if  goodConformation    ;END1
(then ;END1
(log (%   "GOOD: Found minimum energy conformation with good geometry after %d tries"   minCount   ) )

(setComment   stage   "Minimized_ok"   )

(=   newState   "min"   )

(=   newStateMessage   "Minimized_ok"   )
)

(else ;END1
(log (%   "BAD: Failed to find minimum energy conformation with good geometry after %d tries"   minCount   ) )

(=   newState   "bad"   )

(=   newStateMessage   (%   "Minimization failed after %d tries"   minCount   ) )
)
)

(setStateAndMessage   (getHeader   (slot   self   _MolTrainer   )) newState  newStateMessage   )

(log  "Writing state"    )

(finish   stage   newState   )

(addToHistory   (getHistory   (getHeader   (slot   self   _MolTrainer   ))) stage   )

(write   (slot   self   _MolTrainer   ))
)
)
