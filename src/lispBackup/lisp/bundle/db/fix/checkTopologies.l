; fixed code
(defClass MetaOutPlugChecker  ()(_OutPlugs   ) ;END1
(method __init__  (self   ) ;END1
(setq (slot   self   _OutPlugs   )() )
)

(method hasMetaOutPlug  (self  metaOutPlug   ) ;END1
(foreach mop  (slot   self   _OutPlugs   ) ;END1
(ifTrue (==   mop   metaOutPlug   ) ;END1
(return true   )
)
)

(return false   )
)

(method addMetaOutPlug  (self  metaOutPlug   ) ;END1
(ifFalse (hasMetaOutPlug   self   metaOutPlug   ) ;END1
(setq (slot   self   _OutPlugs   )(Cons  metaOutPlug  (slot   self   _OutPlugs   ) ) )
)
)

(method getMetaOutPlugWithMostFrameFinishers  (self   ) ;END1
(let (masterMetaOutPlug  maxCompleteFrames   ) ;END1
(setq masterMetaOutPlug  () )

(setq maxCompleteFrames  0   )

(foreach mop  (slot   self   _OutPlugs   ) ;END1
(ifTrue (<   maxCompleteFrames   (numberOfFrameFinishers   mop  ) ) ;END1
(setq maxCompleteFrames  (numberOfFrameFinishers   mop  ) )

(setq masterMetaOutPlug  mop   )
)
)

(return masterMetaOutPlug   )
)
)

(method allMetaOutPlugsAreConsistent  (self  fullReport   ) ;END1
(let (masterMetaOutPlug  report  allConsistent   ) ;END1
(println "------starting allMetaOutPlugsAreConsistent-----"   )

(setq masterMetaOutPlug  (getMetaOutPlugWithMostFrameFinishers   self  ) )

(println (%   "Master MetaOutPlug = %s"   (description   masterMetaOutPlug  ) ) )

(setq report  (StringStream   ) )

(setq allConsistent  true   )

(ifTrue (isNil   masterMetaOutPlug  ) ;END1
(return true   )
)

(foreach mop  (slot   self   _OutPlugs   ) ;END1
(println (%   "  comparing to MetaOutPlug = %s"   (description   masterMetaOutPlug  ) ) )

(if (not  (matches   mop   masterMetaOutPlug   ) ) ;END1
(then ;END1
(println  "       it does NOT match"    )

(setq allConsistent  false   )

(writeln   report   (%   "%s"   (description   mop  ) ) )
)

(else ;END1
(println  "       they do match"    )
)
)
)

(ifFalse allConsistent   ;END1
(writeln   fullReport    "!!!!!!!!!!!!!  Saw a problem, compiling report"    )

(writeln   fullReport    "Every OutPlug that has the same name must be identical"    )

(writeln   fullReport    "If they aren't then a Master is chosen that has the most comprehensive description"    )

(writeln   fullReport    "and the others have to be made the same or the Master has to"    )

(writeln   fullReport    "have its scope reduced."    )

(writeln   fullReport    "What follows is a list of Masters each followed by the Plugs that don't match it"    )

(writeln   fullReport   (%   "Master ---> %s"   (description   masterMetaOutPlug  ) ) )

(writeln   fullReport   (%   "%s"   (str   report  ) ) )
)

(println (%   "allConsistent = %d"   allConsistent   ) )

(return allConsistent   )
)
)
)

(defClass MetaOutPlugCollection  ()(_Checkers   ) ;END1
(method __init__  (self   ) ;END1
(setq (slot   self   _Checkers   )(ObjectDictionary   ) )
)

(method getCheckers  (self   ) ;END1
(return (slot   self   _Checkers   ) )
)

(method addMetaOutPlug  (self  one   ) ;END1
(let ((name  (getName   one  ) )checker   ) ;END1
(ifFalse (contains   (slot   self   _Checkers   ) name   ) ;END1
(setq checker  (MetaOutPlugChecker   ) )

(put   (slot   self   _Checkers   ) name  checker   )
)

(setq checker  (get   (slot   self   _Checkers   ) name   ) )

(addMetaOutPlug   checker   one   )
)
)
)
