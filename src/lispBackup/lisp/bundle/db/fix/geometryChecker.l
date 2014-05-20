; fixed code
(require "CANDO/db/amideBondTester.l"   )

(require "CANDO/db/stereochemistryTester.l"   )

(defClass GeometryChecker  ()(_AmideTester  _Matter   ) ;END1
(method __init__  (self  matter   ) ;END1
(setq (slot   self   _Matter   )matter   )

(setq (slot   self   _AmideTester   )(AmideBondTester  matter   ) )
)

(method hasGoodGeometry  (self  energyFunction  matter  problems  fails   ) ;END1
(let (beyondThreshold  badStereocenters  badAmides   ) ;END1
(setq beyondThreshold  (checkForBeyondThresholdInteractions   energyFunction  ) )

(when (>   beyondThreshold   0   ) ;END1
(writeln   problems   (%   "STOCHASTIC-FAIL;%3d: !!! There were %d beyond threshold interactions"   fails  beyondThreshold   ) )

(return false   )
)

(setq badStereocenters  (countBadStereocenters  matter  problems   ) )

(ifTrue (>   badStereocenters   0   ) ;END1
(writeln   problems   (%   "STOCHASTIC-FAIL;%3d: !!! There were %d bad stereocenters (centers with the wrong configuration) "   fails  badStereocenters   ) )

(return false   )
)

(=   badAmides   (countBadAmidesInMatter   (slot   self   _AmideTester   ) matter   ) )

(ifTrue (>   badAmides   0   ) ;END1
(writeln   problems   (%   "STOCHASTIC-FAIL;%3d: !!! There were %d bad amide bonds [cis acyclic amides + trans cyclic amides]"   fails  badAmides   ) )

(return false   )
)

(return true   )
)
)
)
