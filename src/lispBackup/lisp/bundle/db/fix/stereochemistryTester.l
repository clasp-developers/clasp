; fixed code
(defun configurationAsString  (conf   ) ;END1
(cond ;END1
(==   conf   Configuration.S_Configuration   )
(return "S"   )


(==   conf   Configuration.R_Configuration   )
(return "R"   )


(true ;END1
(return "undefined"   )
)
)
)

(defun countBadStereocenters  (aggregate  problems   ) ;END1
(let (badCenters  setConf  actualConf   ) ;END1
(setq badCenters  0   )

(foreach atom  (asCons   (allAtoms   aggregate  )) ;END1
(cond ;END1
(==   (getStereochemistryType   atom  ) Stereochemistry.chiralCenter   )
(setq setConf  (getConfiguration   atom  ) )

(setq actualConf  (calculateStereochemicalConfiguration   atom  ) )

(ifTrue (!=   setConf   actualConf   ) ;END1
(setq badCenters  (+   badCenters   1   ) )

(writeln   problems   (%   "Chiral %s has (%s) configuration but it should be %s"   (description   atom  )(configurationAsString  actualConf   )(configurationAsString  setConf   ) ) )
)


(==   (getStereochemistryType   atom  ) Stereochemistry.prochiralCenter   )
(setq setConf  Configuration.R_Configuration   )

(setq actualConf  (calculateStereochemicalConfiguration   atom  ) )

(ifTrue (!=   setConf   actualConf   ) ;END1
(setq badCenters  (+   badCenters   1   ) )

(writeln   problems   (%   "Prochiral %s has (%s) configuration but it should be %s"   (description   atom  )(configurationAsString  actualConf   )(configurationAsString  setConf   ) ) )
)


(true ;END1
(continue )
)
)
)

(return badCenters   )
)
)
