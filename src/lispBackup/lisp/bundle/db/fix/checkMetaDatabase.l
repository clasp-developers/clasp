; fixed code
(defun allMetaConstitutionNamesForPlugMates  (plug   ) ;END1
(let ((mates  (getMates   plug  ) ) ) ;END1
(return mates   )
)
)

(defun mateMetaConstitutionHasTopologyWithPlugName  (metaDatabase  metaConstitutionName  mateMetaConstitutionName  matePlugName   ) ;END1
(let ((mateConstitution  (getMetaConstitution   metaDatabase   mateMetaConstitutionName   ) ) ) ;END1
(return (hasTopologyWithPlugNamed   mateConstitution   matePlugName   ) )
)
)

(defun aggregateMateNames  (allMates   ) ;END1
(let ((names  (StringSet   ) )monomerNames   ) ;END1
(foreach mate  allMates   ;END1
(setq monomerNames  (getUnExpandedNames   mate  ) )

(println (%   "aggregateMateNames mate monomer names: %s"   (repr  monomerNames   ) ) )

(insertStringSet   names   monomerNames   )
)

(return names   )
)
)

(defun mateMetaConstitutionHasPlugWithName  (metaDatabase  mateMetaConstitutionName  matePlugName   ) ;END1
(raise "implement me"   )

(setq constitution  (monomerConstitutionForName   m:*DATABASE*   mateMetaConstitutionName   ) )

(return (hasPlugNamed   constitution   matePlugName   ) )
)

(defun mateMetaConstitutionHasPlugWithMatesThatContainFocus  (metaDatabase  focusMetaConstitutionName  mateMetaConstitutionName  matePlugName   ) ;END1
(let (constitution  matePlug  allMateNames   ) ;END1
(setq constitution  (getMetaConstitution   metaDatabase   mateMetaConstitutionName   ) )

(setq matePlug  (getMetaPlugNamed   constitution   matePlugName   ) )

(setq allMateNames  (aggregateMateNames  (getMates   matePlug  ) ) )

(return (contains   allMateNames   focusMetaConstitutionName   ) )
)
)

(defun checkMates  (metaDatabase  focusMetaConstitution  focusTopology  errorStream  idx   ) ;END1
(let (metaConstitutionName  plugs  problem  allMateNames  otherSidePlugName  problem   ) ;END1
(setq metaConstitutionName  (getGroupName   focusMetaConstitution  ) )

(println (%   "Checking mates for constitution(%s)  trainer(%d)"   metaConstitutionName  idx   ) )

(setq plugs  (getMetaPlugs   focusTopology  ) )

(setq problem  false   )

(foreach plug  plugs   ;END1
(setq allMateNames  (aggregateMateNames  (getMates   plug  ) ) )

(println (%   "Mates for plug(%s) = |%s|"   (getName   plug  )(repr   allMateNames  ) ) )

(foreach mateMetaConstitutionName  (asCons   allMateNames  ) ;END1
(setq otherSidePlugName  (otherSidePlugName   plug  ) )

(println (%   "    otherSidePlugName(%s)"   otherSidePlugName   ) )

(println (%   "    mateMetaConstitutionName = %s"   (repr  mateMetaConstitutionName   ) ) )

(ifFalse (mateMetaConstitutionHasTopologyWithPlugName  metaDatabase  metaConstitutionName  mateMetaConstitutionName  otherSidePlugName   ) ;END1
(writeln   errorStream   (%   "MetaConstitution(%s) has plug(%s) but its mate constititution(%s) doesn't have a single Topology with plug(%s)"   metaConstitutionName  (getName   plug  )mateMetaConstitutionName  (otherSidePlugName   plug  ) ) )

(writeln   errorStream   (%   "    solutions 1: Add a topology to %s with plug(%s) with Mate(%s)"   mateMetaConstitutionName  (otherSidePlugName   plug  )metaConstitutionName   ) )

(writeln   errorStream   (%   "              2: From %s plug(%s) remove Mate %s"   metaConstitutionName  (getName   plug  )mateMetaConstitutionName   ) )

(setq problem  true   )

(continue )
)

(ifFalse (mateMetaConstitutionHasPlugWithMatesThatContainFocus  metaDatabase  metaConstitutionName  mateMetaConstitutionName  (otherSidePlugName   plug  ) ) ;END1
(writeln   errorStream   (%   "Group(%s) has plug(%s) but its mate constititution(%s) plug(%s) doesn't have a mate that matches us"   metaConstitutionName  (getName   plug  )mateMetaConstitutionName  (otherSidePlugName   plug  ) ) )

(writeln   errorStream   (%   "    solutions 1: To %s plug(%s) add the Mate %s"   mateMetaConstitutionName  (otherSidePlugName   plug  )metaConstitutionName   ) )

(writeln   errorStream   (%   "              2: Remove all topologies with plug(%s) from %s"   (getName   plug  )metaConstitutionName   ) )

(setq problem  true   )

(continue )
)
)
)

(ifTrue problem   ;END1
(println "   ---- Found a problem see below for list of problems"   )
)
)
)

(defun checkAllMates  (metaDatabase   ) ;END1
(let (errorStream  idx  m   ) ;END1
(setq errorStream  (StringStream   ) )

(setq idx  0   )

(foreach entry  (entries   (getMetaConstitutions   metaDatabase  )) ;END1
(setq m  (value   entry  ) )

(prognLog (%   "Iterating over MetaConstitution[%s]"   (getGroupName   m  ) ) ;END1
(foreach t  (getMetaTopologies   m  ) ;END1
(prognLog (%   "Iterating over MetaTopology[%s]"   (getName   t  ) ) )

(checkMates metaDatabase  m  t  errorStream  idx   )
)
)

(setq idx  (+   idx   1   ) )
)

(ifTrue (>   (size   (str   errorStream  )) 0   ) ;END1
(println (%   "%s"   (str   errorStream  ) ) )

(exit 1   )
)
)
)
