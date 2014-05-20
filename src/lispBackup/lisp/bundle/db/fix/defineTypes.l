; fixed code
(global *defineTypes_gaff*  (loadArchive  (%   "%s/gaff.cxml"   (databaseDir   ) ) ) )

(defun checkParametersForResidue  (residue   ) ;END1
(let (mol  ef   ) ;END1
(println "checkparametersForResidue starting"   )

(setq mol  (Molecule   ) )

(add   mol   residue   )

(println "defining types"   )

(assignTypes   *defineTypes_gaff*   mol   )

(setq ef  (EnergyFunction   ) )

(println "defining energyFunction "   )

(defineForMatter   ef   mol  *defineTypes_gaff*   )

(println "done defining energyFunction "   )
)
)

(ifTrue (isTopLevelScript   ) ;END1
(let ((databaseFileName  "seed_full.cxml"   )(trainers  (cdr  l:*ARGS*   ) )trainer  confExplorer  mol  c21  ef   ) ;END1
(println (%   "trainers fileName: %s"   (repr  trainers   ) ) )

(foreach fileName  trainers   ;END1
(setq trainer  (readTrainer  fileName   ) )

(setq confExplorer  (getStructures   trainer  ) )

(setq mol  (getMatter   confExplorer  ) )

(saveMol2 mol  "[self slot _mo]l.mol2"   )

(println (%   "Assigning types for %s"   fileName   ) )

(assignTypes   *defineTypes_gaff*   mol   )

(saveArchive mol  "[self slot _mo]l.cxml"   )

(println "----------    Molecule    ------------"   )

(println (asXmlString   mol  ) )

(setq c21  (firstAtomWithName   mol   "C21"   ) )

(println (%   "c21 atom = %s"   (description  c21   ) ) )

(setq ef  (EnergyFunction   ) )

(defineForMatter   ef   mol  *defineTypes_gaff*   )

(println "---------- EnergyFunction ------------"   )

(println (asXmlString   ef  ) )
)
)
)
