

(defun buildABunchOfMoleculesAndScoreThem (scorer &key (number 10 ) (outputFileName "_alchemistBunch.cxml" ))
  (assert (isAssignableTo scorer Scorer )"You must provide a Scorer to this function" )
  (let ( (mols (Table)))
    (appendField mols :molecule Molecule )
    (appendField mols :alchemistState AlchemistState )
    (appendField mols :scorerState ScorerState )
    (appendField mols :score Real )
    (let ( (idx 0)
	   scorerState mol alchemistState molCopy
	   (alchemist (getAlchemist scorer)))
      (foreach r (Range 0 number )
	       (print (% "Building molecule %d" r ))
	       (perturbOligomer generator )
	       (perturbSequence generator )
	       (randomizeConformation generator )
	       (setq scorerState (createScorerState generator ))
	       (evaluateScorer generator scorerState )
	       (setq mol (getBuiltMoleculeWithCoordinates generator ))	
					;	[ generator buildAllUntransformedAtomPositions ]
					;	[ generator calculateAllScoreTransformedAtomPositions ]
					;	[ generator writeCoordinatesToBuiltMolecule ]
	       (setq alchemistState (AlchemistState ))
	       (saveAlchemistState generator alchemistState )
	       (setq molCopy (copy mol ))
	       (appendWrite mols (list :molecule molCopy :alchemistState alchemistState :scorerState  scorerState :score (getScore scorerState )))
	       (setq idx (+ idx 1 )))
      (saveArchive mols outputFileName )
      mols ))
