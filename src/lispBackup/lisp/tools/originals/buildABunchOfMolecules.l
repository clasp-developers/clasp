
defun buildABunchOfMolecules ( builder &key ( number 10 ) (outputFileName "_builderBunch.cxml" ) )
    assert [ builder isAssignableTo Builder ] "You must provide a Builder to this function"
    local mols (Table)
    [ mols appendField :molecule Molecule ]
    local idx 0
    foreach r (Range 0 number )
        [ builder perturbOligomer ]
	[ builder perturbSequence ]
	[ builder randomizeConformation ]
	local mol [ builder getBuiltMolecule ]
	[ builder buildAllUntransformedAtomPositions ]
	[ builder calculateAllScoreTransformedAtomPositions ]
	[ builder writeCoordinatesToBuiltMolecule ]
	[ mols appendWrite (list :molecule [ mol copy ] ) ]
	setq idx [ idx + 1 ]
    saveArchive mols outputFileName
    return mols


defun buildABunchOfMoleculesAndScoreThem ( builderScorer &key ( number 10 ) (outputFileName "_builderBunch.cxml" ) )
    assert [ builderScorer isAssignableTo BuilderScorer ] "You must provide a BuilderScorer to this function"
    local mols (Table)
    [ mols appendField :molecule Molecule ]
    [ mols appendField :builderState BuilderState ]
    [ mols appendField :scorerState ScorerState ]
    [ mols appendField :score Real ]
    local idx 0
    foreach r (Range 0 number )
        println [ "Building molecule %d" % r ]
        [ builderScorer perturbOligomer ]
	[ builderScorer perturbSequence ]
	[ builderScorer randomizeConformation ]
	local scorerState [  builderScorer createScorerState ]
	[ builderScorer evaluateScorer scorerState ]
	local mol [ builderScorer getBuiltMoleculeWithCoordinates ]
;	[ builderScorer buildAllUntransformedAtomPositions ]
;	[ builderScorer calculateAllScoreTransformedAtomPositions ]
;	[ builderScorer writeCoordinatesToBuiltMolecule ]
	local builderState (BuilderState)
	[ builderScorer saveBuilderState builderState ]
	local molCopy [ mol copy ]
	[ mols appendWrite (list :molecule molCopy 
				:builderState builderState
				:scorerState [ scorerState ]
				:score [ scorerState getScore ] ) ]
	setq idx [ idx + 1 ]
    saveArchive mols outputFileName
    return mols

