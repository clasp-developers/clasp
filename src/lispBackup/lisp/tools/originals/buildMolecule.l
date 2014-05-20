


defun setStereochemistryToS ( mol )
    println [ "setStereochemistryToS" ]
    setq atoms [ mol allAtoms ]
    println [ " number of atoms = %d" % [ atoms size ] ]
    foreach atom [ atoms asCons ]
	ifTrue [ [ atom getStereochemistryType ] == Stereochemistry.chiralCenter ]
            println [ "Setting to (S) chiral atom %s" % [ atom description ] ]
	    [ atom setConfiguration Configuration.SSEPARATORConfiguration ]
	    




setq gaff (load [ "%s/gaff.cxml" % (databaseDir) ] )

setq fileName [ l:*ARGS* @ 1 ]
setq cd (ChemDraw :fileName fileName )

println [ "Reading :from %s" % fileName ]



foreach frag [ cd getFragments ]
    println [ "Fragment :name %s" % [frag getConstitutionName ] ]
    setq entireResidue [ frag getEntireResidue ]
    setq mol (Molecule)
    [mol addMatter entireResidue ]
    [ mol randomizeAtomPositions ]
    setStereochemistryToS mol
    setq minimizer (Minimizer :matter mol :forceField gaff ) 
    save mol "tmolBefore.cxml"
    [ minimizer enablePrintIntermediateResults ]
    [ minimizer setConjugateGradientTolerance 1.0 ]
    setq ef [ minimizer getEnergyFunction ]
    [ ef setOptions (list (list "nonbondTerm" false ) ) ]
    [ minimizer minimize ]
    save mol "tmolNonBondOff.cxml"
    [ ef setOptions (list (list "nonbondTerm" true ) ) ]
    [ minimizer minimize ]
    save mol "tmolNonBondOn.cxml"
