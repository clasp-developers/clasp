					; fixed code
(defun buildABunchOfMolecules (alchemist &key (number 10 ) (outputFileName "_alchemistBunch.cxml"))
  (assert (isAssignableTo alchemist Alchemist ) "You must provide a Alchemist to this function" )
  (let ((mols (Table)) mol (idx 0))
    (appendField mols :molecule Molecule )
    (setq idx 0 )
    (foreach r (Range 0 number )
	     (perturbOligomer alchemist )
	     (perturbSequence alchemist )
	     (randomizeConformation alchemist )
	     (setq mol (getBuiltMolecule alchemist ))
	     (buildAllUntransformedAtomPositions alchemist )
	     (writeCoordinatesToBuiltMolecule alchemist )
	     (appendWrite mols (list :molecule (copy mol )))
	     (setq idx (+ idx 1 )))
    (saveArchive mols outputFileName )
    mols))




(when (isTopLevelScript)
  (let* ((alchemistFileName (@ *ARGS* 1))
	 (databaseFileName (@ *ARGS* 2))
	 (alchemist (loadArchive alchemistFileName)))
    (print (% "Building molecules from alchemist: %s" alchemistFileName ))
    (database databaseFileName)
    (buildABunchOfMolecules alchemist :number 100 )))
