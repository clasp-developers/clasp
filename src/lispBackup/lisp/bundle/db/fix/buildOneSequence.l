; fixed code
(global sequenceFileName  (@   l:*ARGS*   1   ) )

(standardDatabase  "full"   )

(let (oligomerSequence  builder   ) ;END1
(=   oligomerSequence   (require  sequenceFileName   ) )

(setOligomer  "olig"  oligomerSequence   )

(=   builder   (Builder   ) )

(addOligomer   builder   olig   )

(firstOligomer   builder  )

(firstSequence   builder  )

(firstAllConformations   builder  )

(buildAllAtoms   builder  )

(=   mol   (getMolecule   builder  ) )

(writeCoordinatesToAtoms   builder  )

(println   "Saving files"    )

(saveArchive  mol  "mol.cxml"   )

(saveMol2  mol  "mol.mol2"   )
)
