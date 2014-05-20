; fixed code
(debugLogOff   )

(standardDatabase  "full"   )

(=   allHits   (loadArchive  (@   l:*ARGS*   1   ) ) )

(=   hitIndices   (cdr   (cdr   l:*ARGS*  )) )

(foreach  hi  (entries   hitIndices  )(progn  (=   hitIndex   (asInt   hi  ) )(=   hit   (getHit   allHits   hitIndex   ) )(println  (%   "------Hit index: %d"   hitIndex   ) )(println  (%   "    Got hit score = %lf"   (getScore   hit  ) ) )(=   data   (getData   hit  ) )(debugLogOn   )(foreach  e  (entries   data  )(progn  (println  (%   "Entry key: %s"   (key   e  ) ) )(if  (subClassOf   (value   e  ) Matter   )(progn  (=   fileName   (%   "_extract_%s_%03d.mol2"   (key   e  )hitIndex   ) )(println  (%   "    Saving molecule with name: %s to :file %s "   (key   e  )fileName   ) )(saveMol2  (value   e  )fileName   ) ) )(if  (isOfClass   (value   e  ) BuilderState   )(progn  (=   builderState   (value   e  ) )(applyToBuilder   builderState  )(=   bldr   (getBuilder   builderState  ) )(=   oligomer   (getCurrentOligomer   bldr  ) )(=   sequence   (sequenceAsString   oligomer  ) )(println  (%   "    Sequence: %s"   sequence   ) ) ) ) ) ) ) )

(println   "Done looking through contents"    )
