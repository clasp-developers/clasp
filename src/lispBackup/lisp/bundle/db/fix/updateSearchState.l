; fixed code
(require "CANDO/db/globalOptions.l"   )

(require "CANDO/db/organizer.l"   )

(require "CANDO/db/jobSetup.l"   )

(defun updateSearchStates  (unsortedjobs  maxRestarts   ) ;END1
(let (jobs  trainer  head  cur  esearch  numberOfConfs  state  searchSeconds  fails   ) ;END1
(setq jobs  (sorted   unsortedjobs  ) )

(foreach fileName  jobs   ;END1
(setq trainer  (readTrainer  fileName   ) )

(setq head  (getHeader   trainer  ) )

(setq cur  (getMaxConformationalSearchFailsRequired   head  ) )

(setq search  (getStructures   trainer  ) )

(setq numberOfConfs  (numberOfEntries   search  ) )

(setq state  (getState   head  ) )

(setq searchSeconds  (getSecondsSpentConformationalSearching   head  ) )

(setq fails  (getMostConformationalSearchFailsSeen   head  ) )

(cond ;END1
(<   cur   maxRestarts   )
(setMaxConformationalSearchFailsRequired   head   maxRestarts   )

(setState   head   "searching"   )

(write   trainer  )

(println (%   "Reset %s state[%10s->%10s] secondsSearching[%5d] searchFails[%3d/%3d->%3d] conformations[%d]"   fileName  state  "searching"  searchSeconds  fails  cur  maxRestarts  numberOfConfs   ) )


(true ;END1
(println (%   "  OK -  %s state[%10s] secondsSearching[%5d] searchFails[%3d/%3d] conformations[%d]"   fileName  state  searchSeconds  fails  cur  numberOfConfs   ) )
)
)
)
)
)

(let ((jobs  (gatherTrainers  l:*ARGS*   ) )(maxRestarts  (getGlobalOption  "exhaustive_maxRestartFails"   ) ) ) ;END1
(updateSearchStates jobs  maxRestarts   )
)
