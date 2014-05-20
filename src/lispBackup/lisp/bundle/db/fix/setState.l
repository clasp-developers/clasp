; fixed code
(require "CANDO/db/jobSetup.l"   )

(require "CANDO/db/organizer.l"   )

(defun setState  (jobs  fromStateSet  newState   ) ;END1
(let (trainer  state  partOfSeed   ) ;END1
(foreach fileName  jobs   ;END1
(setq trainer  (readTrainer  fileName   ) )

(setq state  (getState   (getHeader   trainer  )) )

(println (%   "Read trainer %s  state=%s"   fileName  state   ) )

(setq partOfSeed  (not  (getNotPartOfSeedDatabase   (getHeader   trainer  )) ) )

(ifTrue partOfSeed   ;END1
(ifTrue (contains   fromStateSet   state   ) ;END1
(setState   (getHeader   trainer  ) newState   )

(println (%   "   Changing state of trainer %s"   fileName   ) )

(write   trainer  )
)
)
)
)
)

(println  "Starting"    )

(let ((fromStateSet  (StringSet  :entries  (list  "bad"  "searchFailed"   ) ) )(toState  "start"   )(unsortedJobs  (directory  (Path  :path  "./data/"   )".*cxml"   ) )jobs   ) ;END1
(setq jobs  (sorted   unsortedJobs  ) )

(setState jobs  fromStateSet  toState   )
)
