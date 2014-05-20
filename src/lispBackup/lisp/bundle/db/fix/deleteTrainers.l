; fixed code
(defun doesTrainerOligomerContainMonomer  (trainer  monomerNames   ) ;END1
(assert (notNil   monomerNames  )"needs 'monomerNames:'"   )

(let (header  body  fileName  oligomer   ) ;END1
(setq header  (getHeader   trainer  ) )

(setq body  trainer   )

(setq fileName  (getFileName   header  ) )

(log (%   "trainer====structure: %s"   fileName   ) )

(setq oligomer  (getData   body   "oligomer"   ) )

(log  "oligomer="    )

(log (%   "%s"   (asXmlString   oligomer  ) ) )

(foreach mon  (monomersAsCons   oligomer  ) ;END1
(ifTrue (contains   monomerNames   (getName   mon  ) ) ;END1
(return true   )
)
)

(return false   )
)
)

(defun deleteTrainersWithOligomersContainingMonomers  (&key  (standardDatabaseName  () )(trainerDirectory  () )(monomerNames  () )(deleteTrainers  false   ) ) ;END1
(assert (notNil   standardDatabaseName  )"You must provide 'standardDatabaseName:'"   )

(assert (notNil   trainerDirectory  )"You must provide 'trainerDirectory:'"   )

(assert (notNil   monomerNames  )"You must provide StringSet 'monomerNames:'"   )

(standardDatabase standardDatabaseName   )

(let (path  jobs  deleteFileNames  trainer  remPath  approveDelete   ) ;END1
(setq path  (Path  :path  trainerDirectory   ) )

(setq jobs  (directory  path   ) )

(setq deleteFileNames  () )

(foreach fileName  jobs   ;END1
(setq trainer  (readTrainer  fileName   ) )

(print (%   "Read trainer: %s"   fileName   ) )

(if (doesTrainerOligomerContainMonomer  trainer  monomerNames   ) ;END1
(then ;END1
(print "   FLAGGED TO DELETE"   )

(setq deleteFileNames  (Cons  fileName  deleteFileNames   ) )
)

(else ;END1
(print "    keeping this one"   )
)
)

(println ""   )
)

(println "Trainers flagged to delete:"   )

(foreach fn  deleteFileNames   ;END1
(println (%   "     Trainer: %s"   fn   ) )
)

(println (%   "That's %d trainers to delete"   (length   deleteFileNames  ) ) )

(setq approveDelete  (getline  "Do you really want to delete these (yes==accept)?"   ) )

(ifTrue (==   approveDelete   "yes"   ) ;END1
(foreach fileName  deleteFileNames   ;END1
(setq remPath  (Path  :path  fileName   ) )

(println (%   "Removing trainer %s "   fileName   ) )

(remove remPath   )
)
)
)
)

(ifTrue (isTopLevelScript   ) ;END1
(let ((args  (cdr  l:*ARGS*   ) ) ) ;END1
(println (%   "args = %s"   (repr  args   ) ) )

(invoke "deleteTrainersWithOligomersContainingMonomers"  args   )
)
)
