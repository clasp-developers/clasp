(include "CANDO/db/jobSetup.l")

(include "CANDO/db/organizer.l")

(defun summary ( jobs )
    (let ( totalSeconds searched trainer head search numberOfConfs state curMax searchSeconds 
			totalSeconds fails searched totalDuration )
      (setq totalSeconds 0L)
      (setq searched 0)
      (foreach fileName jobs
	       (setq trainer (readTrainer fileName))
	       (setq head (getHeader trainer))
	       (setq search (getConformationExplorer trainer))
	       (setq numberOfConfs (numberOfEntries search))
	       (setq state (getState head))
	       (setq curMax (getMaxConformationalSearchFailsRequired head))
	       (setq searchSeconds (getSecondsSpentConformationalSearching head))
	       (setq totalSeconds (+ totalSeconds searchSeconds ))
	       (setq fails (getMostConformationalSearchFailsSeen head))
	       (cond
		((== state "searched" )
		 (setq searched (+ searched 1 ))
		 (print (% "%s state(%10s) secondsSearching(%5d) searchFails(%3d/%3d) conformations(%d)"
			   fileName state searchSeconds fails curMax numberOfConfs )))
		((== state "searching" )
		 (print (% "%s state(%10s) secondsSearching(%5d) searchFails(%3d/%3d) conformations(%d)"
			   fileName state searchSeconds fails curMax numberOfConfs )))
		(true
		 (print (% "%s state(%s) conformations(%d)" fileName state numberOfConfs )))))
      (print (% "Total time searching %d seconds" totalSeconds ))
      (print (% "There are %d trainers that have searched their search" searched ))
      (setq totalDuration ( PosixTimeDuration :seconds totalSeconds ))
      (print (% "Total time searching %s HH-MM-SS" (toSimpleString totalDuration)))))


(database (seedDatabaseName) true)
(let ((jobs (gatherTrainers *ARGS*)))
  (summary jobs))

