(include "CANDO/db/jobSetup.l")
(include "CANDO/db/organizer.l")

(defun showContexts ( jobs &key focus )
  (for fileName jobs
       (let* (( trainer (readTrainer fileName))
	      ( head (getHeader trainer))
	      (context (getContext head)))
	 (print (% "--------------- fileName: %s" fileName ))
	 (print (% "Context: %s" (getKey context))))))


(database (seedDatabaseName) true)
(let ((jobs (gatherTrainers *ARGS*)))
  (showContexts jobs ))

