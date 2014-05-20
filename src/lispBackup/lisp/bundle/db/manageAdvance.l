					; fixed code
					;
					;
					; MpiManager and MpiWorker classes for advancing Trainers as
					; far as possible
					;
					;

(defvar *mpi* ())
(defvar *rank* 0 )
(defvar *numberOfProcesses* 0 )
(when (mpiEnabled)
  (setq *mpi* (mpiCommWorld) )
  (setq *rank* (mpiRank))
  (setq *numberOfProcesses* (mpiSize)))

(defvar *cmd_job* 1 )
(defvar *cmd_shutDown* 2 )
(defClass MpiManager () (_JobsToDo _JobsDone _StartTime )
  (method initJobs (self jobs )
	  (setq (slot self _JobsToDo ) jobs )
	  (setq (slot self _JobsDone ) ())
	  (setq (slot self _StartTime ) (setToLocalTime (PosixTime))) ;
	  )
					; Listen for a request and handle it
					; If there are no more jobs to do then retrn false, otherwise true
					;
  (method waitForAndProcessOneJobRequest (self )
	  (let (curTime dur durStr msg source job )
	    (when (== (slot self _JobsToDo )())
	      (return-from waitForAndProcessOneJobRequest false ))
	    (setq curTime (setToLocalTime (PosixTime )))
	    (setq dur (sub curTime (slot self _StartTime )))
	    (setq durStr (toSimpleString dur ))
	    (print (% "-------- waiting for job request ------------ time: %s" durStr ))
	    (setq msg (Recv *mpi* ANY_SOURCE ANY_TAG ))
	    (setq source (GetSource *mpi* ))
	    (print (% "Received job request from source: %d" source ))
	    (log (% "MpiManager received request from: %1% " (repr source ))) ; Now handle the request

	    (setq job (car (slot self _JobsToDo )))
	    (setq (slot self _JobsDone )(cons job (slot self _JobsDone )))
	    (setq (slot self _JobsToDo )(cdr (slot self _JobsToDo )))
	    (print (% "Sending job %s to source: %d" (repr job )source ))
	    (Send *mpi* job source *cmd_job* )
	    (print (% "Sent job %s to source: %d" (repr job )source ))
	    (print (% "    There remain %d jobs to do" (length (slot self _JobsToDo ))))
	    true ))
  (method processJobRequests (self )
	  (while (waitForAndProcessOneJobRequest self )()))
  (method processOneShutdown (self numLeft )
	  (let (msg source )
	    (print "Waiting for a job request - I will send a shutdown command when I get it" )
	    (setq msg (Recv *mpi* ANY_SOURCE ANY_TAG ))
	    (setq source (GetSource *mpi* ))
	    (print (% "Got a request for a job from process[%d] I'm shutting it down" source ))
	    (log (% "MpiManager received request from: %d" source )) ; Now handle the request

	    (Send *mpi* ()source *cmd_shutDown* )
	    (print (% "I shut down the process, there are %d processes left to shut down (+/-1)" numLeft ))))
  (method shutdownAllProcesses (self numberOfWorkers )
	  (print (% "--------- Time to shutdown all workers, there are %d workers" numberOfWorkers ))
	  (foreach p (Range 0 numberOfWorkers )
		   (processOneShutdown self (- numberOfWorkers p )))
	  (print "Finished shutting down all workers" )))



(defClass MpiWorker ()()
  (method init (self )
	  ())
  (method requestJob (self )
	  (let (job )
	    (print (% "Sending a job request from worker: %d" (mpiRank )))
	    (Send *mpi* () 0 0 )
	    (print (% "Worker[%d] waiting for a job from manager" (mpiRank )))
	    (setq job (Recv *mpi*  0 ANY_TAG ))
	    (print (% "Got the job[%s]" (repr job )))
	    job ))
					;
					; Request a job and process it and retrn true, 
					; if a shutdown command comes down then retrn false
					;

  (method requestJobAndProcess (self )
	  (let (job )
	    (setq job (requestJob self ))
	    (when (== job ())
	      (print (% "MpiWorker;%d received an empty job" (mpiRank )))
	      (return-from requestJobAndProcess false ))
	    (print (% "MpiWorker;%d received job: %s" (mpiRank )(repr job )))
	    (handlerCase (processJob self job *forceField* )
			 (CandoException (se )
					 (progn 
					   (print "AN EXCEPTION WAS THROWN when processing the job - caught in manageAdvance" )
					   (print (getStackTraceDump se))
					   (print (% "%s" (message se )))
					   (print "   - continuing to next job" ))))
	    true ))

  (method run (self )
	  (while (requestJobAndProcess self )
	    ()))
					;
					; processJob is the method that you subclass to 
					; change the behavior of the Worker
					;
  (method processJob (self job )
	  (print (% "Process ;%2d> Empty -processJob- method, subclass should implement me" (mpiRank ))))
)
					;
					; distributeJobs starts the MpiManager and the Worker and starts
					; distributing jobs.
					;
					;

(defun distributeJobs (managerClass workerClass jobs )
  (let (numberOfWorkers manager worker )
    (if (== (mpiRank ) 0 )
	(then 
	 (setq numberOfWorkers (- *numberOfProcesses* 1 ))
	 (setq manager (new managerClass))
	 (initJobs manager jobs )
	 (processJobRequests manager )
	 (print "Transitioning to shutdownAlProcesses" )
	 (shutdownAllProcesses manager numberOfWorkers ))
      (else 
       (setq worker (new workerClass ))
       (init worker )
       (run worker )
       (print  "Going into infinite loop until the manager terminates" )
       (while true () )))))