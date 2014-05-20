; fixed code
(defvar *mpi*  (mpiCommWorld   ) )

(defvar *rank*  (GetRank   *mpi*  ) )

(defvar *numberOfProcesses*  (GetSize   *mpi*  ) )

(defvar *cmd_job*  1   )

(defvar *cmd_shutDown*  2   )

(defClass MpiManager  ()(_JobsToDo  _JobsDone  _StartTime   ) ;END1
(method initJobs  (self  jobs   ) ;END1
(=   (slot   self   _JobsToDo   ) jobs   )

(=   (slot   self   _JobsDone   ) () )

(setq (slot   self   _StartTime   )(setToLocalTime   (PosixTime   )) )
)

(method waitForAndProcessOneJobRequest  (self   ) ;END1
(let (curTime  dur  durStr  msg  source  job   ) ;END1
(when (==   (slot   self   _JobsToDo   ) () ) ;END1
(return false   )
)

(setq curTime  (setToLocalTime   (PosixTime   )) )

(setq dur  (sub   curTime   (slot   self   _StartTime   ) ) )

(setq durStr  (toSimpleString   dur  ) )

(println (%   "-------- waiting for job request ------------ time: %s"   durStr   ) )

(=   msg   (Recv   *mpi*   MPI.ANY_SOURCE  MPI.ANY_TAG   ) )

(=   source   (GetSource   *mpi*  ) )

(println (%   "Received job request from source: %d"   source   ) )

(log (%   "MpiManager received request from: %1% "   (repr  source   ) ) )

(=   job   (car  (slot   self   _JobsToDo   ) ) )

(=   (slot   self   _JobsDone   ) (cons  job  (slot   self   _JobsDone   ) ) )

(=   (slot   self   _JobsToDo   ) (cdr  (slot   self   _JobsToDo   ) ) )

(println (%   "Sending job %s to source: %d"   (repr  job   )source   ) )

(Send   *mpi*   job  source  *cmd_job*   )

(println (%   "Sent job %s to source: %d"   (repr  job   )source   ) )

(println (%   "    There remain %d jobs to do"   (length   (slot   self   _JobsToDo   )) ) )

(return true   )
)
)

(method processJobRequests  (self   ) ;END1
(while (waitForAndProcessOneJobRequest   self  )() )
)

(method processOneShutdown  (self  numLeft   ) ;END1
(let (msg  source   ) ;END1
(println "Waiting for a job request - I will send a shutdown command when I get it"   )

(=   msg   (Recv   *mpi*   MPI.ANY_SOURCE  MPI.ANY_TAG   ) )

(=   source   (GetSource   *mpi*  ) )

(println (%   "Got a request for a job from process[%d] I'm shutting it down"   source   ) )

(log (%   "MpiManager received request from: %d"   source   ) )

(Send   *mpi*   ()source  *cmd_shutDown*   )

(println (%   "I shut down the process, there are %d processes left to shut down (+/-1)"   numLeft   ) )
)
)

(method shutdownAllProcesses  (self  numberOfWorkers   ) ;END1
(println (%   "--------- Time to shutdown all workers, there are %d workers"   numberOfWorkers   ) )

(foreach p  (Range  0  numberOfWorkers   ) ;END1
(processOneShutdown   self   (-   numberOfWorkers   p   ) )
)

(println "Finished shutting down all workers"   )
)
)

(defClass MpiWorker  ()() ;END1
(method init  (self   ) ;END1
()
)

(method requestJob  (self   ) ;END1
(let (job   ) ;END1
(println (%   "Sending a job request from worker: %d"   (mpiRank   ) ) )

(Send (mpiCommWorld   )()0  0   )

(println (%   "Worker[%d] waiting for a job from manager"   (mpiRank   ) ) )

(=   job   (Recv  (mpiCommWorld   )0  MPI.ANY_TAG   ) )

(println (%   "Got the job[%s]"   (repr  job   ) ) )

(return job   )
)
)

(method requestJobAndProcess  (self   ) ;END1
(let (job   ) ;END1
(setq job  (requestJob   self  ) )

(when (==   job   () ) ;END1
(println (%   "MpiWorker;%d received an empty job"   (mpiRank   ) ) )

(return false   )
)

(println (%   "MpiWorker;%d received job: %s"   (mpiRank   )(repr  job   ) ) )

(handlerCase  (processJob   self   job  *forceField*   )(CandoException  (se   )(progn  (println  "AN EXCEPTION WAS THROWN when processing the job"   )(println  (%   "%s"   (message   se  ) ) )(println  "   - continuing to next job"   ) ) ) )

(return true   )
)
)

(method run  (self   ) ;END1
(while (requestJobAndProcess   self  ) ;END1
()
)
)

(method processJob  (self  job   ) ;END1
(println (%   "Process ;%2d> Empty -processJob- method, subclass should implement me"   (mpiRank   ) ) )
)
)

(defun distributeJobs  (managerClass  workerClass  jobs   ) ;END1
(let (numberOfWorkers  manager  worker   ) ;END1
(if (==   (mpiRank   ) 0   ) ;END1
(then ;END1
(setq numberOfWorkers  (-   *numberOfProcesses*   1   ) )

(=   manager   (new  managerClass   ) )

(initJobs   manager   jobs   )

(processJobRequests   manager  )

(println  "Transitioning to shutdownAlProcesses"    )

(shutdownAllProcesses   manager   numberOfWorkers   )
)

(else ;END1
(=   worker   (new  workerClass   ) )

(init   worker  )

(run   worker  )

(println  "Going into infinite loop until the manager terminates"    )

(while true   ;END1
()
)
)
)
)
)
