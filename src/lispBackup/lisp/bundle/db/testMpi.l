
(defvar *mpi* ())
(defvar *rank* 0 )
(defvar *numberOfProcesses* 0 )
(when (mpiEnabled)
  (setq *mpi* (mpiCommWorld) )
  (setq *rank* (mpiRank))
  (setq *numberOfProcesses* (mpiSize)))




(defun runMpiTest ()
  (print (% "Starting process[%d]" (mpiRank)))
  (print "--- next line")
  (cond 
   ((== (mpiRank) 0)
    (let ((procs (- (mpiSize) 1)) msg src )
      (while (>= procs 0)
	(setq msg (Recv *mpi* ANY_SOURCE ANY_TAG))
	(setq src (GetSource *mpi*))
	(print (% "Manager received message[%s] from worker[%d]" msg src ))
	)))
   (true
    (print "Sending message to manager")
    (Send *mpi* (% "Worker %d reporting for duty" (mpiRank)) 0 0 )
    (while true ()))))




(if (mpiEnabled)
    (progn
      (runMpiTest))
  (progn
    (print "Mpi is not enabled")))
