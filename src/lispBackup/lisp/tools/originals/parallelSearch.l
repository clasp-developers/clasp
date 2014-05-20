
;
;
; ParallelManager and ParallelWorker classes for advancing Trainers as
; far as possible
;
;


global *parallelMpi* ( mpiCommWorld )
global *parallelRank* [ *parallelMpi* GetRank ]
global *parallelNumberOfProcesses* [ *parallelMpi* GetSize ]

global *cmdSEPARATORinitStart* 0
global *cmdSEPARATORinitDone* 1
global *cmdSEPARATORrequest* 2
global *cmdSEPARATORresults* 3
global *cmdSEPARATORjob* 4
global *cmdSEPARATORshutDown* 5

defClass ParallelManager () ( _ManagerTickFunction _JobsToDo _JobsDone _JobsRunning _WorkersRunning _AccumulatedHits )

    method initJobs ( self jobs hitList managerTickFunction )
		; [ self slot _Jobs ] stores a linked list of all of the 
	[ [ self slot _ManagerTickFunction ] = managerTickFunction ]
	[ [ self slot _JobsToDo ] = jobs ]
	[ [ self slot _JobsDone ] = () ]
	[ [ self slot _JobsRunning ] = 0 ]
	[ [ self slot _AccumulatedHits ] = hitList ]
	[ [ self slot _WorkersRunning ] = [ *parallelNumberOfProcesses* - 1 ] ]

    ;
    ; Listen for a request and handle it
    ; If there are no more jobs to do then return false, otherwise true
    ;
    method waitForAndProcessOneJobRequest ( self ) 
	ifTrue [ [ self slot _WorkersRunning ] == 0 ]
	    return false
	println [ "-------- waiting for job request ------------ [ self slot _JobsRunning ]=%d" % [ self slot _JobsRunning ] ]
	[ msg = [ *parallelMpi* Recv MPI.ANYSEPARATORSOURCE MPI.ANYSEPARATORTAG] ]
	[ source = [ *parallelMpi* GetSource ] ]
	[ cmd = [ *parallelMpi* GetTag ] ]
	cond
	  [ cmd == *cmdSEPARATORrequest* ]
	    println [ "Received job request from source: %d" % source ]
	    LOG [ "ParallelManager received request from: %1% " % (repr source)  ]
		; Now handle the request
	    [ job = (car [ self slot _JobsToDo ] ) ]
	    println [ "The job that I have to send is: %s" % (repr job ) ]
	    if [ job != () ]
	      then
		[ [ self slot _JobsDone ] = ( cons job [ self slot _JobsDone ] ) ]
		[ [ self slot _JobsToDo ] = ( cdr [ self slot _JobsToDo ] ) ]
		println [ "Sending job %s to :source %d" % (repr job) source ]
		[ *parallelMpi* Send job source *cmdSEPARATORjob* ]
		println [ "Sent job %s to :source %d" % (repr job) source ]
		ifTrue [ job != () ]
		    [ [ self slot _JobsRunning ] = [ [ self slot _JobsRunning ] + 1 ] ]
	      else
		println [ "Shutting down worker: %d" % source ]
		[ *parallelMpi* Send () source *cmdSEPARATORshutDown* ]
		[ [ self slot _WorkersRunning ] = [[ self slot _WorkersRunning ] - 1 ] ]
	  [ cmd == *cmdSEPARATORresults* ]
	    println [ "Received results from source: %d" % source ]
	    println [ "Got results for job from source: %d" % source ]
	    [ [ self slot _AccumulatedHits ] addAllHits msg ]
	    [ [ self slot _JobsRunning ] = [ [ self slot _JobsRunning ] - 1 ] ]
	return true

    method getHits ( self )
	return [ self slot _AccumulatedHits ]

    method processJobRequests ( self )
	while [ self waitForAndProcessOneJobRequest ]
	    ( invoke [ self slot _ManagerTickFunction ] (  ) )
	    






defClass ParallelWorker () ( _WorkerFunction ) 

    method init ( self workerFunction )
	[ [ self slot _WorkerFunction ] = workerFunction ]

    method requestJob ( self )
	println [ "Sending a job request from worker: %d" % (mpiRank) ]
	Send (mpiCommWorld) () 0 *cmdSEPARATORrequest*
	println [ "Worker[%d] waiting for a job from manager"%  (mpiRank) ]
	[ job = ( Recv (mpiCommWorld) 0 MPI.ANYSEPARATORTAG ) ]
	println ["Got the job[%s]" % (repr job) ]
	return job

    method sendResultToManager ( self result )
	println [ "Sending results from worker: %d" % (mpiRank) ]
	Send (mpiCommWorld) result 0 *cmdSEPARATORresults*
	println ["Sent the result" ]


    ;
    ; Request a job and process it and return true, 
    ; if a shutdown command comes down then return false
    ;
    method requestJobAndProcess ( self )
	[ job = [ self requestJob ] ]
	ifTrue [ job == () ] 
	    println "I'm being shut down"
	    return false
	println [ "ParallelWorker;%d received job: %s" % (mpiRank) (repr job ) ]
	[ result = [ self processJob job ] ]
	[ self sendResultToManager result ]
	return true

    method run ( self )
	while [ self requestJobAndProcess ]

    ;
    ; processJob is the method that you subclass to 
    ; change the behavior of the Worker
    ;
    method processJob ( self job )
	println [ "Processing job(%s) invoking workerFunction(%s)" % (repr job) [ self slot _WorkerFunction ] ]
	setq args (list job )
	setq result ( invoke [ self slot _WorkerFunction ] args )
	return result


;
; distributeJobs starts the ParallelManager and the Worker and starts
; distributing jobs.
;
;
defun parallelSearch ( jobs hitList workerFunction managerTickFunction )
    if (mpiEnabled)
      then
	if [ (mpiRank)  == 0 ]
	  then
	    println "Staring manager"
	    [ manager = ( new ParallelManager ) ]
	    [ manager initJobs jobs hitList managerTickFunction ]
	    [ manager processJobRequests ]
	  else
	    println "Staring worker"
	    [ worker = ( new ParallelWorker ) ]
	    [ worker init workerFunction ]
	    [ worker run ]
      else
	println [ "Running as single job with workerFunction(%s)" % workerFunction ]
	foreach job jobs
	    setq args (list job )
	    [ oneResult = ( invoke workerFunction args ) ]
	    ( invoke managerTickFunction ( ) )
	    [ hitList addAllHits oneResult ]




;
; If a worker calls this then it will block until the manager
; tells it to proceed.
; this is to control access to large resources like BuilderDatabases that
; are so large that if they are loaded all at once it will cause page swapping

defun parallelSearchSEPARATORcriticalSectionBegin( )
    ifTrue [ *parallelRank* != 0 ]
        setq msg [ *parallelMpi* Recv MPI.ANYSEPARATORSOURCE *cmdSEPARATORinitStart* ]


defun parallelSearchSEPARATORcriticalSectionEnd( )
    if [ *parallelRank* == 0 ]
        then
	    println "Sending initialization message to each process in turn"
	    foreach rank (range 1 *parallelNumberOfProcesses*)
	        println [ "Sending initialization message to process: %d" % rank ]
		    ; send a message to start
		[ *parallelMpi* Send 0 rank *cmdSEPARATORinitStart* ]
		    ; wait for a response
		    ; DANGER, there is a chance for deadlock
		setq msg [ *parallelMpi* Recv MPI.ANYSEPARATORSOURCE *cmdSEPARATORinitDone* ]
	else
	    println "Telling manager we are ready to start processing"
	    [ *parallelMpi* Send 0 0 *cmdSEPARATORinitDone* ]

	

