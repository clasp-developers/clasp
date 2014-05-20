

defClass ProcessStates () ( _StateToName _NameToState _NextStateTag )
    method __init__ ( self )
        setq [ self slot _StateToName ] (IntegerKeyObjectDictionary)
	setq [ self slot _NameToState ] (ObjectDictionary)
	setq [ self slot _NextStateTag ] 1
	[self addHardWiredState "shutdown" [self shutdownState ] ]
	[self addHardWiredState "initialize" [self initializeState ] ]
	[self addHardWiredState "process" [self processState ] ]
	[self addHardWiredState "uploadResults" [self uploadResultsState ] ]
	
    method addHardWiredState ( self name tag )
        [ [ self slot _StateToName ] put tag name ]
	[ [ self slot _NameToState ] put name tag ]
	setq [ self slot _NextStateTag ] (max [ self slot _NextStateTag ] [ tag + 1 ] )

    method addState ( self name )
        setq tag [ self slot _NextStateTag ]
        [ [ self slot _StateToName ] put [ self slot _NextStateTag ] name ]
	[ [ self slot _NameToState ] put name [ self slot _NextStateTag ] ]
	setq [ self slot _NextStateTag ] [ [ self slot _NextStateTag ] + 1 ]
	return tag

    method initializeState ( self )
        return 1

    method processState ( self )
        return 2
        
    method uploadResultsState ( self )
        return 3
        
    method shutdownState ( self )
        return 4
        
    method stateTagFromName (self name)
        assert [ [ self slot _NameToState ] contains name ] [ "There is no state named: %s" % name ]
        return [ [ self slot _NameToState ] get name ] 

    method stateNameFromTag (self tag)
        assert [ [ self slot _StateToName ] contains tag ] [ "There is no state tagged: %d" % tag]
        return [ [ self slot _StateToName ] get tag ] 


global *systemProcessStates* (ProcessStates)

defun createProcessState ( name )
    return [ *systemProcessStates* addState name ]

defun shutdownState ()
    return [ *systemProcessStates* shutdownState ]

defun processState ()
    return [ *systemProcessStates* processState ]

defun uploadResultsState ()
    return [ *systemProcessStates* uploadResultsState ]

defun initializeState ()
    return [ *systemProcessStates* initializeState ]

defun describeState ( s)
    return [ *systemProcessStates* stateNameFromTag s ]





defClass OneProcess () ( _Rank _LastHeardFromTime _CurrentState _Running _Data )

    method __init__ ( self rank )
	setq [ self slot _Rank ] rank
	setq [ self slot _Running ] true
	setq [ self slot _CurrentState ] ()
	[self setLastHeardFromTimeToNow ]
	setq [ self slot _Data ] ()
	return self

    method setLastHeardFromTimeToNow ( self )
        setq [ self slot _LastHeardFromTime ] ( PosixTime )
	[ [ self slot _LastHeardFromTime ] setToLocalTime ]

    method setState ( self stateTag )
        setq [ self slot _CurrentState ] stateTag
	[self setLastHeardFromTimeToNow ]

    method getRank( self )
        return [ self slot _Rank ]

    method getState ( self )
        return [ self slot _CurrentState ]

    method getDurationSinceLastHeardFrom ( self )
        setq now ( PosixTime )
	[ now setToLocalTime ]
	setq duration [ now sub [ self slot _LastHeardFromTime ] ]
	return duration

    method setData ( self data )
        setq [ self slot _Data ] data

    method getData ( self )
        return [ self slot _Data ]

    method dump (self)
        println [ "Process(%d)" % [ self slot _Rank ] ]

    method shutdown ( self )
        setq [ self slot _Running ] false

    method isRunning ( self )
        return [ self slot _Running ]

    method changeState (self newState job)
        [self setState newState ]
	println [ "Sending new state(%s) down to process with rank: %d" % (describeState newState) [ self slot _Rank ] ]
	Send (mpiCommWorld) job [ self slot _Rank ] newState
	when [ newState == (shutdownState) ]
	    setq [ self slot _Running ] false


;
; [ self slot _ProcessManagerObject ] - an object that is called whenever a OneProcess
;			  communicates with the farm
;
; Subclass method (processData (self process) ) -- process data
;		  (finish ) -- finished processing data
;
defClass ProcessFarm () ( _Processes _ProcessManagerObject _Jobs _Worker )
    method __init__ ( self jobs )
        setq [ self slot _Processes ] (IntegerKeyObjectDictionary)
	setq [ self slot _Jobs ] jobs
	setq [ self slot _Worker ] ()
	println [ "Creating processes" ]
	foreach p (Range 1 [ (mpiCommWorld ) GetSize ] )
	    setq pro [ self createProcess p ]
	    [ pro dump ]
	[ self dumpProcesses ]

    method setWorker (self worker)
        setq [ self slot _Worker ] worker	

    method getProcess ( self processRank )
    	setq key processRank
	assert [ [ self slot _Processes ] contains key ] [ "Could not find process with key: %d" % key ]
	return [ [ self slot _Processes ] get key ]
      
    method createProcess ( self processRank )
        setq one ( OneProcess processRank )
	setq key processRank
	println [ "Creating process with rank: %d" % key ]
	[ [ self slot _Processes ] put key one ]
	return one

    method dumpProcesses ( self )
        println [ "Dumping all processes" ]
        foreach p [ [ self slot _Processes ] valuesAsCons ]
	    [ p dump ]

    method waitForProcess ( self )
	println [ "-------- waiting for communication from processes ------------ processes still running= %d" % [self processesStillRunning] ]
	[ msg = [ (mpiCommWorld) Recv MPI.ANY_SOURCE MPI.ANY_TAG] ]
	[ source = [ (mpiCommWorld) GetSource ] ]
	[ stateTag = [ (mpiCommWorld) GetTag ] ]
	[ process = [ self getProcess source ] ]
	[ process setState stateTag ]
	[ process setData msg ]
	println [ "Got communication from process %d" % source ]
	return process	

    method processesStillRunning ( self )
        setq running 0
        foreach p [ [ self slot _Processes ] valuesAsCons ]
	    when [ p isRunning ]
	        setq running [ running + 1 ]
	return running

    method runSingle ( self )
	setq process (OneProcess 1)
	foreach job [ self slot _Jobs ]
	    setq result [ [ self slot _Worker ] processOneJob job ]
	    [ process setData result ]
	    [ self processData process ]
	[ self finish ]

    method runMpi ( self )
	while [ [ self processesStillRunning ] > 0 ]
	    [ self dumpProcesses ]
	    setq process [ self waitForProcess ]
	    when [ process isNil ]
	        raise [ "Handle timeouts" ]
	    println [ "Responding to process %d" % [ process getRank ] ]
	    [ self respondToProcess process ]
	    setq job [ self nextJob ]
	    if [ job isNil ]
		then
		    [process changeState (shutdownState) () ]
		else
		    [process changeState (processState) job ]
	println [ "Done responding" ]
	println [ "Calling finish method" ]
	[ self finish ]
	println [ "The manager is now shutting down, all of the jobs have been carried out" ]
	println [ "and all of the workers have sent us their data" ]
	println ""
	println ""
	println "Exiting"



    method respondToProcess ( self process )
        setq state [ process getState ]
	cond
	    [ state == (initializeState) ]
	        ()
	    [ state == (uploadResultsState) ]
	        setq data [ process getData ]
		[ self processData process ]
	    true
	        raise [ "Manager should never receive state(%s)" % (describeState state) ]

    method nextJob ( self )
        println [ "Current jobs = %s" % (repr [ self slot _Jobs ] ) ]
	when [ [ self slot _Jobs ] notNil ]
	    setq job (car [ self slot _Jobs ] )
	    setq [ self slot _Jobs ] (cdr [ self slot _Jobs ])
	    println [ "Returning job %s" % (repr job) ]
	    return job
	println [ "Ran out of jobs" ]
	return ()
	; There are no jobs left

    method processData ( self process )
        raise "Subclass must implement processData"

    method finish ( self )
        println "Subclass can implement finish - by default do nothing"


defClass ParallelFarmWorker () ( _MpiRank _State _Shutdown _Data )
    method __init__ ( self )
        setq [ self slot _MpiRank ] (mpiRank)
	setq [ self slot _Shutdown ] false
	setq [ self slot _State ] (initializeState)
	setq [ self slot _Data ] ()

    method run ( self )
        while (not [ self slot _Shutdown ] )
	    println [ "Sending state(%s) of worker(%d)" % (describeState [ self slot _State ]) [ self slot _MpiRank ] ]
	    Send (mpiCommWorld) [ self slot _Data ] 0 [ self slot _State ]
	    println [ "Worker[%d] waiting for new state from manager"%  (mpiRank) ]
	    [ newData = ( Recv (mpiCommWorld) 0 MPI.ANY_TAG ) ]
	    [ stateTag = [ (mpiCommWorld) GetTag ] ]
	    setq [ self slot _State ] stateTag
	    println ["Got the job[%s]" % ( describeState stateTag ) ]
	    [ self respondToStateChange stateTag newData ]
	println [ "Waiting for one last message from Manager (that will never come)" ]
	[ lastGasp = ( Recv (mpiCommWorld) 0 MPI.ANY_TAG ) ]
	println "Never reach here but if I do then I'm shutting down"

    method setState ( self s)
        setq [ self slot _State ] s
	      
    method uploadNewStateAndResults ( self newState results )
        setq [ self slot _State ] newState 
	setq [ self slot _Data ] results

    method shutdown ( self )
        setq [ self slot _Shutdown ] true


    method respondToStateChange ( self newState data )
	cond 
	    [ newState == (processState) ]
		setq hits [ self processOneJob data ]
		[self uploadNewStateAndResults (uploadResultsState) hits ]
	    [ newState == (shutdownState) ]
	        [ self shutdown ]
	    true
		println [ "Illegal state: %s" % (describeState newState) ]
		[ self shutdown ]



defun processFarm.mainLoop ( &key manager worker )
    if [ (mpiRank) == 0 ]
	then
	    assert [ manager notNil ] "You must specify the manager"
	    assert [ worker notNil ] "You must specify the worker"
	    println [ "mpiEnabled = %d" % (repr (mpiEnabled)) ]
	    if ( mpiEnabled )
		then
		    println [ "Starting [ manager runMpi] " ]
		    [ manager runMpi ]
		else
		    println [ "Starting [ manager runSingle] " ]
		    [ manager setWorker worker ]
		    [ manager runSingle ]
	else
	    assert [ worker notNil ] "You must specify the worker"
	    [ worker run ]
  



