#! /usr/bin/python

################################################################################
# 1st parameter = trace to write
################################################################################


from otf import *
import sys


def WriteRecords( writer ):
	
	procs= [ 1, 2, 11, 12 ]
	handleid= 0
	
	# definitions
	OTF_Writer_writeDefinitionComment( writer, 0, "this is a definition comment" )
	OTF_Writer_writeDefCreator( writer, 0, "test_complete" )
	OTF_Writer_writeDefTimerResolution( writer, 0, 1234 )
	OTF_Writer_writeDefProcess( writer, 0, procs[0], "process number one", 0 )
	OTF_Writer_writeDefProcess( writer, 0, procs[1], "process number two", 0 )
	OTF_Writer_writeDefProcess( writer, 0, procs[2], "child one of process one", procs[0] )
	OTF_Writer_writeDefProcess( writer, 0, procs[3], "child two of process one", procs[0] )
	OTF_Writer_writeDefProcessGroup( writer, 0, 1000, "commworld", 4, procs )
	
	OTF_Writer_writeDefSclFile( writer, 0, 100000, "main.c" )
	OTF_Writer_writeDefScl( writer, 0, 10000, 100000, 123 ) #/*main*/
	OTF_Writer_writeDefScl( writer, 0, 20000, 100000, 456 ) #/*sub*/
	OTF_Writer_writeDefScl( writer, 0, 30000, 100000, 789 ) #/*sub2*/
	OTF_Writer_writeDefScl( writer, 0, 40000, 100000, 1111 ) #/*sendsnapshot*/
	
	OTF_Writer_writeDefFunctionGroup( writer, 0, 1000, "functiongroup one" )
	OTF_Writer_writeDefFunctionGroup( writer, 0, 2000, "functiongroup two" )
	OTF_Writer_writeDefFunction( writer, 0, 1, "mainfunction", 1000,  10000 )
	OTF_Writer_writeDefFunction( writer, 0, 2, "subfunction one", 2000,  20000 )
	OTF_Writer_writeDefFunction( writer, 0, 3, "subfunction two", 2000,  30000 )


	OTF_Writer_writeDefCollectiveOperation( writer, 0, 1, "MPI_Barrier", OTF_COLLECTIVE_TYPE_BARRIER );

	OTF_Writer_writeDefCounterGroup( writer, 0, 1000, "counter group one" )
	OTF_Writer_writeDefCounter( writer, 0, 1, "flops", OTF_COUNTER_TYPE_ACC|OTF_COUNTER_SCOPE_START,  1000, "#" )
	OTF_Writer_writeDefCounter( writer, 0, 2, "memory usage", OTF_COUNTER_TYPE_ABS|OTF_COUNTER_SCOPE_START,  1000, "MiB" )


	OTF_Writer_writeDefFileGroup( writer, 0, 100, "file group one" )
	OTF_Writer_writeDefFile( writer, 0, 1, "file one", 100 )
	OTF_Writer_writeDefFile( writer, 0, 2, "file two", 100 )
	


	#/* *** events *** */
	for i in range(4):
	
		timebase= i*10000000
		nexttimebase= (i+1)*10000000
		
		#/* *** determine own, previous and next processid *** */
		me= procs[i]
		if 3 == i :
			next= procs[0]
		else :
			next= procs[i+1]
		if 0 == i :
			prev= procs[3]
		else :
			prev= procs[i-1]
		
		#/* *** process start *** */
		OTF_Writer_writeBeginProcess( writer, timebase, me )
		OTF_Writer_writeEventComment( writer, timebase+1, me, "this process just started" )
		OTF_Writer_writeEnter( writer, timebase+2, 1, me, 10000)


		OTF_Writer_writeFileOperation( writer, timebase+3, 1, me, handleid, 123, 456, 789, 0 );
		++handleid;

		OTF_Writer_writeFileOperation( writer, timebase+4, 2, me, handleid, 1230, 4560, 7890, 0 );
		++handleid;

		OTF_Writer_writeBeginFileOperation( writer, timebase +11, me, 100, 0 );
		OTF_Writer_writeEndFileOperation(   writer, timebase +12, me, 1, 100, handleid, 1230, 4567, 0 );

		for j in range(1,1000) :
		#for j in range(1,3) :
		
			time= timebase+j*1000
			
			OTF_Writer_writeCounter( writer, time, me, 1, j*10000 )
			OTF_Writer_writeCounter( writer, time+1, me, 2, j )
			
			time+= 100
			
			OTF_Writer_writeCollectiveOperation( writer, time, me, 1, 1000, 1, 123, 456, 100, 40000 )
			
			time+= 100
			
			OTF_Writer_writeSendMsg( writer,  time, me, next, 1000, 0, 4, 40000 )
				
			time+= 100
			
			for k in range(10) :
			#for k in range(3) :
			
				OTF_Writer_writeEnter( writer, time, 2, me, 20000 )
				time+=10
				OTF_Writer_writeLeave( writer, time, 2, me, 20000 )
				time+=20
				OTF_Writer_writeEnter( writer, time, 3, me, 30000 )
				time+=10
				OTF_Writer_writeLeave( writer, time, 3, me, 30000 )
				time+=10
				
			
			
			time+= 100;
			
			OTF_Writer_writeRecvMsg( writer, time, me, prev, 1000, 0, 4, 40000 )
			



		#/* *** process end *** */
		OTF_Writer_writeLeave( writer, nexttimebase-3, 1, me, 10000 )
		OTF_Writer_writeEventComment( writer, nexttimebase-2, me, "this process will terminate soon" )
		OTF_Writer_writeEndProcess( writer, nexttimebase-1, me )


	
	#/* *** snapshots *** */
	time= 100
	for i in range(4) :
	
		OTF_Writer_writeSnapshotComment( writer, time, procs[i], "snapshot testcomment" )
		time+= 10
		
		OTF_Writer_writeEnterSnapshot( writer, time, 500, 1, procs[i], 10000 )
		time+= 10
		
		OTF_Writer_writeSendSnapshot( writer, time, 1500, procs[i], 1, 1000, 0, 128, 40000 )
		time+= 10

		OTF_Writer_writeOpenFileSnapshot( writer, time, 1, 2, procs[i], 3, 0 )
		time+= 10


	
	#/* *** statistics *** */
	time= 100
	for i in range(4) :
	
		OTF_Writer_writeSummaryComment( writer, time, procs[i], "summary testcomment" )
		time+= 10
		
		OTF_Writer_writeFunctionSummary( writer, time, 2, procs[i], 123, 300, 400 )
		time+= 10

		OTF_Writer_writeFunctionGroupSummary( writer, time, 2000, procs[i], 456, 500, 600 )
		time+= 10

		OTF_Writer_writeMessageSummary( writer, time, procs[i], 1, 1000, 0, 789, 123, 10000, 20000 )
		time+= 10


		OTF_Writer_writeFileOperationSummary( writer, time, 1, procs[i], 2, 3, 4, 5, 6, 7, 8 )
		time+= 10

		OTF_Writer_writeFileGroupOperationSummary( writer, time, 1, procs[i], 2, 3, 4, 5, 6, 7, 8 )
		time+= 10

	


#################################################################################


if __name__ == '__main__':
	
	manager= OTF_FileManager_open( 100 )
	writer= OTF_Writer_open( sys.argv[1], 1000000, manager )


	WriteRecords(writer)


	OTF_Writer_close( writer )
	OTF_FileManager_close( manager )
