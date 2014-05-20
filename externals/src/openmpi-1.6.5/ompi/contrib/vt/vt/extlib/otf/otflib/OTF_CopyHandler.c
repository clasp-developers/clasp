/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_Platform.h"
#include "OTF_CopyHandler.h"
#include "OTF_Writer.h"


/* *** Definition handlers *** ************************************* */

int OTF_CopyHandler_DefinitionComment( void* userData, uint32_t stream,
		const char* comment, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefinitionCommentKV( (OTF_Writer*)userData, stream,
		comment, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefTimerResolution( void* userData,	uint32_t stream,
		uint64_t ticksPerSecond, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefTimerResolutionKV( (OTF_Writer*)userData, stream,
		ticksPerSecond, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefProcess( void* userData, uint32_t stream, uint32_t deftoken,
	const char* name, uint32_t parent, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefProcessKV( (OTF_Writer*)userData, stream,
		deftoken, name, parent, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefProcessGroup( void* userData, uint32_t stream, uint32_t deftoken,
	const char* name, uint32_t n, const uint32_t* procs, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefProcessGroupKV( (OTF_Writer*)userData, stream, deftoken,
		name, n, procs, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_DefAttributeList( void* userData, uint32_t stream, uint32_t attr_token,
		uint32_t num, OTF_ATTR_TYPE* array, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefAttributeListKV( (OTF_Writer*)userData, stream, attr_token,
		num, array, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_DefProcessOrGroupAttributes( void* userData, uint32_t stream, uint32_t proc_token,
		uint32_t attr_token, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefProcessOrGroupAttributesKV( (OTF_Writer*)userData, stream, proc_token,
		attr_token, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_DefFunction(  void* userData, uint32_t stream,
		uint32_t deftoken, const char* name, 
		uint32_t group, uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefFunctionKV( (OTF_Writer*)userData, stream, deftoken,
		name, group, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefFunctionGroup( void* userData, uint32_t stream,
		uint32_t deftoken, const char* name, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefFunctionGroupKV( (OTF_Writer*)userData, stream,
		deftoken, name, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefCollectiveOperation( void* userData, uint32_t stream,
	uint32_t collOp, const char* name, uint32_t type, OTF_KeyValueList* list ) {
	

	return ( 0 == OTF_Writer_writeDefCollectiveOperationKV( (OTF_Writer*)userData, stream,
		collOp, name, type, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefCounter( void* userData, uint32_t stream,
		uint32_t deftoken, const char* name, uint32_t properties, 
		uint32_t countergroup, const char* unit, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefCounterKV( (OTF_Writer*)userData, stream,
		deftoken, name, properties, countergroup, unit, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefCounterGroup( void* userData, uint32_t stream,
		uint32_t deftoken, const char* name, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefCounterGroupKV( (OTF_Writer*)userData, stream,
			deftoken, name, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefScl(  void* userData, uint32_t stream, uint32_t deftoken,
	uint32_t sclfile, uint32_t sclline, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefSclKV( (OTF_Writer*)userData, stream, deftoken,
		sclfile, sclline, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefSclFile(  void* userData, uint32_t stream,
		uint32_t deftoken, const char* filename, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefSclFileKV( (OTF_Writer*)userData, stream,
		deftoken, filename, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefUniqueId( void* userData, uint32_t stream,
		uint64_t uid ) {

	/* unique-id record is written implicitly */

	return OTF_RETURN_OK; /* success */
}


int OTF_CopyHandler_DefVersion( void* userData, uint32_t stream, 
		uint8_t major, uint8_t minor, uint8_t sub, const char* string ) {


	/* version record is written implicitly */

	return OTF_RETURN_OK; /* success */
}


int OTF_CopyHandler_DefCreator( void* userData, uint32_t stream,
		const char* creator, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefCreatorKV( (OTF_Writer*)userData, stream, creator, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefFile( void* userData, uint32_t stream, uint32_t token,
	const char* name, uint32_t group, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefFileKV( (OTF_Writer*)userData, stream, token,
		name, group, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefFileGroup( void* userData, uint32_t stream,
	uint32_t token, const char* name, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefFileGroupKV( (OTF_Writer*)userData, stream, token,
		name, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_DefKeyValue( void* userData, uint32_t stream, uint32_t key,
	OTF_Type type, const char* name, const char *description, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefKeyValueKV( (OTF_Writer*)userData, stream, key,
		type, name, description, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_DefTimeRange( void* userData, uint32_t stream,
	uint64_t minTime, uint64_t maxTime, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefTimeRange( (OTF_Writer*)userData,
		stream, minTime, maxTime, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_DefCounterAssignments( void* userData, uint32_t stream,
	uint32_t counter, uint32_t number_of_members, const uint32_t* procs_or_groups,
	OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefCounterAssignments( (OTF_Writer*)userData,
		stream, counter, number_of_members, procs_or_groups,
		list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_DefProcessSubstitutes( void* userData, uint32_t stream,
	uint32_t representative, uint32_t numberOfProcs, const uint32_t* procs,
	OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeDefProcessSubstitutes( (OTF_Writer*)userData,
		stream, representative, numberOfProcs, procs,
		list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_DefAuxSamplePoint( void*                  userData,
                                       uint32_t               stream,
                                       uint64_t               time,
                                       OTF_AuxSamplePointType type,
                                       OTF_KeyValueList*      list ) {


    return ( 0 == OTF_Writer_writeDefAuxSamplePoint( (OTF_Writer*)userData,
            stream,
            time,
            type,
            list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

/* *** Event handlers *** ****************************************** */

int OTF_CopyHandler_NoOp( void* userData, uint64_t time, uint32_t process,
		OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeNoOpKV( (OTF_Writer*)userData, time,
		process, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_EventComment( void* userData, uint64_t time, uint32_t process,
		const char* comment, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeEventCommentKV( (OTF_Writer*)userData, time,
		process, comment, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_Counter( void* userData, uint64_t time, uint32_t process,
		uint32_t counter_token, uint64_t value, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeCounterKV( (OTF_Writer*)userData, time,
		process, counter_token, value, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_Enter( void* userData, uint64_t time, uint32_t statetoken,
		uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeEnterKV( (OTF_Writer*)userData, time,
		statetoken, cpuid, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_CollectiveOperation( void* userData, uint64_t time, 
    	uint32_t process, uint32_t functionToken, uint32_t communicator, 
    	uint32_t rootprocess, uint32_t sent, uint32_t received, 
    	uint64_t duration, uint32_t scltoken, OTF_KeyValueList* list ) {


    	return ( 0 == OTF_Writer_writeCollectiveOperationKV( (OTF_Writer*)userData, time,
    	process, functionToken, communicator, rootprocess, 
		sent, received, duration, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_BeginCollectiveOperation( void* userData, uint64_t time,
		uint32_t process, uint32_t collOp, uint64_t matchingId,
		uint32_t procGroup, uint32_t rootProc, uint64_t sent,
		uint64_t received, uint32_t scltoken, OTF_KeyValueList* list )
{

	return (0 == OTF_Writer_writeBeginCollectiveOperationKV(
			(OTF_Writer*) userData, time, process, collOp,
			matchingId, procGroup, rootProc, sent, received,
			scltoken, list )) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_EndCollectiveOperation( void* userData, uint64_t time,
		uint32_t process, uint64_t matchingId, OTF_KeyValueList* list )
{

	return (0 == OTF_Writer_writeEndCollectiveOperationKV(
			(OTF_Writer*) userData, time, process,
			matchingId, list )) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_RecvMsg( void* userData, uint64_t time, 
		uint32_t receiver, uint32_t sender, uint32_t communicator, 
		uint32_t msgtype, uint32_t msglength, uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeRecvMsgKV( (OTF_Writer*)userData, time, receiver,
		sender, communicator, msgtype, msglength, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_SendMsg( void* userData, uint64_t time, 
		uint32_t sender, uint32_t receiver, uint32_t communicator, 
		uint32_t msgtype, uint32_t msglength, uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeSendMsgKV( (OTF_Writer*)userData, time, sender,
		receiver, communicator, msgtype, msglength, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_Leave( void* userData, uint64_t time, uint32_t statetoken,
		uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeLeaveKV( (OTF_Writer*)userData, time, statetoken,
		cpuid, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_BeginProcess( void* userData, uint64_t time,
		uint32_t process, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeBeginProcessKV( (OTF_Writer*)userData, time,
		process, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_EndProcess( void* userData, uint64_t time,
		uint32_t process, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeEndProcessKV( (OTF_Writer*)userData, time,
		process, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_FileOperation( void* userData, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeFileOperationKV( (OTF_Writer*)userData, time, fileid,
		process, handleid, operation, bytes, duration, source, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_BeginFileOperation( void* userData, uint64_t time,
		uint32_t process, uint64_t matchingId, uint32_t scltoken,
        OTF_KeyValueList* list )
{

	return (0 == OTF_Writer_writeBeginFileOperationKV(
			(OTF_Writer*) userData, time, process, matchingId,
            scltoken, list )) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_EndFileOperation( void* userData, uint64_t time,
		uint32_t process, uint32_t fileid, uint64_t matchingId,
        uint64_t handleId, uint32_t operation, uint64_t bytes,
        uint32_t scltoken, OTF_KeyValueList* list )
{

	return (0 == OTF_Writer_writeEndFileOperationKV( (OTF_Writer*) userData,
			time, process, fileid, matchingId, handleId, operation, bytes,
			scltoken, list )) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_RMAPut( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list ) {


        return ( 0 == OTF_Writer_writeRMAPutKV( (OTF_Writer*)userData, time,
                process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_RMAPutRemoteEnd( void* userData, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
        uint32_t tag, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list ) {


        return ( 0 == OTF_Writer_writeRMAPutRemoteEndKV( (OTF_Writer*)userData,
                time, process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_RMAGet( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list ) {


        return ( 0 == OTF_Writer_writeRMAGetKV( (OTF_Writer*)userData, time,
                process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_RMAEnd( void* userData, uint64_t time, uint32_t process, uint32_t remote,
	uint32_t communicator, uint32_t tag, uint32_t scltoken, OTF_KeyValueList* list ) {


        return ( 0 == OTF_Writer_writeRMAEndKV( (OTF_Writer*)userData, time,
                process, remote, communicator, tag, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


/* *** snapshot handlers ********************************************** */


int OTF_CopyHandler_SnapshotComment( void* userData, uint64_t time,
	uint32_t process, const char* comment, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeSnapshotCommentKV( (OTF_Writer*)userData,
		time, process, comment, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_EnterSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t function, uint32_t process,
	uint32_t source, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeEnterSnapshotKV( (OTF_Writer*)userData,
		time, originaltime, function, process, source, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_SendSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t length, 
    uint32_t source, OTF_KeyValueList* list ) {
	

	return ( 0 == OTF_Writer_writeSendSnapshotKV( (OTF_Writer*)userData,
		time, originaltime, sender, receiver, procGroup, tag, length, source, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_OpenFileSnapshot( void* userData, uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeOpenFileSnapshotKV( (OTF_Writer*)userData, time,
		originaltime, fileid, process, handleid, source, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_BeginCollopSnapshot( void* userData, uint64_t time,
    uint64_t originaltime, uint32_t process, uint32_t collOp,
    uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
    uint64_t sent, uint64_t received, uint32_t scltoken, OTF_KeyValueList* list ) {


    return ( 0 == OTF_Writer_writeBeginCollopSnapshotKV( (OTF_Writer*)userData, time,
		originaltime, process, collOp, matchingId, procGroup, rootProc, sent, received,
        scltoken, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

}


int OTF_CopyHandler_BeginFileOpSnapshot( void* userData, uint64_t time,
    uint64_t originaltime, uint32_t process, uint64_t matchingId,
    uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeBeginFileOpSnapshotKV( (OTF_Writer*)userData, time,
		originaltime, process, matchingId, scltoken, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

}


int OTF_CopyHandler_CollopCountSnapshot( void* userData,
                                         uint64_t time,
                                         uint32_t process,
                                         uint32_t communicator,
                                         uint64_t count,
                                         OTF_KeyValueList* list ) {

    return ( 0 == OTF_Writer_writeCollopCountSnapshot( (OTF_Writer*)userData,
        time,
        process,
        communicator,
        count,
        list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_CounterSnapshot( void*             userData,
                                     uint64_t          time,
                                     uint64_t          originaltime,
                                     uint32_t          process,
                                     uint32_t          counter,
                                     uint64_t          value,
                                     OTF_KeyValueList* list ) {

    return ( 0 == OTF_Writer_writeCounterSnapshot( (OTF_Writer*)userData,
        time,
        originaltime,
        process,
        counter,
        value,
        list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

/* *** summary handlers ********************************************** */
int OTF_CopyHandler_SummaryComment( void * userData, uint64_t time,
	uint32_t process, const char* comment, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeSummaryCommentKV( (OTF_Writer*)userData,
		time, process, comment, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_FunctionSummary( void* userData, 
		uint64_t time, uint32_t function, uint32_t process, 
		uint64_t count, uint64_t excltime, uint64_t incltime,
		OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeFunctionSummaryKV( (OTF_Writer*)userData,
		time, function, process, count, excltime, incltime, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_FunctionGroupSummary( void* userData, 
		uint64_t time,  uint32_t functiongroup,  uint32_t process,  
		uint64_t count,  uint64_t excltime,  uint64_t incltime,
		OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeFunctionGroupSummaryKV( (OTF_Writer*)userData,
		time, functiongroup, process, count, excltime, incltime, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_MessageSummary( void* userData, uint64_t time,
	uint32_t process, uint32_t peer, uint32_t comm, uint32_t type,
	uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes,
	uint64_t receivedBytes, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeMessageSummaryKV((OTF_Writer*) userData,
		time, process, peer, comm, type, sentNumber, receivedNumber, sentBytes,
		receivedBytes, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_CollopSummary( void *userData, uint64_t time,
	uint32_t process, uint32_t comm, uint32_t collective, uint64_t sentNumber,
	uint64_t receivedNumber, uint64_t sentBytes, uint64_t receivedBytes,
	OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeCollopSummaryKV((OTF_Writer*) userData,
		time, process, comm, collective, sentNumber, receivedNumber, sentBytes,
		receivedBytes, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_FileOperationSummary( void* userData, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeFileOperationSummaryKV( (OTF_Writer*) userData,
		time, fileid, process, nopen, nclose, nread, nwrite, nseek,
		bytesread, byteswrite, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_FileGroupOperationSummary( void* userData, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeFileGroupOperationSummaryKV( (OTF_Writer*) userData,
		time, groupid, process, nopen, nclose, nread, nwrite, nseek,
		bytesread, byteswrite, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_DefMarker( void *userData, uint32_t stream,
	uint32_t token, const char* name, uint32_t type, OTF_KeyValueList* list ) {


	/* even if marker definitions could be read from many streams, they are 
	written to stream 0 forcedly, because this is where all markers belong. */
	stream= 0;

	return ( 0 == OTF_Writer_writeDefMarkerKV( (OTF_Writer*) userData,
		stream, token, name, type, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;;
}


int OTF_CopyHandler_Marker( void *userData, uint64_t time,
	uint32_t process, uint32_t token, const char* text, OTF_KeyValueList* list ) {


	return ( 0 == OTF_Writer_writeMarkerKV( (OTF_Writer*) userData,
		time, process, token, text, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}
