/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_Platform.h"
#include "OTF_CopyHandler_stream.h"
#include "OTF_WStream.h"


/* *** Definition handlers *** ************************************* */

int OTF_CopyHandler_stream_DefinitionComment( void* userData, uint32_t stream,
		const char* comment, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefinitionCommentKV( (OTF_WStream*)userData,
		comment, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefTimerResolution( void* userData,	uint32_t stream,
		uint64_t ticksPerSecond, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefTimerResolutionKV( (OTF_WStream*)userData,
		ticksPerSecond, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefProcess( void* userData, uint32_t stream, uint32_t deftoken,
	const char* name, uint32_t parent, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefProcessKV( (OTF_WStream*)userData,
		deftoken, name, parent, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefProcessGroup( void* userData, uint32_t stream, uint32_t deftoken,
	const char* name, uint32_t n, const uint32_t* procs, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefProcessGroupKV( (OTF_WStream*)userData, deftoken,
		name, n, procs, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_stream_DefAttributeList( void* userData, uint32_t stream, uint32_t attr_token,
		uint32_t num, OTF_ATTR_TYPE* array, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefAttributeListKV( (OTF_WStream*)userData, attr_token,
		num, array, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_stream_DefProcessOrGroupAttributes( void* userData, uint32_t stream, uint32_t proc_token,
		uint32_t attr_token, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefProcessOrGroupAttributesKV( (OTF_WStream*)userData, proc_token,
		attr_token, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_stream_DefFunction(  void* userData, uint32_t stream,
		uint32_t deftoken, const char* name, 
		uint32_t group, uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefFunctionKV( (OTF_WStream*)userData, deftoken,
		name, group, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefFunctionGroup( void* userData, uint32_t stream,
		uint32_t deftoken, const char* name, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefFunctionGroupKV( (OTF_WStream*)userData,
		deftoken, name, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefCollectiveOperation( void* userData, uint32_t stream,
	uint32_t collOp, const char* name, uint32_t type, OTF_KeyValueList* list ) {
	

	return ( 0 == OTF_WStream_writeDefCollectiveOperationKV( (OTF_WStream*)userData,
		collOp, name, type, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefCounter( void* userData, uint32_t stream,
		uint32_t deftoken, const char* name, uint32_t properties, 
		uint32_t countergroup, const char* unit, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefCounterKV( (OTF_WStream*)userData,
		deftoken, name, properties, countergroup, unit, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefCounterGroup( void* userData, uint32_t stream,
		uint32_t deftoken, const char* name, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefCounterGroupKV( (OTF_WStream*)userData,
			deftoken, name, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefScl(  void* userData, uint32_t stream, uint32_t deftoken,
	uint32_t sclfile, uint32_t sclline, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefSclKV( (OTF_WStream*)userData, deftoken,
		sclfile, sclline, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefSclFile(  void* userData, uint32_t stream,
		uint32_t deftoken, const char* filename, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefSclFileKV( (OTF_WStream*)userData,
		deftoken, filename, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefUniqueId( void* userData, uint32_t stream,
		uint64_t uid ) {

	/* update unique-id record */
	return ( 0 == OTF_WStream_writeUniqueId( (OTF_WStream*)userData ) ) ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefVersion( void* userData, uint32_t stream, 
		uint8_t major, uint8_t minor, uint8_t sub, const char* string ) {


	/* update version record */
	return ( 0 == OTF_WStream_writeOtfVersion( (OTF_WStream*)userData ) ) ?
		OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefCreator( void* userData, uint32_t stream,
		const char* creator, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefCreatorKV( (OTF_WStream*)userData, creator, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefFile( void* userData, uint32_t stream, uint32_t token,
	const char* name, uint32_t group, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefFileKV( (OTF_WStream*)userData, token,
		name, group, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefFileGroup( void* userData, uint32_t stream,
	uint32_t token, const char* name, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefFileGroupKV( (OTF_WStream*)userData, token,
		name, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_stream_DefKeyValue( void* userData, uint32_t stream, uint32_t key,
	OTF_Type type, const char* name, const char *description, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefKeyValueKV( (OTF_WStream*)userData, key,
		type, name, description, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_stream_DefTimeRange( void* userData, uint32_t stream,
	uint64_t minTime, uint64_t maxTime, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefTimeRange( (OTF_WStream*)userData,
		minTime, maxTime, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_stream_DefCounterAssignments( void* userData, uint32_t stream,
	uint32_t counter, uint32_t number_of_members, const uint32_t* procs_or_groups,
	OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefCounterAssignments( (OTF_WStream*)userData,
		counter, number_of_members, procs_or_groups,
		list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_stream_DefProcessSubstitutes( void* userData, uint32_t stream,
	uint32_t representative, uint32_t numberOfProcs, const uint32_t* procs,
	OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefProcessSubstitutes( (OTF_WStream*)userData,
		representative, numberOfProcs, procs,
		list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_stream_DefAuxSamplePoint( void*                  userData,
                                              uint32_t               stream,
                                              uint64_t               time,
                                              OTF_AuxSamplePointType type,
                                              OTF_KeyValueList*      list ) {


    return ( 0 == OTF_WStream_writeDefAuxSamplePoint( (OTF_WStream*)userData,
            time,
            type,
            list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

/* *** Event handlers *** ****************************************** */

int OTF_CopyHandler_stream_NoOp( void* userData, uint64_t time, uint32_t process,
		OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeNoOpKV( (OTF_WStream*)userData, time,
		process, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_EventComment( void* userData, uint64_t time, uint32_t process,
		const char* comment, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeEventCommentKV( (OTF_WStream*)userData, time,
		process, comment, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_Counter( void* userData, uint64_t time, uint32_t process,
		uint32_t counter_token, uint64_t value, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeCounterKV( (OTF_WStream*)userData, time,
		process, counter_token, value, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_Enter( void* userData, uint64_t time, uint32_t statetoken,
		uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeEnterKV( (OTF_WStream*)userData, time,
		statetoken, cpuid, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_CollectiveOperation( void* userData, uint64_t time, 
    	uint32_t process, uint32_t functionToken, uint32_t communicator, 
    	uint32_t rootprocess, uint32_t sent, uint32_t received, 
    	uint64_t duration, uint32_t scltoken, OTF_KeyValueList* list ) {


    	return ( 0 == OTF_WStream_writeCollectiveOperationKV( (OTF_WStream*)userData, time,
    	process, functionToken, communicator, rootprocess, 
		sent, received, duration, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_BeginCollectiveOperation( void* userData, uint64_t time,
		uint32_t process, uint32_t collOp, uint64_t matchingId,
		uint32_t procGroup, uint32_t rootProc, uint64_t sent,
		uint64_t received, uint32_t scltoken, OTF_KeyValueList* list )
{

	return (0 == OTF_WStream_writeBeginCollectiveOperationKV(
			(OTF_WStream*) userData, time, process, collOp,
			matchingId, procGroup, rootProc, sent, received,
			scltoken, list )) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_EndCollectiveOperation( void* userData, uint64_t time,
		uint32_t process, uint64_t matchingId, OTF_KeyValueList* list )
{

	return (0 == OTF_WStream_writeEndCollectiveOperationKV(
			(OTF_WStream*) userData, time, process,
			matchingId, list )) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_RecvMsg( void* userData, uint64_t time, 
		uint32_t receiver, uint32_t sender, uint32_t communicator, 
		uint32_t msgtype, uint32_t msglength, uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeRecvMsgKV( (OTF_WStream*)userData, time, receiver,
		sender, communicator, msgtype, msglength, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_SendMsg( void* userData, uint64_t time, 
		uint32_t sender, uint32_t receiver, uint32_t communicator, 
		uint32_t msgtype, uint32_t msglength, uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeSendMsgKV( (OTF_WStream*)userData, time, sender,
		receiver, communicator, msgtype, msglength, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_Leave( void* userData, uint64_t time, uint32_t statetoken,
		uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeLeaveKV( (OTF_WStream*)userData, time, statetoken,
		cpuid, scltoken, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_BeginProcess( void* userData, uint64_t time,
		uint32_t process, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeBeginProcessKV( (OTF_WStream*)userData, time,
		process, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_EndProcess( void* userData, uint64_t time,
		uint32_t process, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeEndProcessKV( (OTF_WStream*)userData, time,
		process, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_FileOperation( void* userData, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeFileOperationKV( (OTF_WStream*)userData, time, fileid,
		process, handleid, operation, bytes, duration, source, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_BeginFileOperation( void* userData, uint64_t time,
		uint32_t process, uint64_t matchingId, uint32_t scltoken,
        OTF_KeyValueList* list )
{

	return (0 == OTF_WStream_writeBeginFileOperationKV(
			(OTF_WStream*) userData, time, process, matchingId,
            scltoken, list )) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_EndFileOperation( void* userData, uint64_t time,
		uint32_t process, uint32_t fileid, uint64_t matchingId,
        uint64_t handleId, uint32_t operation, uint64_t bytes,
        uint32_t scltoken, OTF_KeyValueList* list )
{

	return (0 == OTF_WStream_writeEndFileOperationKV( (OTF_WStream*) userData,
			time, process, fileid, matchingId, handleId, operation, bytes,
			scltoken, list )) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_RMAPut( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list ) {


        return ( 0 == OTF_WStream_writeRMAPutKV( (OTF_WStream*)userData, time,
                process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_RMAPutRemoteEnd( void* userData, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
        uint32_t tag, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list ) {


        return ( 0 == OTF_WStream_writeRMAPutRemoteEndKV( (OTF_WStream*)userData,
                time, process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_RMAGet( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list ) {


        return ( 0 == OTF_WStream_writeRMAGetKV( (OTF_WStream*)userData, time,
                process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_RMAEnd( void* userData, uint64_t time, uint32_t process, uint32_t remote,
	uint32_t communicator, uint32_t tag, uint32_t scltoken, OTF_KeyValueList* list ) {


        return ( 0 == OTF_WStream_writeRMAEndKV( (OTF_WStream*)userData, time,
                process, remote, communicator, tag, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


/* *** snapshot handlers ********************************************** */


int OTF_CopyHandler_stream_SnapshotComment( void* userData, uint64_t time,
	uint32_t process, const char* comment, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeSnapshotCommentKV( (OTF_WStream*)userData,
		time, process, comment, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_EnterSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t function, uint32_t process,
	uint32_t source, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeEnterSnapshotKV( (OTF_WStream*)userData,
		time, originaltime, function, process, source, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_SendSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t length, 
    uint32_t source, OTF_KeyValueList* list ) {
	

	return ( 0 == OTF_WStream_writeSendSnapshotKV( (OTF_WStream*)userData,
		time, originaltime, sender, receiver, procGroup, tag, length, source, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_OpenFileSnapshot( void* userData, uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeOpenFileSnapshotKV( (OTF_WStream*)userData, time,
		originaltime, fileid, process, handleid, source, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_BeginCollopSnapshot( void* userData, uint64_t time,
    uint64_t originaltime, uint32_t process, uint32_t collOp,
    uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
    uint64_t sent, uint64_t received, uint32_t scltoken, OTF_KeyValueList* list ) {


    return ( 0 == OTF_WStream_writeBeginCollopSnapshotKV( (OTF_WStream*)userData, time,
		originaltime, process, collOp, matchingId, procGroup, rootProc, sent, received,
        scltoken, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

}


int OTF_CopyHandler_stream_BeginFileOpSnapshot( void* userData, uint64_t time,
    uint64_t originaltime, uint32_t process, uint64_t matchingId,
    uint32_t scltoken, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeBeginFileOpSnapshotKV( (OTF_WStream*)userData, time,
		originaltime, process, matchingId, scltoken, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

}


int OTF_CopyHandler_stream_CollopCountSnapshot( void* userData,
                                                uint64_t time,
                                                uint32_t process,
                                                uint32_t communicator,
                                                uint64_t count,
                                                OTF_KeyValueList* list ) {

    return ( 0 == OTF_WStream_writeCollopCountSnapshot( (OTF_WStream*)userData,
        time,
        process,
        communicator,
        count,
        list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

int OTF_CopyHandler_stream_CounterSnapshot( void*             userData,
                                            uint64_t          time,
                                            uint64_t          originaltime,
                                            uint32_t          process,
                                            uint32_t          counter,
                                            uint64_t          value,
                                            OTF_KeyValueList* list ) {

    return ( 0 == OTF_WStream_writeCounterSnapshot( (OTF_WStream*)userData,
        time,
        originaltime,
        process,
        counter,
        value,
        list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}

/* *** summary handlers ********************************************** */
int OTF_CopyHandler_stream_SummaryComment( void * userData, uint64_t time,
	uint32_t process, const char* comment, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeSummaryCommentKV( (OTF_WStream*)userData,
		time, process, comment, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_FunctionSummary( void* userData, 
		uint64_t time, uint32_t function, uint32_t process, 
		uint64_t count, uint64_t excltime, uint64_t incltime,
		OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeFunctionSummaryKV( (OTF_WStream*)userData,
		time, function, process, count, excltime, incltime, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_FunctionGroupSummary( void* userData, 
		uint64_t time,  uint32_t functiongroup,  uint32_t process,  
		uint64_t count,  uint64_t excltime,  uint64_t incltime,
		OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeFunctionGroupSummaryKV( (OTF_WStream*)userData,
		time, functiongroup, process, count, excltime, incltime, list )
		 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_MessageSummary( void* userData, uint64_t time,
	uint32_t process, uint32_t peer, uint32_t comm, uint32_t type,
	uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes,
	uint64_t receivedBytes, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeMessageSummaryKV((OTF_WStream*) userData,
		time, process, peer, comm, type, sentNumber, receivedNumber, sentBytes,
		receivedBytes, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_CollopSummary( void *userData, uint64_t time,
	uint32_t process, uint32_t comm, uint32_t collective, uint64_t sentNumber,
	uint64_t receivedNumber, uint64_t sentBytes, uint64_t receivedBytes,
	OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeCollopSummaryKV((OTF_WStream*) userData,
		time, process, comm, collective, sentNumber, receivedNumber, sentBytes,
		receivedBytes, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_FileOperationSummary( void* userData, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeFileOperationSummaryKV( (OTF_WStream*) userData,
		time, fileid, process, nopen, nclose, nread, nwrite, nseek,
		bytesread, byteswrite, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_FileGroupOperationSummary( void* userData, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeFileGroupOperationSummaryKV( (OTF_WStream*) userData,
		time, groupid, process, nopen, nclose, nread, nwrite, nseek,
		bytesread, byteswrite, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}


int OTF_CopyHandler_stream_DefMarker( void *userData, uint32_t stream,
	uint32_t token, const char* name, uint32_t type, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeDefMarkerKV( (OTF_WStream*) userData,
		token, name, type, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;;
}


int OTF_CopyHandler_stream_Marker( void *userData, uint64_t time,
	uint32_t process, uint32_t token, const char* text, OTF_KeyValueList* list ) {


	return ( 0 == OTF_WStream_writeMarkerKV( (OTF_WStream*) userData,
		time, process, token, text, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;
}
