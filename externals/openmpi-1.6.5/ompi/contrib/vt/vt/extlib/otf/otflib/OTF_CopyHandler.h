/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_CopyHandler.h
 * 
 *  @brief Provides handlers for copying a trace.
 *
 *  \ingroup internal
 */

#ifndef OTF_COPYHANDLER_H
#define OTF_COPYHANDLER_H


#include "OTF_inttypes.h"
#include "OTF_Definitions.h"

#include "OTF_KeyValue.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

int OTF_CopyHandler_DefinitionComment( void* userData, uint32_t stream,
	const char* comment, OTF_KeyValueList* list );

int OTF_CopyHandler_DefTimerResolution( void* userData, uint32_t stream,
	uint64_t ticksPerSecond, OTF_KeyValueList* list );

int OTF_CopyHandler_DefProcess( void* userData, uint32_t stream, uint32_t process,
	const char* name, uint32_t parent, OTF_KeyValueList* list );

int OTF_CopyHandler_DefProcessGroup( void* userData, uint32_t stream,
	uint32_t procGroup, const char* name, uint32_t numberOfProcs,
	const uint32_t* procs, OTF_KeyValueList* list );

int OTF_CopyHandler_DefAttributeList( void* userData, uint32_t stream,
	uint32_t attr_token, uint32_t num, OTF_ATTR_TYPE* array, OTF_KeyValueList* list );

int OTF_CopyHandler_DefProcessOrGroupAttributes( void* userData, uint32_t stream,
	uint32_t proc_token, uint32_t attr_token, OTF_KeyValueList* list );

int OTF_CopyHandler_DefFunction( void* userData, uint32_t stream, uint32_t func,
	const char* name, uint32_t funcGroup, uint32_t source, OTF_KeyValueList* list );

int OTF_CopyHandler_DefFunctionGroup( void* userData, uint32_t stream,
	uint32_t funcGroup, const char* name, OTF_KeyValueList* list );

int OTF_CopyHandler_DefCollectiveOperation( void* userData, uint32_t stream,
	uint32_t collOp, const char* name, uint32_t type, OTF_KeyValueList* list );

int OTF_CopyHandler_DefCounter( void* userData, uint32_t stream, uint32_t counter,
	const char* name, uint32_t properties, uint32_t counterGroup,
	const char* unit, OTF_KeyValueList* list );

int OTF_CopyHandler_DefCounterGroup( void* userData, uint32_t stream,
	uint32_t counterGroup, const char* name, OTF_KeyValueList* list );

int OTF_CopyHandler_DefScl( void* userData, uint32_t stream, uint32_t source,
	uint32_t sourceFile, uint32_t line, OTF_KeyValueList* list );

int OTF_CopyHandler_DefSclFile( void* userData, uint32_t stream,
	uint32_t sourceFile, const char* name, OTF_KeyValueList* list );

int OTF_CopyHandler_DefUniqueId( void* userData, uint32_t stream,
	uint64_t uid );

int OTF_CopyHandler_DefVersion( void* userData, uint32_t stream,
	uint8_t major, uint8_t minor, uint8_t sub, const char* string );

int OTF_CopyHandler_DefCreator( void* userData, uint32_t stream,
	const char* creator, OTF_KeyValueList* list );

int OTF_CopyHandler_DefFile( void* userData, uint32_t stream, uint32_t token,
	const char* name, uint32_t group, OTF_KeyValueList* list );
	
int OTF_CopyHandler_DefFileGroup( void* userData, uint32_t stream,
	uint32_t token, const char* name, OTF_KeyValueList* list );

int OTF_CopyHandler_DefKeyValue( void* userData, uint32_t stream, uint32_t key,
	OTF_Type type, const char* name, const char* description, OTF_KeyValueList* list );
	
int OTF_CopyHandler_DefTimeRange( void* userData, uint32_t stream,
	uint64_t minTime, uint64_t maxTime, OTF_KeyValueList* list );

int OTF_CopyHandler_DefCounterAssignments( void* userData, uint32_t stream,
	uint32_t counter, uint32_t number_of_members,
	const uint32_t* procs_or_groups, OTF_KeyValueList* list );

int OTF_CopyHandler_DefProcessSubstitutes( void* userData, uint32_t stream,
	uint32_t representative, uint32_t numberOfProcs, const uint32_t* procs,
	OTF_KeyValueList* list );

int OTF_CopyHandler_DefAuxSamplePoint( void*                  userData,
                                       uint32_t               stream,
                                       uint64_t               time,
                                       OTF_AuxSamplePointType type,
                                       OTF_KeyValueList*      list );

int OTF_CopyHandler_NoOp( void* userData, uint64_t time, uint32_t process,
	OTF_KeyValueList* list );

int OTF_CopyHandler_Enter( void* userData, uint64_t time, uint32_t function, 
	uint32_t process, uint32_t source, OTF_KeyValueList* list );

int OTF_CopyHandler_Leave( void* userData, uint64_t time, uint32_t function, 
	uint32_t process, uint32_t source, OTF_KeyValueList* list );

int OTF_CopyHandler_SendMsg( void* userData, uint64_t time, uint32_t sender, 
	uint32_t receiver, uint32_t group, uint32_t type, uint32_t length,
	uint32_t source, OTF_KeyValueList* list );

int OTF_CopyHandler_RecvMsg( void* userData, uint64_t time, uint32_t recvProc,
	uint32_t sendProc, uint32_t group, uint32_t type, uint32_t length, 
	uint32_t source, OTF_KeyValueList* list );

int OTF_CopyHandler_Counter( void* userData, uint64_t time, uint32_t process, 
	uint32_t counter, uint64_t value, OTF_KeyValueList* list );

int OTF_CopyHandler_CollectiveOperation( void* userData, uint64_t time,
	uint32_t process, uint32_t collective, uint32_t procGroup,
	uint32_t rootProc, uint32_t sent, uint32_t received, uint64_t duration, 
	uint32_t source, OTF_KeyValueList* list );

int OTF_CopyHandler_BeginCollectiveOperation( void* userData, uint64_t time,
	uint32_t process, uint32_t collOp, uint64_t matchingId,
	uint32_t procGroup, uint32_t rootProc, uint64_t sent,
	uint64_t received, uint32_t scltoken, OTF_KeyValueList* list );

int OTF_CopyHandler_EndCollectiveOperation( void* userData, uint64_t time,
	uint32_t process, uint64_t matchingId, OTF_KeyValueList* list );

int OTF_CopyHandler_EventComment( void* userData, uint64_t time, uint32_t process, 
	const char* comment, OTF_KeyValueList* list );

int OTF_CopyHandler_BeginProcess( void* userData, uint64_t time, uint32_t process, OTF_KeyValueList* list );

int OTF_CopyHandler_EndProcess( void* userData, uint64_t time, uint32_t process, OTF_KeyValueList* list );

int OTF_CopyHandler_SnapshotComment( void* userData, uint64_t time,
	uint32_t process, const char* comment, OTF_KeyValueList* list );

int OTF_CopyHandler_FileOperation( void* userData, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source, OTF_KeyValueList* list );

int OTF_CopyHandler_BeginFileOperation( void* userData, uint64_t time,
	uint32_t process, uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList* list );

int OTF_CopyHandler_EndFileOperation( void* userData, uint64_t time,
	uint32_t process, uint32_t fileid, uint64_t matchingId, uint64_t handleId,
	uint32_t operation, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );

int OTF_CopyHandler_RMAPut( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );

int OTF_CopyHandler_RMAPutRemoteEnd( void* userData, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
        uint32_t tag, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );

int OTF_CopyHandler_RMAGet( void* userData, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );

int OTF_CopyHandler_RMAEnd( void* userData, uint64_t time, uint32_t process,
        uint32_t remote, uint32_t communicator, uint32_t tag, uint32_t scltoken, OTF_KeyValueList* list );


int OTF_CopyHandler_EnterSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t function, uint32_t process,
	uint32_t source, OTF_KeyValueList* list );

int OTF_CopyHandler_SendSnapshot( void *userData, uint64_t time,
	uint64_t originaltime, uint32_t sender, uint32_t receiver,
	uint32_t procGroup, uint32_t tag, uint32_t length,
    uint32_t source, OTF_KeyValueList* list );

int OTF_CopyHandler_OpenFileSnapshot( void* userData, uint64_t time,
	uint64_t originaltime, uint32_t fileid, uint32_t process, uint64_t handleid,
	uint32_t source, OTF_KeyValueList* list );

int OTF_CopyHandler_BeginCollopSnapshot( void* userData, uint64_t time,
    uint64_t originaltime, uint32_t process, uint32_t collOp,
    uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
    uint64_t sent, uint64_t received, uint32_t scltoken, OTF_KeyValueList* list );

int OTF_CopyHandler_BeginFileOpSnapshot( void* userData, uint64_t time,
    uint64_t originaltime, uint32_t process, uint64_t matchingId,
    uint32_t scltoken, OTF_KeyValueList* list );

int OTF_CopyHandler_CollopCountSnapshot( void* userData,
                                         uint64_t time,
                                         uint32_t process,
                                         uint32_t communicator,
                                         uint64_t count,
                                         OTF_KeyValueList* list );

int OTF_CopyHandler_CounterSnapshot( void*             userData,
                                     uint64_t          time,
                                     uint64_t          originaltime,
                                     uint32_t          process,
                                     uint32_t          counter,
                                     uint64_t          value,
                                     OTF_KeyValueList* list );

int OTF_CopyHandler_SummaryComment( void * userData, uint64_t time,
	uint32_t process, const char* comment, OTF_KeyValueList* list );

int OTF_CopyHandler_FunctionSummary( void* userData, uint64_t time,
	uint32_t function, uint32_t process, uint64_t invocations,
	uint64_t exclTime, uint64_t inclTime, OTF_KeyValueList* list );

int OTF_CopyHandler_FunctionGroupSummary( void* userData, uint64_t time,
	uint32_t funcGroup, uint32_t process, uint64_t invocations,
	uint64_t exclTime, uint64_t inclTime, OTF_KeyValueList* list );

int OTF_CopyHandler_MessageSummary( void* userData, uint64_t time, uint32_t process,
	uint32_t peer, uint32_t comm, uint32_t type, uint64_t sentNumber,
	uint64_t receivedNumber, uint64_t sentBytes, uint64_t receivedBytes, OTF_KeyValueList* list );

int OTF_CopyHandler_CollopSummary(void *userData, uint64_t time, uint32_t process, uint32_t comm,
	uint32_t collective, uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes,
	uint64_t receivedBytes, OTF_KeyValueList* list );

int OTF_CopyHandler_FileOperationSummary( void* userData, uint64_t time,
	uint32_t fileid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list );

int OTF_CopyHandler_FileGroupOperationSummary( void* userData, uint64_t time,
	uint32_t groupid, uint32_t process, uint64_t nopen, uint64_t nclose,
	uint64_t nread, uint64_t nwrite, uint64_t nseek, uint64_t bytesread,
	uint64_t byteswrite, OTF_KeyValueList* list );

int OTF_CopyHandler_DefMarker( void *userData, uint32_t stream,
	uint32_t token, const char* name, uint32_t type, OTF_KeyValueList* list );

int OTF_CopyHandler_Marker( void *userData, uint64_t time,
	uint32_t process, uint32_t token, const char* text, OTF_KeyValueList* list );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_COPYHANDLER_H */

