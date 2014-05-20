/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef OTFTOVTF3_HANDLER_H
#define OTFTOVTF3_HANDLER_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


#include "OTF_inttypes.h"

#include "otf.h"


/* *** Definition handler *** ************************************* */


int handleDeftimerresolution( void* firsthandlerarg, uint32_t streamid,
        uint64_t ticksPerSecond, OTF_KeyValueList* list );

int handleDefprocess( void* firsthandlerarg, uint32_t streamid,
        uint32_t deftoken, const char* name, uint32_t parent,
        OTF_KeyValueList* list );

int handleDefFunction( void* firsthandlerarg, uint32_t streamid,
        uint32_t deftoken, const char* name, uint32_t group, uint32_t scltoken,
        OTF_KeyValueList* list );

int handleDefFile( void* userData, uint32_t stream, uint32_t token,
        const char* name, uint32_t group, OTF_KeyValueList* list );

int handleDefCollectiveOperation(void *firsthandlerarg, uint32_t stream,
        uint32_t collOp, const char *name, uint32_t type,
        OTF_KeyValueList* list);

int handleDefTimeRange( void* userData, uint32_t streamid, uint64_t minTime,
        uint64_t maxTime, OTF_KeyValueList* kvlist );

int handlerDefKeyValue( void* userData, uint32_t stream, uint32_t key,
        OTF_Type type, const char* name, const char *description,
        OTF_KeyValueList* list );


/* *** Event handler *** ****************************************** */

int handleCounter( void* firsthandlerarg,
                   uint64_t time,
                   uint32_t process,
                   uint32_t counter,
                   uint64_t value,
                   OTF_KeyValueList *kvlist );

int handleEnter( void* firsthandlerarg, uint64_t time, uint32_t statetoken,
	uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList *kvlist );

int handleRecvmsg( void* firsthandlerarg, uint64_t time, uint32_t receiver,
	uint32_t sender, uint32_t procGroup, uint32_t msgtag, uint32_t msglength,
	uint32_t scltoken, OTF_KeyValueList *kvlist );

int handleSendmsg( void* firsthandlerarg, uint64_t time, uint32_t sender,
	uint32_t receiver, uint32_t procGroup, uint32_t msgtag, uint32_t msglength,
	uint32_t scltoken, OTF_KeyValueList *kvlist );

int handleLeave( void* firsthandlerarg, uint64_t time, uint32_t statetoken,
	uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList *kvlist );
	
int handleCollectiveOperation( void* firsthandlerarg, uint64_t time, 
    uint32_t process, uint32_t functionToken, uint32_t communicator, 
    uint32_t rootprocess, uint32_t sent, uint32_t received, 
    uint64_t duration, uint32_t scltoken, OTF_KeyValueList *kvlist );

int handleFileOperation( void* firsthandlerarg, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source, OTF_KeyValueList *kvlist );
    
int handleBeginCollectiveOperation( void *firsthandlerarg, uint64_t time, uint32_t process,
    uint32_t collOp, uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
    uint64_t sent, uint64_t received, uint32_t scltoken, OTF_KeyValueList *kvlist );
    
int handleEndCollectiveOperation( void *firsthandlerarg, uint64_t time,
    uint32_t process, uint64_t matchingId, OTF_KeyValueList *kvlist );

int handleBeginFileOperation( void *firsthandlerarg, uint64_t time, uint32_t process,
    uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList *kvlist );
    
int handleEndFileOperation( void *firsthandlerarg, uint64_t time, uint32_t process,
    uint32_t fileid, uint64_t matchingId, uint64_t handleId, uint32_t operation,
    uint64_t bytes, uint32_t scltoken, OTF_KeyValueList *kvlist );

int handleNoOp( void *firsthandlerarg, uint64_t time, uint32_t process,
		OTF_KeyValueList* list );


int handleEventComment( void *firsthandlerarg, uint64_t time, uint32_t process,
		const char* comment, OTF_KeyValueList* list );


int handleBeginProcess( void *firsthandlerarg, uint64_t time,
		uint32_t process, OTF_KeyValueList* list );


int handleEndProcess( void *firsthandlerarg, uint64_t time,
		uint32_t process, OTF_KeyValueList* list );


int handleRMAPut( void *firsthandlerarg, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );


int handleRMAPutRemoteEnd( void *firsthandlerarg, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
        uint32_t tag, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );


int handleRMAGet( void *firsthandlerarg, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list );


int handleRMAEnd( void *firsthandlerarg, uint64_t time, uint32_t process, uint32_t remote,
	uint32_t communicator, uint32_t tag, uint32_t scltoken, OTF_KeyValueList* list );


#endif /* OTFTOVTF3_HANDLER_H */
