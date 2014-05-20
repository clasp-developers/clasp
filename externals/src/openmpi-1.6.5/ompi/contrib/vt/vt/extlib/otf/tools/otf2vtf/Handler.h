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


/* *** Definition handler *** ************************************* */

int handleDefinitionComment( void* firsthandlerarg, uint32_t streamid,
	const char* comment );

int handleDeftimerresolution( void* firsthandlerarg, uint32_t streamid,
	uint64_t ticksPerSecond );

int handleDefprocess( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t parent );

int handleDefprocessgroup( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t n, uint32_t* array );

int handleDeffunction( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t group, uint32_t scltoken );

int handleDeffunctiongroup( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name );

int handleDefcounter( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t properties, 
	uint32_t countergroup, const char* unit );

int handleDefcountergroup( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name );

int handleDefCollectiveOperation( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t type );

int handleDefscl( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, uint32_t sclfile, uint32_t sclline );

int handleDefsclfile( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* filename );

int handleDefFile( void* firsthandlerarg, uint32_t streamid,
	uint32_t token, const char* name, uint32_t group );
	

/* *** Event handler *** ****************************************** */

int handleEventComment( void* firsthandlerarg, uint64_t time,
	const char* comment );

int handleCounter( void* firsthandlerarg, uint64_t time, uint32_t process,
	uint32_t counter_token, uint64_t value );

int handleEnter( void* firsthandlerarg, uint64_t time, uint32_t statetoken,
	uint32_t cpuid, uint32_t scltoken );

int handleCollectiveOperation( void* firsthandlerarg, uint64_t time,
	uint32_t process, uint32_t globaloptoken, uint32_t communicator,
	uint32_t rootprocess, uint32_t sent, uint32_t received,
	uint64_t duration, uint32_t scltoken );

int handleRecvmsg( void* firsthandlerarg, uint64_t time, uint32_t receiver,
	uint32_t sender, uint32_t communicator, uint32_t msgtype, uint32_t msglength,
	uint32_t scltoken );

int handleSendmsg( void* firsthandlerarg, uint64_t time, uint32_t sender,
	uint32_t receiver, uint32_t communicator, uint32_t msgtype, uint32_t msglength,
	uint32_t scltoken );

int handleLeave( void* firsthandlerarg, uint64_t time, uint32_t statetoken,
	uint32_t cpuid, uint32_t scltoken );
	
int handleBeginProcess( void* firsthandlerarg, uint64_t time,
	uint32_t process );

int handleEndProcess( void* firsthandlerarg, uint64_t time,
	uint32_t process );

int handleFileOperation( void* firsthandlerarg, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source );
	

	
#endif /* OTFTOVTF3_HANDLER_H */
