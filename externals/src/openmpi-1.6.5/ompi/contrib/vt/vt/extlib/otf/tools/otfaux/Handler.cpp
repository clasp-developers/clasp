/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/


#include <cassert>

#include <OTF_CopyHandler.h>
#include <OTF_CopyHandler_stream.h>

#include "Handler.h"
#include "Control.h"


/* *** Definition handler *** ************************************* */


int handleDeftimerresolution( void* firsthandlerarg, uint32_t streamid,
        uint64_t ticksPerSecond, OTF_KeyValueList* list ) {


	Control* control= (Control*) firsthandlerarg;

	/* fprintf( stderr, "  %s\n", __PRETTY_FUNCTION__ ); */
	
    /* copy definition to new def file */
    return OTF_CopyHandler_stream_DefTimerResolution( control->def_wstream,
            0, ticksPerSecond, list );
}


int handleDefprocess( void* firsthandlerarg, uint32_t streamid,
        uint32_t deftoken, const char* name, uint32_t parent,
        OTF_KeyValueList* list ) {
	
	
	/* fprintf( stderr, "  %s\n", __PRETTY_FUNCTION__ ); */

	Control* control= (Control*) firsthandlerarg;

	control->all_processes.push_back( deftoken );
	if ( control->stats )
		control->stats->defProcess( deftoken );

    /* copy definition to new def file */
    return OTF_CopyHandler_stream_DefProcess( control->def_wstream,
            0, deftoken, name, parent, list );
}


int handleDefFunction( void* firsthandlerarg, uint32_t streamid,
	uint32_t deftoken, const char* name, uint32_t group, uint32_t scltoken,
        OTF_KeyValueList* list ) {
	

	/* fprintf( stderr, "  %s\n", __PRETTY_FUNCTION__ ); */
	
	Control* control= (Control*) firsthandlerarg;
	
	if ( control->stats )
		control->stats->defFunction( deftoken, group );

    /* copy definition to new def file */
    return OTF_CopyHandler_stream_DefFunction( control->def_wstream,
            0, deftoken, name, group, scltoken, list );
}


int handleDefFile( void* firsthandlerarg, uint32_t stream, uint32_t token,
        const char* name, uint32_t group, OTF_KeyValueList* list ) {


	/* fprintf( stderr, "  %s\n", __PRETTY_FUNCTION__ ); */
	
	Control* control= (Control*) firsthandlerarg;
	if ( control->stats )
		control->stats->defFile( token, group );

    /* copy definition to new def file */
    return OTF_CopyHandler_stream_DefFile( control->def_wstream,
            0, token, name, group, list );
}

int handleDefCollectiveOperation(void *firsthandlerarg, uint32_t stream,
        uint32_t collOp, const char *name, uint32_t type,
        OTF_KeyValueList* list) {


	Control* control= (Control*) firsthandlerarg;
	
	if ( control->stats )
		control->stats->defCollOp( collOp, type );

    /* copy definition to new def file */
    return OTF_CopyHandler_stream_DefCollectiveOperation( control->def_wstream,
            0, collOp, name, type, list );
}


int handleDefTimeRange( void* firsthandlerarg, uint32_t streamid, uint64_t minTime,
        uint64_t maxTime, OTF_KeyValueList* kvlist ) {

    Control* control= (Control*) firsthandlerarg;

    control->haveTimeRange = true;
    control->minTime = minTime;
    control->maxTime = maxTime;

    /* copy definition to new def file */
    return OTF_CopyHandler_stream_DefTimeRange( control->def_wstream,
            0, minTime, maxTime, kvlist );
}

int handlerDefKeyValue( void* firsthandlerarg, uint32_t stream, uint32_t key,
        OTF_Type type, const char* name, const char *description,
        OTF_KeyValueList* list ) {

    Control* control= (Control*) firsthandlerarg;

    if ( control->msgMatching ) {

        if ( control->maxKeyToken < key )
            control->maxKeyToken = key;

        if ( 0 == strcmp( name, OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_TIME_NAME ) ) {
            control->recvTimeToken = key;
            return OTF_RETURN_OK;
        }
        if ( 0 == strcmp( name, OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SIZE_NAME ) ) {
            control->recvLengthToken = key;
            return OTF_RETURN_OK;
        }
        if ( 0 == strcmp( name, OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SCL_NAME ) ) {
            control->recvSclToken = key;
            return OTF_RETURN_OK;
        }
    }

    /* copy definition to new def file */
    return OTF_CopyHandler_stream_DefKeyValue( control->def_wstream,
            0, key, type, name, description, list );
}


/* *** Event handler *** ****************************************** */


int handleCounter( void* firsthandlerarg,
                   uint64_t time,
                   uint32_t process,
                   uint32_t counter,
                   uint64_t value,
                   OTF_KeyValueList *kvlist ) {


    /* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */

    Control* control= (Control*) firsthandlerarg;

    while ( control->checkTime( time ) )
        ;

    if ( control->doSnapshots && control->aux_state )
    {
        OTFAUX_State_processCounter( control->aux_state,
                                     time,
                                     process,
                                     counter,
                                     value,
                                     OTF_KeyValueList_clone( kvlist ) );
    }

    if ( control->copyEvents )
        return OTF_CopyHandler_Counter( control->writer,
                                        time,
                                        process,
                                        counter,
                                        value,
                                        kvlist );

    return OTF_RETURN_OK;
}


int handleEnter( void* firsthandlerarg, uint64_t time, uint32_t statetoken,
		uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList *kvlist ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */

	Control* control= (Control*) firsthandlerarg;
	
	while ( control->checkTime( time ) )
		;

	if ( control->aux_state )
	{
		OTFAUX_State_processEnter( control->aux_state,
		                           time,
		                           cpuid,
		                           statetoken,
		                           scltoken,
		                           control->doSnapshots ?
		                               OTF_KeyValueList_clone( kvlist ) :
		                               NULL );
	}
	if ( control->stats )
		control->stats->enterFunction( time, cpuid, statetoken );

	if ( control->copyEvents )
		return OTF_CopyHandler_Enter( control->writer, time, statetoken,
				cpuid, scltoken, kvlist );

	return OTF_RETURN_OK;
}


int handleLeave( void* firsthandlerarg, uint64_t time, uint32_t statetoken,
		uint32_t cpuid, uint32_t scltoken, OTF_KeyValueList *kvlist ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */
	Control* control= (Control*) firsthandlerarg;
	
	while ( control->checkTime( time ) )
		;

	if ( control->aux_state )
	{
		OTFAUX_State_processLeave( control->aux_state,
		                           time,
		                           cpuid,
		                           statetoken );
	}
	if ( control->stats )
		control->stats->leaveFunction( time, cpuid, statetoken );

	if ( control->copyEvents )
		return OTF_CopyHandler_Leave( control->writer, time, statetoken,
				cpuid, scltoken, kvlist );

	return OTF_RETURN_OK;
}


int handleRecvmsg( void* firsthandlerarg, uint64_t time, uint32_t receiver,
		uint32_t sender, uint32_t procGroup, uint32_t msgtag, 
		uint32_t msglength, uint32_t scltoken, OTF_KeyValueList *kvlist ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */
	
	Control* control= (Control*) firsthandlerarg;
	
	if ( ( control->doSnapshots || control->msgMatching )
	     && control->collectRecvsOnly
	     && control->aux_state ) {
		OTFAUX_State_enqueueRecvMsg( control->aux_state,
		                             time,
		                             receiver,
		                             sender,
		                             procGroup,
		                             msgtag,
		                             msglength,
		                             scltoken );

		return OTF_RETURN_OK;
	}


	if ( !control->collectRecvsOnly )
	{
		while ( control->checkTime( time ) )
			;

		if ( control->stats )
			control->stats->recvMessage( receiver, msglength );

		if ( control->copyEvents )
			return OTF_CopyHandler_RecvMsg( control->writer, time,
					receiver, sender, procGroup, msgtag,
					msglength, scltoken, kvlist );
	}

	return OTF_RETURN_OK;
}


int handleSendmsg( void* firsthandlerarg, uint64_t time, uint32_t sender,
		uint32_t receiver, uint32_t procGroup, uint32_t msgtag, 
		uint32_t msglength, uint32_t scltoken, OTF_KeyValueList *kvlist ) {

	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */
	
	uint64_t recvTime;
	uint32_t recvLength;
	uint32_t recvScl;
	int ret = 0;
	Control* control= (Control*) firsthandlerarg;
	
	while ( control->checkTime( time ) )
		;

	if ( ( control->doSnapshots || control->msgMatching )
	     && control->aux_state ) {
		ret = OTFAUX_State_processSendMsg( control->aux_state,
		                                   time,
		                                   sender,
		                                   receiver,
		                                   procGroup,
		                                   msgtag,
		                                   msglength,
		                                   scltoken,
		                                   &recvTime,
		                                   &recvLength,
		                                   &recvScl,
		                                   OTF_KeyValueList_clone( kvlist ) );

	}

	if ( control->stats )
		control->stats->sendMessage( sender, msglength );

	if ( control->copyEvents ) {
		if ( control->msgMatching && ret == 1 ) {
			OTF_KeyValueList_removeKey( kvlist, control->recvTimeToken );
			OTF_KeyValueList_appendUint64( kvlist,
			                               control->recvTimeToken,
			                               recvTime );
			OTF_KeyValueList_removeKey( kvlist, control->recvLengthToken );
			OTF_KeyValueList_appendUint32( kvlist,
			                               control->recvLengthToken,
			                               recvLength );
			OTF_KeyValueList_removeKey( kvlist, control->recvSclToken );
			OTF_KeyValueList_appendUint32( kvlist,
			                               control->recvSclToken,
			                               recvScl );
		}
		return OTF_CopyHandler_SendMsg( control->writer, time,
				sender, receiver, procGroup, msgtag,
				msglength, scltoken, kvlist );
	}
	return OTF_RETURN_OK;
}


int handleCollectiveOperation( void* firsthandlerarg, uint64_t time, 
    uint32_t process, uint32_t collective, uint32_t communicator, 
    uint32_t rootprocess, uint32_t sent, uint32_t received, 
    uint64_t duration, uint32_t scltoken, OTF_KeyValueList *kvlist ) {
	
	
	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */

	Control* control= (Control*) firsthandlerarg;
	
	while ( control->checkTime( time ) )
		;

	if ( control->doSnapshots && control->aux_state ) {
		OTFAUX_State_processCollectiveOperation( control->aux_state,
		                                         time,
		                                         process,
		                                         communicator,
		                                         rootprocess,
		                                         collective,
		                                         sent,
		                                         received,
		                                         scltoken );
	}

	if ( control->stats )
		control->stats->collOperation( process, rootprocess, collective,
		                               sent, received );

	if ( control->copyEvents )
		return OTF_CopyHandler_CollectiveOperation( control->writer,
				time, process, collective, communicator,
				rootprocess, sent, received,
				duration, scltoken, kvlist );

	return OTF_RETURN_OK;
}


static void processFileOperation( Control* control, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint32_t source, OTF_KeyValueList *kvlist ) {

	if ( control->doSnapshots && control->aux_state ) {

		switch( operation & OTF_FILEOP_BITS ) {

		case OTF_FILEOP_OPEN:
			OTFAUX_State_processFileOpen( control->aux_state,
			                              time,
			                              process,
			                              fileid,
			                              handleid,
			                              source,
			                              OTF_KeyValueList_clone( kvlist ) );
			break;

		case OTF_FILEOP_CLOSE:
			OTFAUX_State_processFileClose( control->aux_state,
			                               time,
			                               process,
			                               handleid );
			break;
		}
	}

	if ( control->stats )
		control->stats->fileOperation( process, fileid, operation, bytes );
}

int handleFileOperation( void* firsthandlerarg, uint64_t time, uint32_t fileid,
	uint32_t process, uint64_t handleid, uint32_t operation, uint64_t bytes,
	uint64_t duration, uint32_t source, OTF_KeyValueList *kvlist ) {


	/* fprintf( stderr, " %llu:  %s\n", (unsigned long long) time, __PRETTY_FUNCTION__ ); */

	Control* control= (Control*) firsthandlerarg;

	while ( control->checkTime( time ) )
		;

        processFileOperation( control, time, fileid, process, handleid,
			operation, bytes, source, kvlist );

	if ( control->copyEvents )
		return OTF_CopyHandler_FileOperation( control->writer,
				time, fileid, process, handleid, operation,
				bytes, duration, source, kvlist );

	return OTF_RETURN_OK;
}


int handleBeginCollectiveOperation( void *firsthandlerarg, uint64_t time, uint32_t process,
    uint32_t collOp, uint64_t matchingId, uint32_t procGroup, uint32_t rootProc,
    uint64_t sent, uint64_t received, uint32_t scltoken, OTF_KeyValueList *kvlist ) {
  
  
    Control* control= (Control*) firsthandlerarg;
        
    while ( control->checkTime( time ) )
        ;
    
    if ( control->doSnapshots && control->aux_state ) {
	OTFAUX_State_processBeginCollectiveOperation( control->aux_state,
	                                              time,
	                                              process,
	                                              procGroup,
	                                              rootProc,
	                                              collOp,
	                                              matchingId,
	                                              sent,
	                                              received,
	                                              scltoken,
	                                              OTF_KeyValueList_clone( kvlist ) );
    }

    if ( control->stats )
	control->stats->beginCollOperation( process, rootProc,
	collOp,matchingId, sent, received );
    
    if ( control->copyEvents )
	return OTF_CopyHandler_BeginCollectiveOperation( control->writer, time,
	                                                 process, collOp,
	                                                 matchingId, procGroup,
	                                                 rootProc, sent,
	                                                 received, scltoken,
	                                                 kvlist );

        return OTF_RETURN_OK;
  
}

int handleEndCollectiveOperation( void *firsthandlerarg, uint64_t time,
    uint32_t process, uint64_t matchingId, OTF_KeyValueList *kvlist ) {
  
    
    Control* control= (Control*) firsthandlerarg;
        
    while ( control->checkTime( time ) )
        ;
    
    if ( control->doSnapshots && control->aux_state ) {
        OTFAUX_State_processEndCollectiveOperation( control->aux_state,
                                                    time,
                                                    process,
                                                    matchingId );
    }

    if ( control->stats )
	control->stats->endCollOperation( process, matchingId );
  
    if ( control->copyEvents )
	return OTF_CopyHandler_EndCollectiveOperation( control->writer, time,
	                                               process, matchingId,
	                                               kvlist );

    return OTF_RETURN_OK;
    
}

int handleBeginFileOperation( void *firsthandlerarg, uint64_t time, uint32_t process,
    uint64_t matchingId, uint32_t scltoken, OTF_KeyValueList *kvlist ) {
  
  
    Control* control= (Control*) firsthandlerarg;
        
    while ( control->checkTime( time ) )
        ;
    
    if ( control->doSnapshots && control->aux_state ) {
        OTFAUX_State_processBeginFileOperation( control->aux_state,
                                                time,
                                                process,
                                                matchingId,
                                                scltoken,
                                                OTF_KeyValueList_clone( kvlist ) );
    }

    if ( control->copyEvents )
        return OTF_CopyHandler_BeginFileOperation( control->writer, time,
                                                   process, matchingId,
                                                   scltoken, kvlist );

    return OTF_RETURN_OK;
}

int handleEndFileOperation( void *firsthandlerarg, uint64_t time, uint32_t process,
    uint32_t fileid, uint64_t matchingId, uint64_t handleId, uint32_t operation,
    uint64_t bytes, uint32_t scltoken, OTF_KeyValueList *kvlist ) {
  
  
    Control* control= (Control*) firsthandlerarg;
       
    while ( control->checkTime( time ) )
        ;
  
    if ( control->doSnapshots && control->aux_state ) {
        OTFAUX_State_processEndFileOperation( control->aux_state,
                                              time,
                                              process,
                                              matchingId );
    }

    processFileOperation( control, time, fileid, process, handleId,
                          operation, bytes, scltoken, kvlist );

    if ( control->copyEvents )
        OTF_CopyHandler_EndFileOperation( control->writer, time, process,
                                          fileid, matchingId, handleId,
                                          operation, bytes, scltoken, kvlist );


    return OTF_RETURN_OK;
}


int handleNoOp( void *firsthandlerarg, uint64_t time, uint32_t process,
		OTF_KeyValueList* list ) {


    Control* control= (Control*) firsthandlerarg;
       
    while ( control->checkTime( time ) )
        ;
  
    if ( control->copyEvents )
	return ( 0 == OTF_Writer_writeNoOpKV( control->writer, time,
		process, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    return OTF_RETURN_OK;
}


int handleEventComment( void *firsthandlerarg, uint64_t time, uint32_t process,
		const char* comment, OTF_KeyValueList* list ) {


    Control* control= (Control*) firsthandlerarg;
       
    while ( control->checkTime( time ) )
        ;
  
    if ( control->copyEvents )
	return ( 0 == OTF_Writer_writeEventCommentKV( control->writer, time,
		process, comment, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    return OTF_RETURN_OK;
}


int handleBeginProcess( void *firsthandlerarg, uint64_t time,
		uint32_t process, OTF_KeyValueList* list ) {


    Control* control= (Control*) firsthandlerarg;
       
    while ( control->checkTime( time ) )
        ;
  
    if ( control->copyEvents )
	return ( 0 == OTF_Writer_writeBeginProcessKV( control->writer, time,
		process, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    return OTF_RETURN_OK;
}


int handleEndProcess( void *firsthandlerarg, uint64_t time,
		uint32_t process, OTF_KeyValueList* list ) {


    Control* control= (Control*) firsthandlerarg;
       
    while ( control->checkTime( time ) )
        ;
  
    if ( control->copyEvents )
	return ( 0 == OTF_Writer_writeEndProcessKV( control->writer, time,
		process, list ) ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    return OTF_RETURN_OK;
}


int handleRMAPut( void *firsthandlerarg, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list ) {


    Control* control= (Control*) firsthandlerarg;
       
    while ( control->checkTime( time ) )
        ;
  
    if ( control->copyEvents )
        return ( 0 == OTF_Writer_writeRMAPutKV( control->writer, time,
                process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    return OTF_RETURN_OK;
}


int handleRMAPutRemoteEnd( void *firsthandlerarg, uint64_t time,
        uint32_t process, uint32_t origin, uint32_t target, uint32_t communicator,
        uint32_t tag, uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list ) {


    Control* control= (Control*) firsthandlerarg;
       
    while ( control->checkTime( time ) )
        ;
  
    if ( control->copyEvents )
        return ( 0 == OTF_Writer_writeRMAPutRemoteEndKV( control->writer,
                time, process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    return OTF_RETURN_OK;
}


int handleRMAGet( void *firsthandlerarg, uint64_t time, uint32_t process,
        uint32_t origin, uint32_t target, uint32_t communicator, uint32_t tag,
        uint64_t bytes, uint32_t scltoken, OTF_KeyValueList* list ) {


    Control* control= (Control*) firsthandlerarg;
       
    while ( control->checkTime( time ) )
        ;
  
    if ( control->copyEvents )
        return ( 0 == OTF_Writer_writeRMAGetKV( control->writer, time,
                process, origin, target, communicator, tag, bytes, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    return OTF_RETURN_OK;
}


int handleRMAEnd( void *firsthandlerarg, uint64_t time, uint32_t process, uint32_t remote,
	uint32_t communicator, uint32_t tag, uint32_t scltoken, OTF_KeyValueList* list ) {


    Control* control= (Control*) firsthandlerarg;
       
    while ( control->checkTime( time ) )
        ;
  
    if ( control->copyEvents )
        return ( 0 == OTF_Writer_writeRMAEndKV( control->writer, time,
                process, remote, communicator, tag, scltoken, list )
                 ) ? OTF_RETURN_ABORT : OTF_RETURN_OK;

    return OTF_RETURN_OK;
}
