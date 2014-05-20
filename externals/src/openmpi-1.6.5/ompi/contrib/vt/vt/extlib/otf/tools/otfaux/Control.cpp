/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#include <cassert>

#include <iostream>
using namespace std;


#include "Control.h"


Control::Control( OTF_Writer* w, OTF_WStream* def_w, bool _verbose, bool _usefunctiongroups,
		bool _usefilegroups, bool _doSnapshots, bool _doThumbnail, bool _doStatistics,
		bool _inlineSnapshots, bool _msgMatching, bool _copyEvents ) :
		aux_state( 0 ), kvlist( 0 ), stats( 0 ),
		collectRecvsOnly( true ), doSnapshots( _doSnapshots ),
		inlineSnapshots( _inlineSnapshots ), msgMatching( _msgMatching ),
		copyEvents( _copyEvents || _inlineSnapshots || _msgMatching ),
		writer( w ), def_wstream( def_w ),
		minTime( 0 ), maxTime( 0 ), haveTimeRange( false ),
		verbose( _verbose ), usefunctiongroups( _usefunctiongroups ),
		recvTimeToken( 0 ), recvLengthToken( 0 ), recvSclToken( 0 ),
		maxKeyToken( 0 ) {


	nextTime= (uint64_t) -1;
	if ( doSnapshots || _doThumbnail || msgMatching ) {
		aux_state= OTFAUX_State_create();
		if ( doSnapshots ) {
			kvlist= OTF_KeyValueList_new();

			OTFAUX_State_setReleaseEventDataCallback( aux_state,
				(OTFAUX_ReleaseEventData)Control::releaseEventData, 0 );

			OTFAUX_State_setWriteEnterSnapshotCallback( aux_state,
				(OTFAUX_WriteEnterSnapshotCallback)Control::writeEnterSnapshot );
			OTFAUX_State_setWriteSendSnapshotCallback( aux_state,
				(OTFAUX_WriteSendSnapshotCallback)Control::writeSendSnapshot );
			OTFAUX_State_setWriteOpenFileSnapshotCallback( aux_state,
				(OTFAUX_WriteOpenFileSnapshotCallback)Control::writeOpenFileSnapshot );
			OTFAUX_State_setWriteBeginCollopSnapshotCallback( aux_state,
				(OTFAUX_WriteBeginCollopSnapshotCallback)Control::writeBeginCollopSnapshot );
			OTFAUX_State_setWriteBeginFileOpSnapshotCallback( aux_state,
				(OTFAUX_WriteBeginFileOpSnapshotCallback)Control::writeBeginFileOpSnapshot );
			OTFAUX_State_setWriteCollopCountSnapshotCallback( aux_state,
				(OTFAUX_WriteCollopCountSnapshotCallback)Control::writeCollopCountSnapshot );
			OTFAUX_State_setWriteCounterSnapshotCallback( aux_state,
				(OTFAUX_WriteCounterSnapshotCallback)Control::writeCounterSnapshot );
		}
		if ( !doSnapshots && !msgMatching ) {
			collectRecvsOnly= false;
		}
	}
	if ( _doStatistics ) {
		stats= new Stats( _usefunctiongroups, _usefilegroups );
	}
}


Control::~Control() {

    if ( aux_state )
        OTFAUX_State_destroy( aux_state );
    if ( kvlist )
        OTF_KeyValueList_close( kvlist );
    if ( stats )
        delete stats;
}


/** add time stamp where to generate a snapshot */
void Control::addTime( uint64_t time ) {
 
	timestamps.insert( time );
	
	nextTime= *( timestamps.begin() );
}


uint64_t Control::getLastTime() {

    if ( ! timestamps.empty() ) {

        return *( timestamps.rbegin() );

    } else {

        return (uint64_t) -1;
    }
}


bool Control::checkTime( uint64_t time ) {


	if ( time >= nextTime ) {
	

		if ( verbose ) {
		
			cerr << hex << "[" << time << " >= " << nextTime << "]" << dec << endl;
		}
	
		//stats->printStatistics( time );
		if ( stats )
		    stats->writeStatistics( writer, def_wstream, nextTime );

		/* write snapshot only when its not the very end of the trace */
		if ( doSnapshots && aux_state && nextTime != getLastTime() ) {

			//state->printStack();
			//state->printSends();
			//state->printOpenFiles();
			/* write the aux sample point to the definitions */
			OTF_WStream_writeDefAuxSamplePoint( def_wstream,
			                                    nextTime,
			                                    inlineSnapshots
			                                    ? OTF_AUX_SAMPLE_POINT_INLINE_SNAPSHOT
			                                    : OTF_AUX_SAMPLE_POINT_SNAPSHOT,
			                                    NULL );
			OTFAUX_State_writeSnapshot( aux_state, nextTime, this );
		}


		timestamps.erase( nextTime );

		if ( ! timestamps.empty() ) {

			nextTime= *( timestamps.begin() );

		} else {

			nextTime= (uint64_t) -1;
		}
		
		return true;
	}
	
	return false;
}

void Control::releaseEventData( void* userData,
                                void* eventData ) {

    if ( eventData )
        OTF_KeyValueList_close( ( OTF_KeyValueList* )eventData );
}

int
Control::writeEnterSnapshot( void*    userData,
                             uint64_t snapshotTime,
                             uint64_t eventTime,
                             uint64_t processId,
                             uint32_t function,
                             uint32_t scl,
                             void*    eventData ) {

    Control *control= ( Control* )userData;

    /* make a copy of the key-value list to keep the data */
    if ( eventData )
        OTF_KeyValueList_appendKeyValueList( control->kvlist,
                                             ( OTF_KeyValueList* )eventData );

    /* this will reset the key-value list */
    OTF_Writer_writeEnterSnapshotKV( control->writer,
                                     snapshotTime,
                                     eventTime,
                                     function,
                                     processId,
                                     scl,
                                     control->kvlist );

    return 1;
}

int
Control::writeSendSnapshot( void*    userData,
                            uint64_t snapshotTime,
                            uint64_t eventTime,
                            uint64_t senderProcessId,
                            uint64_t receiverProcessId,
                            uint32_t comm,
                            uint32_t tag,
                            uint32_t length,
                            uint32_t scl,
                            uint64_t recvTime,
                            uint32_t recvLength,
                            uint32_t recvScl,
                            void*    eventData ) {

    Control *control= ( Control* )userData;


    /* make a copy of the key-value list to keep the data */
    if ( eventData )
        OTF_KeyValueList_appendKeyValueList( control->kvlist,
                                             ( OTF_KeyValueList* )eventData );

    if ( control->msgMatching ) {
        OTF_KeyValueList_removeKey( control->kvlist, control->recvTimeToken );
        OTF_KeyValueList_appendUint64( control->kvlist,
                                       control->recvTimeToken,
                                       recvTime );
        OTF_KeyValueList_removeKey( control->kvlist, control->recvLengthToken );
        OTF_KeyValueList_appendUint32( control->kvlist,
                                       control->recvLengthToken,
                                       recvLength );
        OTF_KeyValueList_removeKey( control->kvlist, control->recvSclToken );
        OTF_KeyValueList_appendUint32( control->kvlist,
                                       control->recvSclToken,
                                       recvScl );
    }

    /* this will reset the key-value list */
    OTF_Writer_writeSendSnapshotKV( control->writer,
                                    snapshotTime,
                                    eventTime,
                                    senderProcessId,
                                    receiverProcessId,
                                    comm,
                                    tag,
                                    length,
                                    scl,
                                    control->kvlist );

    return 1;
}

int
Control::writeOpenFileSnapshot( void*    userData,
                                uint64_t snapshotTime,
                                uint64_t eventTime,
                                uint64_t processId,
                                uint32_t fileId,
                                uint64_t handleId,
                                uint32_t scl,
                                void*    eventData ) {

    Control *control= ( Control* )userData;

    /* make a copy of the key-value list to keep the data */
    if ( eventData )
        OTF_KeyValueList_appendKeyValueList( control->kvlist,
                                             ( OTF_KeyValueList* )eventData );

    /* this will reset the key-value list */
    OTF_Writer_writeOpenFileSnapshotKV( control->writer,
                                        snapshotTime,
                                        eventTime,
                                        fileId,
                                        processId,
                                        handleId,
                                        scl,
                                        control->kvlist );

    return 1;
}

int
Control::writeBeginCollopSnapshot( void*    userData,
                                   uint64_t snapshotTime,
                                   uint64_t eventTime,
                                   uint64_t processId,
                                   uint32_t collOp,
                                   uint64_t matchingId,
                                   uint32_t comm,
                                   uint32_t root,
                                   uint64_t sent,
                                   uint64_t received,
                                   uint32_t scl,
                                   void*    eventData ) {

    Control *control= ( Control* )userData;

    /* make a copy of the key-value list to keep the data */
    if ( eventData )
        OTF_KeyValueList_appendKeyValueList( control->kvlist,
                                             ( OTF_KeyValueList* )eventData );

    OTF_Writer_writeBeginCollopSnapshotKV( control->writer,
                                           snapshotTime,
                                           eventTime,
                                           processId,
                                           collOp,
                                           matchingId,
                                           comm,
                                           root,
                                           sent,
                                           received,
                                           scl,
                                           control->kvlist );

    return 1;
}

int
Control::writeBeginFileOpSnapshot( void*    userData,
                                   uint64_t snapshotTime,
                                   uint64_t eventTime,
                                   uint64_t processId,
                                   uint64_t matchingId,
                                   uint32_t scl,
                                   void*    eventData ) {

    Control *control= ( Control* )userData;

    /* make a copy of the key-value list to keep the data */
    if ( eventData )
        OTF_KeyValueList_appendKeyValueList( control->kvlist,
                                             ( OTF_KeyValueList* )eventData );

    /* this will reset the key-value list */
    OTF_Writer_writeBeginFileOpSnapshotKV( control->writer,
                                           snapshotTime,
                                           eventTime,
                                           processId,
                                           matchingId,
                                           scl,
                                           control->kvlist );

    return 1;
}

int
Control::writeCollopCountSnapshot( void*    userData,
                                   uint64_t snapshotTime,
                                   uint64_t processId,
                                   uint32_t comm,
                                   uint64_t count ) {

    Control *control= ( Control* )userData;

    OTF_Writer_writeCollopCountSnapshot( control->writer,
                                         snapshotTime,
                                         processId,
                                         comm,
                                         count,
                                         NULL );

    return 1;
}

int
Control::writeCounterSnapshot( void*    userData,
                               uint64_t snapshotTime,
                               uint64_t eventTime,
                               uint64_t processId,
                               uint32_t counter,
                               uint64_t value,
                               void*    eventData ) {

    Control *control= ( Control* )userData;

    /* make a copy of the key-value list to keep the data */
    if ( eventData )
        OTF_KeyValueList_appendKeyValueList( control->kvlist,
                                             ( OTF_KeyValueList* )eventData );

    /* this will reset the key-value list */
    OTF_Writer_writeCounterSnapshot( control->writer,
        snapshotTime,
        eventTime,
        processId,
        counter,
        value,
        control->kvlist );

    return 1;
}

