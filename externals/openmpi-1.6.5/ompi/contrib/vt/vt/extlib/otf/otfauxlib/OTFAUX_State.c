#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdarg.h>

#include <otf.h>

#include <jenkins_hash.h>

#include "OTFAUX_State.h"
#include "OTFAUX_Process.h"

#include "OTFAUX_SharedState.h"

#include "OTFAUX_Thumbnail.h"

#define PROCESSES_HASH_SHIFT 10
#define PROCESSES_HASH_SIZE  (1 << PROCESSES_HASH_SHIFT)
#define PROCESSES_HASH_MASK  (PROCESSES_HASH_SIZE - 1)

struct OTFAUX_State
{
    /** The processes */
    OTFAUX_Process* processes[ PROCESSES_HASH_SIZE ];

    /** sampling timestamps */
    uint32_t timestampsSize, timestampsPosition;
    uint64_t* timestamps;

    OTFAUX_SharedState sharedState;
};


OTFAUX_State*
OTFAUX_State_create( void )
{
    OTFAUX_State* new_state = NULL;

    /* Please unused funciton warning */
    ( void )stack_next;
    ( void )stack_prev;
    ( void )stack_empty;
    ( void )stack_push;
    ( void )stack_add;

    new_state = calloc( 1, sizeof( *new_state ) );
    if ( !new_state )
        return NULL;

    stack_init( &new_state->sharedState.functionCalls );
    stack_init( &new_state->sharedState.files );
    stack_init( &new_state->sharedState.collOps );
    stack_init( &new_state->sharedState.fileOps );

    return new_state;
}


void
OTFAUX_State_destroy( OTFAUX_State* auxState )
{
    int i;
    Stack* entry;

    for ( i = 0; i < PROCESSES_HASH_SIZE; i++ )
    {
        while ( auxState->processes[ i ] )
        {
            OTFAUX_Process* next = auxState->processes[ i ]->next;
            OTFAUX_Process_destroy( auxState->processes[ i ] );
            auxState->processes[ i ] = next;
        }
    }

    while ( ( entry = stack_pop( &auxState->sharedState.functionCalls ) ) )
    {
        /* entries do not have eventData attached */
        free( entry );
    }

    while ( ( entry = stack_pop( &auxState->sharedState.files ) ) )
    {
        /* entries do not have eventData attached */
        free( entry );
    }

    while ( ( entry = stack_pop( &auxState->sharedState.collOps ) ) )
    {
        /* entries do not have eventData attached */
        free( entry );
    }

    while ( ( entry = stack_pop( &auxState->sharedState.fileOps ) ) )
    {
        /* entries do not have eventData attached */
        free( entry );
    }

    free( auxState->timestamps );
    free( auxState );
}


int
OTFAUX_State_setupThumbnail( OTFAUX_State* auxState,
                             uint64_t      minTime,
                             uint64_t      maxTime,
                             uint32_t      width )
{
    uint64_t timeDiff;
    double timeStep;
    uint32_t i;

    if ( width == 0 || minTime >= maxTime || ( maxTime - minTime ) < width )
        return 0;

    auxState->timestamps = calloc( width, sizeof( uint64_t ) );
    if ( !auxState->timestamps )
    {
        return 0;
    }

    /* callculate sample time stamps */
    timeDiff = maxTime - minTime;
    timeStep = ( double )timeDiff / ( double )width;

    auxState->timestampsSize = width;
    auxState->timestampsPosition = 0;

    for ( i = 0; i < width; ++i )
    {
        auxState->timestamps[ i ] = minTime + i * timeStep;
    }

    return 1;
}


static OTFAUX_Process*
get_process( OTFAUX_State* auxState, uint64_t processId )
{
    uint32_t process_hash;
    OTFAUX_Process** process_bucket;
    OTFAUX_Process*  process;

    process_hash = hash( &processId, sizeof( processId ), 0 );
    process_bucket = &auxState->processes[ process_hash & PROCESSES_HASH_MASK ];
    process = *process_bucket;

    /* search in hash chain */
    while ( process )
    {
        if ( process->id == processId )
        {
            /* found, is this an error? */
            return process;
        }

        process = process->next;
    }

    /* create new process */
    process = OTFAUX_Process_create( processId, &auxState->sharedState );
    if ( !process )
        return NULL;

    /* chain into hash table */
    process->next = *process_bucket;
    *process_bucket = process;

    return process;
}


static void
update_thumbnail( OTFAUX_State* auxState, uint64_t timestamp )
{
    if ( !auxState || !auxState->timestamps )
        return;

    if ( auxState->timestampsPosition == auxState->timestampsSize )
        return;

    while ( auxState->timestampsPosition < auxState->timestampsSize
           && auxState->timestamps[ auxState->timestampsPosition ] < timestamp )
    {
        int i;
        for ( i = 0; i < PROCESSES_HASH_SIZE; i++ )
        {
            OTFAUX_Process* process = auxState->processes[i];
            while ( process )
            {
                OTFAUX_Process_updateThumbnail( process,
                                                auxState->timestampsPosition );
                process = process->next;
            }
        }
        auxState->timestampsPosition++;
    }
}


int
OTFAUX_State_declareProcess( OTFAUX_State* auxState,
                             uint64_t      processId,
                             int           isThumbnailProcess )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, processId );
    if ( !process )
        return 0;

    if ( isThumbnailProcess )
        return OTFAUX_Process_enableThumbnail( process,
                                               auxState->timestampsSize );

    return 1;
}


int
OTFAUX_State_setReleaseEventDataCallback( OTFAUX_State*           auxState,
                                          OTFAUX_ReleaseEventData releaseEventDataCallback,
                                          void*                   userData )
{
    if ( !auxState )
        return 0;

    auxState->sharedState.releaseEventData = releaseEventDataCallback;
    auxState->sharedState.userDataForReleaseEventData = userData;

    return 1;
}


int
OTFAUX_State_setWriteEnterSnapshotCallback( OTFAUX_State* auxState,
                                            OTFAUX_WriteEnterSnapshotCallback writeEnterSnapshotCallback )
{
    if ( !auxState )
        return 0;

    auxState->sharedState.writeEnterSnapshot = writeEnterSnapshotCallback;

    return 1;
}


int
OTFAUX_State_setWriteSendSnapshotCallback( OTFAUX_State* auxState,
                                           OTFAUX_WriteSendSnapshotCallback writeSendSnapshotCallback )
{
    if ( !auxState )
        return 0;

    auxState->sharedState.writeSendSnapshot = writeSendSnapshotCallback;

    return 1;
}


int
OTFAUX_State_setWriteOpenFileSnapshotCallback( OTFAUX_State* auxState,
                                               OTFAUX_WriteOpenFileSnapshotCallback writeOpenFileSnapshotCallback )
{
    if ( !auxState )
        return 0;

    auxState->sharedState.writeOpenFileSnapshot = writeOpenFileSnapshotCallback;

    return 1;
}


int
OTFAUX_State_setWriteBeginCollopSnapshotCallback( OTFAUX_State* auxState,
                                                  OTFAUX_WriteBeginCollopSnapshotCallback writeBeginCollopSnapshotCallback )
{
    if ( !auxState )
        return 0;

    auxState->sharedState.writeBeginCollopSnapshot = writeBeginCollopSnapshotCallback;

    return 1;
}


int
OTFAUX_State_setWriteBeginFileOpSnapshotCallback( OTFAUX_State* auxState,
                                                  OTFAUX_WriteBeginFileOpSnapshotCallback writeBeginFileOpSnapshotCallback )
{
    if ( !auxState )
        return 0;

    auxState->sharedState.writeBeginFileOpSnapshot = writeBeginFileOpSnapshotCallback;

    return 1;
}


int
OTFAUX_State_setWriteCollopCountSnapshotCallback( OTFAUX_State* auxState,
                                                  OTFAUX_WriteCollopCountSnapshotCallback writeCollopCountSnapshotCallback )
{
    if ( !auxState )
        return 0;

    auxState->sharedState.writeCollopCountSnapshot = writeCollopCountSnapshotCallback;

    return 1;
}


int
OTFAUX_State_setWriteCounterSnapshotCallback( OTFAUX_State* auxState,
                                              OTFAUX_WriteCounterSnapshotCallback writeCounterSnapshotCallback )
{
    if ( !auxState )
        return 0;

    auxState->sharedState.writeCounterSnapshot = writeCounterSnapshotCallback;

    return 1;
}


int
OTFAUX_State_enqueueRecvMsg( OTFAUX_State* auxState,
                             uint64_t      eventTime,
                             uint64_t      receiverProcessId,
                             uint64_t      senderProcessId,
                             uint32_t      comm,
                             uint32_t      tag,
                             uint32_t      length,
                             uint32_t      scl )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, senderProcessId );
    if ( !process )
        return 0;

    return OTFAUX_Process_enqueueRecv( process,
                                       eventTime,
                                       receiverProcessId,
                                       comm,
                                       tag,
                                       length,
                                       scl );
}


int
OTFAUX_State_processEnter( OTFAUX_State* auxState,
                           uint64_t      eventTime,
                           uint64_t      processId,
                           uint32_t      function,
                           uint32_t      scl,
                           void*         eventData )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, processId );

    if ( !process )
        return 0;

    update_thumbnail( auxState, eventTime );

    return OTFAUX_Process_enterFunction( process,
                                         eventTime,
                                         function,
                                         scl,
                                         eventData );
}


int
OTFAUX_State_processLeave( OTFAUX_State* auxState,
                           uint64_t      eventTime,
                           uint64_t      processId,
                           uint32_t      function )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, processId );
    if ( !process )
        return 0;

    update_thumbnail( auxState, eventTime );

    return OTFAUX_Process_leaveFunction( process,
                                         eventTime,
                                         function );
}


int
OTFAUX_State_processSendMsg( OTFAUX_State* auxState,
                             uint64_t      eventTime,
                             uint64_t      senderProcessId,
                             uint64_t      receiverProcessId,
                             uint32_t      comm,
                             uint32_t      tag,
                             uint32_t      length,
                             uint32_t      scl,
                             uint64_t*     recvTime,
                             uint32_t*     recvLength,
                             uint32_t*     recvScl,
                             void*         eventData )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, senderProcessId );
    if ( !process )
        return 0;

    return OTFAUX_Process_sendMessage( process,
                                       eventTime,
                                       receiverProcessId,
                                       comm,
                                       tag,
                                       length,
                                       scl,
                                       recvTime,
                                       recvLength,
                                       recvScl,
                                       eventData );
}


int
OTFAUX_State_processBeginCollectiveOperation( OTFAUX_State* auxState,
                                              uint64_t      eventTime,
                                              uint64_t      processId,
                                              uint32_t      comm,
                                              uint32_t      root,
                                              uint32_t      collOp,
                                              uint64_t      matchingId,
                                              uint64_t      bytesSent,
                                              uint64_t      bytesRecv,
                                              uint32_t      scl,
                                              void*         eventData )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, processId );
    if ( !process )
        return 0;

    return OTFAUX_Process_beginCollOp( process,
                                       eventTime,
                                       comm,
                                       root,
                                       collOp,
                                       matchingId,
                                       bytesSent,
                                       bytesRecv,
                                       scl,
                                       eventData );
}


int
OTFAUX_State_processEndCollectiveOperation( OTFAUX_State* auxState,
                                            uint64_t eventTime,
                                            uint64_t processId,
                                            uint64_t matchingId )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, processId );
    if ( !process )
        return 0;

    return OTFAUX_Process_endCollOp( process,
                                     eventTime,
                                     matchingId );
}


int
OTFAUX_State_processCollectiveOperation( OTFAUX_State* auxState,
                                         uint64_t eventTime,
                                         uint64_t processId,
                                         uint32_t comm,
                                         uint32_t root,
                                         uint32_t collOp,
                                         uint64_t bytesSent,
                                         uint64_t bytesRecv,
                                         uint32_t scl )
{
    OTFAUX_Process* process;

    ( void )eventTime;
    ( void )processId;
    ( void )root;
    ( void )collOp;
    ( void )bytesSent;
    ( void )bytesRecv;
    ( void )scl;

    if ( !auxState )
        return 0;

    process = get_process( auxState, processId );
    if ( !process )
        return 0;

    return OTFAUX_Process_countCollOp( process,
                                       comm );
}


int
OTFAUX_State_processFileOpen( OTFAUX_State* auxState,
                              uint64_t      eventTime,
                              uint64_t      processId,
                              uint32_t      fileId,
                              uint64_t      handleId,
                              uint32_t      scl,
                              void*         eventData )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, processId );
    if ( !process )
        return 0;

    return OTFAUX_Process_openFile( process,
                                    eventTime,
                                    fileId,
                                    handleId,
                                    scl,
                                    eventData );
}


int
OTFAUX_State_processFileClose( OTFAUX_State* auxState,
                               uint64_t      eventTime,
                               uint64_t      processId,
                               uint64_t      handleId )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, processId );
    if ( !process )
        return 0;

    return OTFAUX_Process_closeFile( process,
                                     eventTime,
                                     handleId );
}


int
OTFAUX_State_processBeginFileOperation( OTFAUX_State* auxState,
                                        uint64_t      eventTime,
                                        uint64_t      processId,
                                        uint64_t      matchingId,
                                        uint32_t      scl,
                                        void*         eventData )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, processId );
    if ( !process )
        return 0;

    return OTFAUX_Process_beginFileOp( process,
                                       eventTime,
                                       matchingId,
                                       scl,
                                       eventData );
}


int
OTFAUX_State_processEndFileOperation( OTFAUX_State* auxState,
                                      uint64_t      eventTime,
                                      uint64_t      processId,
                                      uint64_t      matchingId )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, processId );
    if ( !process )
        return 0;

    return OTFAUX_Process_endFileOp( process,
                                     eventTime,
                                     matchingId );
}


int
OTFAUX_State_processCounter( OTFAUX_State* auxState,
                             uint64_t      eventTime,
                             uint64_t      processId,
                             uint32_t      counterId,
                             uint64_t      value,
                             void*         eventData )
{
    OTFAUX_Process* process;

    if ( !auxState )
        return 0;

    process = get_process( auxState, processId );
    if ( !process )
        return 0;

    return OTFAUX_Process_updateCounter( process,
                                         eventTime,
                                         counterId,
                                         value,
                                         eventData );
}


int
OTFAUX_State_writeSnapshot( OTFAUX_State* auxState,
                            uint64_t      snapshotTime,
                            void*         userData )
{
    int i, ret = 1;

    for ( i = 0; ret && i < PROCESSES_HASH_SIZE; i++ )
    {
        OTFAUX_Process* process = auxState->processes[ i ];
        while ( ret && process )
        {
            ret = ret && OTFAUX_Process_writeStack( process,
                                                    snapshotTime,
                                                    userData );
            ret = ret && OTFAUX_Process_writeSends( process,
                                                    snapshotTime,
                                                    userData );
            ret = ret && OTFAUX_Process_writeOpenFiles( process,
                                                        snapshotTime,
                                                        userData );
            ret = ret && OTFAUX_Process_writeCollOps( process,
                                                      snapshotTime,
                                                      userData );
            ret = ret && OTFAUX_Process_writeFileOps( process,
                                                      snapshotTime,
                                                      userData );
            ret = ret && OTFAUX_Process_writeCounters( process,
                                                       snapshotTime,
                                                       userData );

            process = process->next;
        }
    }

    return ret;
}


int
OTFAUX_State_writeThumbnail( OTFAUX_State* auxState,
                             const char*   namestub,
                             int           create,
                             ... )
{
    int ret = 1;
    char* filename;
    FILE* file;
    int i;

    if ( !namestub || !auxState || !auxState->timestamps )
        return 0;

    /* finalize thumbnail sample points with one past the last timestamp */
    update_thumbnail( auxState,
                      auxState->timestamps[ auxState->timestampsSize - 1 ] + 1 );

    filename = OTFAUX_Thumbnail_getFilename( namestub );
    if ( !filename )
        return 0;

    file = fopen( filename, create ? "w" : "a" );
    free( filename );
    if ( !file )
    {
        return 0;
    }

    /* write header */
    if ( create ) {
        uint32_t total_number_of_procs;
        va_list args;

        va_start( args, create );
        total_number_of_procs = va_arg( args, uint32_t );
        va_end( args );
        fprintf( file, "0:%x,%x\n",
                 auxState->timestampsSize,
                 total_number_of_procs );
    }
 
    /* write processes */
    for ( i = 0; i < PROCESSES_HASH_SIZE; i++ )
    {
        OTFAUX_Process* process = auxState->processes[ i ];
        while ( process )
        {
            OTFAUX_Process_writeThumbnail( process,
                                           auxState->timestampsSize,
                                           file );
            process = process->next;
        }
    }
    ret = !ferror( file );

    fclose( file );

    return ret;
}
