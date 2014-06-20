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

#include "OTFAUX_Stack.h"

typedef struct OTFAUX_FunctionCall
{
    Stack e;
    uint64_t eventTime;
    uint32_t function;
    uint32_t scl;
    void* eventData;
} OTFAUX_FunctionCall;

struct OTFAUX_ReciveQueue
{
    /** for hash chaining */
    OTFAUX_ReciveQueue* next;

    /** cached hash value of this queue */
    uint32_t hash;

    uint64_t receiver;
    uint32_t comm, tag;

    Stack receives;
};

typedef struct OTFAUX_Message
{
    Stack e;
    uint64_t eventTime;
    uint64_t receiver;
    uint32_t comm;
    uint32_t tag;
    uint32_t length;
    uint32_t scl;
    uint64_t recvTime;
    uint32_t recvLength;
    uint32_t recvScl;
    void* eventData;
} OTFAUX_Message;

typedef struct OTFAUX_CollOp
{
    Stack e;
    uint64_t eventTime;
    uint32_t comm;
    uint32_t root;
    uint32_t collOp;
    uint64_t matchingId;
    uint64_t bytesSent;
    uint64_t bytesRecv;
    uint32_t scl;
    void* eventData;
} OTFAUX_CollOp;

typedef struct OTFAUX_File
{
    Stack e;
    uint64_t eventTime;
    uint32_t id;
    uint64_t handleId;
    uint32_t scl;
    void* eventData;
} OTFAUX_File;

typedef struct OTFAUX_FileOp
{
    Stack e;
    uint64_t eventTime;
    uint64_t matchingId;
    uint32_t scl;
    void* eventData;
} OTFAUX_FileOp;

struct OTFAUX_CollOpCount
{
    OTFAUX_CollOpCount* next;
    uint32_t comm;
    uint64_t count;
};

struct OTFAUX_Counter
{
    OTFAUX_Counter* next;
    uint64_t eventTime;
    uint32_t id;
    uint64_t value;
    void* eventData;
};

static uint32_t
hash_queue( uint64_t receiver, uint32_t comm, uint32_t tag )
{
    uint32_t queue_hash = 0;

    queue_hash += hash( &receiver, sizeof( receiver ), queue_hash );
    queue_hash += hash( &comm, sizeof( comm ), queue_hash );
    queue_hash += hash( &tag, sizeof( tag ), queue_hash );

    return queue_hash;
}


static OTFAUX_ReciveQueue*
create_queue( uint32_t queue_hash,
              uint64_t receiver,
              uint32_t comm,
              uint32_t tag )
{
    OTFAUX_ReciveQueue* new_queue = calloc( 1, sizeof( *new_queue ) );

    if ( !new_queue )
        return NULL;

    /* cache hash value for this queue */
    new_queue->hash = queue_hash;

    /* store queue atttributes */
    new_queue->receiver = receiver;
    new_queue->comm = comm;
    new_queue->tag = tag;

    /* initialize the recv queue */
    stack_init( &new_queue->receives );

    return new_queue;
}


static OTFAUX_ReciveQueue*
get_queue( OTFAUX_Process* process,
           uint64_t receiver,
           uint32_t comm,
           uint32_t tag,
           int create )
{
    uint32_t queue_hash = hash_queue( receiver, comm, tag );
    OTFAUX_ReciveQueue** queue_bucket = &process->receiveQueues[ queue_hash & QUEUE_HASH_MASK ];
    OTFAUX_ReciveQueue* queue = *queue_bucket;

    /* search in hash chain */
    while ( queue )
    {
        if ( queue->hash == queue_hash
             && queue->receiver == receiver
             && queue->comm == comm
             && queue->tag == tag )
        {
            /* found */
            return queue;
        }

        queue = queue->next;
    }

    if ( !create )
        return queue;

    queue = create_queue( queue_hash, receiver, comm, tag );
    if ( !queue )
        return NULL;

    /* chain into hash table */
    queue->next = *queue_bucket;
    *queue_bucket = queue;

    return queue;
}


static void
release_event_data( OTFAUX_Process* process, void* eventData )
{
    if ( !eventData
            || !process
            || !process->sharedState
            || !process->sharedState->releaseEventData )
        return;

    process->sharedState->releaseEventData(
            process->sharedState->userDataForReleaseEventData,
            eventData );
}


static void
cleanup_pending_sends( OTFAUX_Process* process, uint64_t time )
{
    Stack* elem;

    if ( !process )
        return;

    elem = stack_next( &process->pendingSends );
    while ( elem != &process->pendingSends )
    {
        OTFAUX_Message* msg = ( OTFAUX_Message* )elem;
        elem = stack_next( elem );

        if ( msg->recvTime < time )
        {
            stack_remove( &msg->e );
            release_event_data( process, msg->eventData );
            free( msg );
        }
    }
}


OTFAUX_Process*
OTFAUX_Process_create( uint64_t processId,
                       OTFAUX_SharedState* sharedState )
{
    OTFAUX_Process* new_process = calloc( 1, sizeof( *new_process ) );
    if ( !new_process )
        return NULL;

    new_process->id = processId;

    stack_init( &new_process->functionStack );
    stack_init( &new_process->pendingSends );
    stack_init( &new_process->openFiles );
    stack_init( &new_process->pendingCollOps );
    stack_init( &new_process->pendingFileOps );

    new_process->sharedState = sharedState;

    return new_process;
}


void
OTFAUX_Process_destroy( OTFAUX_Process* process )
{
    int i;
    OTFAUX_FunctionCall* call;
    OTFAUX_Message* msg;
    OTFAUX_File* file;
    OTFAUX_CollOp* collop;
    OTFAUX_FileOp* fileop;

    if ( !process )
        return;

    while ( ( call = ( OTFAUX_FunctionCall* )stack_pop( &process->functionStack ) ) )
    {
        release_event_data( process, call->eventData );
        free( call );
    }

    for ( i = 0; i < QUEUE_HASH_SIZE; i++ )
    {
        while ( process->receiveQueues[ i ] )
        {
            OTFAUX_ReciveQueue* queue = process->receiveQueues[ i ];
            process->receiveQueues[ i ] = queue->next;

            while ( ( msg = ( OTFAUX_Message* )stack_pop( &queue->receives ) ) )
            {
                free( msg );
            }
            free( queue );
        }
    }

    while ( ( msg = ( OTFAUX_Message* )stack_pop( &process->pendingSends ) ) )
    {
        release_event_data( process, msg->eventData );
        free( msg );
    }

    while ( ( file = ( OTFAUX_File* )stack_pop( &process->openFiles ) ) )
    {
        release_event_data( process, file->eventData );
        free( file );
    }

    while ( ( collop = ( OTFAUX_CollOp* )stack_pop( &process->pendingCollOps ) ) )
    {
        release_event_data( process, collop->eventData );
        free( collop );
    }

    for ( i = 0; i < COLLOPCOUNT_HASH_SIZE; i++ )
    {
        while ( process->collOpCounts[ i ] )
        {
            OTFAUX_CollOpCount* collop_count = process->collOpCounts[ i ];
            process->collOpCounts[ i ] = collop_count->next;
            free( collop_count );
        }
    }

    while ( ( fileop = ( OTFAUX_FileOp* )stack_pop( &process->pendingFileOps ) ) )
    {
        release_event_data( process, fileop->eventData );
        free( fileop );
    }

    for ( i = 0; i < COUNTER_HASH_SIZE; i++ )
    {
        while ( process->counters[ i ] )
        {
            OTFAUX_Counter* counter = process->counters[ i ];
            process->counters[ i ] = counter->next;
            release_event_data( process, counter->eventData );
            free( counter );
        }
    }

    if ( process->thumbnail )
        free( process->thumbnail );

    free( process );
}


int
OTFAUX_Process_enableThumbnail( OTFAUX_Process* process,
                                uint32_t        thumbnailSize )
{
    if ( !process )
        return 0;

    if ( process->thumbnail )
        return 1;

    process->thumbnail = calloc( thumbnailSize, sizeof( *process->thumbnail ) );
    if ( !process->thumbnail )
        return 0;

    return 1;
}


void
OTFAUX_Process_updateThumbnail( OTFAUX_Process* process,
                                uint32_t timestampsPosition )
{
    OTFAUX_FunctionCall* call;

    if ( !process || !process->thumbnail )
        return;

    /* function stack is empty, keep zero, which indicates the invalid functions */
    if ( stack_empty( &process->functionStack ) )
        return;

    call = ( OTFAUX_FunctionCall* )stack_next( &process->functionStack );
    process->thumbnail[ timestampsPosition ] = call->function;
}


void
OTFAUX_Process_writeThumbnail( OTFAUX_Process* process,
                               uint32_t timestampsSize,
                               FILE* out )
{
    uint32_t i;

    if ( !process || !process->thumbnail )
        return;

    fprintf( out, "%llx:", ( unsigned long long )process->id );

    for ( i = 0; i < timestampsSize; i++ )
    {
        fprintf( out, "%x,", process->thumbnail[ i ] );
    }
    fprintf( out, "\n" );
}


int
OTFAUX_Process_enqueueRecv( OTFAUX_Process* process,
                            uint64_t eventTime,
                            uint32_t receiverProcessId,
                            uint32_t comm,
                            uint32_t tag,
                            uint32_t length,
                            uint32_t scl )
{
    OTFAUX_ReciveQueue* queue;
    OTFAUX_Message* recv;

    if ( !process )
        return 0;

    queue = get_queue( process, receiverProcessId, comm, tag, 1 );
    if ( !queue )
        return 0;

    recv = calloc( 1, sizeof( *recv ) );
    if ( !recv )
        return 0;

    recv->receiver = receiverProcessId;
    recv->comm = comm;
    recv->tag = tag;
    recv->recvTime = eventTime;
    recv->recvLength = length;
    recv->recvScl = scl;
    
    stack_init( &recv->e );
    stack_add( &queue->receives, &recv->e );

    return 1;
}


int
OTFAUX_Process_enterFunction( OTFAUX_Process* process,
                              uint64_t eventTime,
                              uint32_t function,
                              uint32_t scl,
                              void* eventData )
{
    OTFAUX_FunctionCall* call;

    if ( !process )
        return 0;

    if ( !stack_empty( &process->sharedState->functionCalls ) )
    {
        /* take it out of the object pool */
        call = ( OTFAUX_FunctionCall* )stack_pop( &process->sharedState->functionCalls );
    }
    else
    {
        call = calloc( 1, sizeof( *call ) );
        if ( !call )
            return 0;
        stack_init( &call->e );
    }

    call->eventTime = eventTime;
    call->function = function;
    call->scl = scl;
    call->eventData = eventData;

    stack_push( &process->functionStack, &call->e );

    return 1;
}


int
OTFAUX_Process_leaveFunction( OTFAUX_Process* process,
                              uint64_t        eventTime,
                              uint32_t        function )
{
    OTFAUX_FunctionCall* call;

    ( void )function;

    if ( !process )
        return 0;

    if ( stack_empty( &process->functionStack ) )
        return 0;

    call = ( OTFAUX_FunctionCall* )stack_pop( &process->functionStack );

    release_event_data( process, call->eventData );
    stack_push( &process->sharedState->functionCalls, &call->e );

    return 1;
}


int
OTFAUX_Process_sendMessage( OTFAUX_Process* process,
                            uint64_t eventTime,
                            uint32_t receiverProcessId,
                            uint32_t comm,
                            uint32_t tag,
                            uint32_t length,
                            uint32_t scl,
                            uint64_t* recvTime,
                            uint32_t* recvLength,
                            uint32_t* recvScl,
                            void* eventData )
{
    OTFAUX_ReciveQueue* queue;
    OTFAUX_Message* msg;

    if ( !process )
        return 0;

    /* MsgMatching */
    queue = get_queue( process, receiverProcessId, comm, tag, 0 );
    if ( !queue )
        return 2;

    if ( stack_empty( &queue->receives ) )
        return 2;

    msg = ( OTFAUX_Message* )stack_pop( &queue->receives );
    msg->eventTime = eventTime;
    msg->length = length;
    msg->scl = scl;
    *recvTime = msg->recvTime;
    *recvLength = msg->recvLength;
    *recvScl = msg->recvScl;
    msg->eventData = eventData;

    /* only maintain the pending messages, if we want to write snapshots */
    if ( process->sharedState->writeSendSnapshot )
    {
        stack_add( &process->pendingSends, &msg->e );
    }
    else
    {
        release_event_data( process, msg->eventData );
        free( msg );
    }

    return 1;
}


int
OTFAUX_Process_countCollOp( OTFAUX_Process* process,
                            uint32_t comm )
{
    uint32_t comm_hash;
    OTFAUX_CollOpCount* collop_count;
    OTFAUX_CollOpCount** bucket;

    if ( !process )
        return 0;

    comm_hash = hash( &comm, sizeof( comm ), 0 );
    bucket = &process->collOpCounts[ comm_hash & COLLOPCOUNT_HASH_MASK ];

    collop_count = *bucket;
    while ( collop_count )
    {
        if ( collop_count->comm == comm )
        {
            break;
        }

        collop_count = collop_count->next;
    }
    if ( !collop_count )
    {
        collop_count = calloc( 1, sizeof( *collop_count ) );
        if ( !collop_count )
            return 0;
        collop_count->comm = comm;

        collop_count->next = *bucket;
        *bucket = collop_count;
    }
    collop_count->count++;

    return 1;
}

int
OTFAUX_Process_beginCollOp( OTFAUX_Process* process,
                            uint64_t eventTime,
                            uint32_t comm,
                            uint32_t root,
                            uint32_t collOp,
                            uint64_t matchingId,
                            uint64_t bytesSent,
                            uint64_t bytesRecv,
                            uint32_t scl,
                            void* eventData )
{
    OTFAUX_CollOp* collop;
    Stack *entry;

    if ( !process )
        return 0;

    entry = stack_next( &process->pendingCollOps );
    while ( entry != &process->pendingCollOps )
    {
        collop = ( OTFAUX_CollOp* )entry;
        entry = stack_next( entry );

        if ( collop->matchingId == matchingId )
        {
            /* ups, same matchingId used twice */
            release_event_data( process, collop->eventData );
            stack_remove( &collop->e );
            stack_push( &process->sharedState->collOps, &collop->e );
        }
    }

    if ( !stack_empty( &process->sharedState->collOps ) )
    {
        collop = ( OTFAUX_CollOp* )stack_pop( &process->sharedState->collOps );
    }
    else
    {
        collop = calloc( 1, sizeof( *collop ) );
        if ( !collop )
            return 0;
    }

    collop->eventTime = eventTime;
    collop->comm = comm;
    collop->root = root;
    collop->collOp = collOp;
    collop->matchingId = matchingId;
    collop->bytesSent = bytesSent;
    collop->bytesRecv = bytesRecv;
    collop->scl = scl;
    collop->eventData = eventData;

    stack_init( &collop->e );
    stack_add( &process->pendingCollOps, &collop->e );

    return 1;
}

int
OTFAUX_Process_endCollOp( OTFAUX_Process* process,
                          uint64_t eventTime,
                          uint64_t matchingId )
{
    OTFAUX_CollOp* collop;
    Stack *entry;

    if ( !process )
        return 0;

    entry = stack_next( &process->pendingCollOps );
    while ( entry != &process->pendingCollOps )
    {
        collop = ( OTFAUX_CollOp* )entry;
        entry = stack_next( entry );

        if ( collop->matchingId == matchingId )
        {
            release_event_data( process, collop->eventData );
            stack_remove( &collop->e );
            OTFAUX_Process_countCollOp( process, collop->comm );
            stack_push( &process->sharedState->collOps, &collop->e );
            return 1;
        }
    }

    /* matchingId not found */
    return 0;
}

int
OTFAUX_Process_openFile( OTFAUX_Process* process,
                         uint64_t eventTime,
                         uint32_t fileId,
                         uint64_t handleId,
                         uint32_t scl,
                         void* eventData )
{
    OTFAUX_File* file;
    Stack *entry;

    if ( !process )
        return 0;

    entry = stack_next( &process->openFiles );
    while ( entry != &process->openFiles )
    {
        file = ( OTFAUX_File* )entry;
        entry = stack_next( entry );

        if ( file->handleId == handleId )
        {
            /* ups, same handleId used twice */
            release_event_data( process, file->eventData );
            stack_remove( &file->e );
            stack_push( &process->sharedState->files, &file->e );
        }
    }

    if ( !stack_empty( &process->sharedState->files ) )
    {
        file = ( OTFAUX_File* )stack_pop( &process->sharedState->files );
    }
    else
    {
        file = calloc( 1, sizeof( *file ) );
        if ( !file )
            return 0;
    }

    file->eventTime = eventTime;
    file->id = fileId;
    file->handleId = handleId;
    file->scl = scl;
    file->eventData = eventData;

    stack_init( &file->e );
    stack_add( &process->openFiles, &file->e );

    return 1;
}


int
OTFAUX_Process_closeFile( OTFAUX_Process* process,
                          uint64_t eventTime,
                          uint64_t handleId )
{
    OTFAUX_File* file;
    Stack *entry;

    if ( !process )
        return 0;

    entry = stack_next( &process->openFiles );
    while ( entry != &process->openFiles )
    {
        file = ( OTFAUX_File* )entry;
        entry = stack_next( entry );

        if ( file->handleId == handleId )
        {
            release_event_data( process, file->eventData );
            stack_remove( &file->e );
            stack_push( &process->sharedState->files, &file->e );
            return 1;
        }
    }

    /* handleId not found */
    return 0;
}


int
OTFAUX_Process_beginFileOp( OTFAUX_Process* process,
                            uint64_t eventTime,
                            uint64_t matchingId,
                            uint32_t scl,
                            void* eventData )
{
    OTFAUX_FileOp* fileop;
    Stack *entry;

    if ( !process )
        return 0;

    entry = stack_next( &process->pendingFileOps );
    while ( entry != &process->pendingFileOps )
    {
        fileop = ( OTFAUX_FileOp* )entry;
        entry = stack_next( entry );

        if ( fileop->matchingId == matchingId )
        {
            /* ups, same matchingId used twice */
            release_event_data( process, fileop->eventData );
            stack_remove( &fileop->e );
            stack_push( &process->sharedState->fileOps, &fileop->e );
        }
    }

    if ( !stack_empty( &process->sharedState->fileOps ) )
    {
        fileop = ( OTFAUX_FileOp* )stack_pop( &process->sharedState->fileOps );
    }
    else
    {
        fileop = calloc( 1, sizeof( *fileop ) );
        if ( !fileop )
            return 0;
    }

    fileop->eventTime = eventTime;
    fileop->matchingId = matchingId;
    fileop->scl = scl;
    fileop->eventData = eventData;

    stack_init( &fileop->e );
    stack_add( &process->pendingFileOps, &fileop->e );

    return 1;
}


int
OTFAUX_Process_endFileOp( OTFAUX_Process* process,
                          uint64_t eventTime,
                          uint64_t matchingId )
{
    OTFAUX_FileOp* fileop;
    Stack *entry;

    if ( !process )
        return 0;

    entry = stack_next( &process->pendingFileOps );
    while ( entry != &process->pendingFileOps )
    {
        fileop = ( OTFAUX_FileOp* )entry;
        entry = stack_next( entry );

        if ( fileop->matchingId == matchingId )
        {
            /* ups, same matchingId used twice */
            release_event_data( process, fileop->eventData );
            stack_remove( &fileop->e );
            stack_push( &process->sharedState->fileOps, &fileop->e );
            return 1;
        }
    }

    /* matchingId not found */
    return 0;
}


int
OTFAUX_Process_updateCounter( OTFAUX_Process* process,
                              uint64_t eventTime,
                              uint32_t counterId,
                              uint64_t value,
                              void* eventData )
{
    uint32_t counter_hash;
    OTFAUX_Counter* counter;
    OTFAUX_Counter** bucket;

    if ( !process )
        return 0;

    counter_hash = hash( &counterId, sizeof( counterId ), 0 );
    bucket = &process->counters[ counter_hash & COUNTER_HASH_MASK ];

    counter = *bucket;
    while ( counter )
    {
        if ( counter->id == counterId )
        {
            release_event_data( process, counter->eventData );
            break;
        }

        counter = counter->next;
    }
    if ( !counter )
    {
        counter = calloc( 1, sizeof( *counter ) );
        if ( !counter )
            return 0;
        counter->id = counterId;

        counter->next = *bucket;
        *bucket = counter;
    }
    counter->eventTime = eventTime;
    counter->value = value;
    counter->eventData = eventData;

    return 1;
}


int
OTFAUX_Process_writeStack( OTFAUX_Process* process,
                           uint64_t snapshotTime,
                           void* userData )
{
    int ret = 1;
    Stack* entry;

    if ( !process )
        return 0;

    if ( !process->sharedState->writeEnterSnapshot )
        return 1;

    entry = stack_prev( &process->functionStack );
    while ( ret && entry != &process->functionStack )
    {
        OTFAUX_FunctionCall* call = ( OTFAUX_FunctionCall* )entry;
        entry = stack_prev( entry );

        ret = process->sharedState->writeEnterSnapshot( userData,
                                                        snapshotTime,
                                                        call->eventTime,
                                                        process->id,
                                                        call->function,
                                                        call->scl,
                                                        call->eventData );
    }

    return ret;
}


int
OTFAUX_Process_writeSends( OTFAUX_Process* process,
                           uint64_t snapshotTime,
                           void* userData )
{
    int ret = 1;
    Stack* entry;

    if ( !process )
        return 0;

    if ( !process->sharedState->writeSendSnapshot )
        return 1;

    cleanup_pending_sends( process, snapshotTime );

    entry = stack_next( &process->pendingSends );
    while ( ret && entry != &process->pendingSends )
    {
        OTFAUX_Message* msg = ( OTFAUX_Message* )entry;
        entry = stack_next( entry );

        ret = process->sharedState->writeSendSnapshot( userData,
                                                       snapshotTime,
                                                       msg->eventTime,
                                                       process->id,
                                                       msg->receiver,
                                                       msg->comm,
                                                       msg->tag,
                                                       msg->length,
                                                       msg->scl,
                                                       msg->recvTime,
                                                       msg->recvLength,
                                                       msg->recvScl,
                                                       msg->eventData );
    }

    return ret;
}


int
OTFAUX_Process_writeOpenFiles( OTFAUX_Process* process,
                           uint64_t snapshotTime,
                           void* userData )
{
    int ret = 1;
    Stack* entry;

    if ( !process )
        return 0;

    if ( !process->sharedState->writeOpenFileSnapshot )
        return 1;

    entry = stack_next( &process->openFiles );
    while ( ret && entry != &process->openFiles )
    {
        OTFAUX_File* file = ( OTFAUX_File* )entry;
        entry = stack_next( entry );

        ret = process->sharedState->writeOpenFileSnapshot( userData,
                                                           snapshotTime,
                                                           file->eventTime,
                                                           process->id,
                                                           file->id,
                                                           file->handleId,
                                                           file->scl,
                                                           file->eventData );
    }

    return ret;
}


int
OTFAUX_Process_writeCollOps( OTFAUX_Process* process,
                               uint64_t snapshotTime,
                             void* userData )
{
    int ret = 1;
    int i;
    Stack* entry;

    if ( !process )
        return 0;

    if ( !process->sharedState->writeBeginCollopSnapshot
            && !process->sharedState->writeCollopCountSnapshot )
        return 1;

    entry = stack_next( &process->pendingCollOps );
    while ( ret && entry != &process->pendingCollOps )
    {
        OTFAUX_CollOp* collop = ( OTFAUX_CollOp* )entry;
        entry = stack_next( entry );

        ret = process->sharedState->writeBeginCollopSnapshot( userData,
                                                              snapshotTime,
                                                              collop->eventTime,
                                                              process->id,
                                                              collop->collOp,
                                                              collop->matchingId,
                                                              collop->comm,
                                                              collop->root,
                                                              collop->bytesSent,
                                                              collop->bytesRecv,
                                                              collop->scl,
                                                              collop->eventData );
    }

    for ( i = 0; ret && i < COLLOPCOUNT_HASH_SIZE; i++ )
    {
        OTFAUX_CollOpCount* collop_count = process->collOpCounts[ i ];
        while ( ret && collop_count )
        {
            ret = process->sharedState->writeCollopCountSnapshot( userData,
                                                                  snapshotTime,
                                                                  process->id,
                                                                  collop_count->comm,
                                                                  collop_count->count );

            collop_count = collop_count->next;
        }
    }

    return ret;
}


int
OTFAUX_Process_writeFileOps( OTFAUX_Process* process,
                           uint64_t snapshotTime,
                           void* userData )
{
    int ret = 1;
    Stack* entry;

    if ( !process )
        return 0;

    if ( !process->sharedState->writeBeginFileOpSnapshot )
        return 1;

    entry = stack_next( &process->pendingFileOps );
    while ( ret && entry != &process->pendingFileOps )
    {
        OTFAUX_FileOp* fileop = ( OTFAUX_FileOp* )entry;
        entry = stack_next( entry );

        ret = process->sharedState->writeBeginFileOpSnapshot( userData,
                                                              snapshotTime,
                                                              fileop->eventTime,
                                                              process->id,
                                                              fileop->matchingId,
                                                              fileop->scl,
                                                              fileop->eventData );
    }

    return ret;
}


int
OTFAUX_Process_writeCounters( OTFAUX_Process* process,
                           uint64_t snapshotTime,
                           void* userData )
{
    int ret = 1;
    int i;

    if ( !process )
        return 0;

    if ( !process->sharedState->writeCounterSnapshot )
        return 1;

    for ( i = 0; ret && i < COUNTER_HASH_SIZE; i++ )
    {
        OTFAUX_Counter* counter = process->counters[ i ];
        while ( ret && counter )
        {
            ret = process->sharedState->writeCounterSnapshot( userData,
                                                              snapshotTime,
                                                              counter->eventTime,
                                                              process->id,
                                                              counter->id,
                                                              counter->value,
                                                              counter->eventData );

            counter = counter->next;
        }
    }

    return ret;
}
