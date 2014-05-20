#ifndef OTFAUX_PROCESS_H
#define OTFAUX_PROCESS_H

#include "OTFAUX_SharedState.h"

#include "OTFAUX_Stack.h"

#define QUEUE_HASH_SHIFT 10
#define QUEUE_HASH_SIZE (1 << QUEUE_HASH_SHIFT)
#define QUEUE_HASH_MASK (QUEUE_HASH_SIZE - 1)

#define COLLOPCOUNT_HASH_SHIFT 8
#define COLLOPCOUNT_HASH_SIZE  (1 << COLLOPCOUNT_HASH_SHIFT)
#define COLLOPCOUNT_HASH_MASK  (COLLOPCOUNT_HASH_SIZE - 1)

#define COUNTER_HASH_SHIFT 4
#define COUNTER_HASH_SIZE  (1 << COUNTER_HASH_SHIFT)
#define COUNTER_HASH_MASK  (COUNTER_HASH_SIZE - 1)

typedef struct OTFAUX_ReciveQueue OTFAUX_ReciveQueue;
typedef struct OTFAUX_CollOpCount OTFAUX_CollOpCount;
typedef struct OTFAUX_Counter OTFAUX_Counter;

typedef struct OTFAUX_Process OTFAUX_Process;
struct OTFAUX_Process
{
    /* For hash chaining in OTFAUX_State */
    OTFAUX_Process* next;

    uint64_t id;

    uint32_t* thumbnail;

    /* function stack */
    Stack functionStack;

    /* cache of recived messages */
    OTFAUX_ReciveQueue* receiveQueues[ QUEUE_HASH_SIZE ];

    /* list of pending messages */
    Stack pendingSends;

    /* list of open files */
    Stack openFiles;

    /* map of begun collective operations */
    Stack pendingCollOps;

    /* cache of recived messages */
    OTFAUX_CollOpCount* collOpCounts[ COLLOPCOUNT_HASH_SIZE ];

    /* unfinished file operations */
    Stack pendingFileOps;

    /* last value of counter */
    OTFAUX_Counter* counters[ COUNTER_HASH_SIZE ];

    /* Shared data among all processes from the OTFAUX_State */
    OTFAUX_SharedState* sharedState;
};

OTFAUX_Process*
OTFAUX_Process_create( uint64_t processId,
                       OTFAUX_SharedState* sharedState );

void
OTFAUX_Process_destroy( OTFAUX_Process* process );

int
OTFAUX_Process_enableThumbnail( OTFAUX_Process* process,
                                uint32_t        thumbnailSize );
                                   
void
OTFAUX_Process_updateThumbnail( OTFAUX_Process* process,
                                uint32_t timestampsPosition );

void
OTFAUX_Process_writeThumbnail( OTFAUX_Process* process,
                               uint32_t timestampsSize,
                               FILE* out );

int
OTFAUX_Process_enqueueRecv( OTFAUX_Process* process,
                            uint64_t eventTime,
                            uint32_t receiverProcessId,
                            uint32_t comm,
                            uint32_t tag,
                            uint32_t length,
                            uint32_t scl );

int
OTFAUX_Process_enterFunction( OTFAUX_Process* process,
                              uint64_t eventTime,
                              uint32_t function,
                              uint32_t scl,
                              void* eventData );

int
OTFAUX_Process_leaveFunction( OTFAUX_Process* process,
                              uint64_t        eventTime,
                              uint32_t        function );

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
                            void* eventData );

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
                            void* eventData );

int
OTFAUX_Process_endCollOp( OTFAUX_Process* process,
                          uint64_t eventTime,
                          uint64_t matchingId );

int
OTFAUX_Process_countCollOp( OTFAUX_Process* process,
                            uint32_t comm );

int
OTFAUX_Process_openFile( OTFAUX_Process* process,
                         uint64_t eventTime,
                         uint32_t fileId,
                         uint64_t handleId,
                         uint32_t scl,
                         void* eventData );

int
OTFAUX_Process_closeFile( OTFAUX_Process* process,
                          uint64_t eventTime,
                          uint64_t handleId );

int
OTFAUX_Process_beginFileOp( OTFAUX_Process* process,
                            uint64_t eventTime,
                            uint64_t matchingId,
                            uint32_t scl,
                            void* eventData );

int
OTFAUX_Process_endFileOp( OTFAUX_Process* process,
                          uint64_t eventTime,
                          uint64_t matchingId );

int
OTFAUX_Process_updateCounter( OTFAUX_Process* process,
                              uint64_t eventTime,
                              uint32_t counterId,
                              uint64_t value,
                              void* eventData );

int
OTFAUX_Process_writeStack( OTFAUX_Process* process,
                           uint64_t snapshotTime,
                           void* userData );

int
OTFAUX_Process_writeSends( OTFAUX_Process* process,
                           uint64_t snapshotTime,
                           void* userData );

int
OTFAUX_Process_writeOpenFiles( OTFAUX_Process* process,
                               uint64_t snapshotTime,
                               void* userData );

int
OTFAUX_Process_writeCollOps( OTFAUX_Process* process,
                             uint64_t snapshotTime,
                             void* userData );

int
OTFAUX_Process_writeFileOps( OTFAUX_Process* process,
                             uint64_t snapshotTime,
                             void* userData );

int
OTFAUX_Process_writeCounters( OTFAUX_Process* process,
                              uint64_t snapshotTime,
                              void* userData );

#endif /* OTFAUX_PROCESS_H */
