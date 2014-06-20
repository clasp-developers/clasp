#ifndef OTFAUX_STATE_H
#define OTFAUX_STATE_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct OTFAUX_State OTFAUX_State;



OTFAUX_State*
OTFAUX_State_create( void );


void
OTFAUX_State_destroy( OTFAUX_State* auxState );


/**
 * Create a context for thumbnail generation.
 *
 * @param minTime   Minimum timestamp of the trace file.
 * @param maxTime   Maximum timestamp of the trace file.
 * @param width     The width in pixels of the thumbnail.
 *
 * @return          1 on success.
 */
int
OTFAUX_State_setupThumbnail( OTFAUX_State* auxState,
                             uint64_t      minTime,
                             uint64_t      maxTime,
                             uint32_t      width );


/**
 * Only for processes declared with this function and isThumbnailProcess set
 * record a thumbnail.
 *
 * @return          1 on success.
 */
int
OTFAUX_State_declareProcess( OTFAUX_State* auxState,
                             uint64_t      processId,
                             int           isThumbnailProcess );


/**
 * Provide a release function for per-event data passed viw the @a eventData
 * parameter.
 *
 * @return          1 on success.
 *
 * @{
 */


typedef void
( *OTFAUX_ReleaseEventData )( void* userData,
                              void* eventData );

int
OTFAUX_State_setReleaseEventDataCallback( OTFAUX_State*           auxState,
                                          OTFAUX_ReleaseEventData releaseEventDataCallback,
                                          void*                   userData );


/**
 * @}
 */


/**
 * Provide callbacks when a snapshot will be triggered.
 *
 * @return          1 on success.
 *
 * @{
 */


typedef int
( *OTFAUX_WriteEnterSnapshotCallback )( void*    userData,
                                        uint64_t snapshotTime,
                                        uint64_t eventTime,
                                        uint64_t processId,
                                        uint32_t function,
                                        uint32_t scl,
                                        void*    eventData );

int
OTFAUX_State_setWriteEnterSnapshotCallback( OTFAUX_State* auxState,
                                            OTFAUX_WriteEnterSnapshotCallback writeEnterSnapshotCallback );


typedef int
( *OTFAUX_WriteSendSnapshotCallback )( void*    userData,
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
                                       void*    eventData );

int
OTFAUX_State_setWriteSendSnapshotCallback( OTFAUX_State* auxState,
                                           OTFAUX_WriteSendSnapshotCallback writeSendSnapshotCallback );


typedef int
( *OTFAUX_WriteOpenFileSnapshotCallback )( void*    userData,
                                           uint64_t snapshotTime,
                                           uint64_t eventTime,
                                           uint64_t processId,
                                           uint32_t fileId,
                                           uint64_t handleId,
                                           uint32_t scl,
                                           void*    eventData );

int
OTFAUX_State_setWriteOpenFileSnapshotCallback( OTFAUX_State* auxState,
                                               OTFAUX_WriteOpenFileSnapshotCallback writeOpenFileSnapshotCallback );


typedef int
( *OTFAUX_WriteBeginCollopSnapshotCallback )( void*    userData,
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
                                              void*    eventData );

int
OTFAUX_State_setWriteBeginCollopSnapshotCallback( OTFAUX_State* auxState,
                                                  OTFAUX_WriteBeginCollopSnapshotCallback writeBeginCollopSnapshotCallback );


typedef int
( *OTFAUX_WriteBeginFileOpSnapshotCallback )( void*    userData,
                                              uint64_t snapshotTime,
                                              uint64_t eventTime,
                                              uint64_t processId,
                                              uint64_t matchingId,
                                              uint32_t scl,
                                              void*    eventData );

int
OTFAUX_State_setWriteBeginFileOpSnapshotCallback( OTFAUX_State* auxState,
                                                  OTFAUX_WriteBeginFileOpSnapshotCallback writeBeginFileOpSnapshotCallback );


typedef int
( *OTFAUX_WriteCollopCountSnapshotCallback )( void*    userData,
                                              uint64_t snapshotTime,
                                              uint64_t processId,
                                              uint32_t comm,
                                              uint64_t count );

int
OTFAUX_State_setWriteCollopCountSnapshotCallback( OTFAUX_State* auxState,
                                                  OTFAUX_WriteCollopCountSnapshotCallback writeCollopCountSnapshotCallback );


typedef int
( *OTFAUX_WriteCounterSnapshotCallback )( void*    userData,
                                          uint64_t snapshotTime,
                                          uint64_t eventTime,
                                          uint64_t processId,
                                          uint32_t counter,
                                          uint64_t value,
                                          void*    eventData );

int
OTFAUX_State_setWriteCounterSnapshotCallback( OTFAUX_State* auxState,
                                              OTFAUX_WriteCounterSnapshotCallback writeCounterSnapshotCallback );


/**
 * @}
 */


/**
 * Provide all receive events a-priori before any other sends.
 *
 * @return          1 on success.
 */
int
OTFAUX_State_enqueueRecvMsg( OTFAUX_State* auxState,
                             uint64_t      eventTime,
                             uint64_t      receiverProcessId,
                             uint64_t      senderProcessId,
                             uint32_t      comm,
                             uint32_t      tag,
                             uint32_t      length,
                             uint32_t      scl );


/**
 * @group Event input functions.
 *
 * Should be called from an event handler when reading the trace.
 *
 * @return          1 on success.
 *                  2 when no matching receive was available
 *                  (for @a OTFAUX_State_processSendMsg())
 *
 * @{
 */

int
OTFAUX_State_processEnter( OTFAUX_State* auxState,
                           uint64_t      eventTime,
                           uint64_t      processId,
                           uint32_t      function,
                           uint32_t      scl,
                           void*         eventData );

int
OTFAUX_State_processLeave( OTFAUX_State* auxState,
                           uint64_t      eventTime,
                           uint64_t      processId,
                           uint32_t      function );

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
                             void*         eventData );

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
                                              void*         eventData );

int
OTFAUX_State_processEndCollectiveOperation( OTFAUX_State* auxState,
                                            uint64_t      eventTime,
                                            uint64_t      processId,
                                            uint64_t      matchingId );

int
OTFAUX_State_processCollectiveOperation( OTFAUX_State* auxState,
                                         uint64_t eventTime,
                                         uint64_t processId,
                                         uint32_t comm,
                                         uint32_t root,
                                         uint32_t collOp,
                                         uint64_t bytesSent,
                                         uint64_t bytesRecv,
                                         uint32_t scl );

int
OTFAUX_State_processFileOpen( OTFAUX_State* auxState,
                              uint64_t      eventTime,
                              uint64_t      processId,
                              uint32_t      fileId,
                              uint64_t      handleId,
                              uint32_t      scl,
                              void*         eventData );

int
OTFAUX_State_processFileClose( OTFAUX_State* auxState,
                               uint64_t      eventTime,
                               uint64_t      processId,
                               uint64_t      handleId );

int
OTFAUX_State_processBeginFileOperation( OTFAUX_State* auxState,
                                        uint64_t      eventTime,
                                        uint64_t      processId,
                                        uint64_t      matchingId,
                                        uint32_t      scl,
                                        void*         eventData );

/*
 * Also call OTFAUX_State_processFileOpen or OTFAUX_State_processFileClose
 * if the operation was an open or close, respectivly.
 */
int
OTFAUX_State_processEndFileOperation( OTFAUX_State* auxState,
                                      uint64_t      eventTime,
                                      uint64_t      processId,
                                      uint64_t      matchingId );

int
OTFAUX_State_processCounter( OTFAUX_State* auxState,
                             uint64_t      eventTime,
                             uint64_t      processId,
                             uint32_t      counterId,
                             uint64_t      value,
                             void*         eventData );

/**
 * @}
 */


/**
 * Trigger snapshot writing.
 *
 * @return 1 on success.
 */
int
OTFAUX_State_writeSnapshot( OTFAUX_State* auxState,
                            uint64_t      snapshotTime,
                            void*         userData );


/**
 * Writes the processes data of the context to a file.
 *
 * The writing is designed so that the data of multuiple contexts can be
 * written to one file to form a thumbnail. The @a create parameter alows
 * this. The writing of the first context should set the @a create
 * parameter and provide in the variable argument list the total number of
 * processes which will be written, over all comming contexts as an uint32_t.
 * If the @a create parameter is not set, no file will be created and no
 * header will be written, only the data from the given context will be
 * appended to the file. The width of the file should match the width of this
 * context.
 *
 * @param tn_context    The context.
 * @param namestub      The name of the file.
 * @param create        Create the thumb file, or append.
 * @param ...           The total number of processes as an uint32_t, if @a
 *                      create is set.
 *
 * @return             1 on success.
 */
int
OTFAUX_State_writeThumbnail( OTFAUX_State* auxState,
                             const char*   namestub,
                             int           create,
                             ... );



#ifdef __cplusplus
}
#endif

#endif /* OTFAUX_STATE_H */
