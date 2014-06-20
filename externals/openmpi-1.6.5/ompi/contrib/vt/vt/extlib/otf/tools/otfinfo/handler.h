/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Michael Heyde
*/

#ifndef HANDLER_H
#define HANDLER_H

#include "hash.h"

#include "OTF_inttypes.h"

typedef struct counterS
{
  char            *name;
  uint32_t         id;
  uint32_t         properties;
  mapInfoProcessT *processMap;
 } counterT;

typedef struct definitionInfoS
{
  char       *filePrefix;
  char       *creatorName;
  char      **definitionComments;
  char      **sourceFileNames;
  char      **processNames;
  char      **processGroupNames;
  char      **functionNames;
  char      **functionGroupNames;
  char      **collectiveOperationNames;
  counterT   *counters;
  char      **counterGroupNames;
  char      **markerNames;
  char       *otfVersionString;
  int         infoLevel;
  uint8_t     otfVersionMajor;
  uint8_t     otfVersionMinor;
  uint8_t     otfVersionSub;
  uint64_t    traceFileSize;
  uint64_t    traceUniqueId;
  uint64_t    counterDefinitionComment;
  uint64_t    counterSourceFileName;
  uint64_t    counterFunctionGroupDefinition;
  uint64_t    counterFunctionDefinition;
  uint64_t    counterCollectiveOperationDefinition;
  uint64_t    counterProcessGroupDefinition;
  uint64_t    counterProcessDefinition;
  uint64_t    counterCounterDefinition;
  uint64_t    counterCounterGroupDefinition;
  uint64_t    counterMarkerDefinition;
  uint64_t    counterLeave;
  uint64_t    counterEnter;
  uint64_t    counterSend;
  uint64_t    counterReceive;
  uint64_t    timerResolution;
  uint64_t    counterRMAPut;
  uint64_t    counterRMAPutRemoteEnd;
  uint64_t    counterRMAGet;
  uint64_t    counterRMAEnd;
  uint64_t    counterMarker;
  uint64_t    counterCollectiveOperation;
  uint64_t    counterFileOperation;
  uint64_t    counterSnapshot;
} definitionInfoT;

/* Level 1/4 handles */

int handleUnknownRecord( void *userData, uint64_t time, uint32_t process,
                         const char *record );

int handleDefCreator( void *userData, uint32_t stream, const char *creator );

int handleDefUniqueId( void *userData, uint32_t stream, uint64_t uid );

int handleDefVersion( void *userData, uint32_t stream, uint8_t major,
                      uint8_t minor, uint8_t sub, const char *string );

int handleDefProcess( void *userData, uint32_t stream, uint32_t process,
                      const char *name, uint32_t parent );

int handleDefTimerResolution( void *userData, uint32_t stream,
                              uint64_t ticksPerSecond );

int handleDefinitionComment( void *userData, uint32_t stream,
                             const char *comment );

/* Level 2 handles */

int handleDefFunction( void *userData, uint32_t stream, uint32_t func,
                       const char *name, uint32_t funcGroup, uint32_t source );

int handleDefCounter( void *userData, uint32_t stream, uint32_t counter,
                      const char *name, uint32_t properties,
                      uint32_t counterGroup, const char *unit );

int handleDefMarker( void *userData, uint32_t stream, uint32_t token,
                     const char *name, uint32_t type );

int handleDefCollectiveOperation( void *userData, uint32_t stream,
                                  uint32_t collOp, const char *name,
                                  uint32_t type );

int handleDefProcessGroup( void *userData, uint32_t stream, uint32_t procGroup,
                           const char *name, uint32_t numberOfProcs,
                           const uint32_t *procs );

int handleDefFunctionGroup( void *userData, uint32_t stream,
                            uint32_t funcGroup, const char *name );

int handleDefCounterGroup( void *userData, uint32_t stream,
                           uint32_t counterGroup, const char *name );

int handleDefSclFile( void *userData, uint32_t stream, uint32_t sourceFile,
                      const char *name );

/* Level 3 handles */

int handleEnter( void *userData, uint64_t time, uint32_t function,
                 uint32_t process, uint32_t source );

int handleLeave( void *userData, uint64_t time, uint32_t function,
                 uint32_t process, uint32_t source );

int handleSendMsg( void *userData, uint64_t time, uint32_t sender,
                   uint32_t receiver, uint32_t group, uint32_t type,
                   uint32_t length, uint32_t source );

int handleRecvMsg( void *userData, uint64_t time, uint32_t recvProc,
                   uint32_t sendProc, uint32_t group, uint32_t type,
                   uint32_t length, uint32_t source );

int handleRMAPut( void *userData, uint64_t time, uint32_t process,
                  uint32_t origin, uint32_t target, uint32_t communicator,
                  uint32_t tag, uint64_t bytes, uint32_t source );

int handleRMAPutRemoteEnd( void *userData, uint64_t time, uint32_t process,
                           uint32_t origin, uint32_t target,
                           uint32_t communicator, uint32_t tag, uint64_t bytes,
                           uint32_t source );

int handleRMAGet( void *userData, uint64_t time, uint32_t process,
                  uint32_t origin, uint32_t target, uint32_t communicator,
                  uint32_t tag, uint64_t bytes, uint32_t source );

int handleRMAEnd( void *userData, uint64_t time, uint32_t process,
                  uint32_t remote, uint32_t communicator, uint32_t tag,
                  uint32_t source );

int handleMarker( void *userData, uint64_t time, uint32_t process,
                  uint32_t token, const char* text );

int handleCollectiveOperation( void *userData, uint64_t time,
                               uint32_t process, uint32_t collective,
                               uint32_t procGroup, uint32_t rootProc,
                               uint32_t sent, uint32_t received,
                               uint64_t duration, uint32_t source );

int handleEndCollectiveOperation( void *userData, uint64_t time,
                                  uint32_t process, uint64_t matchingId );

int handleFileOperation( void *userData, uint64_t time, uint32_t fileid,
                         uint32_t process, uint64_t handleid,
                         uint32_t operation, uint64_t bytes, uint64_t duration,
                         uint32_t source );

int handleEndFileOperation( void *userData, uint64_t time, uint32_t process,
                            uint32_t fileid, uint64_t handleid,
                            uint32_t operation, uint64_t bytes,
                            uint32_t source );

int handleEnterSnapshot( void *userData, uint64_t time, uint64_t originaltime,
                         uint32_t function, uint32_t process,
                         uint32_t source );

int handleCounter( void* userData, uint64_t time, uint32_t process,
                   uint32_t counter, uint64_t value );


uint64_t process_get_sum_time( mapInfoProcessT *set );

uint64_t process_get_sum_value( mapInfoProcessT *set );

double process_get_highest_rate(mapInfoProcessT *set );

#endif /* HANDLER_H */
