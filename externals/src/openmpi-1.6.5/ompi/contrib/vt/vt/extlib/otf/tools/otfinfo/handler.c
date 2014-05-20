/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Michael Heyde
*/

#include "handler.h"
#include "otfinfo_error.h"

#include "otf.h"

#include <stdlib.h>
#include <string.h>

#define MAXINFOLEVEL 4

/* Level 1/4 handles */

int handleUnknownRecord( void *userData, uint64_t time, uint32_t process,
                         const char *record )
{
  otfinfo_assert(0);
  return OTF_RETURN_ABORT;
}

int handleDefCreator( void *userData, uint32_t stream, const char *creator )
{
  ((definitionInfoT*)userData)->creatorName = strdup(creator);
  return OTF_RETURN_OK;
}

int handleDefUniqueId( void *userData, uint32_t stream, uint64_t uid )
{
  definitionInfoT *info  = userData;
  info->traceUniqueId = uid;

  return OTF_RETURN_OK;
}

int handleDefVersion( void *userData, uint32_t stream, uint8_t major,
                      uint8_t minor, uint8_t sub, const char *string )
{
  definitionInfoT *info  = userData;
  info->otfVersionMajor  = major;
  info->otfVersionMinor  = minor;
  info->otfVersionSub    = sub;

  if( string != NULL ) {
  	info->otfVersionString = strdup(string);
  } else {
	info->otfVersionString = strdup("");
  }

  return OTF_RETURN_OK;
}

int handleDefProcess( void *userData, uint32_t stream, uint32_t process,
                      const char *name, uint32_t parent )
{
  /* in a low info level, increment the process counter */
  if( MAXINFOLEVEL > ((definitionInfoT*)userData)->infoLevel )
  {
    ((definitionInfoT*)userData)->counterProcessDefinition++;
  }
  else
  {
    /* in the max info level, get the process names */
    int index=0;
    definitionInfoT *info = (definitionInfoT*)userData;
    while(info->processNames[index])
      index++;
    (info->processNames)[index] = strdup(name);
  }
  return OTF_RETURN_OK;
}

int handleDefTimerResolution( void *userData, uint32_t stream,
                              uint64_t ticksPerSecond )
{ 
  ((definitionInfoT*)userData)->timerResolution = ticksPerSecond;
  return OTF_RETURN_OK;
}

int handleDefinitionComment( void *userData, uint32_t stream,
                             const char *comment )
{
  definitionInfoT *info = (definitionInfoT*)userData;
  uint32_t index = (info->counterDefinitionComment)++;
  (info->definitionComments) = (char**)realloc(info->definitionComments,(index + 1) * sizeof(char*));
  (info->definitionComments)[index] = strdup(comment);
  return OTF_RETURN_OK;
}

/* Level 2 handles */

int handleDefFunction( void *userData, uint32_t stream, uint32_t func,
                       const char *name, uint32_t funcGroup, uint32_t source )
{
  /*in a low info level, increment the function counter*/
  if( MAXINFOLEVEL > ((definitionInfoT*)userData)->infoLevel )
  {
    ((definitionInfoT*)userData)->counterFunctionDefinition++;
  }
  else
  {
    /*in the max info level, get the function names*/
    int index = 0;
    definitionInfoT *info = (definitionInfoT*)userData;
    while(info->functionNames[index])
      index++;
    (info->functionNames)[index] = strdup(name);
  }
  return OTF_RETURN_OK;
}

int handleDefCounter( void *userData, uint32_t stream, uint32_t counter,
                      const char *name, uint32_t properties,
                      uint32_t counterGroup, const char *unit )
{
  /*in a low info level, increment the counter counter*/
  if( 3 > ((definitionInfoT*)userData)->infoLevel)
  {
    ((definitionInfoT*)userData)->counterCounterDefinition++;
  }
  else
  {
    /*in the max info level, get the counter names*/
    definitionInfoT *info = (definitionInfoT*)userData;
    uint64_t i = 0;
    while( ((info->counters)[i].name) && (i < info->counterCounterDefinition) )
      i++;
    (info->counters)[i].name = strdup(name);
    (info->counters)[i].id = counter;
    (info->counters)[i].properties = properties;
  }
  return OTF_RETURN_OK;
}

int handleDefMarker( void *userData, uint32_t stream, uint32_t token,
                     const char *name, uint32_t type )
{
  /*in a low info level, increment the marker counter*/
  if( MAXINFOLEVEL > ((definitionInfoT*)userData)->infoLevel )
  {
    ((definitionInfoT*)userData)->counterMarkerDefinition++;
  }
  else
  {
    /*in the max info level, get the marker names*/
    uint32_t index = 0;
    definitionInfoT *info = (definitionInfoT*)userData;
    while(info->markerNames[index])
      index++;
    (info->markerNames)[index] = strdup(name);
  }
  return OTF_RETURN_OK;
}

int handleDefCollectiveOperation( void *userData, uint32_t stream,
                                  uint32_t collOp, const char *name,
                                  uint32_t type )
{
  /*in a low info level, increment the collective counter*/
  if( 3 > ((definitionInfoT*)userData)->infoLevel )
  {
    ((definitionInfoT*)userData)->counterCollectiveOperationDefinition++;
  }
  else
  {
    /*in the max info level, get the collective operation names*/
    int index = 0;
    definitionInfoT *info = (definitionInfoT*)userData;
    while(info->collectiveOperationNames[index])
      index++;
    (info->collectiveOperationNames)[index] = strdup(name);
  }
  return OTF_RETURN_OK;
}

int handleDefProcessGroup( void *userData, uint32_t stream, uint32_t procGroup,
                           const char *name, uint32_t numberOfProcs, const uint32_t *procs )
{
  /*in a low info level, increment the process group counter*/
  if( MAXINFOLEVEL > ((definitionInfoT*)userData)->infoLevel )
  {
    ((definitionInfoT*)userData)->counterProcessGroupDefinition++;
  }
  else
  {
    /*in the max info level, get the process group names*/
    int index = 0;
    definitionInfoT *info = (definitionInfoT*)userData;
    while( info->processGroupNames[index] )
      index++;
    (info->processGroupNames)[index] = strdup(name);
  }
  return OTF_RETURN_OK;
}

int handleDefFunctionGroup( void *userData, uint32_t stream,
                            uint32_t funcGroup, const char *name)
{
  /*in a low info level, increment the function group counter*/
  if( MAXINFOLEVEL > ((definitionInfoT*)userData)->infoLevel )
  {
    ((definitionInfoT*)userData)->counterFunctionGroupDefinition++;
  }
  else
  {
    /*in the max info level, get the function group names*/
    int index = 0;
    definitionInfoT *info = (definitionInfoT*)userData;
    while(info->functionGroupNames[index])
      index++;
    (info->functionGroupNames)[index] = strdup(name);
  }
  return OTF_RETURN_OK;
}

int handleDefCounterGroup( void *userData, uint32_t stream,
                           uint32_t counterGroup, const char *name )
{
  /*in a low info level, increment the counter group counter*/
  if( MAXINFOLEVEL > ((definitionInfoT*)userData)->infoLevel )
  {
    ((definitionInfoT*)userData)->counterCounterGroupDefinition++;
  }
  else
  {
    /*in the max info level, get the counter group names*/
    int index = 0;
    definitionInfoT *info = (definitionInfoT*)userData;
    while(info->counterGroupNames[index])
      index++;
    (info->counterGroupNames)[index] = strdup(name);
  }
  return OTF_RETURN_OK;
}

int handleDefSclFile( void *userData, uint32_t stream, uint32_t sourceFile,
                      const char *name)
{
  definitionInfoT *info = (definitionInfoT*)userData;
  uint32_t index = (info->counterSourceFileName)++;
  (info->sourceFileNames) = (char**)realloc( info->sourceFileNames,(index + 1)
                                             * sizeof(char*) );
  (info->sourceFileNames)[index] = strdup(name);
  return OTF_RETURN_OK;
}

/* Level 3 handles */

int handleEnter( void *userData, uint64_t time, uint32_t function,
                 uint32_t process, uint32_t source )
{
  ((definitionInfoT*)userData)->counterEnter++;
  return OTF_RETURN_OK;
}

int handleLeave( void *userData, uint64_t time, uint32_t function,
                 uint32_t process, uint32_t source )
{
  ((definitionInfoT*)userData)->counterLeave++;
  return OTF_RETURN_OK;
}

int handleSendMsg( void *userData, uint64_t time, uint32_t sender,
                   uint32_t receiver, uint32_t group, uint32_t type,
                   uint32_t length, uint32_t source )
{
  ((definitionInfoT*)userData)->counterSend++;
  return OTF_RETURN_OK;
}

int handleRecvMsg( void *userData, uint64_t time, uint32_t recvProc,
                   uint32_t sendProc, uint32_t group, uint32_t type,
                   uint32_t length, uint32_t source )
{
  ((definitionInfoT*)userData)->counterReceive++;
  return OTF_RETURN_OK;
}

int handleRMAPut( void *userData, uint64_t time, uint32_t process,
                  uint32_t origin, uint32_t target, uint32_t communicator,
                  uint32_t tag, uint64_t bytes, uint32_t source )
{
  ((definitionInfoT*)userData)->counterRMAPut++;
  return OTF_RETURN_OK;
}
int handleRMAPutRemoteEnd( void *userData, uint64_t time, uint32_t process,
                           uint32_t origin, uint32_t target,
                           uint32_t communicator, uint32_t tag, uint64_t bytes,
                           uint32_t source )
{
  ((definitionInfoT*)userData)->counterRMAPutRemoteEnd++;
  return OTF_RETURN_OK;
}
int handleRMAGet( void *userData, uint64_t time, uint32_t process,
                  uint32_t origin, uint32_t target, uint32_t communicator,
                  uint32_t tag, uint64_t bytes, uint32_t source )
{
  ((definitionInfoT*)userData)->counterRMAGet++;
  return OTF_RETURN_OK;
}
int handleRMAEnd( void *userData, uint64_t time, uint32_t process,
                  uint32_t remote, uint32_t communicator, uint32_t tag,
                  uint32_t source )
{
  ((definitionInfoT*)userData)->counterRMAEnd++;
  return OTF_RETURN_OK;
}

int handleMarker( void *userData, uint64_t time, uint32_t process,
                  uint32_t token, const char* text )
{
  ((definitionInfoT*)userData)->counterMarker++;
  return OTF_RETURN_OK;
}

int handleCollectiveOperation( void *userData, uint64_t time,
                               uint32_t process, uint32_t collective,
                               uint32_t procGroup, uint32_t rootProc,
                               uint32_t sent, uint32_t received,
                               uint64_t duration, uint32_t source )
{
  ((definitionInfoT*)userData)->counterCollectiveOperation++;
  return OTF_RETURN_OK;
}

int handleEndCollectiveOperation( void *userData, uint64_t time,
                                  uint32_t process, uint64_t matchingId )
{
  ((definitionInfoT*)userData)->counterCollectiveOperation++;
  return OTF_RETURN_OK;
}

int handleFileOperation( void *userData, uint64_t time, uint32_t fileid,
                         uint32_t process, uint64_t handleid,
                         uint32_t operation, uint64_t bytes, uint64_t duration,
                         uint32_t source )
{
  ((definitionInfoT*)userData)->counterFileOperation++;
  return OTF_RETURN_OK;
}

int handleEndFileOperation( void *userData, uint64_t time, uint32_t process,
                            uint32_t fileid, uint64_t handleid,
                            uint32_t operation, uint64_t bytes,
                            uint32_t source )
{
  ((definitionInfoT*)userData)->counterFileOperation++;
  return OTF_RETURN_OK;
}

int handleEnterSnapshot( void *userData, uint64_t time, uint64_t originaltime,
                         uint32_t function, uint32_t process, uint32_t source )
{
  ((definitionInfoT*)userData)->counterSnapshot++;
  return OTF_RETURN_OK;
}

int handleCounter( void* userData, uint64_t time, uint32_t process,
                   uint32_t counter, uint64_t value )
{
  uint64_t i = 0;
  double valueDif = 0.0;
  double timeDif = 0.0;
  double rate = 0.0;
  mapInfoProcessT *currentElement = NULL;
  definitionInfoT *info = (definitionInfoT*)userData;

  while( (i < info->counterCounterDefinition) &&
         (info->counters[i].id != counter))
  {
    i++;
  }
  if( i >= info->counterCounterDefinition )
    return OTF_RETURN_ABORT;

  /*for the counter type*/
  if( ((info->counters[i].properties) & OTF_COUNTER_TYPE_BITS)
      == OTF_COUNTER_TYPE_ACC )
  {
    if( (info->counters)[i].processMap == NULL )
      (info->counters)[i].processMap = hash_new();

    /*calculate the current counter rate*/
    currentElement = hash_search( info->counters[i].processMap, process );

    if( !currentElement )
    {
      currentElement = hash_add( info->counters[i].processMap, process );
    }
    else
    {
      valueDif = value - currentElement->lastValue;
      timeDif = time - currentElement->lastTime;

      if( timeDif > 0 )
      {
        rate = (valueDif / timeDif) * (double)(info->timerResolution);
      }
      if( rate > currentElement->highestRate )
        currentElement->highestRate = rate;
    }

    currentElement->lastValue = value;
    currentElement->lastTime = time;
  }

  return OTF_RETURN_OK;
}

uint64_t process_get_sum_time( mapInfoProcessT *set )
{
  uint64_t sumTime = 0;
  uint32_t i;
  mapInfoProcessT *currentElement = NULL;
  for( i = 0; i < HASH_SIZE; i++ )
  {
    currentElement = &set[i];
    while( currentElement )
    {
      if( (uint32_t)-1 != currentElement->process )
        sumTime += currentElement->lastTime;
      currentElement = currentElement->next;
    }
  }
  return sumTime;
}

uint64_t process_get_sum_value( mapInfoProcessT *set )
{
  uint64_t sumValue = 0;
  uint32_t i;
  mapInfoProcessT *currentElement = NULL;
  for( i = 0; i < HASH_SIZE; i++ )
  {
    currentElement = &set[i];
    while( currentElement )
    {
      if( (uint32_t)-1 != currentElement->process )
        sumValue += currentElement->lastValue;
      currentElement = currentElement->next;
    }
  }
  return sumValue;
}

double process_get_highest_rate( mapInfoProcessT *set )
{
  double highestRate = 0;
  uint32_t i;
  mapInfoProcessT *currentElement = NULL;
  for( i = 0; i < HASH_SIZE; i++ )
  {
    currentElement = &set[i];
    while( currentElement )
    {
      if( (uint32_t)-1 != currentElement->process )
      {
        if( highestRate < currentElement->highestRate )
          highestRate = currentElement->highestRate;
      }
      currentElement = currentElement->next;
    }
  }
  return highestRate;
}
