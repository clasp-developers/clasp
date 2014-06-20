/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2013.
 Authors: Johannes Spazier
*/

#ifndef HANDLER_H
#define HANDLER_H

#ifdef HAVE_CONFIG_H
#   include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>

#if defined(HAVE_SYS_SELECT_H) && HAVE_SYS_SELECT_H
#   include <sys/select.h>
#else /* HAVE_SYS_SELECT_H */
#   include <sys/time.h>
#   include <sys/types.h>
#   include <unistd.h>
#endif /* HAVE_SYS_SELECT_H */

#ifdef OTFMERGE_MPI
#   include "mpi.h"
#   if defined(INSIDE_OPENMPI) || (defined(HAVE_MPI_GET_ADDRESS) && HAVE_MPI_GET_ADDRESS)
#       define MPI_Address MPI_Get_address
#   endif /* HAVE_MPI_GET_ADDRESS */
#   if defined(INSIDE_OPENMPI) || defined(HAVE_MPI_TYPE_CREATE_STRUCT) && HAVE_MPI_TYPE_CREATE_STRUCT
#       define MPI_Type_struct MPI_Type_create_struct
#   endif /* HAVE_MPI_TYPE_CREATE_STRUCT */
#endif /* OTFMERGE_MPI */

#include "otf.h"


/* data structures */

typedef struct {
  int id;
  int num_cpus;
  int *cpus;
} OutStream;

typedef struct {
  int num_ostreams;
  OutStream *ostreams;
} RankData;

typedef struct {
  double progress;
  uint8_t is_alive;
} ProgressBuf;

typedef struct {
#ifdef OTFMERGE_MPI
  MPI_Request request;
#endif /* OTFMERGE_MPI */
  ProgressBuf value;
  ProgressBuf buf;
  int num_cpus;
  double percent;
} ProgressInfo;

typedef struct {
  int my_rank;
  int num_ranks;
  int ranks_alive;
  double tmp_progress;
#ifdef OTFMERGE_MPI
  MPI_Datatype buftype;
#endif /* OTFMERGE_MPI */
} GlobalData;


/* function declarations */

double update_progress( ProgressInfo* info, GlobalData* data, int cur_ostream,
           int num_ostreams );

int finish_everything( char *infile, char* outfile, ProgressInfo* info,
        RankData* rank_data, int ret );

void setDefinitionHandlerArray( OTF_HandlerArray* handlers,
         OTF_WStream* wstream);

void setEventHandlerArray( OTF_HandlerArray* handlers, OTF_WStream* wstream );


/* handlers */

int handleDefinitionComment( void *userData, uint32_t stream,
        const char *comment, OTF_KeyValueList *list );

int handleDefTimerResolution( void *userData, uint32_t stream,
        uint64_t ticksPerSecond, OTF_KeyValueList *list );

int handleDefProcess( void *userData, uint32_t stream, uint32_t process,
        const char *name, uint32_t parent, OTF_KeyValueList *list );

int handleDefProcessGroup( void *userData, uint32_t stream, uint32_t procGroup,
        const char *name, uint32_t numberOfProcs, const uint32_t *procs,
        OTF_KeyValueList *list );

int handleDefAttributeList( void *userData, uint32_t stream,
        uint32_t attr_token, uint32_t num, OTF_ATTR_TYPE *array,
        OTF_KeyValueList *list );

int handleDefProcessOrGroupAttributes( void *userData, uint32_t stream,
        uint32_t proc_token, uint32_t attr_token, OTF_KeyValueList *list);

int handleDefFunction( void *userData, uint32_t stream, uint32_t func,
        const char *name, uint32_t funcGroup, uint32_t source,
        OTF_KeyValueList *list );

int handleDefFunctionGroup( void *userData, uint32_t stream, uint32_t funcGroup,
        const char *name, OTF_KeyValueList *list );

int handleDefCollectiveOperation(void *userData, uint32_t stream,
        uint32_t collOp, const char *name, uint32_t type,
        OTF_KeyValueList *list );

int handleDefCounter( void *userData, uint32_t stream, uint32_t counter,
        const char *name, uint32_t properties, uint32_t counterGroup,
        const char *unit, OTF_KeyValueList *list );

int handleDefCounterGroup( void *userData, uint32_t stream,
        uint32_t counterGroup, const char *name, OTF_KeyValueList *list );

int handleDefScl( void *userData, uint32_t stream, uint32_t source,
        uint32_t sourceFile, uint32_t line, OTF_KeyValueList *list );

int handleDefSclFile( void *userData, uint32_t stream, uint32_t sourceFile,
        const char *name, OTF_KeyValueList *list );

int handleDefCreator( void *userData, uint32_t stream, const char *creator,
        OTF_KeyValueList *list );

int handleDefUniqueId( void *userData, uint32_t stream, uint64_t uid );

int handleDefVersion( void *userData, uint32_t stream, uint8_t major,
        uint8_t minor, uint8_t sub, const char *string );

int handleDefFile( void *userData, uint32_t stream, uint32_t token,
        const char *name, uint32_t group, OTF_KeyValueList *list );

int handleDefFileGroup( void *userData, uint32_t stream, uint32_t token,
        const char *name, OTF_KeyValueList *list );

int handleDefKeyValue( void *userData, uint32_t stream, uint32_t token,
        OTF_Type type, const char *name, const char *desc,
        OTF_KeyValueList *list );

int handleDefTimeRange( void* userData, uint32_t stream, uint64_t minTime,
        uint64_t maxTime, OTF_KeyValueList *list );

int handleDefCounterAssignments( void* userData, uint32_t stream,
        uint32_t counter_token, uint32_t number_of_members,
        const uint32_t* procs_or_groups, OTF_KeyValueList *list );

int handleDefProcessSubstitutes( void* userData, uint32_t stream,
        uint32_t representative, uint32_t numberOfProcs, const uint32_t* procs,
        OTF_KeyValueList *list );

int handleDefAuxSamplePoint( void *fcbx,
                             uint32_t               streamid,
                             uint64_t               time,
                             OTF_AuxSamplePointType type,
                             OTF_KeyValueList*      list );

int handleDefMarker( void *userData, uint32_t stream, uint32_t token,
        const char *name, uint32_t type, OTF_KeyValueList *list );

int handleMarker( void *userData, uint64_t time, uint32_t process,
        uint32_t token, const char* text, OTF_KeyValueList* list );

int handleUnknownRecord( void *userData, uint64_t time, uint32_t process,
        const char *record );

#endif /* HANDLER_H */
