/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2013, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vt_defs.h"
#include "vt_env.h"
#include "vt_error.h"
#include "vt_esync.h"
#include "vt_inttypes.h"
#include "vt_iowrap.h"
#include "vt_pform.h"
#include "vt_trc.h"

#define LOOP_COUNT 20

typedef struct Sync_MapS {
  VT_MPI_INT id;     /* process id mapped to sync. process id */
  uint64_t time;     /* local start time of synchronization phase */
  uint64_t duration; /* duration of clock synchronization phase */
  struct Sync_MapS* next;
} Sync_Map;

typedef struct Sync_TsPerPhaseS {
  VT_MPI_INT id1, id2;    /* id's of pairwise comm. with id1 < id2 */
  uint64_t   t1,t2,t3,t4; /* timestamps with shortest message delay in
			     each direction */
  struct Sync_TsPerPhaseS* next;
} Sync_TsPerPhase;

typedef struct Sync_TsPerRunS {
  Sync_TsPerPhase* sync_phase; /* list of sync. timestamps of a sync. phase */
  struct Sync_TsPerRunS*   next;
} Sync_TsPerRun;

static Sync_Map*      SyncMapIdFirst    = NULL;
static Sync_Map*      SyncMapIdLast     = NULL;
static Sync_TsPerRun* SyncTsPerRunFirst = NULL;
static Sync_TsPerRun* SyncTsPerRunLast  = NULL;

static uint64_t SyncIntv  = 0; /* sync. interval */
static uint64_t SyncNext  = 0; /* next timestamp for sync. */
static uint32_t SyncRound = 0; /* number of sync. rounds */

static VT_MPI_INT esync_commpartner(VT_MPI_INT id, VT_MPI_INT n,
				    VT_MPI_INT round)
{
  double d_n;
   
  d_n = (double) n;
   
  if( id < (VT_MPI_INT)( d_n/2 + .5 ) )
  {
     VT_MPI_INT i = id - round % ( (VT_MPI_INT)( d_n/2 +.5 ) );
     return ( -i - 1 + (VT_MPI_INT)( d_n/2 + .5 ) ) % ( (VT_MPI_INT)( d_n/2 + .5 ) )
	+ (VT_MPI_INT)( d_n/2 + .5 );
  }
  else
  {
     VT_MPI_INT i = ( ( ( (VT_MPI_INT)( d_n/2 + .5 ) ) - 1 ) * 
		      ( ( id - ( (VT_MPI_INT)( d_n/2 + .5 ) ) + 1 ) )) %
	( (VT_MPI_INT)( d_n/2 + .5 ) );
     i = ( i + ( (VT_MPI_INT)( d_n/2 + .5 ) ) ) % ( (VT_MPI_INT)( d_n/2 + .5) );
     return ( i + round ) % ( (VT_MPI_INT)( d_n/2 + .5 ) ) ;
  }
}

static void esync_master(VT_MPI_INT slave, MPI_Comm comm, VT_MPI_INT masterid)
{
  int i;
   
  uint64_t tsend, trecv, tslave;
  uint64_t t1, t2, t3, t4;
   
  MPI_Status stat;
  MPI_Request req;
  Sync_TsPerPhase* temp;
   
  /* exchange LOOP_COUNT ping pong messages with the communication partner */
   
  t1 = vt_pform_wtime();
  PMPI_Isend( &t1, 1, MPI_LONG_LONG_INT, slave, 0, comm, &req );
  PMPI_Recv( &t2, 1, MPI_LONG_LONG_INT, slave, 0, comm, &stat );
  t4 = vt_pform_wtime();
  t3 = t2;
  PMPI_Waitall( 1, &req, &stat );
   
  for( i = 1; i < LOOP_COUNT; i++ )
  {
    tsend = vt_pform_wtime();
      
    /* message exchange */

    PMPI_Isend(&tsend, 1, MPI_LONG_LONG_INT, slave, i, comm, &req);
    PMPI_Recv(&tslave, 1, MPI_LONG_LONG_INT, slave, i, comm, &stat);
    trecv = vt_pform_wtime();
      
    PMPI_Waitall(1, &req, &stat);

    /* select timestamps with minimum message delay in each direction */

    if ( ( (int64_t)tslave - (int64_t)tsend ) < ( (int64_t)t2 - (int64_t)t1 ) )
    {
      t1 = tsend;
      t2 = tslave;
    }
    if ( ( (int64_t)trecv - (int64_t)tslave ) < ( (int64_t)t4 - (int64_t)t3 ) )
    {
      t3 = tslave;
      t4 = trecv;
    }
  }

  /* save synchronization measurement data into internal data structure */

  temp = (Sync_TsPerPhase*)malloc(sizeof(Sync_TsPerPhase));
  if (!temp) vt_error();
  temp->id1  = masterid;
  temp->id2  = slave;
  temp->t1   = t1;
  temp->t2   = t2;
  temp->t3   = t3;
  temp->t4   = t4;
  temp->next = SyncTsPerRunLast->sync_phase;
  SyncTsPerRunLast->sync_phase = temp;
}

static void esync_slave(VT_MPI_INT master, MPI_Comm comm)
{
  int i;
  uint64_t t, tslave;
  MPI_Status stat;
   
  /* start communication with master */

  for (i = 0; i < LOOP_COUNT; i++)
  {
    PMPI_Recv( &t, 1, MPI_LONG_LONG_INT, master, i, comm, &stat );
    tslave = vt_pform_wtime();
    PMPI_Send( &tslave, 1, MPI_LONG_LONG_INT, master, i, comm );
  }
}

void vt_esync_init()
{
  /* get sync. interval */

  SyncIntv = (uint64_t)vt_env_etimesync_intv() * vt_pform_clockres();
  if (SyncIntv > 0)
    SyncNext = vt_pform_wtime() + SyncIntv;
  else
    SyncNext = (uint64_t)-1;
}

void vt_esync_finalize()
{
  Sync_Map* sync_map_temp;
  Sync_TsPerRun* sync_tsperrun_temp;
  Sync_TsPerPhase* sync_tsperphase_temp;

  /* free map id's */

  while(SyncMapIdFirst)
  {
    sync_map_temp = SyncMapIdFirst->next;
    free(SyncMapIdFirst);
    SyncMapIdFirst = sync_map_temp;
  }

  /* free synchronization information for each phase */

  while(SyncTsPerRunFirst)
  {
    sync_tsperrun_temp = SyncTsPerRunFirst->next;
    while(SyncTsPerRunFirst->sync_phase)
    {
      sync_tsperphase_temp = SyncTsPerRunFirst->sync_phase->next;
      free(SyncTsPerRunFirst->sync_phase);
      SyncTsPerRunFirst->sync_phase = sync_tsperphase_temp;
    }
    free(SyncTsPerRunFirst);
    SyncTsPerRunFirst = sync_tsperrun_temp;
  }
}

void vt_esync(MPI_Comm comm)
{
  uint64_t time, etime;
  Sync_TsPerRun* temp_ts;
  Sync_Map* temp_map;

  VT_MPI_INT myrank;
  VT_MPI_INT numnodes;
  VT_MPI_INT partnerid, numslots;
  VT_MPI_INT i;

  VT_SUSPEND_IO_TRACING(VT_CURRENT_THREAD);

  /* mark begin of clock synchronization */
  time = vt_pform_wtime();
  vt_enter(VT_CURRENT_THREAD, &time, vt_trc_regid[VT__TRC_SYNCTIME]);
  /* ... also as comment for vtunify */
  vt_comment(VT_CURRENT_THREAD, &time, VT_UNIFY_STRID_ETIMESYNC_COMMENT);

  /* barrier at entry */
  PMPI_Barrier(comm);

  temp_ts = (Sync_TsPerRun*) malloc(sizeof(Sync_TsPerRun));
  if (temp_ts == NULL) vt_error();

  temp_ts->sync_phase = NULL;
  temp_ts->next       = NULL;

  if (SyncTsPerRunFirst == NULL)
  {
    SyncTsPerRunFirst = temp_ts;
    SyncTsPerRunLast  = temp_ts;
  }
  else
  {
    SyncTsPerRunLast->next = temp_ts;    
    SyncTsPerRunLast = temp_ts;
  }

  /* measure time synchronization */

  PMPI_Comm_rank(comm, &myrank);
  PMPI_Comm_size(comm, &numnodes);

  numslots = (VT_MPI_INT)ceil(log((double)(numnodes)) / log(2.0));

  for(i = 0; i < numslots; i++)
  {
    partnerid = esync_commpartner(myrank, numnodes, i);
    if( partnerid < numnodes )
    {
      if( myrank < partnerid )
      {
	esync_master(partnerid, comm, myrank);
      }
      else
      {
	esync_slave(partnerid, comm);
      }
    }
  }
   
  /* add myrank to list of map ids */

  temp_map = (Sync_Map*)malloc(sizeof(Sync_Map));
  if (temp_map == NULL) vt_error();
  temp_map->id       = myrank;
  temp_map->time     = time;
  temp_map->duration = (uint32_t) 0;
  temp_map->next     = NULL;

  if (SyncMapIdFirst == NULL)
  {
    SyncMapIdFirst = temp_map;
    SyncMapIdLast = temp_map;
  }
  else
  {
    SyncMapIdLast->next = temp_map;
    SyncMapIdLast = temp_map;
  }

  /* barrier at exit */
  PMPI_Barrier(comm);

  /* mark end of clock synchronization */
  etime = vt_pform_wtime();
  vt_exit(VT_CURRENT_THREAD, &etime);

  /* increment number of sync. phases */
  SyncRound++;

  /* set timestamp of next synchronization if necessary */
  if (SyncNext != (uint64_t)-1)
    SyncNext = etime + SyncIntv;

  /* calculate sync. duration */
  SyncMapIdLast->duration = etime - time;

  VT_RESUME_IO_TRACING(VT_CURRENT_THREAD);
}

void vt_esync_app_uctl_data(char** data)
{
  Sync_Map* sync_map_temp;
  Sync_TsPerRun* sync_tsperrun_temp;
  Sync_TsPerPhase* sync_tsperphase_temp;
  size_t size;
  uint32_t i;

  vt_libassert(*data != NULL);

  size = strlen(*data);

  /* the first iteration calculates the size needed for the additional data
     and the second iteration writes them */

  for(i = 0; i < 2; i++)
  {
    /* map ids */

    sync_map_temp = SyncMapIdFirst;
    while(sync_map_temp)
    {
      if (i == 0)
      {
        size += 3 * (16 + 1);
      }
      else /* i == 1 */
      {
        sprintf(*data + strlen(*data),
                "%llx:%llx:%llx:",
                (long long int)(sync_map_temp->id + 1),
                (long long int)(sync_map_temp->time),
                (long long int)(sync_map_temp->duration));
      }
      sync_map_temp = sync_map_temp->next;
    }

    if (i == 0)
      size++;
    else /* i == 1 */
      strcat(*data, "\n");

    /* synchronization information for each phase */

    sync_tsperrun_temp = SyncTsPerRunFirst;
    while(sync_tsperrun_temp)
    {
      sync_tsperphase_temp = sync_tsperrun_temp->sync_phase;
      while(sync_tsperphase_temp)
      {
        if (i == 0)
        {
          size += 6 * (16 + 1);
        }
        else /* i == 1 */
        {
          sprintf(*data + strlen(*data),
                  "%llx:%llx:%llx:%llx:%llx:%llx:",
                  (unsigned long long int)(sync_tsperphase_temp->id1),
                  (unsigned long long int)(sync_tsperphase_temp->id2),
                  (unsigned long long int)(sync_tsperphase_temp->t1),
                  (unsigned long long int)(sync_tsperphase_temp->t2),
                  (unsigned long long int)(sync_tsperphase_temp->t3),
                  (unsigned long long int)(sync_tsperphase_temp->t4));
        }
        sync_tsperphase_temp = sync_tsperphase_temp->next;
      }

      if (i == 0)
        size++;
      else /* i == 1 */
        strcat(*data, "\n");
      sync_tsperrun_temp = sync_tsperrun_temp->next;
    }

    /* enlarge buffer for uctl data at the end of the first iteration */

    if (i == 0)
    {
      *data = (char*)realloc(*data, size * sizeof(char));
      if (*data == NULL)
        vt_error();
    }
  }
}

uint64_t vt_esync_next()
{
  return SyncNext;
}










