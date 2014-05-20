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

#include <stdlib.h>
#include <stdio.h>

#include "vt_defs.h"
#include "vt_inttypes.h"
#include "vt_iowrap.h"
#include "vt_pform.h"
#include "vt_sync.h"
#include "vt_trc.h"

#define LOOP_COUNT 10

static int64_t sync_master(uint64_t* ltime, VT_MPI_INT slave, MPI_Comm comm)
{
  VT_MPI_INT min;
  MPI_Status stat;
  uint64_t tsend[LOOP_COUNT], trecv[LOOP_COUNT];
  uint64_t pingpong_time, sync_time;
  int i;

  /* exchange LOOP_COUNT ping pong messages with slave */

  for (i = 0; i < LOOP_COUNT; i++)
  {
    tsend[i] = vt_pform_wtime();
    PMPI_Send(NULL, 0, MPI_INT, slave, 1, comm);
    PMPI_Recv(NULL, 0, MPI_INT, slave, 2, comm, &stat);
    trecv[i] = vt_pform_wtime();
  }

  /* select ping pong with shortest transfer time */
  
  pingpong_time = trecv[0] - tsend[0];
  min = 0;

  for (i = 1; i < LOOP_COUNT; i++)
  {
    if ((trecv[i] - tsend[i]) < pingpong_time)
    {
      pingpong_time = (trecv[i] - tsend[i]);
      min = (VT_MPI_INT)i;
    }
  }
  
  sync_time = tsend[min] + (pingpong_time / 2);

  /* send sync_time together with corresponding measurement index to slave */
  
  PMPI_Send(&min, 1, MPI_INT, slave, 3, comm);
  PMPI_Send(&sync_time, 1, MPI_LONG_LONG_INT, slave, 4, comm);

  /* the process considered as the global clock returns 0 as offset */

  *ltime = vt_pform_wtime();
  return 0;
}


static int64_t sync_slave(uint64_t* ltime, VT_MPI_INT master, MPI_Comm comm)
{
  VT_MPI_INT min;
  MPI_Status stat;
  uint64_t tsendrecv[LOOP_COUNT];
  uint64_t sync_time;
  int i;

  for (i = 0; i < LOOP_COUNT; i++)
  {
    PMPI_Recv(NULL, 0, MPI_INT, master, 1, comm, &stat);
    tsendrecv[i] = vt_pform_wtime();
    PMPI_Send(NULL, 0, MPI_INT, master, 2, comm);
  }

  /* receive corresponding time together with its index from master */

  PMPI_Recv(&min, 1, MPI_INT, master, 3, comm,  &stat);
  PMPI_Recv(&sync_time, 1, MPI_LONG_LONG_INT, master, 4, comm, &stat);

  *ltime = tsendrecv[min];
  return (int64_t)(sync_time - *ltime);
}

void vt_sync(MPI_Comm comm, uint64_t* ltime, int64_t* offset)
{
  VT_MPI_INT myrank, myrank_host, myrank_sync;
  VT_MPI_INT numnodes;
  uint64_t time;

  MPI_Comm host_comm;
  MPI_Comm sync_comm;

  VT_SUSPEND_IO_TRACING(VT_CURRENT_THREAD);

  /* mark begin of clock synchronization */
  time = vt_pform_wtime();
  vt_enter(VT_CURRENT_THREAD, &time, vt_trc_regid[VT__TRC_SYNCTIME]);

  /* barrier at entry */
  PMPI_Barrier(comm);

  *offset = 0;
  *ltime = vt_pform_wtime();

  PMPI_Comm_rank(comm, &myrank);

  /* create communicator containing all processes on the same node */

  PMPI_Comm_split(comm, (vt_pform_node_id() & 0x7FFFFFFF), 0, &host_comm);
  PMPI_Comm_rank(host_comm, &myrank_host);

  /* create communicator containing all processes with rank zero in the
     previously created communicators */
  
  PMPI_Comm_split(comm, myrank_host, 0, &sync_comm);
  PMPI_Comm_rank(sync_comm, &myrank_sync);
  PMPI_Comm_size(sync_comm, &numnodes);

  /* measure offsets between all nodes and the root node (rank 0 in sync_comm) */

  if (myrank_host == 0)
  {
    VT_MPI_INT i;

    for (i = 1; i < numnodes; i++)
    {
      PMPI_Barrier(sync_comm);
      if (myrank_sync == i)
	*offset = sync_slave(ltime, 0, sync_comm);
      else if (myrank_sync == 0)
	*offset = sync_master(ltime, i, sync_comm);
    }
  }

  /* distribute offset and ltime across all processes on the same node */

  PMPI_Bcast(offset, 1, MPI_LONG_LONG_INT, 0, host_comm);
  PMPI_Bcast(ltime, 1, MPI_LONG_LONG_INT, 0, host_comm);

  PMPI_Comm_free(&host_comm);
  PMPI_Comm_free(&sync_comm);

  /* barrier at exit */
  PMPI_Barrier(comm);

  /* mark end of clock synchronization */
  time = vt_pform_wtime();
  vt_exit(VT_CURRENT_THREAD, &time);

  VT_RESUME_IO_TRACING(VT_CURRENT_THREAD);
}























