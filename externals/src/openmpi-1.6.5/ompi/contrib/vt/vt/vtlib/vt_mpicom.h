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

#ifndef _VT_MPICOM_H
#define _VT_MPICOM_H

#ifdef __cplusplus
# define EXTERN extern "C" 
#else
# define EXTERN extern 
#endif

#include "config.h"

#include "vt_inttypes.h"
#include "vt_trc.h"

#include "mpi.h"

/* MPI communicator |-> VampirTrace communicator id */
#define VT_COMM_ID(c) \
  (((c)==MPI_COMM_WORLD) ? vt_mpi_comm_world_cid : \
   ((c)==MPI_COMM_SELF)  ? vt_mpi_comm_self_cid  : \
   vt_comm_id(c))

/* Rank with respect to arbitrary communicator |-> global rank */
#define VT_RANK_TO_PE(r,c) \
  (((c)==MPI_COMM_WORLD) ? (uint32_t)r :  \
   ((c)==MPI_COMM_SELF)  ? (uint32_t)vt_my_trace : \
   vt_rank_to_pe(r,c))

/* Rank with respect to arbitrary group |-> global rank */
#define VT_RANK_TO_PE_BY_GROUP(r,g) \
  (((g)==vt_mpi_comm_world_group) ? (uint32_t)r           : \
   ((g)==vt_mpi_comm_self_group)  ? (uint32_t)vt_my_trace : \
   vt_rank_to_pe_by_group(r,g))

EXTERN void     vt_comm_init(void);
EXTERN void     vt_comm_finalize(void);
EXTERN uint32_t vt_rank_to_pe(VT_MPI_INT rank, MPI_Comm comm);
EXTERN uint32_t vt_rank_to_pe_by_group(VT_MPI_INT rank, MPI_Group group);

EXTERN void     vt_group_create(MPI_Group group);
EXTERN void     vt_group_free(MPI_Group group);
EXTERN uint32_t vt_group_id(MPI_Group group);

EXTERN void     vt_comm_create(MPI_Comm comm);
EXTERN void     vt_comm_free(MPI_Comm comm);
EXTERN uint32_t vt_comm_id(MPI_Comm comm);

#if defined(HAVE_MPI2_1SIDED) && HAVE_MPI2_1SIDED
EXTERN void     vt_win_create(MPI_Win win, MPI_Comm comm);
EXTERN void     vt_win_free(MPI_Win win);
EXTERN void     vt_win_id(MPI_Win win, MPI_Comm* comm, uint32_t* gid, uint32_t* wid);
EXTERN void     vt_win_set_gid(MPI_Win win, uint32_t gid);
#endif /* HAVE_MPI2_1SIDED */

/* group of MPI_COMM_WORLD */
EXTERN MPI_Group vt_mpi_comm_world_group;

/* group of MPI_COMM_SELF */
EXTERN MPI_Group vt_mpi_comm_self_group;

/* process group id of MPI_COMM_WORLD */
EXTERN uint32_t  vt_mpi_comm_world_cid;

/* process group id of MPI_COMM_SELF */
EXTERN uint32_t  vt_mpi_comm_self_cid;

#endif
