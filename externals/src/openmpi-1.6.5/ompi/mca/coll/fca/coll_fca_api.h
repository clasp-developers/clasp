/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */

#include "ompi_config.h"

#include <fca_api.h>
#include <config/fca_parse_specfile.h>

#ifndef FCA_API
#define OMPI_FCA_VERSION 12
#else
#define OMPI_FCA_VERSION FCA_API
#endif

/*
 * FCA API compatibility layer.
 * MPI build must define an FCA version macro.
 */

#define OMPI_FCA_BARRIER            1
#define OMPI_FCA_BCAST              1
#define OMPI_FCA_REDUCE             1
#define OMPI_FCA_ALLREDUCE          1

#define OMPI_FCA_REDUCE_SCATTER     0
#define OMPI_FCA_GATHER             0
#define OMPI_FCA_GATHERV            0
#define OMPI_FCA_ALLTOALL           0
#define OMPI_FCA_ALLTOALLV          0
#define OMPI_FCA_ALLTOALLW          0


#if OMPI_FCA_VERSION == 12

#define OMPI_FCA_ALLGATHER          0

#define FCA_API_ABI_MAJOR           1
#define FCA_API_ABI_MINOR           2
#define FCA_MAJOR_BIT               24ul
#define FCA_MINOR_BIT               16ul
#define EUSEMPI                     287

static inline void mca_coll_fca_get_bcast_root(int root_rank, int *local_ranks,
                                               int num_local_ranks,
                                               fca_bcast_spec_t *spec)
{
    int i;

    for (i = 0; i < num_local_ranks; ++i) {
        if (local_ranks[i] == root_rank) {
            spec->root_indx = i;
            return;
        }
    }
    spec->root_indx = -1;
}

static inline void mca_coll_fca_get_reduce_root(int root_rank, int my_rank,
                                                fca_reduce_spec_t *spec)
{
    spec->is_root = root_rank == my_rank;
}

#elif OMPI_FCA_VERSION >= 20 

#define OMPI_FCA_ALLGATHER          1
#define OMPI_FCA_ALLGATHERV         1
#define OMPI_FCA_PROGRESS           1

static inline int mca_coll_fca_comm_init(fca_t *fca_context, int rank, int comm_size,
                                         int local_proc_idx, int num_local_procs,
                                         fca_comm_desc_t *comm_desc,
                                         fca_comm_t **fca_comm)
{
    fca_comm_init_spec_t spec;

    spec.rank = rank;
    spec.size = comm_size;
    spec.desc = *comm_desc;
    spec.proc_idx = local_proc_idx;
    spec.num_procs = num_local_procs;
    return fca_comm_init(fca_context, &spec, fca_comm);
}

static inline void mca_coll_fca_get_bcast_root(int root_rank, int *local_ranks,
                                               int num_local_ranks,
                                               fca_bcast_spec_t *spec)
{
    spec->root = root_rank;
}

static inline void mca_coll_fca_get_reduce_root(int root_rank, int my_rank,
                                                fca_reduce_spec_t *spec)
{
    spec->root = root_rank;
}

#else

#error "FCA API version is unsupported"

#endif
