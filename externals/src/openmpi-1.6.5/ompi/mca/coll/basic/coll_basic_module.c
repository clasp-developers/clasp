/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_basic.h"

#include <stdio.h>

#include "mpi.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "coll_basic.h"


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int
mca_coll_basic_init_query(bool enable_progress_threads,
                          bool enable_mpi_threads)
{
    /* Nothing to do */

    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_basic_comm_query(struct ompi_communicator_t *comm, 
                          int *priority)
{
    int size;
    mca_coll_basic_module_t *basic_module;

    basic_module = OBJ_NEW(mca_coll_basic_module_t);
    if (NULL == basic_module) return NULL;

    *priority = mca_coll_basic_priority;

    /* Allocate the data that hangs off the communicator */

    if (OMPI_COMM_IS_INTER(comm)) {
        size = ompi_comm_remote_size(comm);
    } else {
        size = ompi_comm_size(comm);
    }
    basic_module->mccb_num_reqs = size * 2;
    basic_module->mccb_reqs = (ompi_request_t**) 
        malloc(sizeof(ompi_request_t *) * basic_module->mccb_num_reqs);

    /* Choose whether to use [intra|inter], and [linear|log]-based
     * algorithms. */
    basic_module->super.coll_module_enable = mca_coll_basic_module_enable;
    basic_module->super.ft_event = mca_coll_basic_ft_event;

    if (OMPI_COMM_IS_INTER(comm)) {
        basic_module->super.coll_allgather  = mca_coll_basic_allgather_inter;
        basic_module->super.coll_allgatherv = mca_coll_basic_allgatherv_inter;
        basic_module->super.coll_allreduce  = mca_coll_basic_allreduce_inter;
        basic_module->super.coll_alltoall   = mca_coll_basic_alltoall_inter;
        basic_module->super.coll_alltoallv  = mca_coll_basic_alltoallv_inter;
        basic_module->super.coll_alltoallw  = mca_coll_basic_alltoallw_inter;
        basic_module->super.coll_barrier    = mca_coll_basic_barrier_inter_lin;
        basic_module->super.coll_bcast      = mca_coll_basic_bcast_lin_inter;
        basic_module->super.coll_exscan     = NULL;
        basic_module->super.coll_gather     = mca_coll_basic_gather_inter;
        basic_module->super.coll_gatherv    = mca_coll_basic_gatherv_inter;
        basic_module->super.coll_reduce     = mca_coll_basic_reduce_lin_inter;
        basic_module->super.coll_reduce_scatter = mca_coll_basic_reduce_scatter_inter;
        basic_module->super.coll_scan       = NULL;
        basic_module->super.coll_scatter    = mca_coll_basic_scatter_inter;
        basic_module->super.coll_scatterv   = mca_coll_basic_scatterv_inter;
    } else if (ompi_comm_size(comm) <= mca_coll_basic_crossover) {
        basic_module->super.coll_allgather  = mca_coll_basic_allgather_intra;
        basic_module->super.coll_allgatherv = mca_coll_basic_allgatherv_intra;
        basic_module->super.coll_allreduce  = mca_coll_basic_allreduce_intra;
        basic_module->super.coll_alltoall   = mca_coll_basic_alltoall_intra;
        basic_module->super.coll_alltoallv  = mca_coll_basic_alltoallv_intra;
        basic_module->super.coll_alltoallw  = mca_coll_basic_alltoallw_intra;
        basic_module->super.coll_barrier    = mca_coll_basic_barrier_intra_lin;
        basic_module->super.coll_bcast      = mca_coll_basic_bcast_lin_intra;
        basic_module->super.coll_exscan     = mca_coll_basic_exscan_intra;
        basic_module->super.coll_gather     = mca_coll_basic_gather_intra;
        basic_module->super.coll_gatherv    = mca_coll_basic_gatherv_intra;
        basic_module->super.coll_reduce     = mca_coll_basic_reduce_lin_intra;
        basic_module->super.coll_reduce_scatter = mca_coll_basic_reduce_scatter_intra;
        basic_module->super.coll_scan       = mca_coll_basic_scan_intra;
        basic_module->super.coll_scatter    = mca_coll_basic_scatter_intra;
        basic_module->super.coll_scatterv   = mca_coll_basic_scatterv_intra;
    } else {
        basic_module->super.coll_allgather  = mca_coll_basic_allgather_intra;
        basic_module->super.coll_allgatherv = mca_coll_basic_allgatherv_intra;
        basic_module->super.coll_allreduce  = mca_coll_basic_allreduce_intra;
        basic_module->super.coll_alltoall   = mca_coll_basic_alltoall_intra;
        basic_module->super.coll_alltoallv  = mca_coll_basic_alltoallv_intra;
        basic_module->super.coll_alltoallw  = mca_coll_basic_alltoallw_intra;
        basic_module->super.coll_barrier    = mca_coll_basic_barrier_intra_log;
        basic_module->super.coll_bcast      = mca_coll_basic_bcast_log_intra;
        basic_module->super.coll_exscan     = mca_coll_basic_exscan_intra;
        basic_module->super.coll_gather     = mca_coll_basic_gather_intra;
        basic_module->super.coll_gatherv    = mca_coll_basic_gatherv_intra;
        basic_module->super.coll_reduce     = mca_coll_basic_reduce_log_intra;
        basic_module->super.coll_reduce_scatter = mca_coll_basic_reduce_scatter_intra;
        basic_module->super.coll_scan       = mca_coll_basic_scan_intra;
        basic_module->super.coll_scatter    = mca_coll_basic_scatter_intra;
        basic_module->super.coll_scatterv   = mca_coll_basic_scatterv_intra;
    }

    return &(basic_module->super);
}


/*
 * Init module on the communicator
 */
int
mca_coll_basic_module_enable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm)
{
    /* All done */
    return OMPI_SUCCESS;
}


int
mca_coll_basic_ft_event(int state) {
    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
