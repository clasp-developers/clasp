/**
  Copyright (c) 2011 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */
#include "coll_fca.h"
#include "opal/mca/paffinity/paffinity.h"

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_coll_fca_init_query(bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}

static int have_remote_peers(ompi_group_t *group, size_t size, int *local_peers)
{
    ompi_proc_t *proc;
    size_t i;
    int ret;

    *local_peers = 0;
    ret = 0;
    for (i = 0; i < size; ++i) {
        proc = ompi_group_peer_lookup(group, i);
        if (FCA_IS_LOCAL_PROCESS(proc->proc_flags)) {
            ++*local_peers;
        } else {
            ret = 1;
        }
    }
    return ret;
}

static inline ompi_proc_t* __local_rank_lookup(ompi_communicator_t *comm, int rank)
{
    return ompi_group_peer_lookup(comm->c_local_group, rank);
}

/**
 * Fills local rank information in fca_module.
 */
static int __get_local_ranks(mca_coll_fca_module_t *fca_module)
{
    ompi_communicator_t *comm = fca_module->comm;
    ompi_proc_t* proc;
    int i, rank;

    /* Count the local ranks */
    fca_module->num_local_procs = 0;
    for (rank = 0; rank < ompi_comm_size(comm); ++rank) {
        proc = __local_rank_lookup(comm, rank);
        if (FCA_IS_LOCAL_PROCESS(proc->proc_flags)) {
            if (rank == fca_module->rank) {
                fca_module->local_proc_idx = fca_module->num_local_procs;
            }
            ++fca_module->num_local_procs;
        }
    }

    /* Make a list of local ranks */
    fca_module->local_ranks = calloc(fca_module->num_local_procs,
                                     sizeof *fca_module->local_ranks);
    if (!fca_module->local_ranks) {
        FCA_ERROR("Failed to allocate memory for %d local ranks",
                  fca_module->num_local_procs);
        return OMPI_ERROR;
    }

    i = 0;
    for (rank = 0; rank < ompi_comm_size(comm); ++rank) {
        proc = __local_rank_lookup(comm, rank);
        if (FCA_IS_LOCAL_PROCESS(proc->proc_flags)) {
            fca_module->local_ranks[i++] = rank;
        }
    }

    FCA_MODULE_VERBOSE(fca_module, 3, "i am %d/%d", fca_module->local_proc_idx,
                       fca_module->num_local_procs);
    return OMPI_SUCCESS;
}

static int __fca_comm_new(mca_coll_fca_module_t *fca_module)
{
    ompi_communicator_t *comm = fca_module->comm;
    fca_comm_new_spec_t spec;
    int info_size, all_info_size;
    void *all_info, *my_info;
    int *rcounts, *disps;
    int i, rc, ret;

    /* call fca_get_rank_info() on node managers only*/
    if (fca_module->local_proc_idx == 0) {
        my_info = fca_get_rank_info(mca_coll_fca_component.fca_context,
                                                               &info_size);
        if (!my_info) {
            FCA_ERROR("fca_get_rank_info returned NULL");
            return OMPI_ERROR;
        }

    } else {
        info_size = 0;
    }
    FCA_MODULE_VERBOSE(fca_module, 1, "Info size: %d", info_size);

    /* Allocate gather buffer on the root rank */
    if (fca_module->rank == 0) {
        rcounts = calloc(ompi_comm_size(comm), sizeof *rcounts);
    }

    /* Get all rank info sizes using MPI_Gather */
    rc = comm->c_coll.coll_gather(&info_size, 1, MPI_INT, rcounts, 1, MPI_INT, 0,
                                  comm, comm->c_coll.coll_gather_module);
    if (rc != OMPI_SUCCESS)
        return rc;

    /* Allocate buffer for gathering rank information on rank0 */
    if (fca_module->rank == 0) {
        all_info_size = 0;
        FCA_MODULE_VERBOSE(fca_module, 1, "Total rank_info size: %d", all_info_size);
        disps = calloc(ompi_comm_size(comm), sizeof *disps);
        for (i = 0; i < ompi_comm_size(comm); ++i) {
            disps[i] = all_info_size;
            all_info_size += rcounts[i];
        }
        all_info = calloc(all_info_size, 1);
    }

    /* Send all node managers information to rank0 using MPI_Gatherv*/
    rc = comm->c_coll.coll_gatherv(my_info, info_size, MPI_BYTE,
                                   all_info, rcounts, disps, MPI_BYTE, 0,
                                   comm, comm->c_coll.coll_gather_module);
    if (rc != OMPI_SUCCESS) {
        FCA_ERROR("Failed to gather rank information to rank0: %d", rc);
        return rc;
    }

    /* Rank0 calls fca_comm_new() and fills fca_comm_spec filed */
    if (fca_module->rank == 0) {
        spec.rank_info  = all_info;
        spec.is_comm_world = comm == MPI_COMM_WORLD;
        spec.rank_count = 0;
        for (i = 0; i < ompi_comm_size(comm); ++i) {
            FCA_MODULE_VERBOSE(fca_module, 1, "rcounts[%d]=%d disps[%d]=%d",
                               i, rcounts[i], i, disps[i]);
            if (rcounts[i] > 0)
                ++spec.rank_count;
        }
        free(disps);
        free(rcounts);

        FCA_MODULE_VERBOSE(fca_module, 1, "starting fca_comm_new(), rank_count: %d",
                           spec.rank_count);

        ret = fca_comm_new(mca_coll_fca_component.fca_context,
                                                      &spec, &fca_module->fca_comm_desc);
        free(all_info);
    }

    /* Broadcast return value from rank0 to all other ranks */
    rc = fca_module->previous_bcast(&ret, 1, MPI_INT, 0, comm,
                                    fca_module->previous_bcast_module);
    if (rc != OMPI_SUCCESS) {
        FCA_ERROR("Failed to broadcast comm_new return value from rank0: %d", rc);
        return rc;
    }

    /* Examine comm_new return value */
    if (ret < 0) {
        FCA_ERROR("COMM_NEW failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }

    /* Release allocate rank_info on node managers */
    if (fca_module->local_proc_idx == 0) {
        fca_free_rank_info(my_info);
    }

    /* Pass fca_comm_desc to all ranks using MPI_Bcast */
    rc = fca_module->previous_bcast(&fca_module->fca_comm_desc,
                                    sizeof(fca_module->fca_comm_desc), MPI_BYTE, 0,
                                    comm, fca_module->previous_bcast_module);
    if (rc != OMPI_SUCCESS) {
        FCA_ERROR("Failed to broadcast comm_desc from rank0: %d", rc);
        return rc;
    }

    FCA_MODULE_VERBOSE(fca_module, 1, "Received FCA communicator spec, comm_id %d",
                       fca_module->fca_comm_desc.comm_id);
    return OMPI_SUCCESS;
}

static int __create_fca_comm(mca_coll_fca_module_t *fca_module)
{
    int comm_size;
    int rc, ret;

    rc = __fca_comm_new(fca_module);
    if (rc != OMPI_SUCCESS)
        return rc;

    /* allocate comm_init_spec */
    FCA_MODULE_VERBOSE(fca_module, 1, "Starting COMM_INIT comm_id %d proc_idx %d num_procs %d",
                       fca_module->fca_comm_desc.comm_id, fca_module->local_proc_idx,
                       fca_module->num_local_procs);

    comm_size = ompi_comm_size(fca_module->comm);
    ret = mca_coll_fca_comm_init(mca_coll_fca_component.fca_context,
                                 fca_module->rank, comm_size,
                                 fca_module->local_proc_idx, fca_module->num_local_procs,
                                 &fca_module->fca_comm_desc, &fca_module->fca_comm);
    if (ret < 0) {
        FCA_ERROR("COMM_INIT failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }

    /* get communicator capabilities */
    ret = fca_comm_get_caps(fca_module->fca_comm,
                                                       &fca_module->fca_comm_caps);
    if (ret < 0) {
        FCA_ERROR("GET_COMM_CAPS failed: %s", fca_strerror(ret));
        return OMPI_ERROR;
    }

    /* by this point every rank in the communicator is set up */
    FCA_MODULE_VERBOSE(fca_module, 1, "Initialized FCA communicator, comm_id %d",
                       fca_module->fca_comm_desc.comm_id);
    return OMPI_SUCCESS;
}

static void __destroy_fca_comm(mca_coll_fca_module_t *fca_module)
{
    int ret;

    fca_comm_destroy(fca_module->fca_comm);
    if (fca_module->rank == 0) {
        ret = fca_comm_end(mca_coll_fca_component.fca_context,
                                                      fca_module->fca_comm_desc.comm_id);
        if (ret < 0) {
            FCA_ERROR("COMM_END failed: %s", fca_strerror(ret));
        }
    }

    FCA_MODULE_VERBOSE(fca_module, 1, "Destroyed FCA communicator, comm_id %d",
                       fca_module->fca_comm_desc.comm_id);
}

#define FCA_SAVE_PREV_COLL_API(__api) do {\
    fca_module->previous_ ## __api            = comm->c_coll.coll_ ## __api;\
    fca_module->previous_ ## __api ## _module = comm->c_coll.coll_ ## __api ## _module;\
    if (!comm->c_coll.coll_ ## __api || !comm->c_coll.coll_ ## __api ## _module) {\
	    FCA_VERBOSE(1, "(%d/%s): no underlying " # __api"; disqualifying myself",\
                    comm->c_contextid, comm->c_name);\
        return OMPI_ERROR;\
    }\
    OBJ_RETAIN(fca_module->previous_ ## __api ## _module);\
} while(0)

static int __save_coll_handlers(mca_coll_fca_module_t *fca_module)
{
    ompi_communicator_t *comm = fca_module->comm;

    FCA_SAVE_PREV_COLL_API(barrier);
    FCA_SAVE_PREV_COLL_API(bcast);
    FCA_SAVE_PREV_COLL_API(reduce);
    FCA_SAVE_PREV_COLL_API(allreduce);
    FCA_SAVE_PREV_COLL_API(allgather);
    FCA_SAVE_PREV_COLL_API(allgatherv);
    FCA_SAVE_PREV_COLL_API(gather);
    FCA_SAVE_PREV_COLL_API(gatherv);
    FCA_SAVE_PREV_COLL_API(alltoall);
    FCA_SAVE_PREV_COLL_API(alltoallv);
    FCA_SAVE_PREV_COLL_API(alltoallw);
    FCA_SAVE_PREV_COLL_API(reduce_scatter);

    return OMPI_SUCCESS;
}

/*
 * Initialize module on the communicator
 */
static int mca_coll_fca_module_enable(mca_coll_base_module_t *module,
                                      struct ompi_communicator_t *comm)
{

    mca_coll_fca_module_t *fca_module = (mca_coll_fca_module_t*) module;
    int rc;

    fca_module->comm = comm;
    fca_module->rank = ompi_comm_rank(comm);

    rc = mca_coll_fca_get_fca_lib(comm);
    if (rc != OMPI_SUCCESS)
        return rc;

    rc = __save_coll_handlers(fca_module);
    if (rc != OMPI_SUCCESS)
        return rc;

    rc = __get_local_ranks(fca_module);
    if (rc != OMPI_SUCCESS)
        return rc;

    rc = __create_fca_comm(fca_module);
    if (rc != OMPI_SUCCESS)
        return rc;

    FCA_MODULE_VERBOSE(fca_module, 1, "FCA Module initialized");
    return OMPI_SUCCESS;
}


static int mca_coll_fca_ft_event(int state)
{    
    return OMPI_SUCCESS;
}

static void mca_coll_fca_module_clear(mca_coll_fca_module_t *fca_module)
{
    fca_module->num_local_procs = 0;
    fca_module->local_ranks = NULL;
    fca_module->fca_comm = NULL;

    fca_module->previous_barrier    = NULL;
    fca_module->previous_bcast      = NULL;
    fca_module->previous_reduce     = NULL;
    fca_module->previous_allreduce  = NULL;
    fca_module->previous_allgather  = NULL;
    fca_module->previous_allgatherv = NULL;
    fca_module->previous_gather     = NULL;
    fca_module->previous_gatherv    = NULL;
    fca_module->previous_alltoall   = NULL;
    fca_module->previous_alltoallv  = NULL;
    fca_module->previous_alltoallw  = NULL;
    fca_module->previous_reduce_scatter  = NULL;
}

static void mca_coll_fca_module_construct(mca_coll_fca_module_t *fca_module)
{
    FCA_VERBOSE(5, "==>");
    mca_coll_fca_module_clear(fca_module);
}

static void mca_coll_fca_module_destruct(mca_coll_fca_module_t *fca_module)
{
    FCA_VERBOSE(5, "==>");
    OBJ_RELEASE(fca_module->previous_barrier_module);
    OBJ_RELEASE(fca_module->previous_bcast_module);
    OBJ_RELEASE(fca_module->previous_reduce_module);
    OBJ_RELEASE(fca_module->previous_allreduce_module);
    OBJ_RELEASE(fca_module->previous_allgather_module);
    OBJ_RELEASE(fca_module->previous_allgatherv_module);
    OBJ_RELEASE(fca_module->previous_gather_module);
    OBJ_RELEASE(fca_module->previous_gatherv_module);
    OBJ_RELEASE(fca_module->previous_alltoall_module);
    OBJ_RELEASE(fca_module->previous_alltoallv_module);
    OBJ_RELEASE(fca_module->previous_alltoallw_module);
    OBJ_RELEASE(fca_module->previous_reduce_scatter_module);
    if (fca_module->fca_comm)
        __destroy_fca_comm(fca_module);
    free(fca_module->local_ranks);
    mca_coll_fca_module_clear(fca_module);
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_fca_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    mca_coll_base_module_t *module;
    int size = ompi_comm_size(comm);
    int local_peers;
    mca_coll_fca_module_t *fca_module;

    *priority = 0;
    module = NULL;

    if (!mca_coll_fca_component.fca_enable)
        goto exit;

    if (size < mca_coll_fca_component.fca_np)
        goto exit;

    if (!have_remote_peers(comm->c_local_group, size, &local_peers) || OMPI_COMM_IS_INTER(comm))
        goto exit;

    fca_module = OBJ_NEW(mca_coll_fca_module_t);
    if (!fca_module)
        goto exit;

    fca_module->super.coll_module_enable = mca_coll_fca_module_enable;
    fca_module->super.ft_event        = mca_coll_fca_ft_event;
    fca_module->super.coll_allgather  = mca_coll_fca_component.fca_enable_allgather?  mca_coll_fca_allgather  : NULL;
    fca_module->super.coll_allgatherv = mca_coll_fca_component.fca_enable_allgatherv? mca_coll_fca_allgatherv : NULL;
    fca_module->super.coll_allreduce  = mca_coll_fca_component.fca_enable_allreduce?  mca_coll_fca_allreduce  : NULL;
    fca_module->super.coll_alltoall   = mca_coll_fca_component.fca_enable_alltoall?   mca_coll_fca_alltoall   : NULL;
    fca_module->super.coll_alltoallv  = mca_coll_fca_component.fca_enable_alltoallv?  mca_coll_fca_alltoallv  : NULL;
    fca_module->super.coll_alltoallw  = mca_coll_fca_component.fca_enable_alltoallw?  mca_coll_fca_alltoallw  : NULL;
    fca_module->super.coll_barrier    = mca_coll_fca_component.fca_enable_barrier?    mca_coll_fca_barrier    : NULL;
    fca_module->super.coll_bcast      = mca_coll_fca_component.fca_enable_bcast?      mca_coll_fca_bcast      : NULL;
    fca_module->super.coll_exscan     = NULL;
    fca_module->super.coll_gather     = mca_coll_fca_component.fca_enable_gather?     mca_coll_fca_gather     : NULL;
    fca_module->super.coll_gatherv    = mca_coll_fca_component.fca_enable_gatherv?    mca_coll_fca_gatherv    : NULL;
    fca_module->super.coll_reduce     = mca_coll_fca_component.fca_enable_reduce?     mca_coll_fca_reduce     : NULL;
    fca_module->super.coll_reduce_scatter = mca_coll_fca_component.fca_enable_reduce_scatter? mca_coll_fca_reduce_scatter  : NULL;
    fca_module->super.coll_scan       = NULL;
    fca_module->super.coll_scatter    = NULL;
    fca_module->super.coll_scatterv   = NULL;

    *priority = mca_coll_fca_component.fca_priority;
    module = &fca_module->super;

exit:
    FCA_VERBOSE(4, "Query FCA module for comm %p size %d rank %d local_peers=%d: priority=%d %s",
                (void *)comm, size, ompi_comm_rank(comm), local_peers,
                *priority, module ? "enabled" : "disabled");
    return module;
}

OBJ_CLASS_INSTANCE(mca_coll_fca_module_t,
                   mca_coll_base_module_t,
                   mca_coll_fca_module_construct,
                   mca_coll_fca_module_destruct);
