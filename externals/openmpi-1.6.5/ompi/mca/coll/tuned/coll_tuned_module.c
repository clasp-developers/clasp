/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_tuned.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"
#include "coll_tuned_dynamic_rules.h"
#include "coll_tuned_dynamic_file.h"

static int tuned_module_enable(mca_coll_base_module_t *module,
			       struct ompi_communicator_t *comm);
/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.
 */
int ompi_coll_tuned_init_query(bool enable_progress_threads,
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
ompi_coll_tuned_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    mca_coll_tuned_module_t *tuned_module;

    OPAL_OUTPUT((ompi_coll_tuned_stream, "coll:tuned:module_tuned query called"));

    /**
     * No support for inter-communicator yet.
     */
    if (OMPI_COMM_IS_INTER(comm)) {
        *priority = 0;
        return NULL;
    }

    /**
     * If it is inter-communicator and size is less than 2 we have specialized modules
     * to handle the intra collective communications.
     */
    if (OMPI_COMM_IS_INTRA(comm) && ompi_comm_size(comm) < 2) {
        *priority = 0;
        return NULL;
    }

    tuned_module = OBJ_NEW(mca_coll_tuned_module_t);
    if (NULL == tuned_module) return NULL;

    *priority = ompi_coll_tuned_priority;

    /* 
     * Choose whether to use [intra|inter] decision functions 
     * and if using fixed OR dynamic rule sets.
     * Right now you cannot mix them, maybe later on it can be changed
     * but this would probably add an extra if and funct call to the path
     */
    tuned_module->super.coll_module_enable = tuned_module_enable;
    tuned_module->super.ft_event = mca_coll_tuned_ft_event;

    /* By default stick with the fied version of the tuned collectives. Later on,
     * when the module get enabled, set the correct version based on the availability
     * of the dynamic rules.
     */
    tuned_module->super.coll_allgather  = ompi_coll_tuned_allgather_intra_dec_fixed;
    tuned_module->super.coll_allgatherv = ompi_coll_tuned_allgatherv_intra_dec_fixed;
    tuned_module->super.coll_allreduce  = ompi_coll_tuned_allreduce_intra_dec_fixed;
    tuned_module->super.coll_alltoall   = ompi_coll_tuned_alltoall_intra_dec_fixed;
    tuned_module->super.coll_alltoallv  = ompi_coll_tuned_alltoallv_intra_dec_fixed;
    tuned_module->super.coll_alltoallw  = NULL;
    tuned_module->super.coll_barrier    = ompi_coll_tuned_barrier_intra_dec_fixed;
    tuned_module->super.coll_bcast      = ompi_coll_tuned_bcast_intra_dec_fixed;
    tuned_module->super.coll_exscan     = NULL;
    tuned_module->super.coll_gather     = ompi_coll_tuned_gather_intra_dec_fixed;
    tuned_module->super.coll_gatherv    = NULL;
    tuned_module->super.coll_reduce     = ompi_coll_tuned_reduce_intra_dec_fixed;
    tuned_module->super.coll_reduce_scatter = ompi_coll_tuned_reduce_scatter_intra_dec_fixed;
    tuned_module->super.coll_scan       = NULL;
    tuned_module->super.coll_scatter    = ompi_coll_tuned_scatter_intra_dec_fixed;
    tuned_module->super.coll_scatterv   = NULL;

    return &(tuned_module->super);
}

/* We put all routines that handle the MCA user forced algorithm and parameter choices here */
/* recheck the setting of forced, called on module create (i.e. for each new comm) */
                                                                                                          
static int
ompi_coll_tuned_forced_getvalues( enum COLLTYPE type, 
                                  coll_tuned_force_algorithm_params_t *forced_values )
{
    coll_tuned_force_algorithm_mca_param_indices_t* mca_params;

    mca_params = &(ompi_coll_tuned_forced_params[type]);

    /**
     * Set the selected algorithm to 0 by default. Later on we can check this against 0
     * to see if it was setted explicitly (if we suppose that setting it to 0 enable the
     * default behavior) or not.
     */
    forced_values->algorithm = 0;
    mca_base_param_lookup_int (mca_params->algorithm_param_index,    &(forced_values->algorithm));
    if( BARRIER != type ) {
        mca_base_param_lookup_int (mca_params->segsize_param_index,      &(forced_values->segsize));
        mca_base_param_lookup_int (mca_params->tree_fanout_param_index,  &(forced_values->tree_fanout));
        mca_base_param_lookup_int (mca_params->chain_fanout_param_index, &(forced_values->chain_fanout));
        mca_base_param_lookup_int (mca_params->max_requests_param_index, &(forced_values->max_requests));
    }
    return (MPI_SUCCESS);
}

#define COLL_TUNED_EXECUTE_IF_DYNAMIC(DATA, TYPE, EXECUTE)              \
    {                                                                   \
        int need_dynamic_decision = 0;                                  \
        ompi_coll_tuned_forced_getvalues( (TYPE), &((DATA)->user_forced[(TYPE)]) ); \
        (DATA)->com_rules[(TYPE)] = NULL;                               \
        if( 0 != (DATA)->user_forced[(TYPE)].algorithm ) {              \
            need_dynamic_decision = 1;                                  \
            EXECUTE;                                                    \
        }                                                               \
        if( NULL != mca_coll_tuned_component.all_base_rules ) {         \
            (DATA)->com_rules[(TYPE)]                                   \
                = ompi_coll_tuned_get_com_rule_ptr( mca_coll_tuned_component.all_base_rules, \
                                                    (TYPE), size );     \
            if( NULL != (DATA)->com_rules[(TYPE)] ) {                   \
                need_dynamic_decision = 1;                              \
            }                                                           \
        }                                                               \
        if( 1 == need_dynamic_decision ) {                              \
            OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned: enable dynamic selection for "#TYPE)); \
            ompi_coll_tuned_use_dynamic_rules = 1;                      \
            EXECUTE;                                                    \
        }                                                               \
    }

/*
 * Init module on the communicator
 */
static int
tuned_module_enable( mca_coll_base_module_t *module,
                     struct ompi_communicator_t *comm )
{
    int size;
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t *) module;
    mca_coll_tuned_comm_t *data = NULL;

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init called."));

    /* Allocate the data that hangs off the communicator */
    if (OMPI_COMM_IS_INTER(comm)) {
        size = ompi_comm_remote_size(comm);
    } else {
        size = ompi_comm_size(comm);
    }

    /**
     * we still malloc data as it is used by the TUNED modules
     * if we don't allocate it and fall back to a BASIC module routine then confuses debuggers 
     * we place any special info after the default data
     *
     * BUT on very large systems we might not be able to allocate all this memory so
     * we do check a MCA parameter to see if if we should allocate this memory
     *
     * The default is set very high  
     *
     */

    /* if we within the memory/size limit, allow preallocated data */
    if( size <= ompi_coll_tuned_preallocate_memory_comm_size_limit ) {
        data = (mca_coll_tuned_comm_t*)malloc(sizeof(struct mca_coll_tuned_comm_t) +
                                              (sizeof(ompi_request_t *) * size * 2));
        if (NULL == data) {
            return OMPI_ERROR;
        }
        data->mcct_reqs = (ompi_request_t **) (data + 1);
        data->mcct_num_reqs = size * 2;
    } else {
        data = (mca_coll_tuned_comm_t*)malloc(sizeof(struct mca_coll_tuned_comm_t)); 
        if (NULL == data) {
            return OMPI_ERROR;
        }
        data->mcct_reqs = (ompi_request_t **) NULL;
        data->mcct_num_reqs = 0;
    }

    if (ompi_coll_tuned_use_dynamic_rules) {
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init MCW & Dynamic"));
        /**
         * Reset it to 0, it will be enabled again if we discover any need for dynamic decisions.
         */
        ompi_coll_tuned_use_dynamic_rules = 0;

        /**
         * next dynamic state, recheck all forced rules as well
         * warning, we should check to make sure this is really an INTRA comm here...
         */
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, ALLGATHER,
                                      tuned_module->super.coll_allgather  = ompi_coll_tuned_allgather_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, ALLGATHERV,
                                      tuned_module->super.coll_allgatherv = ompi_coll_tuned_allgatherv_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, ALLREDUCE,
                                      tuned_module->super.coll_allreduce  = ompi_coll_tuned_allreduce_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, ALLTOALL,
                                      tuned_module->super.coll_alltoall   = ompi_coll_tuned_alltoall_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, ALLTOALLV,
                                      tuned_module->super.coll_alltoallv  = ompi_coll_tuned_alltoallv_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, ALLTOALLW,
                                      tuned_module->super.coll_alltoallw  = NULL);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, BARRIER,
                                      tuned_module->super.coll_barrier    = ompi_coll_tuned_barrier_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, BCAST,
                                      tuned_module->super.coll_bcast      = ompi_coll_tuned_bcast_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, EXSCAN,
                                      tuned_module->super.coll_exscan     = NULL);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, GATHER,
                                      tuned_module->super.coll_gather     = ompi_coll_tuned_gather_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, GATHERV,
                                      tuned_module->super.coll_gatherv    = NULL);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, REDUCE,
                                      tuned_module->super.coll_reduce     = ompi_coll_tuned_reduce_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, REDUCESCATTER,
                                      tuned_module->super.coll_reduce_scatter = ompi_coll_tuned_reduce_scatter_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, SCAN,
                                      tuned_module->super.coll_scan       = NULL);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, SCATTER,
                                      tuned_module->super.coll_scatter    = ompi_coll_tuned_scatter_intra_dec_dynamic);
        COLL_TUNED_EXECUTE_IF_DYNAMIC(data, SCATTERV,
                                      tuned_module->super.coll_scatterv   = NULL);

        if( 0 == ompi_coll_tuned_use_dynamic_rules ) {
            /* no real need for dynamic decisions */
            OPAL_OUTPUT((ompi_coll_tuned_stream, "coll:tuned:module_enable switch back to fixed"
                         " decision by lack of dynamic rules"));
        }
    }
    
    /* general n fan out tree */
    data->cached_ntree = NULL;
    /* binary tree */
    data->cached_bintree = NULL;
    /* binomial tree */
    data->cached_bmtree = NULL;
    /* binomial tree */
    data->cached_in_order_bmtree = NULL;
    /* chains (fanout followed by pipelines) */
    data->cached_chain = NULL;
    /* standard pipeline */
    data->cached_pipeline = NULL;
    /* in-order binary tree */
    data->cached_in_order_bintree = NULL;

    /* All done */
    tuned_module->tuned_data = data;

    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:module_init Tuned is in use"));
    return OMPI_SUCCESS;
}

int mca_coll_tuned_ft_event(int state) {
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
