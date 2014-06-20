/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"
#include "coll_tuned_util.h"

int
ompi_coll_tuned_alltoallv_intra_pairwise(void *sbuf, int *scounts, int *sdisps,
                                         struct ompi_datatype_t *sdtype,
                                         void* rbuf, int *rcounts, int *rdisps,
                                         struct ompi_datatype_t *rdtype,
                                         struct ompi_communicator_t *comm,
                                         mca_coll_base_module_t *module)
{
    int line = -1, err = 0;
    int rank, size, step;
    int sendto, recvfrom;
    void *psnd, *prcv;
    ptrdiff_t sext, rext;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:alltoallv_intra_pairwise rank %d", rank));

    ompi_datatype_type_extent(sdtype, &sext);
    ompi_datatype_type_extent(rdtype, &rext);

    psnd = ((char *) sbuf) + (sdisps[rank] * sext);
    prcv = ((char *) rbuf) + (rdisps[rank] * rext);

    if (0 != scounts[rank]) {
        err = ompi_datatype_sndrcv(psnd, scounts[rank], sdtype,
                              prcv, rcounts[rank], rdtype);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* If only one process, we're done. */
    if (1 == size) {
        return MPI_SUCCESS;
    }

    /* Perform pairwise exchange starting from 1 since local exhange is done */
    for (step = 1; step < size + 1; step++) {

        /* Determine sender and receiver for this step. */
        sendto  = (rank + step) % size;
        recvfrom = (rank + size - step) % size;

        /* Determine sending and receiving locations */
        psnd = (char*)sbuf + sdisps[sendto] * sext;
        prcv = (char*)rbuf + rdisps[recvfrom] * rext;

        /* send and receive */
        err = ompi_coll_tuned_sendrecv( psnd, scounts[sendto], sdtype, sendto, 
                                        MCA_COLL_BASE_TAG_ALLTOALLV,
                                        prcv, rcounts[recvfrom], rdtype, recvfrom, 
                                        MCA_COLL_BASE_TAG_ALLTOALLV,
                                        comm, MPI_STATUS_IGNORE, rank);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
    }

    return MPI_SUCCESS;
 
 err_hndl:
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "%s:%4d\tError occurred %d, rank %2d", __FILE__, line, 
                 err, rank));
    return err;
}

/*  
 * Linear functions are copied from the basic coll module.  For
 * some small number of nodes and/or small data sizes they are just as
 * fast as tuned/tree based segmenting operations and as such may be
 * selected by the decision functions.  These are copied into this module
 * due to the way we select modules in V1. i.e. in V2 we will handle this
 * differently and so will not have to duplicate code.  
 * GEF Oct05 after asking Jeff.  
 */
int
ompi_coll_tuned_alltoallv_intra_basic_linear(void *sbuf, int *scounts, int *sdisps,
                                            struct ompi_datatype_t *sdtype,
                                            void *rbuf, int *rcounts, int *rdisps,
                                            struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module)
{
    int i, size, rank, err;
    char *psnd, *prcv;
    int nreqs;
    ptrdiff_t sext, rext;
    MPI_Request *preq;
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t*) module;
    mca_coll_tuned_comm_t *data = tuned_module->tuned_data;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:alltoallv_intra_basic_linear rank %d", rank));

    ompi_datatype_type_extent(sdtype, &sext);
    ompi_datatype_type_extent(rdtype, &rext);

    /* Simple optimization - handle send to self first */
    psnd = ((char *) sbuf) + (sdisps[rank] * sext);
    prcv = ((char *) rbuf) + (rdisps[rank] * rext);
    if (0 != scounts[rank]) {
        err = ompi_datatype_sndrcv(psnd, scounts[rank], sdtype,
                              prcv, rcounts[rank], rdtype);
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* If only one process, we're done. */
    if (1 == size) {
        return MPI_SUCCESS;
    }

    /* Now, initiate all send/recv to/from others. */
    nreqs = 0;
    preq = data->mcct_reqs;

    /* Post all receives first */
    for (i = 0; i < size; ++i) {
        if (i == rank || 0 == rcounts[i]) {
            continue;
        }

        prcv = ((char *) rbuf) + (rdisps[i] * rext);
        err = MCA_PML_CALL(irecv_init(prcv, rcounts[i], rdtype,
                                      i, MCA_COLL_BASE_TAG_ALLTOALLV, comm,
                                      preq++));
        ++nreqs;
        if (MPI_SUCCESS != err) {
            ompi_coll_tuned_free_reqs(data->mcct_reqs, nreqs);
            return err;
        }
    }

    /* Now post all sends */
    for (i = 0; i < size; ++i) {
        if (i == rank || 0 == scounts[i]) {
            continue;
        }

        psnd = ((char *) sbuf) + (sdisps[i] * sext);
        err = MCA_PML_CALL(isend_init(psnd, scounts[i], sdtype,
                                      i, MCA_COLL_BASE_TAG_ALLTOALLV,
                                      MCA_PML_BASE_SEND_STANDARD, comm,
                                      preq++));
        ++nreqs;
        if (MPI_SUCCESS != err) {
            ompi_coll_tuned_free_reqs(data->mcct_reqs, nreqs);
            return err;
        }
    }

    /* Start your engines.  This will never return an error. */
    MCA_PML_CALL(start(nreqs, data->mcct_reqs));

    /* Wait for them all.  If there's an error, note that we don't care
     * what the error was -- just that there *was* an error.  The PML
     * will finish all requests, even if one or more of them fail.
     * i.e., by the end of this call, all the requests are free-able.
     * So free them anyway -- even if there was an error, and return the
     * error after we free everything. */
    err = ompi_request_wait_all(nreqs, data->mcct_reqs,
                                MPI_STATUSES_IGNORE);

    /* Free the requests. */
    ompi_coll_tuned_free_reqs(data->mcct_reqs, nreqs);

    return err;
}

/* 
 * The following are used by dynamic and forced rules.  Publish
 * details of each algorithm and if its forced/fixed/locked in as you add
 * methods/algorithms you must update this and the query/map routines.
 * This routine is called by the component only.  This makes sure that
 * the mca parameters are set to their initial values and perms.
 * Module does not call this.  They call the forced_getvalues routine
 * instead.
 */
int ompi_coll_tuned_alltoallv_intra_check_forced_init(coll_tuned_force_algorithm_mca_param_indices_t
                                                      *mca_param_indices)
{
    int max_alg = 2, requested_alg;

    ompi_coll_tuned_forced_max_algorithms[ALLTOALLV] = max_alg;

    mca_base_param_reg_int (&mca_coll_tuned_component.super.collm_version,
                            "alltoallv_algorithm_count",
                            "Number of alltoallv algorithms available",
                            false, true, max_alg, NULL);

    mca_param_indices->algorithm_param_index
        = mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                                 "alltoallv_algorithm",
                                 "Which alltoallv algorithm is used. "
                                 "Can be locked down to choice of: 0 ignore, "
                                 "1 basic linear, 2 pairwise.",
                                 false, false, 0, NULL);
    if (mca_param_indices->algorithm_param_index < 0) {
        return mca_param_indices->algorithm_param_index;
    }
    mca_base_param_lookup_int(mca_param_indices->algorithm_param_index, &(requested_alg));
    if (0 > requested_alg || requested_alg > max_alg) {
        if (0 == ompi_comm_rank( MPI_COMM_WORLD)) {
            opal_output(0, "Alltoallv algorithm #%d is not available (range [0..%d]). "
                        "Switching back to ignore(0)\n",
                        requested_alg, max_alg );
        }
        mca_base_param_set_int(mca_param_indices->algorithm_param_index, 0);
    }
    
    return (MPI_SUCCESS);
}



int ompi_coll_tuned_alltoallv_intra_do_forced(void *sbuf, int *scounts, int *sdisps, 
                                              struct ompi_datatype_t *sdtype,
                                              void* rbuf, int *rcounts, int *rdisps, 
                                              struct ompi_datatype_t *rdtype,
                                              struct ompi_communicator_t *comm,
                                              mca_coll_base_module_t *module)
{
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t*) module;
    mca_coll_tuned_comm_t *data = tuned_module->tuned_data;

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:alltoallv_intra_do_forced selected algorithm %d",
                 data->user_forced[ALLTOALLV].algorithm));

    switch (data->user_forced[ALLTOALLV].algorithm) {
    case (0):   
        return ompi_coll_tuned_alltoallv_intra_dec_fixed(sbuf, scounts, sdisps, sdtype, 
                                                         rbuf, rcounts, rdisps, rdtype,
                                                         comm, module);
    case (1):
        return ompi_coll_tuned_alltoallv_intra_basic_linear(sbuf, scounts, sdisps, sdtype,
                                                            rbuf, rcounts, rdisps, rdtype,      
                                                            comm, module);
    case (2): 
        return ompi_coll_tuned_alltoallv_intra_pairwise(sbuf, scounts, sdisps, sdtype,
                                                        rbuf, rcounts, rdisps, rdtype,
                                                        comm, module);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,
                     "coll:tuned:alltoallv_intra_do_forced attempt to "
                     "select algorithm %d when only 0-%d is valid.", 
                     data->user_forced[ALLTOALLV].algorithm,
                     ompi_coll_tuned_forced_max_algorithms[ALLTOALLV]));
        return (MPI_ERR_ARG);
    }
}

/* If the user selects dynamic rules and specifies the algorithm to
 * use, then this function is called.  */
int ompi_coll_tuned_alltoallv_intra_do_this(void *sbuf, int *scounts, int *sdisps,
                                            struct ompi_datatype_t *sdtype,
                                            void* rbuf, int *rcounts, int *rdisps,
                                            struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module,
                                            int algorithm)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:alltoallv_intra_do_this selected algorithm %d ",
                 algorithm));

    switch (algorithm) {
    case (0):
        return ompi_coll_tuned_alltoallv_intra_dec_fixed(sbuf, scounts, sdisps, sdtype,
                                                        rbuf, rcounts, rdisps, rdtype,
                                                        comm, module);
    case (1):
        return ompi_coll_tuned_alltoallv_intra_basic_linear(sbuf, scounts, sdisps, sdtype,
                                                           rbuf, rcounts, rdisps, rdtype,
                                                           comm, module);
    case (2):
        return ompi_coll_tuned_alltoallv_intra_pairwise(sbuf, scounts, sdisps, sdtype,
                                                       rbuf, rcounts, rdisps, rdtype,
                                                       comm, module);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,
                     "coll:tuned:alltoall_intra_do_this attempt to select "
                     "algorithm %d when only 0-%d is valid.", 
                     algorithm, ompi_coll_tuned_forced_max_algorithms[ALLTOALLV]));
        return (MPI_ERR_ARG);
    }
}
