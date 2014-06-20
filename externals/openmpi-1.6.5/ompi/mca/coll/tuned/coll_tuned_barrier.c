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
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_tuned.h"
#include "coll_tuned_topo.h"
#include "coll_tuned_util.h"

/*
 * Barrier is ment to be a synchronous operation, as some BTLs can mark 
 * a request done before its passed to the NIC and progress might not be made 
 * elsewhere we cannot allow a process to exit the barrier until its last 
 * [round of] sends are completed.
 *
 * It is last round of sends rather than 'last' individual send as each pair of 
 * peers can use different channels/devices/btls and the receiver of one of 
 * these sends might be forced to wait as the sender
 * leaves the collective and does not make progress until the next mpi call 
 *
 */

/*
 * Simple double ring version of barrier
 *
 * synchronous gurantee made by last ring of sends are synchronous
 *
 */
int ompi_coll_tuned_barrier_intra_doublering(struct ompi_communicator_t *comm,
					     mca_coll_base_module_t *module)
{
    int rank, size;
    int err=0, line=0;
    int left, right;


    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    OPAL_OUTPUT((ompi_coll_tuned_stream,"ompi_coll_tuned_barrier_intra_doublering rank %d", rank));
  
    left = ((rank-1)%size);
    right = ((rank+1)%size);

    if (rank > 0) { /* receive message from the left */
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left, 
                                MCA_COLL_BASE_TAG_BARRIER, comm, 
                                MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
    }

    /* Send message to the right */
    err = MCA_PML_CALL(send((void*)NULL, 0, MPI_BYTE, right, 
                            MCA_COLL_BASE_TAG_BARRIER, 
                            MCA_PML_BASE_SEND_STANDARD, comm));
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }

    /* root needs to receive from the last node */
    if (rank == 0) {
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left, 
                                MCA_COLL_BASE_TAG_BARRIER, comm, 
                                MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
    }

    /* Allow nodes to exit */
    if (rank > 0) { /* post Receive from left */
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left, 
                                MCA_COLL_BASE_TAG_BARRIER, comm, 
                                MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl; }
    }

    /* send message to the right one */
    err = MCA_PML_CALL(send((void*)NULL, 0, MPI_BYTE, right, 
                            MCA_COLL_BASE_TAG_BARRIER, 
                            MCA_PML_BASE_SEND_SYNCHRONOUS, comm));
    if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
 
    /* rank 0 post receive from the last node */
    if (rank == 0) {
        err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, left, 
                                MCA_COLL_BASE_TAG_BARRIER, comm, 
                                MPI_STATUS_IGNORE));
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;  }
    }

    return MPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", 
                 __FILE__, line, err, rank));
    return err;
}


/*
 * To make synchronous, uses sync sends and sync sendrecvs
 */

int ompi_coll_tuned_barrier_intra_recursivedoubling(struct ompi_communicator_t *comm,
						    mca_coll_base_module_t *module)
{
    int rank, size, adjsize;
    int err, line;
    int mask, remote;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "ompi_coll_tuned_barrier_intra_recursivedoubling rank %d", 
                 rank));

    /* do nearest power of 2 less than size calc */
    for( adjsize = 1; adjsize <= size; adjsize <<= 1 );
    adjsize >>= 1;

    /* if size is not exact power of two, perform an extra step */
    if (adjsize != size) {
        if (rank >= adjsize) {
            /* send message to lower ranked node */
            remote = rank - adjsize;
            err = ompi_coll_tuned_sendrecv_actual(NULL, 0, MPI_BYTE, remote,
                                                  MCA_COLL_BASE_TAG_BARRIER,
                                                  NULL, 0, MPI_BYTE, remote,
                                                  MCA_COLL_BASE_TAG_BARRIER,
                                                  comm, MPI_STATUS_IGNORE);
            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}

        } else if (rank < (size - adjsize)) {

            /* receive message from high level rank */
            err = MCA_PML_CALL(recv((void*)NULL, 0, MPI_BYTE, rank+adjsize,
                                    MCA_COLL_BASE_TAG_BARRIER, comm, 
                                    MPI_STATUS_IGNORE));

            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}
        }
    }

    /* exchange messages */
    if ( rank < adjsize ) {
        mask = 0x1;
        while ( mask < adjsize ) {
            remote = rank ^ mask;
            mask <<= 1;
            if (remote >= adjsize) continue;

            /* post receive from the remote node */
            err = ompi_coll_tuned_sendrecv_actual(NULL, 0, MPI_BYTE, remote,
                                                  MCA_COLL_BASE_TAG_BARRIER,
                                                  NULL, 0, MPI_BYTE, remote,
                                                  MCA_COLL_BASE_TAG_BARRIER,
                                                  comm, MPI_STATUS_IGNORE);
            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}
        }
    }

    /* non-power of 2 case */
    if (adjsize != size) {
        if (rank < (size - adjsize)) {
            /* send enter message to higher ranked node */
            remote = rank + adjsize;
            err = MCA_PML_CALL(send((void*)NULL, 0, MPI_BYTE, remote, 
                                    MCA_COLL_BASE_TAG_BARRIER, 
                                    MCA_PML_BASE_SEND_SYNCHRONOUS, comm));

            if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}
        }
    }

    return MPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d",
                 __FILE__, line, err, rank));
    return err;
}


/*
 * To make synchronous, uses sync sends and sync sendrecvs
 */

int ompi_coll_tuned_barrier_intra_bruck(struct ompi_communicator_t *comm,
					mca_coll_base_module_t *module)
{
    int rank, size;
    int distance, to, from;
    int err, line = 0;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "ompi_coll_tuned_barrier_intra_bruck rank %d", rank));

    /* exchange data with rank-2^k and rank+2^k */
    for (distance = 1; distance < size; distance <<= 1) { 
        from = (rank + size - distance) % size;
        to   = (rank + distance) % size;

        /* send message to lower ranked node */
        err = ompi_coll_tuned_sendrecv_actual(NULL, 0, MPI_BYTE, to, 
                                              MCA_COLL_BASE_TAG_BARRIER,
                                              NULL, 0, MPI_BYTE, from, 
                                              MCA_COLL_BASE_TAG_BARRIER,
                                              comm, MPI_STATUS_IGNORE);
        if (err != MPI_SUCCESS) { line = __LINE__; goto err_hndl;}
    }

    return MPI_SUCCESS;

 err_hndl:
    OPAL_OUTPUT((ompi_coll_tuned_stream,"%s:%4d\tError occurred %d, rank %2d", 
                 __FILE__, line, err, rank));
    return err;
}


/*
 * To make synchronous, uses sync sends and sync sendrecvs
 */
/* special case for two processes */
int ompi_coll_tuned_barrier_intra_two_procs(struct ompi_communicator_t *comm,
					    mca_coll_base_module_t *module)
{
    int remote, err;

    remote = ompi_comm_rank(comm);
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "ompi_coll_tuned_barrier_intra_two_procs rank %d", remote));
    remote = (remote + 1) & 0x1;

    err = ompi_coll_tuned_sendrecv_actual(NULL, 0, MPI_BYTE, remote, 
                                          MCA_COLL_BASE_TAG_BARRIER, 
                                          NULL, 0, MPI_BYTE, remote, 
                                          MCA_COLL_BASE_TAG_BARRIER,
                                          comm, MPI_STATUS_IGNORE);
    return (err);
}


/*
 * Linear functions are copied from the BASIC coll module
 * they do not segment the message and are simple implementations
 * but for some small number of nodes and/or small data sizes they
 * are just as fast as tuned/tree based segmenting operations
 * and as such may be selected by the decision functions
 * These are copied into this module due to the way we select modules
 * in V1. i.e. in V2 we will handle this differently and so will not
 * have to duplicate code.
 * GEF Oct05 after asking Jeff.
 */

/* copied function (with appropriate renaming) starts here */

static int ompi_coll_tuned_barrier_intra_basic_linear(struct ompi_communicator_t *comm,
						      mca_coll_base_module_t *module)
{
    int i, err;
    int size = ompi_comm_size(comm);
    int rank = ompi_comm_rank(comm);

    /* All non-root send & receive zero-length message. */

    if (rank > 0) {
        err = MCA_PML_CALL(send (NULL, 0, MPI_BYTE, 0, 
                                 MCA_COLL_BASE_TAG_BARRIER,
                                 MCA_PML_BASE_SEND_STANDARD, comm));
        if (MPI_SUCCESS != err) {
            return err;
        }

        err = MCA_PML_CALL(recv (NULL, 0, MPI_BYTE, 0, 
                                 MCA_COLL_BASE_TAG_BARRIER,
                                 comm, MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* The root collects and broadcasts the messages. */

    else {
        ompi_request_t** requests;

        requests = (ompi_request_t**)malloc( size * sizeof(ompi_request_t*) );
        for (i = 1; i < size; ++i) {
            err = MCA_PML_CALL(irecv(NULL, 0, MPI_BYTE, MPI_ANY_SOURCE,
                                     MCA_COLL_BASE_TAG_BARRIER, comm, 
                                     &(requests[i])));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
        ompi_request_wait_all( size-1, requests+1, MPI_STATUSES_IGNORE );

        for (i = 1; i < size; ++i) {
            err = MCA_PML_CALL(isend(NULL, 0, MPI_BYTE, i,
                                     MCA_COLL_BASE_TAG_BARRIER, 
                                     MCA_PML_BASE_SEND_STANDARD, comm,
                                     &(requests[i])));
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
        ompi_request_wait_all( size-1, requests+1, MPI_STATUSES_IGNORE );
        free( requests );
    }

    /* All done */

    return MPI_SUCCESS;

}
/* copied function (with appropriate renaming) ends here */

/*
 * Another recursive doubling type algorithm, but in this case
 * we go up the tree and back down the tree.  
 */
int ompi_coll_tuned_barrier_intra_tree(struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module)
{
    int rank, size, depth;
    int err, jump, partner;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);
    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "ompi_coll_tuned_barrier_intra_tree %d", 
                 rank));

    /* Find the nearest power of 2 of the communicator size. */
    for(depth = 1; depth < size; depth <<= 1 );

    for (jump=1; jump<depth; jump<<=1) {
        partner = rank ^ jump;
        if (!(partner & (jump-1)) && partner < size) {
            if (partner > rank) {
                err = MCA_PML_CALL(recv (NULL, 0, MPI_BYTE, partner, 
                                         MCA_COLL_BASE_TAG_BARRIER, comm,
                                         MPI_STATUS_IGNORE));
                if (MPI_SUCCESS != err)
                    return err;
            } else if (partner < rank) {
                err = MCA_PML_CALL(send (NULL, 0, MPI_BYTE, partner,
                                         MCA_COLL_BASE_TAG_BARRIER, 
                                         MCA_PML_BASE_SEND_STANDARD, comm));
                if (MPI_SUCCESS != err)
                    return err;
            }
        }
    }
    
    depth>>=1;
    for (jump = depth; jump>0; jump>>=1) {
        partner = rank ^ jump;
        if (!(partner & (jump-1)) && partner < size) {
            if (partner > rank) {
                err = MCA_PML_CALL(send (NULL, 0, MPI_BYTE, partner,
                                         MCA_COLL_BASE_TAG_BARRIER,
                                         MCA_PML_BASE_SEND_STANDARD, comm));
                if (MPI_SUCCESS != err)
                    return err;
            } else if (partner < rank) {
                err = MCA_PML_CALL(recv (NULL, 0, MPI_BYTE, partner, 
                                         MCA_COLL_BASE_TAG_BARRIER, comm,
                                         MPI_STATUS_IGNORE));
                if (MPI_SUCCESS != err)
                    return err;
            }
        }
    }

    return MPI_SUCCESS;
}


/* The following are used by dynamic and forced rules */

/* publish details of each algorithm and if its forced/fixed/locked in */
/* as you add methods/algorithms you must update this and the query/map  */
/* routines */

/* this routine is called by the component only */
/* this makes sure that the mca parameters are set to their initial values */
/* and perms */
/* module does not call this they call the forced_getvalues routine instead */

int ompi_coll_tuned_barrier_intra_check_forced_init (coll_tuned_force_algorithm_mca_param_indices_t *mca_param_indices)
{
    int max_alg = 6, requested_alg;

    ompi_coll_tuned_forced_max_algorithms[BARRIER] = max_alg;

    mca_base_param_reg_int (&mca_coll_tuned_component.super.collm_version,
                            "barrier_algorithm_count",
                            "Number of barrier algorithms available",
                            false, true, max_alg, NULL);

    mca_param_indices->algorithm_param_index = 
       mca_base_param_reg_int(&mca_coll_tuned_component.super.collm_version,
                              "barrier_algorithm",
                              "Which barrier algorithm is used. Can be locked down to choice of: 0 ignore, 1 linear, 2 double ring, 3: recursive doubling 4: bruck, 5: two proc only, 6: tree",
                              false, false, 0, NULL);
    if (mca_param_indices->algorithm_param_index < 0) {
        return mca_param_indices->algorithm_param_index;
    }
    mca_base_param_lookup_int(mca_param_indices->algorithm_param_index, 
                              &(requested_alg));
    if( 0 > requested_alg || requested_alg > max_alg ) {
        if( 0 == ompi_comm_rank( MPI_COMM_WORLD ) ) {
            opal_output( 0, "Barrier algorithm #%d is not available (range [0..%d]). Switching back to ignore(0)\n",
                         requested_alg, max_alg );
        }
        mca_base_param_set_int( mca_param_indices->algorithm_param_index, 0);
    }

    return (MPI_SUCCESS);
}



int ompi_coll_tuned_barrier_intra_do_forced(struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module)
{
    mca_coll_tuned_module_t *tuned_module = (mca_coll_tuned_module_t*) module;
    mca_coll_tuned_comm_t *data = tuned_module->tuned_data;

    OPAL_OUTPUT((ompi_coll_tuned_stream,
                 "coll:tuned:barrier_intra_do_forced selected algorithm %d",
                 data->user_forced[BARRIER].algorithm));

    switch (data->user_forced[BARRIER].algorithm) {
    case (0):   return ompi_coll_tuned_barrier_intra_dec_fixed (comm, module);
    case (1):   return ompi_coll_tuned_barrier_intra_basic_linear (comm, module); 
    case (2):   return ompi_coll_tuned_barrier_intra_doublering (comm, module);
    case (3):   return ompi_coll_tuned_barrier_intra_recursivedoubling (comm, module);
    case (4):   return ompi_coll_tuned_barrier_intra_bruck (comm, module);
    case (5):   return ompi_coll_tuned_barrier_intra_two_procs (comm, module);
    case (6):   return ompi_coll_tuned_barrier_intra_tree (comm, module);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:barrier_intra_do_forced attempt to select algorithm %d when only 0-%d is valid?",
                     data->user_forced[BARRIER].algorithm,
                     ompi_coll_tuned_forced_max_algorithms[BARRIER]));
        return (MPI_ERR_ARG);
    } /* switch */

}


int ompi_coll_tuned_barrier_intra_do_this (struct ompi_communicator_t *comm,
                                           mca_coll_base_module_t *module,
                                           int algorithm, int faninout, int segsize)
{
    OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:barrier_intra_do_this selected algorithm %d topo fanin/out%d", algorithm, faninout));

    switch (algorithm) {
    case (0):   return ompi_coll_tuned_barrier_intra_dec_fixed (comm, module);
    case (1):   return ompi_coll_tuned_barrier_intra_basic_linear (comm, module); 
    case (2):   return ompi_coll_tuned_barrier_intra_doublering (comm, module);
    case (3):   return ompi_coll_tuned_barrier_intra_recursivedoubling (comm, module);
    case (4):   return ompi_coll_tuned_barrier_intra_bruck (comm, module);
    case (5):   return ompi_coll_tuned_barrier_intra_two_procs (comm, module);
    case (6):   return ompi_coll_tuned_barrier_intra_tree (comm, module);
    default:
        OPAL_OUTPUT((ompi_coll_tuned_stream,"coll:tuned:barrier_intra_do_this attempt to select algorithm %d when only 0-%d is valid?",
                     algorithm, ompi_coll_tuned_forced_max_algorithms[BARRIER]));
        return (MPI_ERR_ARG);
    } /* switch */
}

