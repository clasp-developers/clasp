/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "coll_basic.h"
#include "ompi/mca/pml/pml.h"


/*
 *	allreduce_intra
 *
 *	Function:	- allreduce using other MPI collectives
 *	Accepts:	- same as MPI_Allreduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_allreduce_intra(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    int err;

    /* Reduce to 0 and broadcast. */

    if (MPI_IN_PLACE == sbuf) {
        if (0 == ompi_comm_rank(comm)) {
            err = comm->c_coll.coll_reduce(MPI_IN_PLACE, rbuf, count, dtype, op, 0, comm, comm->c_coll.coll_reduce_module);
        } else {
            err = comm->c_coll.coll_reduce(rbuf, NULL, count, dtype, op, 0, comm, comm->c_coll.coll_reduce_module);
        }
    } else {
        err = comm->c_coll.coll_reduce(sbuf, rbuf, count, dtype, op, 0, comm, comm->c_coll.coll_reduce_module);
    }
    if (MPI_SUCCESS != err) {
        return err;
    }

    return comm->c_coll.coll_bcast(rbuf, count, dtype, 0, comm, comm->c_coll.coll_bcast_module);
}


/*
 *	allreduce_inter
 *
 *	Function:	- allreduce using other MPI collectives
 *	Accepts:	- same as MPI_Allreduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_allreduce_inter(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    int err, i, rank, root = 0, rsize;
    ptrdiff_t lb, extent;
    char *tmpbuf = NULL, *pml_buffer = NULL;
    ompi_request_t *req[2];
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t*) module;
    ompi_request_t **reqs = basic_module->mccb_reqs;

    rank = ompi_comm_rank(comm);
    rsize = ompi_comm_remote_size(comm);

    /* determine result of the remote group, you cannot
     * use coll_reduce for inter-communicators, since than
     * you would need to determine an order between the
     * two groups (e.g. which group is providing the data
     * and which one enters coll_reduce with providing
     * MPI_PROC_NULL as root argument etc.) Here,
     * we execute the data exchange for both groups
     * simultaniously. */
    /*****************************************************************/
    if (rank == root) {
        err = ompi_datatype_get_extent(dtype, &lb, &extent);
        if (OMPI_SUCCESS != err) {
            return OMPI_ERROR;
        }

        tmpbuf = (char *) malloc(count * extent);
        if (NULL == tmpbuf) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        pml_buffer = tmpbuf - lb;

        /* Do a send-recv between the two root procs. to avoid deadlock */
        err = MCA_PML_CALL(irecv(rbuf, count, dtype, 0,
                                 MCA_COLL_BASE_TAG_ALLREDUCE, comm,
                                 &(req[0])));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = MCA_PML_CALL(isend(sbuf, count, dtype, 0,
                                 MCA_COLL_BASE_TAG_ALLREDUCE,
                                 MCA_PML_BASE_SEND_STANDARD,
                                 comm, &(req[1])));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = ompi_request_wait_all(2, req, MPI_STATUSES_IGNORE);
        if (OMPI_SUCCESS != err) {
            goto exit;
        }


        /* Loop receiving and calling reduction function (C or Fortran). */
        for (i = 1; i < rsize; i++) {
            err = MCA_PML_CALL(recv(pml_buffer, count, dtype, i,
                                    MCA_COLL_BASE_TAG_ALLREDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) {
                goto exit;
            }

            /* Perform the reduction */
            ompi_op_reduce(op, pml_buffer, rbuf, count, dtype);
        }
    } else {
        /* If not root, send data to the root. */
        err = MCA_PML_CALL(send(sbuf, count, dtype, root,
                                MCA_COLL_BASE_TAG_ALLREDUCE,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }
    }


    /* now we have on one process the result of the remote group. To distribute
     * the data to all processes in the local group, we exchange the data between
     * the two root processes. They then send it to every other process in the 
     * remote group. */
    /***************************************************************************/
    if (rank == root) {
        /* sendrecv between the two roots */
        err = MCA_PML_CALL(irecv(pml_buffer, count, dtype, 0,
                                 MCA_COLL_BASE_TAG_ALLREDUCE,
                                 comm, &(req[1])));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = MCA_PML_CALL(isend(rbuf, count, dtype, 0,
                                 MCA_COLL_BASE_TAG_ALLREDUCE,
                                 MCA_PML_BASE_SEND_STANDARD, comm,
                                 &(req[0])));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }
        err = ompi_request_wait_all(2, req, MPI_STATUSES_IGNORE);
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        /* distribute the data to other processes in remote group.
         * Note that we start from 1 (not from zero), since zero
         * has already the correct data AND we avoid a potential 
         * deadlock here. 
         */
        if (rsize > 1) {
            for (i = 1; i < rsize; i++) {
                err = MCA_PML_CALL(isend(pml_buffer, count, dtype, i,
                                         MCA_COLL_BASE_TAG_ALLREDUCE,
                                         MCA_PML_BASE_SEND_STANDARD, comm,
                                         &reqs[i - 1]));
                if (OMPI_SUCCESS != err) {
                    goto exit;
                }
            }

            err =
                ompi_request_wait_all(rsize - 1, reqs,
                                      MPI_STATUSES_IGNORE);
            if (OMPI_SUCCESS != err) {
                goto exit;
            }
        }
    } else {
        err = MCA_PML_CALL(recv(rbuf, count, dtype, root,
                                MCA_COLL_BASE_TAG_ALLREDUCE,
                                comm, MPI_STATUS_IGNORE));
    }

  exit:
    if (NULL != tmpbuf) {
        free(tmpbuf);
    }


    return err;
}
