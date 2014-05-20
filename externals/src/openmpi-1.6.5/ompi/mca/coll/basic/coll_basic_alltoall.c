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
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"


/*
 *	alltoall_intra
 *
 *	Function:	- MPI_Alltoall 
 *	Accepts:	- same as MPI_Alltoall()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int
mca_coll_basic_alltoall_intra(void *sbuf, int scount,
                              struct ompi_datatype_t *sdtype,
                              void *rbuf, int rcount,
                              struct ompi_datatype_t *rdtype,
                              struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    int i;
    int rank;
    int size;
    int err;
    int nreqs;
    char *psnd;
    char *prcv;
    MPI_Aint lb;
    MPI_Aint sndinc;
    MPI_Aint rcvinc;

    ompi_request_t **req;
    ompi_request_t **sreq;
    ompi_request_t **rreq;
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t*) module;

    /* Initialize. */

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    err = ompi_datatype_get_extent(sdtype, &lb, &sndinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    sndinc *= scount;

    err = ompi_datatype_get_extent(rdtype, &lb, &rcvinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    rcvinc *= rcount;

    /* simple optimization */

    psnd = ((char *) sbuf) + (rank * sndinc);
    prcv = ((char *) rbuf) + (rank * rcvinc);

    err = ompi_datatype_sndrcv(psnd, scount, sdtype, prcv, rcount, rdtype);
    if (MPI_SUCCESS != err) {
        return err;
    }

    /* If only one process, we're done. */

    if (1 == size) {
        return MPI_SUCCESS;
    }

    /* Initiate all send/recv to/from others. */

    req = rreq = basic_module->mccb_reqs;
    sreq = rreq + size - 1;

    prcv = (char *) rbuf;
    psnd = (char *) sbuf;

    /* Post all receives first -- a simple optimization */

    for (nreqs = 0, i = (rank + 1) % size; i != rank; i = (i + 1) % size, ++rreq, ++nreqs) {
        err =
            MCA_PML_CALL(irecv_init
                         (prcv + (i * rcvinc), rcount, rdtype, i,
                          MCA_COLL_BASE_TAG_ALLTOALL, comm, rreq));
        if (MPI_SUCCESS != err) {
            mca_coll_basic_free_reqs(req, nreqs);
            return err;
        }
    }

    /* Now post all sends */

    for (nreqs = 0, i = (rank + 1) % size; i != rank; i = (i + 1) % size, ++sreq, ++nreqs) {
        err =
            MCA_PML_CALL(isend_init
                         (psnd + (i * sndinc), scount, sdtype, i,
                          MCA_COLL_BASE_TAG_ALLTOALL,
                          MCA_PML_BASE_SEND_STANDARD, comm, sreq));
        if (MPI_SUCCESS != err) {
            mca_coll_basic_free_reqs(req, nreqs);
            return err;
        }
    }

    nreqs = (size - 1) * 2;
    /* Start your engines.  This will never return an error. */

    MCA_PML_CALL(start(nreqs, req));

    /* Wait for them all.  If there's an error, note that we don't
     * care what the error was -- just that there *was* an error.  The
     * PML will finish all requests, even if one or more of them fail.
     * i.e., by the end of this call, all the requests are free-able.
     * So free them anyway -- even if there was an error, and return
     * the error after we free everything. */

    err = ompi_request_wait_all(nreqs, req, MPI_STATUSES_IGNORE);

    /* Free the reqs */

    mca_coll_basic_free_reqs(req, nreqs);

    /* All done */

    return err;
}


/*
 *	alltoall_inter
 *
 *	Function:	- MPI_Alltoall 
 *	Accepts:	- same as MPI_Alltoall()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int
mca_coll_basic_alltoall_inter(void *sbuf, int scount,
                              struct ompi_datatype_t *sdtype,
                              void *rbuf, int rcount,
                              struct ompi_datatype_t *rdtype,
                              struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    int i;
    int size;
    int err;
    int nreqs;
    char *psnd;
    char *prcv;
    MPI_Aint lb;
    MPI_Aint sndinc;
    MPI_Aint rcvinc;

    ompi_request_t **req;
    ompi_request_t **sreq;
    ompi_request_t **rreq;

    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t*) module;

    /* Initialize. */

    size = ompi_comm_remote_size(comm);

    err = ompi_datatype_get_extent(sdtype, &lb, &sndinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    sndinc *= scount;

    err = ompi_datatype_get_extent(rdtype, &lb, &rcvinc);
    if (OMPI_SUCCESS != err) {
        return err;
    }
    rcvinc *= rcount;

    /* Initiate all send/recv to/from others. */
    nreqs = size * 2;
    req = rreq = basic_module->mccb_reqs;
    sreq = rreq + size;

    prcv = (char *) rbuf;
    psnd = (char *) sbuf;

    /* Post all receives first */
    for (i = 0; i < size; i++, ++rreq) {
        err = MCA_PML_CALL(irecv(prcv + (i * rcvinc), rcount, rdtype, i,
                                 MCA_COLL_BASE_TAG_ALLTOALL, comm, rreq));
        if (OMPI_SUCCESS != err) {
            return err;
        }
    }

    /* Now post all sends */
    for (i = 0; i < size; i++, ++sreq) {
        err = MCA_PML_CALL(isend(psnd + (i * sndinc), scount, sdtype, i,
                                 MCA_COLL_BASE_TAG_ALLTOALL,
                                 MCA_PML_BASE_SEND_STANDARD, comm, sreq));
        if (OMPI_SUCCESS != err) {
            return err;
        }
    }

    /* Wait for them all.  If there's an error, note that we don't
     * care what the error was -- just that there *was* an error.  The
     * PML will finish all requests, even if one or more of them fail.
     * i.e., by the end of this call, all the requests are free-able.
     * So free them anyway -- even if there was an error, and return
     * the error after we free everything. */
    err = ompi_request_wait_all(nreqs, req, MPI_STATUSES_IGNORE);

    /* All done */
    return err;
}
