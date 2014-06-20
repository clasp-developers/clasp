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
 *	alltoallv_intra
 *
 *	Function:	- MPI_Alltoallv
 *	Accepts:	- same as MPI_Alltoallv()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int
mca_coll_basic_alltoallv_intra(void *sbuf, int *scounts, int *sdisps,
                               struct ompi_datatype_t *sdtype,
                               void *rbuf, int *rcounts, int *rdisps,
                               struct ompi_datatype_t *rdtype,
                               struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    int i;
    int size;
    int rank;
    int err;
    char *psnd;
    char *prcv;
    int nreqs;
    MPI_Aint sndextent;
    MPI_Aint rcvextent;
    MPI_Request *preq;

    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t*) module;

    /* Initialize. */

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    ompi_datatype_type_extent(sdtype, &sndextent);
    ompi_datatype_type_extent(rdtype, &rcvextent);

    /* simple optimization */

    psnd = ((char *) sbuf) + (sdisps[rank] * sndextent);
    prcv = ((char *) rbuf) + (rdisps[rank] * rcvextent);

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

    /* Initiate all send/recv to/from others. */

    nreqs = 0;
    preq = basic_module->mccb_reqs;

    /* Post all receives first -- a simple optimization */

    for (i = 0; i < size; ++i) {
        if (i == rank || 0 == rcounts[i]) {
            continue;
        }

        prcv = ((char *) rbuf) + (rdisps[i] * rcvextent);
        err = MCA_PML_CALL(irecv_init(prcv, rcounts[i], rdtype,
                                      i, MCA_COLL_BASE_TAG_ALLTOALLV, comm,
                                      preq++));
        ++nreqs;
        if (MPI_SUCCESS != err) {
            mca_coll_basic_free_reqs(basic_module->mccb_reqs, nreqs);
            return err;
        }
    }

    /* Now post all sends */

    for (i = 0; i < size; ++i) {
        if (i == rank || 0 == scounts[i]) {
            continue;
        }

        psnd = ((char *) sbuf) + (sdisps[i] * sndextent);
        err = MCA_PML_CALL(isend_init(psnd, scounts[i], sdtype,
                                      i, MCA_COLL_BASE_TAG_ALLTOALLV,
                                      MCA_PML_BASE_SEND_STANDARD, comm,
                                      preq++));
        ++nreqs;
        if (MPI_SUCCESS != err) {
            mca_coll_basic_free_reqs(basic_module->mccb_reqs, nreqs);
            return err;
        }
    }

    /* Start your engines.  This will never return an error. */

    MCA_PML_CALL(start(nreqs, basic_module->mccb_reqs));

    /* Wait for them all.  If there's an error, note that we don't care
     * what the error was -- just that there *was* an error.  The PML
     * will finish all requests, even if one or more of them fail.
     * i.e., by the end of this call, all the requests are free-able.
     * So free them anyway -- even if there was an error, and return the
     * error after we free everything. */

    err = ompi_request_wait_all(nreqs, basic_module->mccb_reqs,
                                MPI_STATUSES_IGNORE);

    /* Free the requests. */

    mca_coll_basic_free_reqs(basic_module->mccb_reqs, nreqs);

    /* All done */

    return err;
}


/*
 *	alltoallv_inter
 *
 *	Function:	- MPI_Alltoallv
 *	Accepts:	- same as MPI_Alltoallv()
 *	Returns:	- MPI_SUCCESS or an MPI error code
 */
int
mca_coll_basic_alltoallv_inter(void *sbuf, int *scounts, int *sdisps,
                               struct ompi_datatype_t *sdtype, void *rbuf,
                               int *rcounts, int *rdisps,
                               struct ompi_datatype_t *rdtype,
                               struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    int i;
    int rsize;
    int err;
    char *psnd;
    char *prcv;
    size_t nreqs;
    MPI_Aint sndextent;
    MPI_Aint rcvextent;

    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t*) module;
    ompi_request_t **preq = basic_module->mccb_reqs;

    /* Initialize. */

    rsize = ompi_comm_remote_size(comm);

    ompi_datatype_type_extent(sdtype, &sndextent);
    ompi_datatype_type_extent(rdtype, &rcvextent);

    /* Initiate all send/recv to/from others. */
    nreqs = rsize * 2;

    /* Post all receives first  */
    /* A simple optimization: do not send and recv msgs of length zero */
    for (i = 0; i < rsize; ++i) {
        prcv = ((char *) rbuf) + (rdisps[i] * rcvextent);
        if (rcounts[i] > 0) {
            err = MCA_PML_CALL(irecv(prcv, rcounts[i], rdtype,
                                     i, MCA_COLL_BASE_TAG_ALLTOALLV, comm,
                                     &preq[i]));
            if (MPI_SUCCESS != err) {
                return err;
            }
        } else {
            preq[i] = MPI_REQUEST_NULL;
        }
    }

    /* Now post all sends */
    for (i = 0; i < rsize; ++i) {
        psnd = ((char *) sbuf) + (sdisps[i] * sndextent);
        if (scounts[i] > 0) {
            err = MCA_PML_CALL(isend(psnd, scounts[i], sdtype,
                                     i, MCA_COLL_BASE_TAG_ALLTOALLV,
                                     MCA_PML_BASE_SEND_STANDARD, comm,
                                     &preq[rsize + i]));
            if (MPI_SUCCESS != err) {
                return err;
            }
        } else {
            preq[rsize + i] = MPI_REQUEST_NULL;
        }
    }

    err = ompi_request_wait_all(nreqs, preq, MPI_STATUSES_IGNORE);

    /* All done */
    return err;
}
