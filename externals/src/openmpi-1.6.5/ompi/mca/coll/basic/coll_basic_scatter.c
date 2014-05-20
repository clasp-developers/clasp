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
#include "coll_basic.h"


/*
 *	scatter_intra
 *
 *	Function:	- scatter operation
 *	Accepts:	- same arguments as MPI_Scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_scatter_intra(void *sbuf, int scount,
                             struct ompi_datatype_t *sdtype,
                             void *rbuf, int rcount,
                             struct ompi_datatype_t *rdtype,
                             int root, struct ompi_communicator_t *comm,
                             mca_coll_base_module_t *module)
{
    int i, rank, size, err;
    char *ptmp;
    ptrdiff_t lb, incr;

    /* Initialize */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* If not root, receive data. */

    if (rank != root) {
        err = MCA_PML_CALL(recv(rbuf, rcount, rdtype, root,
                                MCA_COLL_BASE_TAG_SCATTER,
                                comm, MPI_STATUS_IGNORE));
        return err;
    }

    /* I am the root, loop sending data. */

    err = ompi_datatype_get_extent(sdtype, &lb, &incr);
    if (OMPI_SUCCESS != err) {
        return OMPI_ERROR;
    }

    incr *= scount;
    for (i = 0, ptmp = (char *) sbuf; i < size; ++i, ptmp += incr) {

        /* simple optimization */

        if (i == rank) {
            if (MPI_IN_PLACE != rbuf) {
                err =
                    ompi_datatype_sndrcv(ptmp, scount, sdtype, rbuf, rcount,
                                    rdtype);
            }
        } else {
            err = MCA_PML_CALL(send(ptmp, scount, sdtype, i,
                                    MCA_COLL_BASE_TAG_SCATTER,
                                    MCA_PML_BASE_SEND_STANDARD, comm));
        }
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* All done */

    return MPI_SUCCESS;
}


/*
 *	scatter_inter
 *
 *	Function:	- scatter operation
 *	Accepts:	- same arguments as MPI_Scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_scatter_inter(void *sbuf, int scount,
                             struct ompi_datatype_t *sdtype,
                             void *rbuf, int rcount,
                             struct ompi_datatype_t *rdtype,
                             int root, struct ompi_communicator_t *comm,
                             mca_coll_base_module_t *module)
{
    int i, size, err;
    char *ptmp;
    ptrdiff_t lb, incr;
    mca_coll_basic_module_t *basic_module = (mca_coll_basic_module_t*) module;
    ompi_request_t **reqs = basic_module->mccb_reqs;

    /* Initialize */
    size = ompi_comm_remote_size(comm);

    if (MPI_PROC_NULL == root) {
        /* do nothing */
        err = OMPI_SUCCESS;
    } else if (MPI_ROOT != root) {
        /* If not root, receive data. */
        err = MCA_PML_CALL(recv(rbuf, rcount, rdtype, root,
                                MCA_COLL_BASE_TAG_SCATTER,
                                comm, MPI_STATUS_IGNORE));
    } else {
        /* I am the root, loop sending data. */
        err = ompi_datatype_get_extent(sdtype, &lb, &incr);
        if (OMPI_SUCCESS != err) {
            return OMPI_ERROR;
        }

        incr *= scount;
        for (i = 0, ptmp = (char *) sbuf; i < size; ++i, ptmp += incr) {
            err = MCA_PML_CALL(isend(ptmp, scount, sdtype, i,
                                     MCA_COLL_BASE_TAG_SCATTER,
                                     MCA_PML_BASE_SEND_STANDARD, comm,
                                     reqs++));
            if (OMPI_SUCCESS != err) {
                return err;
            }
        }

        err =
            ompi_request_wait_all(size, basic_module->mccb_reqs,
                                  MPI_STATUSES_IGNORE);
    }

    return err;
}
