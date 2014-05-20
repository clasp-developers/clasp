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
#include "ompi/constants.h"
#include "ompi/op/op.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"
#include "coll_basic.h"


/*
 *	scan
 *
 *	Function:	- basic scan operation
 *	Accepts:	- same arguments as MPI_Scan()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_scan_intra(void *sbuf, void *rbuf, int count,
                          struct ompi_datatype_t *dtype,
                          struct ompi_op_t *op,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module)
{
    int size, rank, err;
    ptrdiff_t true_lb, true_extent, lb, extent;
    char *free_buffer = NULL;
    char *pml_buffer = NULL;

    /* Initialize */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* If I'm rank 0, just copy into the receive buffer */

    if (0 == rank) {
        if (MPI_IN_PLACE != sbuf) {
            err = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
            if (MPI_SUCCESS != err) {
                return err;
            }
        }
    }

    /* Otherwise receive previous buffer and reduce. */

    else {
        /* Allocate a temporary buffer.  Rationale for this size is
         * listed in coll_basic_reduce.c.  Use this temporary buffer to
         * receive into, later. */

        ompi_datatype_get_extent(dtype, &lb, &extent);
        ompi_datatype_get_true_extent(dtype, &true_lb, &true_extent);

        free_buffer = (char*)malloc(true_extent + (count - 1) * extent);
        if (NULL == free_buffer) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        pml_buffer = free_buffer - lb;

        /* Copy the send buffer into the receive buffer. */

        if (MPI_IN_PLACE != sbuf) {
            err = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
            if (MPI_SUCCESS != err) {
                if (NULL != free_buffer) {
                    free(free_buffer);
                }
                return err;
            }
        }

        /* Receive the prior answer */

        err = MCA_PML_CALL(recv(pml_buffer, count, dtype,
                                rank - 1, MCA_COLL_BASE_TAG_SCAN, comm,
                                MPI_STATUS_IGNORE));
        if (MPI_SUCCESS != err) {
            if (NULL != free_buffer) {
                free(free_buffer);
            }
            return err;
        }

        /* Perform the operation */

        ompi_op_reduce(op, pml_buffer, rbuf, count, dtype);

        /* All done */

        if (NULL != free_buffer) {
            free(free_buffer);
        }
    }

    /* Send result to next process. */

    if (rank < (size - 1)) {
        return MCA_PML_CALL(send(rbuf, count, dtype, rank + 1,
                                 MCA_COLL_BASE_TAG_SCAN,
                                 MCA_PML_BASE_SEND_STANDARD, comm));
    }

    /* All done */

    return MPI_SUCCESS;
}
