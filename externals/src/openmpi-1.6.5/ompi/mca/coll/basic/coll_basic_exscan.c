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
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
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
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	exscan_intra
 *
 *	Function:	- basic exscan operation
 *	Accepts:	- same arguments as MPI_Exscan()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_exscan_intra(void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype,
                            struct ompi_op_t *op,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    int size, rank, err;
    ptrdiff_t true_lb, true_extent, lb, extent;
    char *free_buffer = NULL;
    char *reduce_buffer = NULL;

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm);

    /* For MPI_IN_PLACE, just adjust send buffer to point to
     * receive buffer. */
    if (MPI_IN_PLACE == sbuf) {
        sbuf = rbuf;
    }

    /* If we're rank 0, then just send our sbuf to the next rank, and
     * we are done. */
    if (0 == rank) {
        return MCA_PML_CALL(send(sbuf, count, dtype, rank + 1,
                                 MCA_COLL_BASE_TAG_EXSCAN,
                                 MCA_PML_BASE_SEND_STANDARD, comm));
    }

    /* If we're the last rank, then just receive the result from the
     * prior rank, and we are done. */
    else if ((size - 1) == rank) {
        return MCA_PML_CALL(recv(rbuf, count, dtype, rank - 1,
                                 MCA_COLL_BASE_TAG_EXSCAN, comm,
                                 MPI_STATUS_IGNORE));
    }

    /* Otherwise, get the result from the prior rank, combine it with my
     * data, and send it to the next rank */

    /* Get a temporary buffer to perform the reduction into.  Rationale
     * for malloc'ing this size is provided in coll_basic_reduce.c. */
    ompi_datatype_get_extent(dtype, &lb, &extent);
    ompi_datatype_get_true_extent(dtype, &true_lb, &true_extent);

    free_buffer = (char*)malloc(true_extent + (count - 1) * extent);
    if (NULL == free_buffer) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    reduce_buffer = free_buffer - lb;
    err = ompi_datatype_copy_content_same_ddt(dtype, count, 
                                              reduce_buffer, (char*)sbuf);

    /* Receive the reduced value from the prior rank */
    err = MCA_PML_CALL(recv(rbuf, count, dtype, rank - 1,
                            MCA_COLL_BASE_TAG_EXSCAN, comm, MPI_STATUS_IGNORE));
    if (MPI_SUCCESS != err) {
        goto error;
    }

    /* Now reduce the prior rank's result with my source buffer.  The source
     * buffer had been previously copied into the temporary reduce_buffer. */
    ompi_op_reduce(op, rbuf, reduce_buffer, count, dtype);

    /* Send my result off to the next rank */
    err = MCA_PML_CALL(send(reduce_buffer, count, dtype, rank + 1,
                            MCA_COLL_BASE_TAG_EXSCAN,
                            MCA_PML_BASE_SEND_STANDARD, comm));
    /* Error */
  error:
    free(free_buffer);

    /* All done */
    return err;
}


/*
 *	exscan_inter
 *
 *	Function:	- basic exscan operation
 *	Accepts:	- same arguments as MPI_Exscan()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_exscan_inter(void *sbuf, void *rbuf, int count,
                            struct ompi_datatype_t *dtype,
                            struct ompi_op_t *op,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}
