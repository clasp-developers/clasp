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
 * Copyright (c) 2006-2007 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_inter.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "ompi/op/op.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"

/*
 *	allreduce_inter
 *
 *	Function:	- allreduce using other MPI collectives
 *	Accepts:	- same as MPI_Allreduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_inter_allreduce_inter(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    int err, rank, root = 0, rsize;
    ptrdiff_t lb, extent;
    char *tmpbuf = NULL, *pml_buffer = NULL;
    ompi_request_t *req[2];

    rank = ompi_comm_rank(comm);
    rsize = ompi_comm_remote_size(comm);
    
    /* Perform the reduction locally */
    err = ompi_datatype_get_extent(dtype, &lb, &extent);
    if (OMPI_SUCCESS != err) {
	return OMPI_ERROR;
    }
    
    tmpbuf = (char *) malloc(count * extent);
    if (NULL == tmpbuf) {
	return OMPI_ERR_OUT_OF_RESOURCE;
    }
    pml_buffer = tmpbuf - lb;
   
    err = comm->c_local_comm->c_coll.coll_reduce(sbuf, pml_buffer, count,
						 dtype, op, root, 
						 comm->c_local_comm,
                                                 comm->c_local_comm->c_coll.coll_reduce_module);
    if (OMPI_SUCCESS != err) {
	goto exit;
    }

    if (rank == root) {
	/* Do a send-recv between the two root procs. to avoid deadlock */
        err = MCA_PML_CALL(irecv(rbuf, count, dtype, 0,
                                 MCA_COLL_BASE_TAG_ALLREDUCE, comm,
                                 &(req[0])));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = MCA_PML_CALL(isend(pml_buffer, count, dtype, 0,
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
    }

    /* bcast the message to all the local processes */
    err = comm->c_local_comm->c_coll.coll_bcast(rbuf, count, dtype, 
						root, comm->c_local_comm,
                                                comm->c_local_comm->c_coll.coll_bcast_module);
    if (OMPI_SUCCESS != err) {
            goto exit;
    }

  exit:
    if (NULL != tmpbuf) {
        free(tmpbuf);
    }

    return err;
}
