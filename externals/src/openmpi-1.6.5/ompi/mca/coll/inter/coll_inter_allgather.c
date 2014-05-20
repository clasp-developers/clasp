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
 * Copyright (c) 2006-2010 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_inter.h"

#include <stdlib.h>

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/request/request.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_tags.h"

/*
 *	allgather_inter
 *
 *	Function:	- allgather using other MPI collections
 *	Accepts:	- same as MPI_Allgather()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_inter_allgather_inter(void *sbuf, int scount,
                               struct ompi_datatype_t *sdtype,
                               void *rbuf, int rcount,
                               struct ompi_datatype_t *rdtype,
                               struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    int rank, root = 0, size, rsize, err;
    char *ptmp = NULL;
    ptrdiff_t slb, sextent, incr;
    ompi_request_t *req[2];

    rank = ompi_comm_rank(comm);
    size = ompi_comm_size(comm->c_local_comm);
    rsize = ompi_comm_remote_size(comm);

    /* Perform the gather locally at the root */
    err = ompi_datatype_get_extent(sdtype, &slb, &sextent);
    if (OMPI_SUCCESS != err) {
	return OMPI_ERROR;
    }
    
    if ( scount > 0 ) {
	incr = sextent * scount;
	ptmp = (char*)malloc(size * incr); 
	if (NULL == ptmp) {
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}

	err = comm->c_local_comm->c_coll.coll_gather(sbuf, scount, sdtype, 
						     ptmp, scount, sdtype, 
						     0, comm->c_local_comm,
						     comm->c_local_comm->c_coll.coll_gather_module);
	if (OMPI_SUCCESS != err) {
	    goto exit;
	}
    }

    if (rank == root) {
	/* Do a send-recv between the two root procs. to avoid deadlock */
        err = MCA_PML_CALL(irecv(rbuf, rcount*rsize, rdtype, 0,
                                 MCA_COLL_BASE_TAG_ALLGATHER, comm,
                                 &(req[0])));
        if (OMPI_SUCCESS != err) {
            goto exit;
        }

        err = MCA_PML_CALL(isend(ptmp, scount*size, sdtype, 0,
                                 MCA_COLL_BASE_TAG_ALLGATHER,
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
    if ( rcount > 0 ) {
	err = comm->c_local_comm->c_coll.coll_bcast(rbuf, rcount*rsize, rdtype, 
						    root, comm->c_local_comm,
						    comm->c_local_comm->c_coll.coll_bcast_module);
	if (OMPI_SUCCESS != err) {
	    goto exit;
	}
    }

 exit:
    if (NULL != ptmp) {
        free(ptmp);
    }

    return err;
}
