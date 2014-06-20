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
 * Copyright (c) 2006-2008 University of Houston. All rights reserved.
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
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"

/*
 *	scatter_inter
 *
 *	Function:	- scatter operation
 *	Accepts:	- same arguments as MPI_Scatter()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_inter_scatter_inter(void *sbuf, int scount,
                             struct ompi_datatype_t *sdtype,
                             void *rbuf, int rcount,
                             struct ompi_datatype_t *rdtype,
                             int root, struct ompi_communicator_t *comm,
                             mca_coll_base_module_t *module)
{
    int rank, size, size_local, err;
    char *ptmp = NULL;
    ptrdiff_t lb, incr;

    /* Initialize */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_remote_size(comm);

    if (MPI_PROC_NULL == root) {
        /* do nothing */
        err = OMPI_SUCCESS;
    } else if (MPI_ROOT != root) {
        /* First process receives the data from root */
	if(0 == rank) { 
	    err = ompi_datatype_get_extent(rdtype, &lb, &incr);
	    if (OMPI_SUCCESS != err) {
		return OMPI_ERROR;
	    }
	    
	    incr *= rcount;
	    size_local = ompi_comm_size(comm->c_local_comm);
	    ptmp = (char*)malloc(size_local * incr);
	    if (NULL == ptmp) {
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }

	    err = MCA_PML_CALL(recv(ptmp, rcount*size_local, rdtype, 
				    root, MCA_COLL_BASE_TAG_SCATTER,
				    comm, MPI_STATUS_IGNORE));
	    if (OMPI_SUCCESS != err) {
                return err;
            }
	}
	/* Perform the scatter locally with the first process as root */
	err = comm->c_local_comm->c_coll.coll_scatter(ptmp, rcount, rdtype, 
						      rbuf, rcount, rdtype, 
						      0, comm->c_local_comm,
                                                      comm->c_local_comm->c_coll.coll_scatter_module);
	if (NULL != ptmp) {
	    free(ptmp);
	}
    } else {
	/* Root sends data to the first process in the remote group */
	err = MCA_PML_CALL(send(sbuf, scount*size, sdtype, 0,
				MCA_COLL_BASE_TAG_SCATTER,
				MCA_PML_BASE_SEND_STANDARD, comm));
	if (OMPI_SUCCESS != err) {
	    return err;
	}
    }

    return err;
}
