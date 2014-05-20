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
 * Copyright (c) 2006-2010 University of Houston. All rights reserved.
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
 *	scatterv_inter
 *
 *	Function:	- scatterv operation
 *	Accepts:	- same arguments as MPI_Scatterv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_inter_scatterv_inter(void *sbuf, int *scounts,
                              int *disps, struct ompi_datatype_t *sdtype,
                              void *rbuf, int rcount,
                              struct ompi_datatype_t *rdtype, int root,
                              struct ompi_communicator_t *comm,
                              mca_coll_base_module_t *module)
{
    int i, rank, size, err, total, size_local;
    int *counts=NULL,*displace=NULL;
    char *ptmp=NULL;
    MPI_Aint incr;
    MPI_Aint extent;
    MPI_Aint lb;
    ompi_datatype_t *ndtype;

    /* Initialize */

    rank = ompi_comm_rank(comm);
    size = ompi_comm_remote_size(comm);
    size_local = ompi_comm_size(comm);

    if (MPI_PROC_NULL == root) {
        /* do nothing */
        err = OMPI_SUCCESS;
    } else if (MPI_ROOT != root) {
	if(0 == rank) {
	    /* local root recieves the counts from the root */
	    counts = (int *)malloc(sizeof(int) * size_local);
	    err = MCA_PML_CALL(recv(counts, size_local, MPI_INT,
				    root, MCA_COLL_BASE_TAG_SCATTERV,
				    comm, MPI_STATUS_IGNORE));
	    if (OMPI_SUCCESS != err) {
		return err;
	    }
	    /* calculate the whole buffer size and recieve it from root */
	    err = ompi_datatype_get_extent(rdtype, &lb, &extent);
	    if (OMPI_SUCCESS != err) {
		return OMPI_ERROR;
	    }
	    incr = 0;
	    for (i = 0; i < size_local; i++) {
		incr = incr + extent*counts[i];
	    }
	    if ( incr > 0 ) {
		ptmp = (char*)malloc(incr); 
		if (NULL == ptmp) {
		    return OMPI_ERR_OUT_OF_RESOURCE;
		}
	    }
	    total = 0;
	    for (i = 0; i < size_local; i++) {
		total = total + counts[i];
	    }
	    err = MCA_PML_CALL(recv(ptmp, total, rdtype,
				    root, MCA_COLL_BASE_TAG_SCATTERV,
				    comm, MPI_STATUS_IGNORE));
	    if (OMPI_SUCCESS != err) {
		return err;
	    }
	    /* set the local displacement i.e. no displacements here */
	    displace = (int *)malloc(sizeof(int) * size_local);
	    displace[0] = 0;
	    for (i = 1; i < size_local; i++) {
		displace[i] = displace[i-1] + counts[i-1];
	    }
	}
	/* perform the scatterv locally */
	err = comm->c_local_comm->c_coll.coll_scatterv(ptmp, counts, displace, 
						       rdtype, rbuf, rcount, 
						       rdtype, 0, comm->c_local_comm,
                                                       comm->c_local_comm->c_coll.coll_scatterv_module);
	if (OMPI_SUCCESS != err) {
	    return err;
	}

	if (NULL != ptmp) {
	    free(ptmp);
	}
	if (NULL != displace) {
	    free(displace);
	}
	if (NULL != counts) {
	    free(counts);
	}

    } else {
	err = MCA_PML_CALL(send(scounts, size, MPI_INT, 0,
				MCA_COLL_BASE_TAG_SCATTERV,
				MCA_PML_BASE_SEND_STANDARD, comm));
	if (OMPI_SUCCESS != err) {
	    return err;
	}

	ompi_datatype_create_indexed(size,scounts,disps,sdtype,&ndtype);
	ompi_datatype_commit(&ndtype);
	
	err = MCA_PML_CALL(send(sbuf, 1, ndtype, 0,
				MCA_COLL_BASE_TAG_SCATTERV,
				MCA_PML_BASE_SEND_STANDARD, comm));
	if (OMPI_SUCCESS != err) {
	    return err;
	}
	ompi_datatype_destroy(&ndtype);

    }

    /* All done */
    return err;
}
