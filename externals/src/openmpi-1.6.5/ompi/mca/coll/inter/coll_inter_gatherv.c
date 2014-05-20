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
 *	gatherv_inter
 *
 *	Function:	- gatherv operation using a local gather on c_local_comm
 *	Accepts:	- same arguments as MPI_Gatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_inter_gatherv_inter(void *sbuf, int scount,
                             struct ompi_datatype_t *sdtype,
                             void *rbuf, int *rcounts, int *disps,
                             struct ompi_datatype_t *rdtype, int root,
                             struct ompi_communicator_t *comm,
                             mca_coll_base_module_t *module)
{
    
    int i, rank, size, size_local, total=0, err;
    int *count=NULL,*displace=NULL;
    char *ptmp=NULL;
    MPI_Aint incr;
    MPI_Aint extent;
    MPI_Aint lb;
    ompi_datatype_t *ndtype;

    size = ompi_comm_remote_size(comm);
    rank = ompi_comm_rank(comm);
    size_local = ompi_comm_size(comm);

    if (MPI_PROC_NULL == root) {
        /* do nothing */
        err = OMPI_SUCCESS;
    } else if (MPI_ROOT != root) {
	if (0 == rank) {
	    count = (int *)malloc(sizeof(int) * size_local);
	    if (NULL == count) {
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }
	    displace = (int *)malloc(sizeof(int) * size_local);
	    if (NULL == displace) {
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }
	}

	err = comm->c_local_comm->c_coll.coll_gather(&scount, 1, MPI_INT, 
						     count, 1, MPI_INT, 
						     0, comm->c_local_comm,
                                                     comm->c_local_comm->c_coll.coll_gather_module);
	if (OMPI_SUCCESS != err) {
	    return err;
	}
	if(0 == rank) {
	    displace[0] = 0;
	    for (i = 1; i < size_local; i++) {
		displace[i] = displace[i-1] + count[i-1];
	    }
	    /* Perform the gatherv locally with the first process as root */
	    err = ompi_datatype_get_extent(sdtype, &lb, &extent);
	    if (OMPI_SUCCESS != err) {
		return OMPI_ERROR;
	    }
	    incr = 0;
	    for (i = 0; i < size_local; i++) {
		incr = incr + extent*count[i];
	    }
	    if ( incr > 0 ) {
		ptmp = (char*)malloc(incr); 
		if (NULL == ptmp) {
		    return OMPI_ERR_OUT_OF_RESOURCE;
		}
	    }
	}
	err = comm->c_local_comm->c_coll.coll_gatherv(sbuf, scount, sdtype, 
						     ptmp, count, displace, 
                                                      sdtype,0, comm->c_local_comm,
                                                      comm->c_local_comm->c_coll.coll_gatherv_module);
	if (OMPI_SUCCESS != err) {
	    return err;
	}

	if (0 == rank) { 
	    for (i = 0; i < size_local; i++) {
		total = total + count[i];
	    }
	    /* First process sends data to the root */
	    err = MCA_PML_CALL(send(ptmp, total, sdtype, root,
				    MCA_COLL_BASE_TAG_GATHERV,
				    MCA_PML_BASE_SEND_STANDARD, comm));
	    if (OMPI_SUCCESS != err) {
                return err;
            }
	}
	if (NULL != ptmp) {
	    free(ptmp);
	}
	if (NULL != displace) {
	    free(displace);
	}
	if (NULL != count) {
	    free(count);
	}
    } else {
        /* I am the root, loop receiving the data. */
	ompi_datatype_create_indexed(size,rcounts,disps,rdtype,&ndtype);
	ompi_datatype_commit(&ndtype);
	
	err = MCA_PML_CALL(recv(rbuf, 1, ndtype, 0,
				MCA_COLL_BASE_TAG_GATHERV,
				comm, MPI_STATUS_IGNORE));
	if (OMPI_SUCCESS != err) {
	    return err;
	}
	ompi_datatype_destroy(&ndtype);
    }

    /* All done */
    return err;
}
