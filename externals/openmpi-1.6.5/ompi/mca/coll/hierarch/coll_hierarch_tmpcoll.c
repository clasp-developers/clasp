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
 * Copyright (c) 2008      University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_hierarch.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"

#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_tags.h"

#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/pml/pml.h"


int mca_coll_hierarch_allreduce_tmp(void *sbuf, void *rbuf, int count,
				    struct ompi_datatype_t *dtype,
				    struct ompi_op_t *op,
				    struct ompi_communicator_t *comm)
{
    int ret;
    
    ret = mca_coll_hierarch_reduce_tmp ( sbuf, rbuf, count, dtype, op, 0, comm);
    if ( OMPI_SUCCESS != ret ) {
	return ret;
    }
    ret = mca_coll_hierarch_bcast_tmp ( rbuf, count, dtype, 0, comm);
    return ret;
}


int mca_coll_hierarch_allgather_tmp(void *sbuf, int scount,
				    struct ompi_datatype_t *sdtype,
				    void *rbuf, int rcount,
				    struct ompi_datatype_t *rdtype,
				    struct ompi_communicator_t *comm)
{
    int ret;
    int size = ompi_comm_size (comm);

    ret = mca_coll_hierarch_gather_tmp ( sbuf, scount, sdtype, rbuf, rcount,
					 rdtype, 0, comm);
    
    if ( OMPI_SUCCESS != ret ) {
	return ret;
    }
    ret = mca_coll_hierarch_bcast_tmp ( rbuf, rcount*size, rdtype, 0, comm);
    return ret;
}    

int mca_coll_hierarch_bcast_tmp ( void *buf, int count,  struct ompi_datatype_t *dtype,
				  int root, struct ompi_communicator_t *comm)
{
    int err = OMPI_SUCCESS;
    int rank = ompi_comm_rank ( comm );

    if ( rank != root ) {
        err = MCA_PML_CALL(recv(buf, count, dtype, root,
        			MCA_COLL_BASE_TAG_BCAST,
        			comm, MPI_STATUS_IGNORE));
        if ( OMPI_SUCCESS != err ) {
            return err;
        }
    }
    else {
        int i;
        int size=ompi_comm_size ( comm );

        for ( i=0; i<size; i++ ) {
	    if ( i == root ) {
		continue;
	    }
            err =  MCA_PML_CALL(send(buf, count, dtype, i,
        			     MCA_COLL_BASE_TAG_BCAST,
        			     MCA_PML_BASE_SEND_STANDARD, comm));
            if ( OMPI_SUCCESS != err ) {
        	return err;
            }
        }        	
    }

    return err;
}

int mca_coll_hierarch_reduce_tmp(void *sbuf, void *rbuf, int count,
				 struct ompi_datatype_t *dtype,
				 struct ompi_op_t *op,
				 int root, struct ompi_communicator_t *comm)
{
    int i, err, size;
    char *pml_buffer = NULL;
    ptrdiff_t extent, lb;
    int rank = ompi_comm_rank(comm);

    /* If not root, send data to the root. */
    if (rank != root) {
        err = MCA_PML_CALL(send(sbuf, count, dtype, root,
				MCA_COLL_BASE_TAG_REDUCE,
                                MCA_PML_BASE_SEND_STANDARD, comm));
        return err;
    }

    size = ompi_comm_size(comm);

    ompi_datatype_get_extent(dtype, &lb, &extent);
    pml_buffer = (char*)malloc(count * extent);
    if (NULL == pml_buffer) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    err = ompi_datatype_copy_content_same_ddt(dtype, count, (char*)rbuf, (char*)sbuf);
    if (MPI_SUCCESS != err) {
        goto exit;
    }

    /* Loop receiving and calling reduction function (C or Fortran). */
    for (i = size - 1; i >= 0; --i) {
        if (rank == i) {
            continue;
        } else {
            err = MCA_PML_CALL(recv(pml_buffer, count, dtype, i,
                                    MCA_COLL_BASE_TAG_REDUCE, comm,
                                    MPI_STATUS_IGNORE));
            if (MPI_SUCCESS != err) {
                goto exit;
            }
        }
        
        /* Perform the reduction */
        ompi_op_reduce(op, pml_buffer, rbuf, count, dtype);
    }
    
 exit:
    if (NULL != pml_buffer) {
        free(pml_buffer);
    }
    return MPI_SUCCESS;
}


int mca_coll_hierarch_gather_tmp(void *sbuf, int scount,
				 struct ompi_datatype_t *sdtype,
				 void *rbuf, int rcount,
				 struct ompi_datatype_t *rdtype,
				 int root, struct ompi_communicator_t *comm)
{
    int i;
    int err;
    int rank;
    int size;
    char *ptmp;
    MPI_Aint incr;
    MPI_Aint extent;
    MPI_Aint lb;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);

    /* Everyone but root sends data and returns. */
    if (rank != root) {
        return MCA_PML_CALL(send(sbuf, scount, sdtype, root,
                                 MCA_COLL_BASE_TAG_GATHER,
                                 MCA_PML_BASE_SEND_STANDARD, comm));
    }

    /* I am the root, loop receiving the data. */
    ompi_datatype_get_extent(rdtype, &lb, &extent);
    incr = extent * rcount;
    for (i = 0, ptmp = (char *) rbuf; i < size; ++i, ptmp += incr) {
        if (i == rank) {
            if (MPI_IN_PLACE != sbuf) {
                err = ompi_datatype_sndrcv(sbuf, scount, sdtype,
                                      ptmp, rcount, rdtype);
            } else {
                err = MPI_SUCCESS;
            }
        } else {
            err = MCA_PML_CALL(recv(ptmp, rcount, rdtype, i,
                                    MCA_COLL_BASE_TAG_GATHER,
                                    comm, MPI_STATUS_IGNORE));
        }
        if (MPI_SUCCESS != err) {
            return err;
        }
    }

    /* All done */
    return MPI_SUCCESS;
}
