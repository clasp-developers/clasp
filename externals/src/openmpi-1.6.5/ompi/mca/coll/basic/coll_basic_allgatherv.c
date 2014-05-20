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

#include "mpi.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "coll_basic.h"


/*
 *	allgatherv_intra
 *
 *	Function:	- allgatherv using other MPI collectives
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_allgatherv_intra(void *sbuf, int scount,
                                struct ompi_datatype_t *sdtype,
                                void *rbuf, int *rcounts, int *disps,
                                struct ompi_datatype_t *rdtype,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    int i, size, rank ;
    int err;
    MPI_Aint extent;
    MPI_Aint lb;
    char *send_buf = NULL;
    struct ompi_datatype_t *newtype, *send_type;

    size = ompi_comm_size(comm);
    rank = ompi_comm_rank(comm);
    /*
     * We don't have a root process defined. Arbitrarily assign root
     * to process with rank 0 (OMPI convention)
     */

    if (MPI_IN_PLACE == sbuf) {
        ompi_datatype_get_extent(rdtype, &lb, &extent);
        send_type = rdtype;
        send_buf = (char*)rbuf;
        for (i = 0; i < rank; ++i) {
            send_buf += (rcounts[i] * extent);
        }
    } else {
        send_buf = (char*)sbuf;
        send_type = sdtype;
    }

    err = comm->c_coll.coll_gatherv(send_buf,
                                    rcounts[rank], send_type,rbuf,
                                    rcounts, disps, rdtype, 0,
                                    comm, comm->c_coll.coll_gatherv_module);

    if (MPI_SUCCESS != err) {
        return err;
    }
    /*
     * we now have all the data in the root's rbuf. Need to
     * broadcast the data out to the other processes
     *
     * Need to define a datatype that captures the different vectors
     * from each process. MPI_TYPE_INDEXED with params 
     *                    size,rcount,displs,rdtype,newtype
     * should do the trick.
     * Use underlying ddt functions to create, and commit the
     * new datatype on each process, then broadcast and destroy the
     * datatype.
     */

    err = ompi_datatype_create_indexed(size,rcounts,disps,rdtype,&newtype);
    if (MPI_SUCCESS != err) {
        return err;
    }
    
    err = ompi_datatype_commit(&newtype);
    if(MPI_SUCCESS != err) {
       return err;
    }

    comm->c_coll.coll_bcast( rbuf, 1 ,newtype,0,comm,
            comm->c_coll.coll_bcast_module);

    ompi_datatype_destroy (&newtype);

    return MPI_SUCCESS;
}


/*
 *	allgatherv_inter
 *
 *	Function:	- allgatherv using other MPI collectives
 *	Accepts:	- same as MPI_Allgatherv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int
mca_coll_basic_allgatherv_inter(void *sbuf, int scount,
                                struct ompi_datatype_t *sdtype,
                                void *rbuf, int *rcounts, int *disps,
                                struct ompi_datatype_t *rdtype,
                                struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    int rsize, err, i;
    int *scounts, *sdisps;

    rsize = ompi_comm_remote_size(comm);

    scounts = (int *) malloc(2 * rsize * sizeof(int));
    sdisps = scounts + rsize;
    if (NULL == scounts) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i = 0; i < rsize; i++) {
        scounts[i] = scount;
        sdisps[i] = 0;
    }

    err = comm->c_coll.coll_alltoallv(sbuf, scounts, sdisps, sdtype,
                                      rbuf, rcounts, disps, rdtype, comm,
                                      comm->c_coll.coll_alltoallv_module);

    if (NULL != scounts) {
        free(scounts);
    }

    return err;
}
