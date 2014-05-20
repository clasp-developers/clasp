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

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/constants.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ALLTOALLW = mpi_alltoallw_f
#pragma weak pmpi_alltoallw = mpi_alltoallw_f
#pragma weak pmpi_alltoallw_ = mpi_alltoallw_f
#pragma weak pmpi_alltoallw__ = mpi_alltoallw_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ALLTOALLW,
                           pmpi_alltoallw,
                           pmpi_alltoallw_,
                           pmpi_alltoallw__,
                           pmpi_alltoallw_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLTOALLW = mpi_alltoallw_f
#pragma weak mpi_alltoallw = mpi_alltoallw_f
#pragma weak mpi_alltoallw_ = mpi_alltoallw_f
#pragma weak mpi_alltoallw__ = mpi_alltoallw_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ALLTOALLW,
                           mpi_alltoallw,
                           mpi_alltoallw_,
                           mpi_alltoallw__,
                           mpi_alltoallw_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtypes, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtypes, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_alltoallw_f(char *sendbuf, MPI_Fint *sendcounts,
		     MPI_Fint *sdispls, MPI_Fint *sendtypes, 
		     char *recvbuf, MPI_Fint *recvcounts,
		     MPI_Fint *rdispls, MPI_Fint *recvtypes,
		     MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype *c_sendtypes, *c_recvtypes;
    int size;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(sdispls);
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(rdispls);

    c_comm = MPI_Comm_f2c(*comm);
    MPI_Comm_size(c_comm, &size);

    c_sendtypes = (MPI_Datatype *) malloc(size * sizeof(MPI_Datatype));
    c_recvtypes = (MPI_Datatype *) malloc(size * sizeof(MPI_Datatype));

    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(sdispls, size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(rdispls, size);

    while (size > 0) {
        c_sendtypes[size - 1] = MPI_Type_f2c(sendtypes[size - 1]);
        c_recvtypes[size - 1] = MPI_Type_f2c(recvtypes[size - 1]);
        --size;
    }

    /* Alltoallw does not support MPI_IN_PLACE */
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    *ierr = OMPI_INT_2_FINT(MPI_Alltoallw(sendbuf, 
					  OMPI_ARRAY_NAME_CONVERT(sendcounts),
					  OMPI_ARRAY_NAME_CONVERT(sdispls),
					  c_sendtypes, 
					  recvbuf, 
					  OMPI_ARRAY_NAME_CONVERT(recvcounts),
					  OMPI_ARRAY_NAME_CONVERT(rdispls),
					  c_recvtypes, c_comm));

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(sdispls);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(rdispls);
    free(c_sendtypes);
    free(c_recvtypes);
}
