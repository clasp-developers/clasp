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
#pragma weak PMPI_ALLTOALLV = mpi_alltoallv_f
#pragma weak pmpi_alltoallv = mpi_alltoallv_f
#pragma weak pmpi_alltoallv_ = mpi_alltoallv_f
#pragma weak pmpi_alltoallv__ = mpi_alltoallv_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ALLTOALLV,
                           pmpi_alltoallv,
                           pmpi_alltoallv_,
                           pmpi_alltoallv__,
                           pmpi_alltoallv_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLTOALLV = mpi_alltoallv_f
#pragma weak mpi_alltoallv = mpi_alltoallv_f
#pragma weak mpi_alltoallv_ = mpi_alltoallv_f
#pragma weak mpi_alltoallv__ = mpi_alltoallv_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ALLTOALLV,
                           mpi_alltoallv,
                           mpi_alltoallv_,
                           mpi_alltoallv__,
                           mpi_alltoallv_f,
                           (char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *rdispls, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_alltoallv_f(char *sendbuf, MPI_Fint *sendcounts, MPI_Fint *sdispls,
		     MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts,
		     MPI_Fint *rdispls, MPI_Fint *recvtype, 
		     MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    int size;
    OMPI_ARRAY_NAME_DECL(sendcounts);
    OMPI_ARRAY_NAME_DECL(sdispls);
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(rdispls);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);

    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(sendcounts, size);
    OMPI_ARRAY_FINT_2_INT(sdispls, size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(rdispls, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    *ierr = OMPI_INT_2_FINT(MPI_Alltoallv(sendbuf, 
					  OMPI_ARRAY_NAME_CONVERT(sendcounts),
					  OMPI_ARRAY_NAME_CONVERT(sdispls), 
					  c_sendtype, 
					  recvbuf, 
					  OMPI_ARRAY_NAME_CONVERT(recvcounts),
					  OMPI_ARRAY_NAME_CONVERT(rdispls),
					  c_recvtype, c_comm));

    OMPI_ARRAY_FINT_2_INT_CLEANUP(sendcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(sdispls);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(recvcounts);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(rdispls);
}
