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
#pragma weak PMPI_GATHERV = mpi_gatherv_f
#pragma weak pmpi_gatherv = mpi_gatherv_f
#pragma weak pmpi_gatherv_ = mpi_gatherv_f
#pragma weak pmpi_gatherv__ = mpi_gatherv_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GATHERV,
                           pmpi_gatherv,
                           pmpi_gatherv_,
                           pmpi_gatherv__,
                           pmpi_gatherv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GATHERV = mpi_gatherv_f
#pragma weak mpi_gatherv = mpi_gatherv_f
#pragma weak mpi_gatherv_ = mpi_gatherv_f
#pragma weak mpi_gatherv__ = mpi_gatherv_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GATHERV,
                           mpi_gatherv,
                           mpi_gatherv_,
                           mpi_gatherv__,
                           mpi_gatherv_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_gatherv_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
		   char *recvbuf, MPI_Fint *recvcounts, MPI_Fint *displs,
		   MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm,
		   MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;
    int size;
    OMPI_ARRAY_NAME_DECL(recvcounts);
    OMPI_ARRAY_NAME_DECL(displs);

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);
    
    MPI_Comm_size(c_comm, &size);
    OMPI_ARRAY_FINT_2_INT(recvcounts, size);
    OMPI_ARRAY_FINT_2_INT(displs, size);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    *ierr = OMPI_INT_2_FINT(MPI_Gatherv(sendbuf, OMPI_FINT_2_INT(*sendcount),
					c_sendtype, recvbuf,
					OMPI_ARRAY_NAME_CONVERT(recvcounts),
					OMPI_ARRAY_NAME_CONVERT(displs),
					c_recvtype, 
					OMPI_FINT_2_INT(*root),
					c_comm));
}
