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
#pragma weak PMPI_ALLGATHER = mpi_allgather_f
#pragma weak pmpi_allgather = mpi_allgather_f
#pragma weak pmpi_allgather_ = mpi_allgather_f
#pragma weak pmpi_allgather__ = mpi_allgather_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ALLGATHER,
                           pmpi_allgather,
                           pmpi_allgather_,
                           pmpi_allgather__,
                           pmpi_allgather_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLGATHER = mpi_allgather_f
#pragma weak mpi_allgather = mpi_allgather_f
#pragma weak mpi_allgather_ = mpi_allgather_f
#pragma weak mpi_allgather__ = mpi_allgather_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ALLGATHER,
                           mpi_allgather,
                           mpi_allgather_,
                           mpi_allgather__,
                           mpi_allgather_f,
                           (char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_allgather_f(char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype,
		     char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype,
		     MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Datatype c_sendtype, c_recvtype;

    c_comm = MPI_Comm_f2c(*comm);
    c_sendtype = MPI_Type_f2c(*sendtype);
    c_recvtype = MPI_Type_f2c(*recvtype);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    *ierr = OMPI_INT_2_FINT(MPI_Allgather(sendbuf,
					  OMPI_FINT_2_INT(*sendcount),
					  c_sendtype, 
					  recvbuf,
					  OMPI_FINT_2_INT(*recvcount),
					  c_recvtype, c_comm));

}
