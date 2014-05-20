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
#pragma weak PMPI_REDUCE = mpi_reduce_f
#pragma weak pmpi_reduce = mpi_reduce_f
#pragma weak pmpi_reduce_ = mpi_reduce_f
#pragma weak pmpi_reduce__ = mpi_reduce_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_REDUCE,
                           pmpi_reduce,
                           pmpi_reduce_,
                           pmpi_reduce__,
                           pmpi_reduce_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, root, comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REDUCE = mpi_reduce_f
#pragma weak mpi_reduce = mpi_reduce_f
#pragma weak mpi_reduce_ = mpi_reduce_f
#pragma weak mpi_reduce__ = mpi_reduce_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_REDUCE,
                           mpi_reduce,
                           mpi_reduce_,
                           mpi_reduce__,
                           mpi_reduce_f,
                           (char *sendbuf, char *recvbuf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *op, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (sendbuf, recvbuf, count, datatype, op, root, comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_reduce_f(char *sendbuf, char *recvbuf, MPI_Fint *count,
		  MPI_Fint *datatype, MPI_Fint *op, 
		  MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr)
{
    MPI_Datatype c_type;
    MPI_Op c_op;
    MPI_Comm c_comm;

    c_type = MPI_Type_f2c(*datatype);
    c_op = MPI_Op_f2c(*op);
    c_comm = MPI_Comm_f2c(*comm);

    sendbuf = (char *) OMPI_F2C_IN_PLACE(sendbuf);
    sendbuf = (char *) OMPI_F2C_BOTTOM(sendbuf);
    recvbuf = (char *) OMPI_F2C_BOTTOM(recvbuf);

    *ierr = OMPI_INT_2_FINT(MPI_Reduce(sendbuf, recvbuf,
				       OMPI_FINT_2_INT(*count),
				       c_type, c_op, 
				       OMPI_FINT_2_INT(*root),
				       c_comm));
}
