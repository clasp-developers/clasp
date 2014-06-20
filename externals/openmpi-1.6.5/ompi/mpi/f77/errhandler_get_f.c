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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ERRHANDLER_GET = mpi_errhandler_get_f
#pragma weak pmpi_errhandler_get = mpi_errhandler_get_f
#pragma weak pmpi_errhandler_get_ = mpi_errhandler_get_f
#pragma weak pmpi_errhandler_get__ = mpi_errhandler_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_GET,
                           pmpi_errhandler_get,
                           pmpi_errhandler_get_,
                           pmpi_errhandler_get__,
                           pmpi_errhandler_get_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_GET = mpi_errhandler_get_f
#pragma weak mpi_errhandler_get = mpi_errhandler_get_f
#pragma weak mpi_errhandler_get_ = mpi_errhandler_get_f
#pragma weak mpi_errhandler_get__ = mpi_errhandler_get_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_GET,
                           mpi_errhandler_get,
                           mpi_errhandler_get_,
                           mpi_errhandler_get__,
                           mpi_errhandler_get_f,
                           (MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (comm, errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_errhandler_get_f(MPI_Fint *comm, MPI_Fint *errhandler, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    MPI_Errhandler c_errhandler;

    c_comm = MPI_Comm_f2c(*comm);

    *ierr = OMPI_INT_2_FINT(MPI_Errhandler_get(c_comm, &c_errhandler));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *errhandler = MPI_Errhandler_c2f(c_errhandler);
    }
}

