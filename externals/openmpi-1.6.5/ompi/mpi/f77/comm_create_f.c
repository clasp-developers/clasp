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
#pragma weak PMPI_COMM_CREATE = mpi_comm_create_f
#pragma weak pmpi_comm_create = mpi_comm_create_f
#pragma weak pmpi_comm_create_ = mpi_comm_create_f
#pragma weak pmpi_comm_create__ = mpi_comm_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_CREATE,
                           pmpi_comm_create,
                           pmpi_comm_create_,
                           pmpi_comm_create__,
                           pmpi_comm_create_f,
                           (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *newcomm, MPI_Fint *ierr),
                           (comm, group, newcomm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_CREATE = mpi_comm_create_f
#pragma weak mpi_comm_create = mpi_comm_create_f
#pragma weak mpi_comm_create_ = mpi_comm_create_f
#pragma weak mpi_comm_create__ = mpi_comm_create_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_CREATE,
                           mpi_comm_create,
                           mpi_comm_create_,
                           mpi_comm_create__,
                           mpi_comm_create_f,
                           (MPI_Fint *comm, MPI_Fint *group, MPI_Fint *newcomm, MPI_Fint *ierr),
                           (comm, group, newcomm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_comm_create_f(MPI_Fint *comm, MPI_Fint *group, MPI_Fint *newcomm, MPI_Fint *ierr)
{
    MPI_Comm c_newcomm;
    MPI_Comm c_comm = MPI_Comm_f2c (*comm);
    MPI_Group c_group = MPI_Group_f2c(*group);

    *ierr = OMPI_INT_2_FINT(MPI_Comm_create (c_comm, c_group, &c_newcomm));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *newcomm = MPI_Comm_c2f (c_newcomm);
    }
}
