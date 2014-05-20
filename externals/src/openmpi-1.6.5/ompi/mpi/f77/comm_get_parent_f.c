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
#pragma weak PMPI_COMM_GET_PARENT = mpi_comm_get_parent_f
#pragma weak pmpi_comm_get_parent = mpi_comm_get_parent_f
#pragma weak pmpi_comm_get_parent_ = mpi_comm_get_parent_f
#pragma weak pmpi_comm_get_parent__ = mpi_comm_get_parent_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_COMM_GET_PARENT,
                           pmpi_comm_get_parent,
                           pmpi_comm_get_parent_,
                           pmpi_comm_get_parent__,
                           pmpi_comm_get_parent_f,
                           (MPI_Fint *parent, MPI_Fint *ierr),
                           (parent, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_GET_PARENT = mpi_comm_get_parent_f
#pragma weak mpi_comm_get_parent = mpi_comm_get_parent_f
#pragma weak mpi_comm_get_parent_ = mpi_comm_get_parent_f
#pragma weak mpi_comm_get_parent__ = mpi_comm_get_parent_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_COMM_GET_PARENT,
                           mpi_comm_get_parent,
                           mpi_comm_get_parent_,
                           mpi_comm_get_parent__,
                           mpi_comm_get_parent_f,
                           (MPI_Fint *parent, MPI_Fint *ierr),
                           (parent, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_comm_get_parent_f(MPI_Fint *parent, MPI_Fint *ierr)
{
    MPI_Comm c_parent;

    *ierr = OMPI_INT_2_FINT(MPI_Comm_get_parent(&c_parent));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *parent = MPI_Comm_c2f(c_parent);
    }
}
