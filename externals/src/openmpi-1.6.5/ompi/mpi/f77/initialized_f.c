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
#pragma weak PMPI_INITIALIZED = mpi_initialized_f
#pragma weak pmpi_initialized = mpi_initialized_f
#pragma weak pmpi_initialized_ = mpi_initialized_f
#pragma weak pmpi_initialized__ = mpi_initialized_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INITIALIZED,
                           pmpi_initialized,
                           pmpi_initialized_,
                           pmpi_initialized__,
                           pmpi_initialized_f,
                           (ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INITIALIZED = mpi_initialized_f
#pragma weak mpi_initialized = mpi_initialized_f
#pragma weak mpi_initialized_ = mpi_initialized_f
#pragma weak mpi_initialized__ = mpi_initialized_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INITIALIZED,
                           mpi_initialized,
                           mpi_initialized_,
                           mpi_initialized__,
                           mpi_initialized_f,
                           (ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_initialized_f(ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    OMPI_LOGICAL_NAME_DECL(flag);
    *ierr = OMPI_INT_2_FINT(MPI_Initialized(OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_LOGICAL(flag);
    }
}
