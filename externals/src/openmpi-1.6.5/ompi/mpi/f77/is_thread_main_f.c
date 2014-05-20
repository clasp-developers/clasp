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
#pragma weak PMPI_IS_THREAD_MAIN = mpi_is_thread_main_f
#pragma weak pmpi_is_thread_main = mpi_is_thread_main_f
#pragma weak pmpi_is_thread_main_ = mpi_is_thread_main_f
#pragma weak pmpi_is_thread_main__ = mpi_is_thread_main_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_IS_THREAD_MAIN,
                           pmpi_is_thread_main,
                           pmpi_is_thread_main_,
                           pmpi_is_thread_main__,
                           pmpi_is_thread_main_f,
                           (ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IS_THREAD_MAIN = mpi_is_thread_main_f
#pragma weak mpi_is_thread_main = mpi_is_thread_main_f
#pragma weak mpi_is_thread_main_ = mpi_is_thread_main_f
#pragma weak mpi_is_thread_main__ = mpi_is_thread_main_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_IS_THREAD_MAIN,
                           mpi_is_thread_main,
                           mpi_is_thread_main_,
                           mpi_is_thread_main__,
                           mpi_is_thread_main_f,
                           (ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_is_thread_main_f(ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    OMPI_LOGICAL_NAME_DECL(flag);

    *ierr = OMPI_INT_2_FINT(MPI_Is_thread_main(OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_LOGICAL(flag);
    }
}
