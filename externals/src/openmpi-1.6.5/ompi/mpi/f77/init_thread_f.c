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
#pragma weak PMPI_INIT_THREAD = mpi_init_thread_f
#pragma weak pmpi_init_thread = mpi_init_thread_f
#pragma weak pmpi_init_thread_ = mpi_init_thread_f
#pragma weak pmpi_init_thread__ = mpi_init_thread_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INIT_THREAD,
                           pmpi_init_thread,
                           pmpi_init_thread_,
                           pmpi_init_thread__,
                           pmpi_init_thread_f,
                           (MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr),
                           (required, provided, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INIT_THREAD = mpi_init_thread_f
#pragma weak mpi_init_thread = mpi_init_thread_f
#pragma weak mpi_init_thread_ = mpi_init_thread_f
#pragma weak mpi_init_thread__ = mpi_init_thread_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INIT_THREAD,
                           mpi_init_thread,
                           mpi_init_thread_,
                           mpi_init_thread__,
                           mpi_init_thread_f,
                           (MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr),
                           (required, provided, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_init_thread_f( MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr )
{
    int argc = 0;
    char** argv = NULL;
    OMPI_SINGLE_NAME_DECL(provided);

    *ierr = OMPI_INT_2_FINT(MPI_Init_thread(&argc, &argv, 
                                            OMPI_FINT_2_INT(*required),
                                            OMPI_SINGLE_NAME_CONVERT(provided)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(provided);
    }
}
