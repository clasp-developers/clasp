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
#pragma weak PMPI_QUERY_THREAD = mpi_query_thread_f
#pragma weak pmpi_query_thread = mpi_query_thread_f
#pragma weak pmpi_query_thread_ = mpi_query_thread_f
#pragma weak pmpi_query_thread__ = mpi_query_thread_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_QUERY_THREAD,
                           pmpi_query_thread,
                           pmpi_query_thread_,
                           pmpi_query_thread__,
                           pmpi_query_thread_f,
                           (MPI_Fint *provided, MPI_Fint *ierr),
                           (provided, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_QUERY_THREAD = mpi_query_thread_f
#pragma weak mpi_query_thread = mpi_query_thread_f
#pragma weak mpi_query_thread_ = mpi_query_thread_f
#pragma weak mpi_query_thread__ = mpi_query_thread_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_QUERY_THREAD,
                           mpi_query_thread,
                           mpi_query_thread_,
                           mpi_query_thread__,
                           mpi_query_thread_f,
                           (MPI_Fint *provided, MPI_Fint *ierr),
                           (provided, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_query_thread_f(MPI_Fint *provided, MPI_Fint *ierr)
{
    OMPI_SINGLE_NAME_DECL(provided);

    *ierr = OMPI_INT_2_FINT(MPI_Query_thread(OMPI_SINGLE_NAME_CONVERT(provided)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(provided);
    }
}
