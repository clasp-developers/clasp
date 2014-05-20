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
#pragma weak PMPI_START = mpi_start_f
#pragma weak pmpi_start = mpi_start_f
#pragma weak pmpi_start_ = mpi_start_f
#pragma weak pmpi_start__ = mpi_start_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_START,
                           pmpi_start,
                           pmpi_start_,
                           pmpi_start__,
                           pmpi_start_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_START = mpi_start_f
#pragma weak mpi_start = mpi_start_f
#pragma weak mpi_start_ = mpi_start_f
#pragma weak mpi_start__ = mpi_start_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_START,
                           mpi_start,
                           mpi_start_,
                           mpi_start__,
                           mpi_start_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_start_f(MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Request c_req = MPI_Request_f2c(*request);
    MPI_Request tmp_req = c_req;

    *ierr = OMPI_INT_2_FINT(MPI_Start(&c_req));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        /* For a persistent request, the underlying request descriptor could
           change (i.e. the old descriptor has not completed and cannot be 
           reused).
           So commit new descriptor.
        */
        if ( tmp_req != c_req ) {
            *request = MPI_Request_c2f(c_req);
        }
    }
}
