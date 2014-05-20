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
#pragma weak PMPI_REQUEST_FREE = mpi_request_free_f
#pragma weak pmpi_request_free = mpi_request_free_f
#pragma weak pmpi_request_free_ = mpi_request_free_f
#pragma weak pmpi_request_free__ = mpi_request_free_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_REQUEST_FREE,
                           pmpi_request_free,
                           pmpi_request_free_,
                           pmpi_request_free__,
                           pmpi_request_free_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REQUEST_FREE = mpi_request_free_f
#pragma weak mpi_request_free = mpi_request_free_f
#pragma weak mpi_request_free_ = mpi_request_free_f
#pragma weak mpi_request_free__ = mpi_request_free_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_REQUEST_FREE,
                           mpi_request_free,
                           mpi_request_free_,
                           mpi_request_free__,
                           mpi_request_free_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_request_free_f(MPI_Fint *request, MPI_Fint *ierr)
{
    int err;

    MPI_Request c_req = MPI_Request_f2c( *request ); 
    err = MPI_Request_free(&c_req);
    *ierr = OMPI_INT_2_FINT(err);

    if (MPI_SUCCESS == err) {
        *request = OMPI_INT_2_FINT(MPI_REQUEST_NULL->req_f_to_c_index);
    }
}
