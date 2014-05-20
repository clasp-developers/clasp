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
#pragma weak PMPI_WAIT = mpi_wait_f
#pragma weak pmpi_wait = mpi_wait_f
#pragma weak pmpi_wait_ = mpi_wait_f
#pragma weak pmpi_wait__ = mpi_wait_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WAIT,
                           pmpi_wait,
                           pmpi_wait_,
                           pmpi_wait__,
                           pmpi_wait_f,
                           (MPI_Fint *request, MPI_Fint *status, MPI_Fint *ierr),
                           (request, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WAIT = mpi_wait_f
#pragma weak mpi_wait = mpi_wait_f
#pragma weak mpi_wait_ = mpi_wait_f
#pragma weak mpi_wait__ = mpi_wait_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WAIT,
                           mpi_wait,
                           mpi_wait_,
                           mpi_wait__,
                           mpi_wait_f,
                           (MPI_Fint *request, MPI_Fint *status, MPI_Fint *ierr),
                           (request, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_wait_f(MPI_Fint *request, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Request c_req = MPI_Request_f2c(*request);
    MPI_Status  c_status;

    *ierr = OMPI_INT_2_FINT(MPI_Wait(&c_req, &c_status));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *request = OMPI_INT_2_FINT(c_req->req_f_to_c_index);
        if (!OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
            MPI_Status_c2f(&c_status, status);
        }
    }
}
