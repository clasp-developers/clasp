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
#pragma weak PMPI_GREQUEST_COMPLETE = mpi_grequest_complete_f
#pragma weak pmpi_grequest_complete = mpi_grequest_complete_f
#pragma weak pmpi_grequest_complete_ = mpi_grequest_complete_f
#pragma weak pmpi_grequest_complete__ = mpi_grequest_complete_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GREQUEST_COMPLETE,
                           pmpi_grequest_complete,
                           pmpi_grequest_complete_,
                           pmpi_grequest_complete__,
                           pmpi_grequest_complete_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GREQUEST_COMPLETE = mpi_grequest_complete_f
#pragma weak mpi_grequest_complete = mpi_grequest_complete_f
#pragma weak mpi_grequest_complete_ = mpi_grequest_complete_f
#pragma weak mpi_grequest_complete__ = mpi_grequest_complete_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GREQUEST_COMPLETE,
                           mpi_grequest_complete,
                           mpi_grequest_complete_,
                           mpi_grequest_complete__,
                           mpi_grequest_complete_f,
                           (MPI_Fint *request, MPI_Fint *ierr),
                           (request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_grequest_complete_f(MPI_Fint *request, MPI_Fint *ierr)
{
    MPI_Request c_req = MPI_Request_f2c(*request);

    *ierr = OMPI_INT_2_FINT(MPI_Grequest_complete(c_req));
}
