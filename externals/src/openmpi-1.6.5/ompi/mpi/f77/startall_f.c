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
#include "ompi/errhandler/errhandler.h"
#include "ompi/communicator/communicator.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_STARTALL = mpi_startall_f
#pragma weak pmpi_startall = mpi_startall_f
#pragma weak pmpi_startall_ = mpi_startall_f
#pragma weak pmpi_startall__ = mpi_startall_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_STARTALL,
                           pmpi_startall,
                           pmpi_startall_,
                           pmpi_startall__,
                           pmpi_startall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *ierr),
                           (count, array_of_requests, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_STARTALL = mpi_startall_f
#pragma weak mpi_startall = mpi_startall_f
#pragma weak mpi_startall_ = mpi_startall_f
#pragma weak mpi_startall__ = mpi_startall_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_STARTALL,
                           mpi_startall,
                           mpi_startall_,
                           mpi_startall__,
                           mpi_startall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *ierr),
                           (count, array_of_requests, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_STARTALL";


void mpi_startall_f(MPI_Fint *count, MPI_Fint *array_of_requests, 
		    MPI_Fint *ierr)
{
    MPI_Request *c_req;
    int i;

    c_req = (MPI_Request *) malloc(*count * sizeof(MPI_Request));
    if (NULL == c_req) {
        *ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                       FUNC_NAME);
        return;
    }

    for(i = 0; i < *count; i++ ) {
        c_req[i] = MPI_Request_f2c(array_of_requests[i]);
    }

    *ierr = OMPI_INT_2_FINT(MPI_Startall(OMPI_FINT_2_INT(*count), c_req));
    for( i = 0; i < *count; i++ ) {
        array_of_requests[i] = MPI_Request_c2f(c_req[i]);
    }
    free(c_req);
}
