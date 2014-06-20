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
#include "ompi/errhandler/errhandler.h"
#include "ompi/communicator/communicator.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WAITALL = mpi_waitall_f
#pragma weak pmpi_waitall = mpi_waitall_f
#pragma weak pmpi_waitall_ = mpi_waitall_f
#pragma weak pmpi_waitall__ = mpi_waitall_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WAITALL,
                           pmpi_waitall,
                           pmpi_waitall_,
                           pmpi_waitall__,
                           pmpi_waitall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, array_of_statuses, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WAITALL = mpi_waitall_f
#pragma weak mpi_waitall = mpi_waitall_f
#pragma weak mpi_waitall_ = mpi_waitall_f
#pragma weak mpi_waitall__ = mpi_waitall_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WAITALL,
                           mpi_waitall,
                           mpi_waitall_,
                           mpi_waitall__,
                           mpi_waitall_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *array_of_statuses, MPI_Fint *ierr),
                           (count, array_of_requests, array_of_statuses, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_WAITALL";


void mpi_waitall_f(MPI_Fint *count, MPI_Fint *array_of_requests,
		   MPI_Fint *array_of_statuses, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status *c_status;
    int i;

    c_req = (MPI_Request *) malloc(OMPI_FINT_2_INT(*count) *
                   (sizeof(MPI_Request) + sizeof(MPI_Status)));
    if (NULL == c_req) {
        *ierr = OMPI_INT_2_FINT(OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD,
                                                       MPI_ERR_NO_MEM,
                                                       FUNC_NAME));
        return;
    }
    c_status = (MPI_Status*) (c_req + OMPI_FINT_2_INT(*count));

    for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) {
        c_req[i] = MPI_Request_f2c(array_of_requests[i]);
    }

    *ierr = OMPI_INT_2_FINT(MPI_Waitall(OMPI_FINT_2_INT(*count),
					c_req, c_status));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) {
            array_of_requests[i] = c_req[i]->req_f_to_c_index;
            if (!OMPI_IS_FORTRAN_STATUSES_IGNORE(array_of_statuses) &&
                !OMPI_IS_FORTRAN_STATUS_IGNORE(&array_of_statuses[i])) {
                MPI_Status_c2f( &c_status[i], &array_of_statuses[i * (sizeof(MPI_Status) / sizeof(int))]); 
            }
        }
    }
    free(c_req);
}
