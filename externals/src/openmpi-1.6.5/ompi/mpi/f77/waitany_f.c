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
#pragma weak PMPI_WAITANY = mpi_waitany_f
#pragma weak pmpi_waitany = mpi_waitany_f
#pragma weak pmpi_waitany_ = mpi_waitany_f
#pragma weak pmpi_waitany__ = mpi_waitany_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WAITANY,
                           pmpi_waitany,
                           pmpi_waitany_,
                           pmpi_waitany__,
                           pmpi_waitany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *index, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, index, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WAITANY = mpi_waitany_f
#pragma weak mpi_waitany = mpi_waitany_f
#pragma weak mpi_waitany_ = mpi_waitany_f
#pragma weak mpi_waitany__ = mpi_waitany_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WAITANY,
                           mpi_waitany,
                           mpi_waitany_,
                           mpi_waitany__,
                           mpi_waitany_f,
                           (MPI_Fint *count, MPI_Fint *array_of_requests, MPI_Fint *index, MPI_Fint *status, MPI_Fint *ierr),
                           (count, array_of_requests, index, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_WAITANY";


void mpi_waitany_f(MPI_Fint *count, MPI_Fint *array_of_requests,
		   MPI_Fint *index, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Request *c_req;
    MPI_Status c_status;
    int i, c_err;
    OMPI_SINGLE_NAME_DECL(index);

    c_req = (MPI_Request *) malloc(OMPI_FINT_2_INT(*count) * sizeof(MPI_Request));
    if (NULL == c_req) {
        c_err = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                     FUNC_NAME);
	*ierr = OMPI_INT_2_FINT(c_err);
        return;
    }

    for (i = 0; i < OMPI_FINT_2_INT(*count); ++i) {
        c_req[i] = MPI_Request_f2c(array_of_requests[i]);
    }

    *ierr = OMPI_INT_2_FINT(MPI_Waitany(OMPI_FINT_2_INT(*count), c_req, 
					OMPI_SINGLE_NAME_CONVERT(index),
                                        &c_status));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {

        /* Increment index by one for fortran conventions */

        OMPI_SINGLE_INT_2_FINT(index);
        if (MPI_UNDEFINED != *(OMPI_SINGLE_NAME_CONVERT(index))) {
            array_of_requests[OMPI_INT_2_FINT(*index)] =
                c_req[OMPI_INT_2_FINT(*index)]->req_f_to_c_index;
            ++(*index);
        }
        if (!OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
            MPI_Status_c2f(&c_status, status); 
        }
    }
    free(c_req);
}
