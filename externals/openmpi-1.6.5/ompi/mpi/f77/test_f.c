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
#pragma weak PMPI_TEST = mpi_test_f
#pragma weak pmpi_test = mpi_test_f
#pragma weak pmpi_test_ = mpi_test_f
#pragma weak pmpi_test__ = mpi_test_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TEST,
                           pmpi_test,
                           pmpi_test_,
                           pmpi_test__,
                           pmpi_test_f,
                           (MPI_Fint *request, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TEST = mpi_test_f
#pragma weak mpi_test = mpi_test_f
#pragma weak mpi_test_ = mpi_test_f
#pragma weak mpi_test__ = mpi_test_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TEST,
                           mpi_test,
                           mpi_test_,
                           mpi_test__,
                           mpi_test_f,
                           (MPI_Fint *request, ompi_fortran_logical_t *flag, MPI_Fint *status, MPI_Fint *ierr),
                           (request, flag, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_test_f(MPI_Fint *request, ompi_fortran_logical_t *flag,
                MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Request c_req = MPI_Request_f2c(*request);
    MPI_Status c_status;
    OMPI_LOGICAL_NAME_DECL(flag);

    *ierr = OMPI_INT_2_FINT(MPI_Test(&c_req, 
                                     OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag),
                                     &c_status));

    OMPI_SINGLE_INT_2_LOGICAL(flag);

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *request = OMPI_INT_2_FINT(c_req->req_f_to_c_index);
        if (!OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
            MPI_Status_c2f(&c_status, status); 
        }
    }
}
