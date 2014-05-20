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
#pragma weak PMPI_TEST_CANCELLED = mpi_test_cancelled_f
#pragma weak pmpi_test_cancelled = mpi_test_cancelled_f
#pragma weak pmpi_test_cancelled_ = mpi_test_cancelled_f
#pragma weak pmpi_test_cancelled__ = mpi_test_cancelled_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TEST_CANCELLED,
                           pmpi_test_cancelled,
                           pmpi_test_cancelled_,
                           pmpi_test_cancelled__,
                           pmpi_test_cancelled_f,
                           (MPI_Fint *status, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TEST_CANCELLED = mpi_test_cancelled_f
#pragma weak mpi_test_cancelled = mpi_test_cancelled_f
#pragma weak mpi_test_cancelled_ = mpi_test_cancelled_f
#pragma weak mpi_test_cancelled__ = mpi_test_cancelled_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TEST_CANCELLED,
                           mpi_test_cancelled,
                           mpi_test_cancelled_,
                           mpi_test_cancelled__,
                           mpi_test_cancelled_f,
                           (MPI_Fint *status, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (status, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_test_cancelled_f(MPI_Fint *status, ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    MPI_Status c_status;
    OMPI_LOGICAL_NAME_DECL(flag);

    /* This seems silly, but someone will do it */

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        *flag = OMPI_INT_2_LOGICAL(0);
        *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
    } else {
        *ierr = MPI_Status_f2c( status, &c_status );

        if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
            *ierr = OMPI_INT_2_FINT(MPI_Test_cancelled(&c_status,
                                                       OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag)));

            OMPI_SINGLE_INT_2_LOGICAL(flag);
        }
    }
}
