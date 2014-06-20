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
#pragma weak PMPI_GET_ELEMENTS = mpi_get_elements_f
#pragma weak pmpi_get_elements = mpi_get_elements_f
#pragma weak pmpi_get_elements_ = mpi_get_elements_f
#pragma weak pmpi_get_elements__ = mpi_get_elements_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GET_ELEMENTS,
                           pmpi_get_elements,
                           pmpi_get_elements_,
                           pmpi_get_elements__,
                           pmpi_get_elements_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_ELEMENTS = mpi_get_elements_f
#pragma weak mpi_get_elements = mpi_get_elements_f
#pragma weak mpi_get_elements_ = mpi_get_elements_f
#pragma weak mpi_get_elements__ = mpi_get_elements_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GET_ELEMENTS,
                           mpi_get_elements,
                           mpi_get_elements_,
                           mpi_get_elements__,
                           mpi_get_elements_f,
                           (MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr),
                           (status, datatype, count, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_get_elements_f(MPI_Fint *status, MPI_Fint *datatype, MPI_Fint *count, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c(*datatype);
    MPI_Status   c_status;
    OMPI_SINGLE_NAME_DECL(count);

    if (OMPI_IS_FORTRAN_STATUS_IGNORE(status)) {
        *count = OMPI_INT_2_FINT(0);
        *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
    } else {
        *ierr = MPI_Status_f2c(status, &c_status);

        if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
            *ierr = OMPI_INT_2_FINT(MPI_Get_elements(&c_status, c_type, 
                                                     OMPI_SINGLE_NAME_CONVERT(count)));
            OMPI_SINGLE_INT_2_FINT(count);
        }
    }
}
