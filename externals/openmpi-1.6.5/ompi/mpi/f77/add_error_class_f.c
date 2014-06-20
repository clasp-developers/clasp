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
#pragma weak PMPI_ADD_ERROR_CLASS = mpi_add_error_class_f
#pragma weak pmpi_add_error_class = mpi_add_error_class_f
#pragma weak pmpi_add_error_class_ = mpi_add_error_class_f
#pragma weak pmpi_add_error_class__ = mpi_add_error_class_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ADD_ERROR_CLASS,
                           pmpi_add_error_class,
                           pmpi_add_error_class_,
                           pmpi_add_error_class__,
                           pmpi_add_error_class_f,
                           (MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorclass, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ADD_ERROR_CLASS = mpi_add_error_class_f
#pragma weak mpi_add_error_class = mpi_add_error_class_f
#pragma weak mpi_add_error_class_ = mpi_add_error_class_f
#pragma weak mpi_add_error_class__ = mpi_add_error_class_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ADD_ERROR_CLASS,
                           mpi_add_error_class,
                           mpi_add_error_class_,
                           mpi_add_error_class__,
                           mpi_add_error_class_f,
                           (MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorclass, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_add_error_class_f(MPI_Fint *errorclass, MPI_Fint *ierr)
{
    OMPI_SINGLE_NAME_DECL(errorclass);

    *ierr = OMPI_INT_2_FINT(MPI_Add_error_class(OMPI_SINGLE_NAME_CONVERT(errorclass)
						));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(errorclass);
    }
}
