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
#pragma weak PMPI_TYPE_FREE_KEYVAL = mpi_type_free_keyval_f
#pragma weak pmpi_type_free_keyval = mpi_type_free_keyval_f
#pragma weak pmpi_type_free_keyval_ = mpi_type_free_keyval_f
#pragma weak pmpi_type_free_keyval__ = mpi_type_free_keyval_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_FREE_KEYVAL,
                           pmpi_type_free_keyval,
                           pmpi_type_free_keyval_,
                           pmpi_type_free_keyval__,
                           pmpi_type_free_keyval_f,
                           (MPI_Fint *type_keyval, MPI_Fint *ierr),
                           (type_keyval, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_FREE_KEYVAL = mpi_type_free_keyval_f
#pragma weak mpi_type_free_keyval = mpi_type_free_keyval_f
#pragma weak mpi_type_free_keyval_ = mpi_type_free_keyval_f
#pragma weak mpi_type_free_keyval__ = mpi_type_free_keyval_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_FREE_KEYVAL,
                           mpi_type_free_keyval,
                           mpi_type_free_keyval_,
                           mpi_type_free_keyval__,
                           mpi_type_free_keyval_f,
                           (MPI_Fint *type_keyval, MPI_Fint *ierr),
                           (type_keyval, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_type_free_keyval_f(MPI_Fint *type_keyval, MPI_Fint *ierr)
{
    OMPI_SINGLE_NAME_DECL(type_keyval);

    OMPI_SINGLE_FINT_2_INT(type_keyval);

    *ierr = 
	OMPI_INT_2_FINT(MPI_Type_free_keyval(OMPI_SINGLE_NAME_CONVERT(type_keyval)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(type_keyval);
    }
}
