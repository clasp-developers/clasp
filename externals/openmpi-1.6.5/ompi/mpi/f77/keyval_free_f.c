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
#pragma weak PMPI_KEYVAL_FREE = mpi_keyval_free_f
#pragma weak pmpi_keyval_free = mpi_keyval_free_f
#pragma weak pmpi_keyval_free_ = mpi_keyval_free_f
#pragma weak pmpi_keyval_free__ = mpi_keyval_free_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_KEYVAL_FREE,
                           pmpi_keyval_free,
                           pmpi_keyval_free_,
                           pmpi_keyval_free__,
                           pmpi_keyval_free_f,
                           (MPI_Fint *keyval, MPI_Fint *ierr),
                           (keyval, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_KEYVAL_FREE = mpi_keyval_free_f
#pragma weak mpi_keyval_free = mpi_keyval_free_f
#pragma weak mpi_keyval_free_ = mpi_keyval_free_f
#pragma weak mpi_keyval_free__ = mpi_keyval_free_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_KEYVAL_FREE,
                           mpi_keyval_free,
                           mpi_keyval_free_,
                           mpi_keyval_free__,
                           mpi_keyval_free_f,
                           (MPI_Fint *keyval, MPI_Fint *ierr),
                           (keyval, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_keyval_free_f(MPI_Fint *keyval, MPI_Fint *ierr)
{
    OMPI_SINGLE_NAME_DECL(keyval);

    OMPI_SINGLE_FINT_2_INT(keyval);

    *ierr = 
	OMPI_INT_2_FINT(MPI_Keyval_free(OMPI_SINGLE_NAME_CONVERT(keyval)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(keyval);
    }
}
