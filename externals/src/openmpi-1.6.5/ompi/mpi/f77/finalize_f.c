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
#pragma weak PMPI_FINALIZE = mpi_finalize_f
#pragma weak pmpi_finalize = mpi_finalize_f
#pragma weak pmpi_finalize_ = mpi_finalize_f
#pragma weak pmpi_finalize__ = mpi_finalize_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FINALIZE,
                           pmpi_finalize,
                           pmpi_finalize_,
                           pmpi_finalize__,
                           pmpi_finalize_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FINALIZE = mpi_finalize_f
#pragma weak mpi_finalize = mpi_finalize_f
#pragma weak mpi_finalize_ = mpi_finalize_f
#pragma weak mpi_finalize__ = mpi_finalize_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FINALIZE,
                           mpi_finalize,
                           mpi_finalize_,
                           mpi_finalize__,
                           mpi_finalize_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_finalize_f(MPI_Fint *ierr)
{
    *ierr = OMPI_INT_2_FINT(MPI_Finalize());
}
