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
#pragma weak PMPI_INFO_FREE = mpi_info_free_f
#pragma weak pmpi_info_free = mpi_info_free_f
#pragma weak pmpi_info_free_ = mpi_info_free_f
#pragma weak pmpi_info_free__ = mpi_info_free_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INFO_FREE,
                           pmpi_info_free,
                           pmpi_info_free_,
                           pmpi_info_free__,
                           pmpi_info_free_f,
                           (MPI_Fint *info, MPI_Fint *ierr),
                           (info, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INFO_FREE = mpi_info_free_f
#pragma weak mpi_info_free = mpi_info_free_f
#pragma weak mpi_info_free_ = mpi_info_free_f
#pragma weak mpi_info_free__ = mpi_info_free_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INFO_FREE,
                           mpi_info_free,
                           mpi_info_free_,
                           mpi_info_free__,
                           mpi_info_free_f,
                           (MPI_Fint *info, MPI_Fint *ierr),
                           (info, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_info_free_f(MPI_Fint *info, MPI_Fint *ierr)
{
    MPI_Info c_info;

    c_info = MPI_Info_f2c(*info);

    *ierr = OMPI_INT_2_FINT(MPI_Info_free(&c_info));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *info = MPI_Info_c2f(c_info);
    }
}
