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
#pragma weak PMPI_ALLOC_MEM = mpi_alloc_mem_f
#pragma weak pmpi_alloc_mem = mpi_alloc_mem_f
#pragma weak pmpi_alloc_mem_ = mpi_alloc_mem_f
#pragma weak pmpi_alloc_mem__ = mpi_alloc_mem_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ALLOC_MEM,
                           pmpi_alloc_mem,
                           pmpi_alloc_mem_,
                           pmpi_alloc_mem__,
                           pmpi_alloc_mem_f,
                           (MPI_Aint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr),
                           (size, info, baseptr, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ALLOC_MEM = mpi_alloc_mem_f
#pragma weak mpi_alloc_mem = mpi_alloc_mem_f
#pragma weak mpi_alloc_mem_ = mpi_alloc_mem_f
#pragma weak mpi_alloc_mem__ = mpi_alloc_mem_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ALLOC_MEM,
                           mpi_alloc_mem,
                           mpi_alloc_mem_,
                           mpi_alloc_mem__,
                           mpi_alloc_mem_f,
                           (MPI_Aint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr),
                           (size, info, baseptr, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_alloc_mem_f(MPI_Aint *size, MPI_Fint *info, char *baseptr, MPI_Fint *ierr)
{
    MPI_Info c_info = MPI_Info_f2c(*info);

    *ierr = OMPI_INT_2_FINT(MPI_Alloc_mem(*size, c_info, baseptr));
}
