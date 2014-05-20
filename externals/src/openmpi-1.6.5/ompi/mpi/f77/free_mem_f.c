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
#pragma weak PMPI_FREE_MEM = mpi_free_mem_f
#pragma weak pmpi_free_mem = mpi_free_mem_f
#pragma weak pmpi_free_mem_ = mpi_free_mem_f
#pragma weak pmpi_free_mem__ = mpi_free_mem_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FREE_MEM,
                           pmpi_free_mem,
                           pmpi_free_mem_,
                           pmpi_free_mem__,
                           pmpi_free_mem_f,
                           (char *base, MPI_Fint *ierr),
                           (base, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FREE_MEM = mpi_free_mem_f
#pragma weak mpi_free_mem = mpi_free_mem_f
#pragma weak mpi_free_mem_ = mpi_free_mem_f
#pragma weak mpi_free_mem__ = mpi_free_mem_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FREE_MEM,
                           mpi_free_mem,
                           mpi_free_mem_,
                           mpi_free_mem__,
                           mpi_free_mem_f,
                           (char *base, MPI_Fint *ierr),
                           (base, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_free_mem_f(char *base, MPI_Fint *ierr)
{
    *ierr = OMPI_INT_2_FINT(MPI_Free_mem(base));
}
