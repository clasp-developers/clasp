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
#pragma weak PMPI_CART_MAP = mpi_cart_map_f
#pragma weak pmpi_cart_map = mpi_cart_map_f
#pragma weak pmpi_cart_map_ = mpi_cart_map_f
#pragma weak pmpi_cart_map__ = mpi_cart_map_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_MAP,
                           pmpi_cart_map,
                           pmpi_cart_map_,
                           pmpi_cart_map__,
                           pmpi_cart_map_f,
                           (MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *dims, ompi_fortran_logical_t *periods, MPI_Fint *newrank, MPI_Fint *ierr),
                           (comm, ndims, dims, periods, newrank, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_MAP = mpi_cart_map_f
#pragma weak mpi_cart_map = mpi_cart_map_f
#pragma weak mpi_cart_map_ = mpi_cart_map_f
#pragma weak mpi_cart_map__ = mpi_cart_map_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CART_MAP,
                           mpi_cart_map,
                           mpi_cart_map_,
                           mpi_cart_map__,
                           mpi_cart_map_f,
                           (MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *dims, ompi_fortran_logical_t *periods, MPI_Fint *newrank, MPI_Fint *ierr),
                           (comm, ndims, dims, periods, newrank, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_cart_map_f(MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *dims,
                    ompi_fortran_logical_t *periods, MPI_Fint *newrank, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    int size;
    OMPI_ARRAY_NAME_DECL(dims);
    OMPI_LOGICAL_ARRAY_NAME_DECL(periods);
    OMPI_SINGLE_NAME_DECL(newrank);

    c_comm = MPI_Comm_f2c(*comm);

    size = OMPI_FINT_2_INT(*ndims);
    OMPI_ARRAY_FINT_2_INT(dims, size);
    OMPI_ARRAY_LOGICAL_2_INT(periods, size);

    *ierr = OMPI_INT_2_FINT(MPI_Cart_map(c_comm, 
                                         OMPI_FINT_2_INT(*ndims),
                                         OMPI_ARRAY_NAME_CONVERT(dims),
                                         OMPI_LOGICAL_ARRAY_NAME_CONVERT(periods),
                                         OMPI_SINGLE_NAME_CONVERT(newrank)));

    OMPI_ARRAY_FINT_2_INT_CLEANUP(dims);
    OMPI_ARRAY_INT_2_LOGICAL(periods, size);
    OMPI_ARRAY_LOGICAL_2_INT_CLEANUP(periods);
    OMPI_SINGLE_INT_2_FINT(newrank);
}
