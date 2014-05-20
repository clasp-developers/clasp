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
#pragma weak PMPI_CART_COORDS = mpi_cart_coords_f
#pragma weak pmpi_cart_coords = mpi_cart_coords_f
#pragma weak pmpi_cart_coords_ = mpi_cart_coords_f
#pragma weak pmpi_cart_coords__ = mpi_cart_coords_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_COORDS,
                           pmpi_cart_coords,
                           pmpi_cart_coords_,
                           pmpi_cart_coords__,
                           pmpi_cart_coords_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxdims, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, rank, maxdims, coords, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_COORDS = mpi_cart_coords_f
#pragma weak mpi_cart_coords = mpi_cart_coords_f
#pragma weak mpi_cart_coords_ = mpi_cart_coords_f
#pragma weak mpi_cart_coords__ = mpi_cart_coords_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CART_COORDS,
                           mpi_cart_coords,
                           mpi_cart_coords_,
                           mpi_cart_coords__,
                           mpi_cart_coords_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxdims, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, rank, maxdims, coords, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_cart_coords_f(MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxdims,
                       MPI_Fint *coords, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    OMPI_ARRAY_NAME_DECL(coords);

    c_comm = MPI_Comm_f2c(*comm);

    OMPI_ARRAY_FINT_2_INT_ALLOC(coords, OMPI_FINT_2_INT(*maxdims));
    *ierr = OMPI_INT_2_FINT(MPI_Cart_coords(c_comm,
                                            OMPI_FINT_2_INT(*rank),
                                            OMPI_FINT_2_INT(*maxdims),
                                            OMPI_ARRAY_NAME_CONVERT(coords)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_ARRAY_INT_2_FINT(coords, OMPI_FINT_2_INT(*maxdims));
    }
}
