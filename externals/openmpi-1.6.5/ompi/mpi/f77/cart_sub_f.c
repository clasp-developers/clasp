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
#pragma weak PMPI_CART_SUB = mpi_cart_sub_f
#pragma weak pmpi_cart_sub = mpi_cart_sub_f
#pragma weak pmpi_cart_sub_ = mpi_cart_sub_f
#pragma weak pmpi_cart_sub__ = mpi_cart_sub_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_SUB,
                           pmpi_cart_sub,
                           pmpi_cart_sub_,
                           pmpi_cart_sub__,
                           pmpi_cart_sub_f,
                           (MPI_Fint *comm, ompi_fortran_logical_t *remain_dims, MPI_Fint *new_comm, MPI_Fint *ierr),
                           (comm, remain_dims, new_comm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_SUB = mpi_cart_sub_f
#pragma weak mpi_cart_sub = mpi_cart_sub_f
#pragma weak mpi_cart_sub_ = mpi_cart_sub_f
#pragma weak mpi_cart_sub__ = mpi_cart_sub_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CART_SUB,
                           mpi_cart_sub,
                           mpi_cart_sub_,
                           mpi_cart_sub__,
                           mpi_cart_sub_f,
                           (MPI_Fint *comm, ompi_fortran_logical_t *remain_dims, MPI_Fint *new_comm, MPI_Fint *ierr),
                           (comm, remain_dims, new_comm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_cart_sub_f(MPI_Fint *comm, ompi_fortran_logical_t *remain_dims,
                    MPI_Fint *new_comm, MPI_Fint *ierr)
{
    MPI_Comm c_comm, c_new_comm;
    /*
     * Just in the case, when sizeof(logical)!=sizeof(int) and
     * Fortran TRUE-value != 1, we have to convert -- then we need
     * to know the number of dimensions, for the size of remain_dims
     */
#if OMPI_FORTRAN_MUST_CONVERT_LOGICAL_2_INT == 1
    int ndims;
#endif
    OMPI_LOGICAL_ARRAY_NAME_DECL(remain_dims);

    c_comm = MPI_Comm_f2c(*comm);
    c_new_comm = MPI_Comm_f2c(*new_comm);

#if OMPI_FORTRAN_MUST_CONVERT_LOGICAL_2_INT == 1
    *ierr = OMPI_INT_2_FINT(MPI_Cartdim_get(c_comm, &ndims));
    if (MPI_SUCCESS != OMPI_FINT_2_INT(*ierr)) {
        return;
    }
#endif
    OMPI_ARRAY_LOGICAL_2_INT(remain_dims, ndims);

    *ierr = OMPI_INT_2_FINT(MPI_Cart_sub(c_comm,
                              OMPI_LOGICAL_ARRAY_NAME_CONVERT(remain_dims),
                              &c_new_comm));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *new_comm = MPI_Comm_c2f(c_new_comm);
    }

    OMPI_ARRAY_INT_2_LOGICAL(remain_dims, ndims);
    OMPI_ARRAY_LOGICAL_2_INT_CLEANUP(remain_dims);
}
