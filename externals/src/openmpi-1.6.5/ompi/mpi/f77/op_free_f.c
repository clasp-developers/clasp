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
#pragma weak PMPI_OP_FREE = mpi_op_free_f
#pragma weak pmpi_op_free = mpi_op_free_f
#pragma weak pmpi_op_free_ = mpi_op_free_f
#pragma weak pmpi_op_free__ = mpi_op_free_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_OP_FREE,
                           pmpi_op_free,
                           pmpi_op_free_,
                           pmpi_op_free__,
                           pmpi_op_free_f,
                           (MPI_Fint *op, MPI_Fint *ierr),
                           (op, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_OP_FREE = mpi_op_free_f
#pragma weak mpi_op_free = mpi_op_free_f
#pragma weak mpi_op_free_ = mpi_op_free_f
#pragma weak mpi_op_free__ = mpi_op_free_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_OP_FREE,
                           mpi_op_free,
                           mpi_op_free_,
                           mpi_op_free__,
                           mpi_op_free_f,
                           (MPI_Fint *op, MPI_Fint *ierr),
                           (op, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_op_free_f(MPI_Fint *op, MPI_Fint *ierr)
{
    MPI_Op c_op;

    c_op = MPI_Op_f2c(*op);

    *ierr = OMPI_INT_2_FINT(MPI_Op_free(&c_op));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *op = MPI_Op_c2f(c_op);
    }
}
