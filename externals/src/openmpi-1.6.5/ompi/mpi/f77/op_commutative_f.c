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
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_OP_COMMUTATIVE = mpi_op_commutative_f
#pragma weak pmpi_op_commutative = mpi_op_commutative_f
#pragma weak pmpi_op_commutative_ = mpi_op_commutative_f
#pragma weak pmpi_op_commutative__ = mpi_op_commutative_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_OP_COMMUTATIVE,
                           pmpi_op_commutative,
                           pmpi_op_commutative_,
                           pmpi_op_commutative__,
                           pmpi_op_commutative_f,
                           (MPI_Fint *op, MPI_Fint *commute, MPI_Fint *ierr),
                           (op, commute, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_OP_COMMUTATIVE = mpi_op_commutative_f
#pragma weak mpi_op_commutative = mpi_op_commutative_f
#pragma weak mpi_op_commutative_ = mpi_op_commutative_f
#pragma weak mpi_op_commutative__ = mpi_op_commutative_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_OP_COMMUTATIVE,
                           mpi_op_commutative,
                           mpi_op_commutative_,
                           mpi_op_commutative__,
                           mpi_op_commutative_f,
                           (MPI_Fint *op, MPI_Fint *commute, MPI_Fint *ierr),
                           (op, commute, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_op_commutative_f(MPI_Fint *op, MPI_Fint *commute, MPI_Fint *ierr)
{
    MPI_Op c_op;
    OMPI_SINGLE_NAME_DECL(commute);

    c_op = MPI_Op_f2c(*op);

    *ierr = OMPI_INT_2_FINT(MPI_Op_commutative(c_op,
                                               OMPI_SINGLE_NAME_CONVERT(commute)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(commute);
    }
}
