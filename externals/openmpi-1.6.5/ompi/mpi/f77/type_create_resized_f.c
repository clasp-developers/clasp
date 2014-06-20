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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_RESIZED = mpi_type_create_resized_f
#pragma weak pmpi_type_create_resized = mpi_type_create_resized_f
#pragma weak pmpi_type_create_resized_ = mpi_type_create_resized_f
#pragma weak pmpi_type_create_resized__ = mpi_type_create_resized_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_RESIZED,
                           pmpi_type_create_resized,
                           pmpi_type_create_resized_,
                           pmpi_type_create_resized__,
                           pmpi_type_create_resized_f,
                           (MPI_Fint *oldtype, MPI_Aint *lb, MPI_Aint *extent, MPI_Fint *newtype, MPI_Fint *ierr),
                           (oldtype, lb, extent, newtype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_RESIZED = mpi_type_create_resized_f
#pragma weak mpi_type_create_resized = mpi_type_create_resized_f
#pragma weak mpi_type_create_resized_ = mpi_type_create_resized_f
#pragma weak mpi_type_create_resized__ = mpi_type_create_resized_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_RESIZED,
                           mpi_type_create_resized,
                           mpi_type_create_resized_,
                           mpi_type_create_resized__,
                           mpi_type_create_resized_f,
                           (MPI_Fint *oldtype, MPI_Aint *lb, MPI_Aint *extent, MPI_Fint *newtype, MPI_Fint *ierr),
                           (oldtype, lb, extent, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_type_create_resized_f(MPI_Fint *oldtype, MPI_Aint *lb,
			       MPI_Aint *extent, MPI_Fint *newtype, 
			       MPI_Fint *ierr)
{
    MPI_Datatype c_old = MPI_Type_f2c(*oldtype);
    MPI_Datatype c_new;

    *ierr = OMPI_INT_2_FINT(MPI_Type_create_resized(c_old, *lb,
						    *extent, 
						    &c_new));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *newtype = MPI_Type_c2f(c_new);
    }
}
