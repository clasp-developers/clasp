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
#pragma weak PMPI_TYPE_SIZE = mpi_type_size_f
#pragma weak pmpi_type_size = mpi_type_size_f
#pragma weak pmpi_type_size_ = mpi_type_size_f
#pragma weak pmpi_type_size__ = mpi_type_size_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_SIZE,
                           pmpi_type_size,
                           pmpi_type_size_,
                           pmpi_type_size__,
                           pmpi_type_size_f,
                           (MPI_Fint *type, MPI_Fint *size, MPI_Fint *ierr),
                           (type, size, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_SIZE = mpi_type_size_f
#pragma weak mpi_type_size = mpi_type_size_f
#pragma weak mpi_type_size_ = mpi_type_size_f
#pragma weak mpi_type_size__ = mpi_type_size_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_SIZE,
                           mpi_type_size,
                           mpi_type_size_,
                           mpi_type_size__,
                           mpi_type_size_f,
                           (MPI_Fint *type, MPI_Fint *size, MPI_Fint *ierr),
                           (type, size, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_type_size_f(MPI_Fint *type, MPI_Fint *size, MPI_Fint *ierr)
{
    MPI_Datatype c_type = MPI_Type_f2c(*type);
    OMPI_SINGLE_NAME_DECL(size);

    *ierr = OMPI_INT_2_FINT(MPI_Type_size(c_type, 
					  OMPI_SINGLE_NAME_CONVERT(size)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(size);
    }
}
