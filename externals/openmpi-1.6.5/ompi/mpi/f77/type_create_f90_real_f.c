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
#pragma weak PMPI_TYPE_CREATE_F90_REAL = mpi_type_create_f90_real_f
#pragma weak pmpi_type_create_f90_real = mpi_type_create_f90_real_f
#pragma weak pmpi_type_create_f90_real_ = mpi_type_create_f90_real_f
#pragma weak pmpi_type_create_f90_real__ = mpi_type_create_f90_real_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_F90_REAL,
                           pmpi_type_create_f90_real,
                           pmpi_type_create_f90_real_,
                           pmpi_type_create_f90_real__,
                           pmpi_type_create_f90_real_f,
                           (MPI_Fint *p, MPI_Fint *r, MPI_Fint *newtype, MPI_Fint *ierr),
                           (p, r, newtype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_F90_REAL = mpi_type_create_f90_real_f
#pragma weak mpi_type_create_f90_real = mpi_type_create_f90_real_f
#pragma weak mpi_type_create_f90_real_ = mpi_type_create_f90_real_f
#pragma weak mpi_type_create_f90_real__ = mpi_type_create_f90_real_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_F90_REAL,
                           mpi_type_create_f90_real,
                           mpi_type_create_f90_real_,
                           mpi_type_create_f90_real__,
                           mpi_type_create_f90_real_f,
                           (MPI_Fint *p, MPI_Fint *r, MPI_Fint *newtype, MPI_Fint *ierr),
                           (p, r, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_type_create_f90_real_f(MPI_Fint *p, MPI_Fint *r,
				MPI_Fint *newtype, MPI_Fint *ierr)
{
    MPI_Datatype c_new = MPI_Type_f2c(*newtype);

    *ierr = OMPI_INT_2_FINT(MPI_Type_create_f90_real(OMPI_FINT_2_INT(*p),
						     OMPI_FINT_2_INT(*r),
						     &c_new));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *newtype = MPI_Type_c2f(c_new);
    }
}
