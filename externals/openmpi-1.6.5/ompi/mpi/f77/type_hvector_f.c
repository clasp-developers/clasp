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
#pragma weak PMPI_TYPE_HVECTOR = mpi_type_hvector_f
#pragma weak pmpi_type_hvector = mpi_type_hvector_f
#pragma weak pmpi_type_hvector_ = mpi_type_hvector_f
#pragma weak pmpi_type_hvector__ = mpi_type_hvector_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_HVECTOR,
                           pmpi_type_hvector,
                           pmpi_type_hvector_,
                           pmpi_type_hvector__,
                           pmpi_type_hvector_f,
                           (MPI_Fint *count, MPI_Fint *blocklength, MPI_Fint *stride, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, blocklength, stride, oldtype, newtype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_HVECTOR = mpi_type_hvector_f
#pragma weak mpi_type_hvector = mpi_type_hvector_f
#pragma weak mpi_type_hvector_ = mpi_type_hvector_f
#pragma weak mpi_type_hvector__ = mpi_type_hvector_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_HVECTOR,
                           mpi_type_hvector,
                           mpi_type_hvector_,
                           mpi_type_hvector__,
                           mpi_type_hvector_f,
                           (MPI_Fint *count, MPI_Fint *blocklength, MPI_Fint *stride, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, blocklength, stride, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_type_hvector_f(MPI_Fint *count, MPI_Fint *blocklength, 
			MPI_Fint *stride, MPI_Fint *oldtype, 
			MPI_Fint *newtype, MPI_Fint *ierr)
{
    MPI_Datatype c_oldtype, c_newtype;

    c_oldtype = MPI_Type_f2c(*oldtype);

    *ierr = OMPI_INT_2_FINT(MPI_Type_hvector(OMPI_FINT_2_INT(*count),
					     OMPI_FINT_2_INT(*blocklength),
					     (MPI_Aint)*stride,
					     c_oldtype, &c_newtype));
    if( MPI_SUCCESS == OMPI_FINT_2_INT(*ierr) ) {
        *newtype = MPI_Type_c2f(c_newtype);
    }
}
