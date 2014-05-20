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
#pragma weak PMPI_TYPE_CREATE_DARRAY = mpi_type_create_darray_f
#pragma weak pmpi_type_create_darray = mpi_type_create_darray_f
#pragma weak pmpi_type_create_darray_ = mpi_type_create_darray_f
#pragma weak pmpi_type_create_darray__ = mpi_type_create_darray_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_DARRAY,
                           pmpi_type_create_darray,
                           pmpi_type_create_darray_,
                           pmpi_type_create_darray__,
                           pmpi_type_create_darray_f,
                           (MPI_Fint *size, MPI_Fint *rank, MPI_Fint *ndims, MPI_Fint *gsize_array, MPI_Fint *distrib_array, MPI_Fint *darg_array, MPI_Fint *psize_array, MPI_Fint *order, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (size, rank, ndims, gsize_array, distrib_array, darg_array, psize_array, order, oldtype, newtype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_DARRAY = mpi_type_create_darray_f
#pragma weak mpi_type_create_darray = mpi_type_create_darray_f
#pragma weak mpi_type_create_darray_ = mpi_type_create_darray_f
#pragma weak mpi_type_create_darray__ = mpi_type_create_darray_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_DARRAY,
                           mpi_type_create_darray,
                           mpi_type_create_darray_,
                           mpi_type_create_darray__,
                           mpi_type_create_darray_f,
                           (MPI_Fint *size, MPI_Fint *rank, MPI_Fint *ndims, MPI_Fint *gsize_array, MPI_Fint *distrib_array, MPI_Fint *darg_array, MPI_Fint *psize_array, MPI_Fint *order, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (size, rank, ndims, gsize_array, distrib_array, darg_array, psize_array, order, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_type_create_darray_f(MPI_Fint *size, MPI_Fint *rank,
			      MPI_Fint *ndims, MPI_Fint *gsize_array, 
			      MPI_Fint *distrib_array, MPI_Fint *darg_array,
			      MPI_Fint *psize_array, MPI_Fint *order, 
			      MPI_Fint *oldtype, MPI_Fint *newtype,
			      MPI_Fint *ierr)
{
    MPI_Datatype c_old = MPI_Type_f2c(*oldtype);
    MPI_Datatype c_new;
    OMPI_ARRAY_NAME_DECL(gsize_array);
    OMPI_ARRAY_NAME_DECL(distrib_array);
    OMPI_ARRAY_NAME_DECL(darg_array);
    OMPI_ARRAY_NAME_DECL(psize_array);

    OMPI_ARRAY_FINT_2_INT(gsize_array, *ndims);
    OMPI_ARRAY_FINT_2_INT(distrib_array, *ndims);
    OMPI_ARRAY_FINT_2_INT(darg_array, *ndims);
    OMPI_ARRAY_FINT_2_INT(psize_array, *ndims);

    *ierr = OMPI_INT_2_FINT(MPI_Type_create_darray(OMPI_FINT_2_INT(*size),
				   OMPI_FINT_2_INT(*rank),
				   OMPI_FINT_2_INT(*ndims),
				   OMPI_ARRAY_NAME_CONVERT(gsize_array), 
				   OMPI_ARRAY_NAME_CONVERT(distrib_array),
				   OMPI_ARRAY_NAME_CONVERT(darg_array),
				   OMPI_ARRAY_NAME_CONVERT(psize_array),
				   OMPI_FINT_2_INT(*order), c_old, &c_new));

    OMPI_ARRAY_FINT_2_INT_CLEANUP(gsize_array);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(distrib_array);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(darg_array);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(psize_array);

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
      *newtype = MPI_Type_c2f(c_new);
    }
}
