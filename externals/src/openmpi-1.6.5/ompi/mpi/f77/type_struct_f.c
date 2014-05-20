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
#include "ompi/errhandler/errhandler.h"
#include "ompi/communicator/communicator.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_STRUCT = mpi_type_struct_f
#pragma weak pmpi_type_struct = mpi_type_struct_f
#pragma weak pmpi_type_struct_ = mpi_type_struct_f
#pragma weak pmpi_type_struct__ = mpi_type_struct_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_STRUCT,
                           pmpi_type_struct,
                           pmpi_type_struct_,
                           pmpi_type_struct__,
                           pmpi_type_struct_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *array_of_types, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, array_of_types, newtype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_STRUCT = mpi_type_struct_f
#pragma weak mpi_type_struct = mpi_type_struct_f
#pragma weak mpi_type_struct_ = mpi_type_struct_f
#pragma weak mpi_type_struct__ = mpi_type_struct_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_STRUCT,
                           mpi_type_struct,
                           mpi_type_struct_,
                           mpi_type_struct__,
                           mpi_type_struct_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *array_of_types, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, array_of_types, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_TYPE_STRUCT";


void mpi_type_struct_f(MPI_Fint *count, MPI_Fint *array_of_blocklengths,
		       MPI_Fint *array_of_displacements, 
		       MPI_Fint *array_of_types, MPI_Fint *newtype,
		       MPI_Fint *ierr)
{
    MPI_Aint *c_disp_array;
    MPI_Datatype *c_type_old_array;
    MPI_Datatype c_new;
    int i, c_err;
    OMPI_ARRAY_NAME_DECL(array_of_blocklengths);

    c_type_old_array = (MPI_Datatype *) malloc(*count * (sizeof(MPI_Datatype) + 
                                        sizeof(MPI_Aint)));
    if (NULL == c_type_old_array) {
        c_err = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                       FUNC_NAME);
	*ierr = OMPI_INT_2_FINT(c_err);
        return;
    }
    c_disp_array = (MPI_Aint*) c_type_old_array + *count;

    for (i = 0; i < *count; i++) {
        c_disp_array[i] = (MPI_Aint) array_of_displacements[i];
        c_type_old_array[i] = MPI_Type_f2c(array_of_types[i]);
    }

    OMPI_ARRAY_FINT_2_INT(array_of_blocklengths, *count);

    *ierr = OMPI_INT_2_FINT(MPI_Type_struct(OMPI_FINT_2_INT(*count),
			    OMPI_ARRAY_NAME_CONVERT(array_of_blocklengths),
			    c_disp_array,
			    c_type_old_array, &c_new));

    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_of_blocklengths);
    free(c_type_old_array);

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *newtype = MPI_Type_c2f(c_new);
    }
}
