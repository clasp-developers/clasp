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
#pragma weak PMPI_TYPE_GET_CONTENTS = mpi_type_get_contents_f
#pragma weak pmpi_type_get_contents = mpi_type_get_contents_f
#pragma weak pmpi_type_get_contents_ = mpi_type_get_contents_f
#pragma weak pmpi_type_get_contents__ = mpi_type_get_contents_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_CONTENTS,
                           pmpi_type_get_contents,
                           pmpi_type_get_contents_,
                           pmpi_type_get_contents__,
                           pmpi_type_get_contents_f,
                           (MPI_Fint *mtype, MPI_Fint *max_integers, MPI_Fint *max_addresses, MPI_Fint *max_datatypes, MPI_Fint *array_of_integers, MPI_Aint *array_of_addresses, MPI_Fint *array_of_datatypes, MPI_Fint *ierr),
                           (mtype, max_integers, max_addresses, max_datatypes, array_of_integers, array_of_addresses, array_of_datatypes, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_CONTENTS = mpi_type_get_contents_f
#pragma weak mpi_type_get_contents = mpi_type_get_contents_f
#pragma weak mpi_type_get_contents_ = mpi_type_get_contents_f
#pragma weak mpi_type_get_contents__ = mpi_type_get_contents_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_CONTENTS,
                           mpi_type_get_contents,
                           mpi_type_get_contents_,
                           mpi_type_get_contents__,
                           mpi_type_get_contents_f,
                           (MPI_Fint *mtype, MPI_Fint *max_integers, MPI_Fint *max_addresses, MPI_Fint *max_datatypes, MPI_Fint *array_of_integers, MPI_Aint *array_of_addresses, MPI_Fint *array_of_datatypes, MPI_Fint *ierr),
                           (mtype, max_integers, max_addresses, max_datatypes, array_of_integers, array_of_addresses, array_of_datatypes, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_TYPE_GET_CONTENTS";


void mpi_type_get_contents_f(MPI_Fint *mtype, MPI_Fint *max_integers,
			     MPI_Fint *max_addresses, MPI_Fint *max_datatypes,
			     MPI_Fint *array_of_integers, 
			     MPI_Aint *array_of_addresses, 
			     MPI_Fint *array_of_datatypes, MPI_Fint *ierr)
{
    MPI_Aint *c_address_array = NULL;
    MPI_Datatype *c_datatype_array = NULL;
    MPI_Datatype c_mtype = MPI_Type_f2c(*mtype);
    int i;
    OMPI_ARRAY_NAME_DECL(array_of_integers);

    if (*max_datatypes) {
        c_datatype_array = (MPI_Datatype *) malloc(*max_datatypes * sizeof(MPI_Datatype));
        if (NULL == c_datatype_array) {
            *ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                           FUNC_NAME);
            return;
        }
    }

    if (*max_addresses) {
        c_address_array = (MPI_Aint *) malloc(*max_addresses * sizeof(MPI_Aint));
        if (NULL == c_address_array) {
            if (NULL != c_datatype_array) {
              free(c_datatype_array);
            }

            *ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                           FUNC_NAME);
            return;
        }
    }

    OMPI_ARRAY_FINT_2_INT(array_of_integers, *max_integers);

    *ierr = OMPI_INT_2_FINT(MPI_Type_get_contents(c_mtype, 
				  OMPI_FINT_2_INT(*max_integers), 
				  OMPI_FINT_2_INT(*max_addresses),
				  OMPI_FINT_2_INT(*max_datatypes),
				  OMPI_ARRAY_NAME_CONVERT(array_of_integers), 
                                  c_address_array, c_datatype_array));

    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        for (i = 0; i < *max_addresses; i++) {
            array_of_addresses[i] = c_address_array[i];
        }
        for (i = 0; i < *max_datatypes; i++) {
          array_of_datatypes[i] = MPI_Type_c2f(c_datatype_array[i]);
        }
    }
    free(c_address_array);
    free(c_datatype_array);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_of_integers);
}
