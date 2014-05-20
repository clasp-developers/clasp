/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/constants.h"
#include "ompi/mpi/f77/f77_strings.h"
#include "ompi/communicator/communicator.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ADD_ERROR_STRING = mpi_add_error_string_f
#pragma weak pmpi_add_error_string = mpi_add_error_string_f
#pragma weak pmpi_add_error_string_ = mpi_add_error_string_f
#pragma weak pmpi_add_error_string__ = mpi_add_error_string_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ADD_ERROR_STRING,
                           pmpi_add_error_string,
                           pmpi_add_error_string_,
                           pmpi_add_error_string__,
                           pmpi_add_error_string_f,
                           (MPI_Fint *errorcode, char *string, MPI_Fint *ierr,int l),
                           (errorcode, string, ierr, l) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ADD_ERROR_STRING = mpi_add_error_string_f
#pragma weak mpi_add_error_string = mpi_add_error_string_f
#pragma weak mpi_add_error_string_ = mpi_add_error_string_f
#pragma weak mpi_add_error_string__ = mpi_add_error_string_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ADD_ERROR_STRING,
                           mpi_add_error_string,
                           mpi_add_error_string_,
                           mpi_add_error_string__,
                           mpi_add_error_string_f,
                           (MPI_Fint *errorcode, char *string, MPI_Fint *ierr, int l),
                           (errorcode, string, ierr, l) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_add_error_string_f(MPI_Fint *errorcode, char *string,  
			    MPI_Fint *ierr, int len)
{
    char *c_string;
    int c_err;

    if (len > MPI_MAX_ERROR_STRING) {
            c_err = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
					   "MPI_ADD_ERROR_STRING");
	    *ierr = OMPI_INT_2_FINT(c_err);
            return;
    }

    ompi_fortran_string_f2c(string, len, &c_string);
    *ierr = OMPI_INT_2_FINT(MPI_Add_error_string(OMPI_FINT_2_INT(*errorcode),
						 c_string));
    free(c_string);
}
