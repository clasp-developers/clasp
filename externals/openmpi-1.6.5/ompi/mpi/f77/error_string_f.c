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
#include "ompi/mpi/f77/f77_strings.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_ERROR_STRING = mpi_error_string_f
#pragma weak pmpi_error_string = mpi_error_string_f
#pragma weak pmpi_error_string_ = mpi_error_string_f
#pragma weak pmpi_error_string__ = mpi_error_string_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_ERROR_STRING,
                            pmpi_error_string,
                            pmpi_error_string_,
                            pmpi_error_string__,
                            pmpi_error_string_f,
                            (MPI_Fint *errorcode, char *string, MPI_Fint *resultlen, MPI_Fint *ierr, int string_len),
                            (errorcode, string, resultlen, ierr, string_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERROR_STRING = mpi_error_string_f
#pragma weak mpi_error_string = mpi_error_string_f
#pragma weak mpi_error_string_ = mpi_error_string_f
#pragma weak mpi_error_string__ = mpi_error_string_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_ERROR_STRING,
                            mpi_error_string,
                            mpi_error_string_,
                            mpi_error_string__,
                            mpi_error_string_f,
                            (MPI_Fint *errorcode, char *string, MPI_Fint *resultlen, MPI_Fint *ierr, int string_len),
                            (errorcode, string, resultlen, ierr, string_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_ERROR_STRING";

/* Note that the string_len parameter is silently added by the Fortran
   compiler, and will be filled in with the actual length of the
   character array from the caller.  Hence, it's the max length of the
   string that we can use. */

void mpi_error_string_f(MPI_Fint *errorcode, char *string,
			MPI_Fint *resultlen, MPI_Fint *ierr, int string_len)
{
    int c_err, ret;
    char c_string[MPI_MAX_ERROR_STRING + 1];
    OMPI_SINGLE_NAME_DECL(resultlen);

    *ierr = 
	OMPI_INT_2_FINT(MPI_Error_string(OMPI_FINT_2_INT(*errorcode),
					 c_string,
					 OMPI_SINGLE_NAME_CONVERT(resultlen)
					 ));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(resultlen);
        if (OMPI_SUCCESS != (ret = ompi_fortran_string_c2f(c_string, string,
                                                           string_len))) {
            c_err = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret, FUNC_NAME);
            *ierr = OMPI_INT_2_FINT(c_err);
        }
    }
}
