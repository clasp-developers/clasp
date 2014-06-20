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
 * Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mpi/f77/f77_strings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_GET_PROCESSOR_NAME = mpi_get_processor_name_f
#pragma weak pmpi_get_processor_name = mpi_get_processor_name_f
#pragma weak pmpi_get_processor_name_ = mpi_get_processor_name_f
#pragma weak pmpi_get_processor_name__ = mpi_get_processor_name_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GET_PROCESSOR_NAME,
                            pmpi_get_processor_name,
                            pmpi_get_processor_name_,
                            pmpi_get_processor_name__,
                            pmpi_get_processor_name_f,
                            (char *name, MPI_Fint *resultlen, MPI_Fint *ierr, int name_len),
                            (name, resultlen, ierr, name_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_PROCESSOR_NAME = mpi_get_processor_name_f
#pragma weak mpi_get_processor_name = mpi_get_processor_name_f
#pragma weak mpi_get_processor_name_ = mpi_get_processor_name_f
#pragma weak mpi_get_processor_name__ = mpi_get_processor_name_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GET_PROCESSOR_NAME,
                            mpi_get_processor_name,
                            mpi_get_processor_name_,
                            mpi_get_processor_name__,
                            mpi_get_processor_name_f,
                            (char *name, MPI_Fint *resultlen, MPI_Fint *ierr, int name_len),
                            (name, resultlen, ierr, name_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_GET_PROCESSOR_NAME";

/* Note that the name_len parameter is silently added by the Fortran
   compiler, and will be filled in with the actual length of the
   character array from the caller.  Hence, it's the max length of the
   string that we can use. */

void mpi_get_processor_name_f(char *name, MPI_Fint *resultlen, MPI_Fint *ierr,
                              int name_len)
{
    int c_err, ret;
    char c_name[MPI_MAX_PROCESSOR_NAME];
    OMPI_SINGLE_NAME_DECL(resultlen);

    *ierr = OMPI_INT_2_FINT(MPI_Get_processor_name(c_name, 
				   OMPI_SINGLE_NAME_CONVERT(resultlen)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(resultlen);

        /* Use the full length of the Fortran string, not *resultlen.
           See comment in ompi/mpi/f77/strings.c. */
        if (OMPI_SUCCESS != (ret = ompi_fortran_string_c2f(c_name, name,
                                                           name_len))) {
            c_err = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, ret, FUNC_NAME);
            *ierr = OMPI_INT_2_FINT(c_err);
            return;
        }
    }
}
