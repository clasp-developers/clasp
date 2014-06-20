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
#include "ompi/constants.h"
#include "ompi/mpi/f77/f77_strings.h"


#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_GET_NAME = mpi_win_get_name_f
#pragma weak pmpi_win_get_name = mpi_win_get_name_f
#pragma weak pmpi_win_get_name_ = mpi_win_get_name_f
#pragma weak pmpi_win_get_name__ = mpi_win_get_name_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_GET_NAME,
                            pmpi_win_get_name,
                            pmpi_win_get_name_,
                            pmpi_win_get_name__,
                            pmpi_win_get_name_f,
                            (MPI_Fint *win, char *win_name, MPI_Fint *resultlen, MPI_Fint *ierr, int name_len),
                            (win, win_name, resultlen, ierr, name_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_GET_NAME = mpi_win_get_name_f
#pragma weak mpi_win_get_name = mpi_win_get_name_f
#pragma weak mpi_win_get_name_ = mpi_win_get_name_f
#pragma weak mpi_win_get_name__ = mpi_win_get_name_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_GET_NAME,
                            mpi_win_get_name,
                            mpi_win_get_name_,
                            mpi_win_get_name__,
                            mpi_win_get_name_f,
                            (MPI_Fint *win, char *win_name, MPI_Fint *resultlen, MPI_Fint *ierr, int name_len),
                            (win, win_name, resultlen, ierr, name_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_win_get_name_f(MPI_Fint *win, char *win_name,
			MPI_Fint *resultlen, MPI_Fint *ierr, int name_len)
{
    int err, c_len;
    MPI_Win c_win = MPI_Win_f2c(*win);
    char c_name[MPI_MAX_OBJECT_NAME];

    err = MPI_Win_get_name(c_win, c_name, &c_len);
    if (MPI_SUCCESS == err) {
        ompi_fortran_string_c2f(c_name, win_name, name_len);
        *resultlen = OMPI_INT_2_FINT(c_len);
        *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
    } else {
        *ierr = OMPI_INT_2_FINT(err);
    }

}
