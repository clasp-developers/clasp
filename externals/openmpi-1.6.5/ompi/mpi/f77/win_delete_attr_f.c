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
#pragma weak PMPI_WIN_DELETE_ATTR = mpi_win_delete_attr_f
#pragma weak pmpi_win_delete_attr = mpi_win_delete_attr_f
#pragma weak pmpi_win_delete_attr_ = mpi_win_delete_attr_f
#pragma weak pmpi_win_delete_attr__ = mpi_win_delete_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_DELETE_ATTR,
                           pmpi_win_delete_attr,
                           pmpi_win_delete_attr_,
                           pmpi_win_delete_attr__,
                           pmpi_win_delete_attr_f,
                           (MPI_Fint *win, MPI_Fint *win_keyval, MPI_Fint *ierr),
                           (win, win_keyval, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_DELETE_ATTR = mpi_win_delete_attr_f
#pragma weak mpi_win_delete_attr = mpi_win_delete_attr_f
#pragma weak mpi_win_delete_attr_ = mpi_win_delete_attr_f
#pragma weak mpi_win_delete_attr__ = mpi_win_delete_attr_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_DELETE_ATTR,
                           mpi_win_delete_attr,
                           mpi_win_delete_attr_,
                           mpi_win_delete_attr__,
                           mpi_win_delete_attr_f,
                           (MPI_Fint *win, MPI_Fint *win_keyval, MPI_Fint *ierr),
                           (win, win_keyval, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif


void mpi_win_delete_attr_f(MPI_Fint *win, MPI_Fint *win_keyval, MPI_Fint *ierr)
{
    MPI_Win c_win = MPI_Win_f2c(*win);

    *ierr = OMPI_INT_2_FINT(MPI_Win_delete_attr(c_win, 
                                                OMPI_FINT_2_INT(*win_keyval)
                                                )); 
}
