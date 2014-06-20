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
#pragma weak PMPI_WIN_POST = mpi_win_post_f
#pragma weak pmpi_win_post = mpi_win_post_f
#pragma weak pmpi_win_post_ = mpi_win_post_f
#pragma weak pmpi_win_post__ = mpi_win_post_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_POST,
                           pmpi_win_post,
                           pmpi_win_post_,
                           pmpi_win_post__,
                           pmpi_win_post_f,
                           (MPI_Fint *group, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (group, assert, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_POST = mpi_win_post_f
#pragma weak mpi_win_post = mpi_win_post_f
#pragma weak mpi_win_post_ = mpi_win_post_f
#pragma weak mpi_win_post__ = mpi_win_post_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_POST,
                           mpi_win_post,
                           mpi_win_post_,
                           mpi_win_post__,
                           mpi_win_post_f,
                           (MPI_Fint *group, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (group, assert, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_win_post_f(MPI_Fint *group, MPI_Fint *assert,
		    MPI_Fint *win, MPI_Fint *ierr)
{
    MPI_Win c_win = MPI_Win_f2c(*win);
    MPI_Group c_grp = MPI_Group_f2c(*group);

    *ierr = OMPI_INT_2_FINT(MPI_Win_post(c_grp, 
					 OMPI_FINT_2_INT(*assert),
					 c_win));
}
