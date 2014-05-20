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
#pragma weak PMPI_WIN_UNLOCK = mpi_win_unlock_f
#pragma weak pmpi_win_unlock = mpi_win_unlock_f
#pragma weak pmpi_win_unlock_ = mpi_win_unlock_f
#pragma weak pmpi_win_unlock__ = mpi_win_unlock_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_UNLOCK,
                           pmpi_win_unlock,
                           pmpi_win_unlock_,
                           pmpi_win_unlock__,
                           pmpi_win_unlock_f,
                           (MPI_Fint *rank, MPI_Fint *win, MPI_Fint *ierr),
                           (rank, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_UNLOCK = mpi_win_unlock_f
#pragma weak mpi_win_unlock = mpi_win_unlock_f
#pragma weak mpi_win_unlock_ = mpi_win_unlock_f
#pragma weak mpi_win_unlock__ = mpi_win_unlock_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_UNLOCK,
                           mpi_win_unlock,
                           mpi_win_unlock_,
                           mpi_win_unlock__,
                           mpi_win_unlock_f,
                           (MPI_Fint *rank, MPI_Fint *win, MPI_Fint *ierr),
                           (rank, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_win_unlock_f(MPI_Fint *rank, MPI_Fint *win, MPI_Fint *ierr)
{
    MPI_Win c_win = MPI_Win_f2c(*win);

    *ierr = OMPI_INT_2_FINT(MPI_Win_unlock(OMPI_FINT_2_INT(*rank),
					   c_win));
}
