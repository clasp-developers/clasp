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
 * Copyright (c) 2007      Cisco Systems, Inc.   All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_CREATE = mpi_win_create_f
#pragma weak pmpi_win_create = mpi_win_create_f
#pragma weak pmpi_win_create_ = mpi_win_create_f
#pragma weak pmpi_win_create__ = mpi_win_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_CREATE,
                           pmpi_win_create,
                           pmpi_win_create_,
                           pmpi_win_create__,
                           pmpi_win_create_f,
                           (char *base, MPI_Aint *size, MPI_Fint *disp_unit, MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (base, size, disp_unit, info, comm, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_CREATE = mpi_win_create_f
#pragma weak mpi_win_create = mpi_win_create_f
#pragma weak mpi_win_create_ = mpi_win_create_f
#pragma weak mpi_win_create__ = mpi_win_create_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_CREATE,
                           mpi_win_create,
                           mpi_win_create_,
                           mpi_win_create__,
                           mpi_win_create_f,
                           (char *base, MPI_Aint *size, MPI_Fint *disp_unit, MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (base, size, disp_unit, info, comm, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_win_create_f(char *base, MPI_Aint *size, MPI_Fint *disp_unit,
		      MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win,
		      MPI_Fint *ierr)
{
    MPI_Win c_win;
    MPI_Info c_info;
    MPI_Comm c_comm;

    c_comm = MPI_Comm_f2c(*comm);
    c_info = MPI_Info_f2c(*info);

    *ierr = OMPI_INT_2_FINT(MPI_Win_create(base, *size,
					   OMPI_FINT_2_INT(*disp_unit),
					   c_info, c_comm, &c_win));
   if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
       *win = MPI_Win_c2f(c_win);
   }
}
