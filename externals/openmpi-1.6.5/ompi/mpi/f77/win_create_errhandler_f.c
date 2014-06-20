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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
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
#pragma weak PMPI_WIN_CREATE_ERRHANDLER = mpi_win_create_errhandler_f
#pragma weak pmpi_win_create_errhandler = mpi_win_create_errhandler_f
#pragma weak pmpi_win_create_errhandler_ = mpi_win_create_errhandler_f
#pragma weak pmpi_win_create_errhandler__ = mpi_win_create_errhandler_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_CREATE_ERRHANDLER,
                           pmpi_win_create_errhandler,
                           pmpi_win_create_errhandler_,
                           pmpi_win_create_errhandler__,
                           pmpi_win_create_errhandler_f,
                           (ompi_errhandler_fortran_handler_fn_t* function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_CREATE_ERRHANDLER = mpi_win_create_errhandler_f
#pragma weak mpi_win_create_errhandler = mpi_win_create_errhandler_f
#pragma weak mpi_win_create_errhandler_ = mpi_win_create_errhandler_f
#pragma weak mpi_win_create_errhandler__ = mpi_win_create_errhandler_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_CREATE_ERRHANDLER,
                           mpi_win_create_errhandler,
                           mpi_win_create_errhandler_,
                           mpi_win_create_errhandler__,
                           mpi_win_create_errhandler_f,
                           (ompi_errhandler_fortran_handler_fn_t* function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_WIN_CREATE_ERRHANDLER";


void mpi_win_create_errhandler_f(ompi_errhandler_fortran_handler_fn_t* function, 
				 MPI_Fint *errhandler, MPI_Fint *ierr)
{
    MPI_Errhandler c_errhandler = 
        ompi_errhandler_create(OMPI_ERRHANDLER_TYPE_WIN,
                               (ompi_errhandler_generic_handler_fn_t*) function,
                               OMPI_ERRHANDLER_LANG_FORTRAN);
    if (MPI_ERRHANDLER_NULL != c_errhandler) {
        *errhandler = MPI_Errhandler_c2f(c_errhandler);
        *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
    } else {
        *ierr = OMPI_INT_2_FINT(MPI_ERR_INTERN);
        OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INTERN, FUNC_NAME);
    }
}
