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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_SET_ERRHANDLER = mpi_file_set_errhandler_f
#pragma weak pmpi_file_set_errhandler = mpi_file_set_errhandler_f
#pragma weak pmpi_file_set_errhandler_ = mpi_file_set_errhandler_f
#pragma weak pmpi_file_set_errhandler__ = mpi_file_set_errhandler_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_SET_ERRHANDLER,
                           pmpi_file_set_errhandler,
                           pmpi_file_set_errhandler_,
                           pmpi_file_set_errhandler__,
                           pmpi_file_set_errhandler_f,
                           (MPI_Fint *file, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (file, errhandler, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_SET_ERRHANDLER = mpi_file_set_errhandler_f
#pragma weak mpi_file_set_errhandler = mpi_file_set_errhandler_f
#pragma weak mpi_file_set_errhandler_ = mpi_file_set_errhandler_f
#pragma weak mpi_file_set_errhandler__ = mpi_file_set_errhandler_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_SET_ERRHANDLER,
                           mpi_file_set_errhandler,
                           mpi_file_set_errhandler_,
                           mpi_file_set_errhandler__,
                           mpi_file_set_errhandler_f,
                           (MPI_Fint *file, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (file, errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_file_set_errhandler_f(MPI_Fint *fh, MPI_Fint *errhandler,
			       MPI_Fint *ierr)
{
    MPI_File c_fh = MPI_File_f2c(*fh);
    MPI_Errhandler c_err = MPI_Errhandler_f2c(*errhandler);

    *ierr = OMPI_INT_2_FINT(MPI_File_set_errhandler(c_fh, c_err));
}
