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
#pragma weak PMPI_FILE_GET_INFO = mpi_file_get_info_f
#pragma weak pmpi_file_get_info = mpi_file_get_info_f
#pragma weak pmpi_file_get_info_ = mpi_file_get_info_f
#pragma weak pmpi_file_get_info__ = mpi_file_get_info_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_INFO,
                           pmpi_file_get_info,
                           pmpi_file_get_info_,
                           pmpi_file_get_info__,
                           pmpi_file_get_info_f,
                           (MPI_Fint *fh, MPI_Fint *info_used, MPI_Fint *ierr),
                           (fh, info_used, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_INFO = mpi_file_get_info_f
#pragma weak mpi_file_get_info = mpi_file_get_info_f
#pragma weak mpi_file_get_info_ = mpi_file_get_info_f
#pragma weak mpi_file_get_info__ = mpi_file_get_info_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_INFO,
                           mpi_file_get_info,
                           mpi_file_get_info_,
                           mpi_file_get_info__,
                           mpi_file_get_info_f,
                           (MPI_Fint *fh, MPI_Fint *info_used, MPI_Fint *ierr),
                           (fh, info_used, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_file_get_info_f(MPI_Fint *fh, MPI_Fint *info_used, MPI_Fint *ierr)
{
    MPI_File c_fh = MPI_File_f2c(*fh);
    MPI_Info c_info;

    *ierr = OMPI_INT_2_FINT(MPI_File_get_info(c_fh, &c_info));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *info_used = MPI_Info_c2f(c_info);
    }
}
