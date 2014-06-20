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
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_GET_POSITION = mpi_file_get_position_f
#pragma weak pmpi_file_get_position = mpi_file_get_position_f
#pragma weak pmpi_file_get_position_ = mpi_file_get_position_f
#pragma weak pmpi_file_get_position__ = mpi_file_get_position_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_POSITION,
                           pmpi_file_get_position,
                           pmpi_file_get_position_,
                           pmpi_file_get_position__,
                           pmpi_file_get_position_f,
                           (MPI_Fint *fh, MPI_Offset *offset, MPI_Fint *ierr),
                           (fh, offset, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_POSITION = mpi_file_get_position_f
#pragma weak mpi_file_get_position = mpi_file_get_position_f
#pragma weak mpi_file_get_position_ = mpi_file_get_position_f
#pragma weak mpi_file_get_position__ = mpi_file_get_position_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_POSITION,
                           mpi_file_get_position,
                           mpi_file_get_position_,
                           mpi_file_get_position__,
                           mpi_file_get_position_f,
                           (MPI_Fint *fh, MPI_Offset *offset, MPI_Fint *ierr),
                           (fh, offset, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_file_get_position_f(MPI_Fint *fh, MPI_Offset *offset, MPI_Fint *ierr)
{
    MPI_File c_fh = MPI_File_f2c(*fh);
    MPI_Offset c_offset;

    *ierr = OMPI_INT_2_FINT(MPI_File_get_position(c_fh, 
						  &c_offset));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *offset = (MPI_Offset) c_offset;
    }
}
