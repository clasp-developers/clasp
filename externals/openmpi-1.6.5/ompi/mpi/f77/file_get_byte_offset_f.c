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
#pragma weak PMPI_FILE_GET_BYTE_OFFSET = mpi_file_get_byte_offset_f
#pragma weak pmpi_file_get_byte_offset = mpi_file_get_byte_offset_f
#pragma weak pmpi_file_get_byte_offset_ = mpi_file_get_byte_offset_f
#pragma weak pmpi_file_get_byte_offset__ = mpi_file_get_byte_offset_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_BYTE_OFFSET,
                           pmpi_file_get_byte_offset,
                           pmpi_file_get_byte_offset_,
                           pmpi_file_get_byte_offset__,
                           pmpi_file_get_byte_offset_f,
                           (MPI_Fint *fh, MPI_Offset *offset, MPI_Offset *disp, MPI_Fint *ierr),
                           (fh, offset, disp, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_BYTE_OFFSET = mpi_file_get_byte_offset_f
#pragma weak mpi_file_get_byte_offset = mpi_file_get_byte_offset_f
#pragma weak mpi_file_get_byte_offset_ = mpi_file_get_byte_offset_f
#pragma weak mpi_file_get_byte_offset__ = mpi_file_get_byte_offset_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_BYTE_OFFSET,
                           mpi_file_get_byte_offset,
                           mpi_file_get_byte_offset_,
                           mpi_file_get_byte_offset__,
                           mpi_file_get_byte_offset_f,
                           (MPI_Fint *fh, MPI_Offset *offset, MPI_Offset *disp, MPI_Fint *ierr),
                           (fh, offset, disp, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_file_get_byte_offset_f(MPI_Fint *fh, MPI_Offset *offset,
				MPI_Offset *disp, MPI_Fint *ierr)
{
    MPI_File c_fh;

    c_fh = MPI_File_f2c(*fh);

    *ierr = OMPI_INT_2_FINT(MPI_File_get_byte_offset(c_fh, 
						     (MPI_Offset) *offset,
						     disp));
}
