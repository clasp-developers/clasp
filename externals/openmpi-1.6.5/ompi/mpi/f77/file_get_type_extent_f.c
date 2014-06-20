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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_GET_TYPE_EXTENT = mpi_file_get_type_extent_f
#pragma weak pmpi_file_get_type_extent = mpi_file_get_type_extent_f
#pragma weak pmpi_file_get_type_extent_ = mpi_file_get_type_extent_f
#pragma weak pmpi_file_get_type_extent__ = mpi_file_get_type_extent_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_TYPE_EXTENT,
                           pmpi_file_get_type_extent,
                           pmpi_file_get_type_extent_,
                           pmpi_file_get_type_extent__,
                           pmpi_file_get_type_extent_f,
                           (MPI_Fint *fh, MPI_Fint *datatype, MPI_Aint *extent, MPI_Fint *ierr),
                           (fh, datatype, extent, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_TYPE_EXTENT = mpi_file_get_type_extent_f
#pragma weak mpi_file_get_type_extent = mpi_file_get_type_extent_f
#pragma weak mpi_file_get_type_extent_ = mpi_file_get_type_extent_f
#pragma weak mpi_file_get_type_extent__ = mpi_file_get_type_extent_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_TYPE_EXTENT,
                           mpi_file_get_type_extent,
                           mpi_file_get_type_extent_,
                           mpi_file_get_type_extent__,
                           mpi_file_get_type_extent_f,
                           (MPI_Fint *fh, MPI_Fint *datatype, MPI_Aint *extent, MPI_Fint *ierr),
                           (fh, datatype, extent, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_file_get_type_extent_f(MPI_Fint *fh, MPI_Fint *datatype,
				MPI_Aint *extent, MPI_Fint *ierr)
{
    MPI_File c_fh = MPI_File_f2c(*fh);
    MPI_Datatype c_type;

    c_type = MPI_Type_f2c(*datatype);

    *ierr = OMPI_INT_2_FINT(MPI_File_get_type_extent(c_fh, c_type, extent));
}
