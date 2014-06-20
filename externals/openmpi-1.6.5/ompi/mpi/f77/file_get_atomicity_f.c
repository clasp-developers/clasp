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
#pragma weak PMPI_FILE_GET_ATOMICITY = mpi_file_get_atomicity_f
#pragma weak pmpi_file_get_atomicity = mpi_file_get_atomicity_f
#pragma weak pmpi_file_get_atomicity_ = mpi_file_get_atomicity_f
#pragma weak pmpi_file_get_atomicity__ = mpi_file_get_atomicity_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_GET_ATOMICITY,
                           pmpi_file_get_atomicity,
                           pmpi_file_get_atomicity_,
                           pmpi_file_get_atomicity__,
                           pmpi_file_get_atomicity_f,
                           (MPI_Fint *fh, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (fh, flag, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_ATOMICITY = mpi_file_get_atomicity_f
#pragma weak mpi_file_get_atomicity = mpi_file_get_atomicity_f
#pragma weak mpi_file_get_atomicity_ = mpi_file_get_atomicity_f
#pragma weak mpi_file_get_atomicity__ = mpi_file_get_atomicity_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_GET_ATOMICITY,
                           mpi_file_get_atomicity,
                           mpi_file_get_atomicity_,
                           mpi_file_get_atomicity__,
                           mpi_file_get_atomicity_f,
                           (MPI_Fint *fh, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (fh, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_file_get_atomicity_f(MPI_Fint *fh, ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    MPI_File c_fh;
    OMPI_LOGICAL_NAME_DECL(flag);

    c_fh = MPI_File_f2c(*fh);
    *ierr = OMPI_INT_2_FINT(MPI_File_get_atomicity(c_fh,
                                                   OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_LOGICAL(flag);
    }
}
