/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"
#include "ompi/mpi/f77/f77_strings.h"
#include "ompi/file/file.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_DELETE = mpi_file_delete_f
#pragma weak pmpi_file_delete = mpi_file_delete_f
#pragma weak pmpi_file_delete_ = mpi_file_delete_f
#pragma weak pmpi_file_delete__ = mpi_file_delete_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_DELETE,
                           pmpi_file_delete,
                           pmpi_file_delete_,
                           pmpi_file_delete__,
                           pmpi_file_delete_f,
                           (char *filename, MPI_Fint *info, MPI_Fint *ierr, int filename_len),
                           (filename, info, ierr, filename_len)  )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_DELETE = mpi_file_delete_f
#pragma weak mpi_file_delete = mpi_file_delete_f
#pragma weak mpi_file_delete_ = mpi_file_delete_f
#pragma weak mpi_file_delete__ = mpi_file_delete_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_DELETE,
                           mpi_file_delete,
                           mpi_file_delete_,
                           mpi_file_delete__,
                           mpi_file_delete_f,
                           (char *filename, MPI_Fint *info, MPI_Fint *ierr, int filename_len),
                           (filename, info, ierr, filename_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_file_delete_f(char *filename, MPI_Fint *info, MPI_Fint *ierr, int filename_len)
{
    MPI_Info c_info;
    char *c_filename;
    int c_err, ret;

    c_info = MPI_Info_f2c(*info);

    /* Convert the fortran string */
    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(filename, filename_len,
                                                       &c_filename))) {
        c_err = OMPI_ERRHANDLER_INVOKE(MPI_FILE_NULL, ret, "MPI_FILE_DELETE");
        *ierr = OMPI_INT_2_FINT(c_err);
        return;
    }
    
    *ierr = OMPI_INT_2_FINT(MPI_File_delete(c_filename, c_info));

    free(c_filename);
}
