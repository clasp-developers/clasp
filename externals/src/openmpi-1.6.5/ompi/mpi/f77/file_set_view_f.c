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
#pragma weak PMPI_FILE_SET_VIEW = mpi_file_set_view_f
#pragma weak pmpi_file_set_view = mpi_file_set_view_f
#pragma weak pmpi_file_set_view_ = mpi_file_set_view_f
#pragma weak pmpi_file_set_view__ = mpi_file_set_view_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_SET_VIEW,
                           pmpi_file_set_view,
                           pmpi_file_set_view_,
                           pmpi_file_set_view__,
                           pmpi_file_set_view_f,
                           (MPI_Fint *fh, MPI_Offset *disp, MPI_Fint *etype, MPI_Fint *filetype, char *datarep, MPI_Fint *info, MPI_Fint *ierr, int datarep_len),
                           (fh, disp, etype, filetype, datarep, info, ierr, datarep_len) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_SET_VIEW = mpi_file_set_view_f
#pragma weak mpi_file_set_view = mpi_file_set_view_f
#pragma weak mpi_file_set_view_ = mpi_file_set_view_f
#pragma weak mpi_file_set_view__ = mpi_file_set_view_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_SET_VIEW,
                           mpi_file_set_view,
                           mpi_file_set_view_,
                           mpi_file_set_view__,
                           mpi_file_set_view_f,
                           (MPI_Fint *fh, MPI_Offset *disp, MPI_Fint *etype, MPI_Fint *filetype, char *datarep, MPI_Fint *info, MPI_Fint *ierr, int datarep_len),
                           (fh, disp, etype, filetype, datarep, info, ierr, datarep_len) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_file_set_view_f(MPI_Fint *fh, MPI_Offset *disp,
			 MPI_Fint *etype, MPI_Fint *filetype,
			 char *datarep, MPI_Fint *info, MPI_Fint *ierr, 
                         int datarep_len)
{
   MPI_File c_fh = MPI_File_f2c(*fh);
   MPI_Datatype c_etype = MPI_Type_f2c(*etype);
   MPI_Datatype c_filetype = MPI_Type_f2c(*filetype);
   MPI_Info c_info = MPI_Info_f2c(*info);
   char *c_datarep;
   int c_err, ret;

    /* Convert the fortran string */
    if (OMPI_SUCCESS != (ret = ompi_fortran_string_f2c(datarep, datarep_len,
                                                       &c_datarep))) {
        c_err = OMPI_ERRHANDLER_INVOKE(c_fh, ret, "MPI_FILE_SET_VIEW");
        *ierr = OMPI_INT_2_FINT(c_err);
        return;
    }

   *ierr = OMPI_INT_2_FINT(MPI_File_set_view(c_fh, (MPI_Offset) *disp,
                                             c_etype, c_filetype,
                                             c_datarep, c_info));

    free(c_datarep);
}
