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
#include "ompi/mpi/f77/constants.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_FILE_IWRITE_AT = mpi_file_iwrite_at_f
#pragma weak pmpi_file_iwrite_at = mpi_file_iwrite_at_f
#pragma weak pmpi_file_iwrite_at_ = mpi_file_iwrite_at_f
#pragma weak pmpi_file_iwrite_at__ = mpi_file_iwrite_at_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_FILE_IWRITE_AT,
                           pmpi_file_iwrite_at,
                           pmpi_file_iwrite_at_,
                           pmpi_file_iwrite_at__,
                           pmpi_file_iwrite_at_f,
                           (MPI_Fint *fh, MPI_Offset *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr),
                           (fh, offset, buf, count, datatype, request, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_IWRITE_AT = mpi_file_iwrite_at_f
#pragma weak mpi_file_iwrite_at = mpi_file_iwrite_at_f
#pragma weak mpi_file_iwrite_at_ = mpi_file_iwrite_at_f
#pragma weak mpi_file_iwrite_at__ = mpi_file_iwrite_at_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_FILE_IWRITE_AT,
                           mpi_file_iwrite_at,
                           mpi_file_iwrite_at_,
                           mpi_file_iwrite_at__,
                           mpi_file_iwrite_at_f,
                           (MPI_Fint *fh, MPI_Offset *offset, char *buf, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *request, MPI_Fint *ierr),
                           (fh, offset, buf, count, datatype, request, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_file_iwrite_at_f(MPI_Fint *fh, MPI_Offset *offset, char *buf,
			  MPI_Fint *count, MPI_Fint *datatype,
			  MPI_Fint *request, MPI_Fint *ierr)
{
   MPI_File c_fh = MPI_File_f2c(*fh);
   MPI_Datatype c_type = MPI_Type_f2c(*datatype);
   MPI_Request c_request;
   
   *ierr = OMPI_INT_2_FINT(MPI_File_iwrite_at(c_fh, (MPI_Offset) *offset,
                                              OMPI_F2C_BOTTOM(buf),
                                              OMPI_FINT_2_INT(*count),
                                              c_type, &c_request));
   if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
      *request = MPI_Request_c2f(c_request);
   }
}
