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
#pragma weak PMPI_BUFFER_ATTACH = mpi_buffer_attach_f
#pragma weak pmpi_buffer_attach = mpi_buffer_attach_f
#pragma weak pmpi_buffer_attach_ = mpi_buffer_attach_f
#pragma weak pmpi_buffer_attach__ = mpi_buffer_attach_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_BUFFER_ATTACH,
                           pmpi_buffer_attach,
                           pmpi_buffer_attach_,
                           pmpi_buffer_attach__,
                           pmpi_buffer_attach_f,
                           (char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (buffer, size, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BUFFER_ATTACH = mpi_buffer_attach_f
#pragma weak mpi_buffer_attach = mpi_buffer_attach_f
#pragma weak mpi_buffer_attach_ = mpi_buffer_attach_f
#pragma weak mpi_buffer_attach__ = mpi_buffer_attach_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_BUFFER_ATTACH,
                           mpi_buffer_attach,
                           mpi_buffer_attach_,
                           mpi_buffer_attach__,
                           mpi_buffer_attach_f,
                           (char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (buffer, size, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_buffer_attach_f(char *buffer, MPI_Fint *size, MPI_Fint *ierr)
{
  *ierr = OMPI_INT_2_FINT(MPI_Buffer_attach(buffer, 
					    OMPI_FINT_2_INT(*size)));
}
