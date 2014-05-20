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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/f77/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_BUFFER_DETACH = mpi_buffer_detach_f
#pragma weak pmpi_buffer_detach = mpi_buffer_detach_f
#pragma weak pmpi_buffer_detach_ = mpi_buffer_detach_f
#pragma weak pmpi_buffer_detach__ = mpi_buffer_detach_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_BUFFER_DETACH,
                           pmpi_buffer_detach,
                           pmpi_buffer_detach_,
                           pmpi_buffer_detach__,
                           pmpi_buffer_detach_f,
                           (char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (buffer, size, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BUFFER_DETACH = mpi_buffer_detach_f
#pragma weak mpi_buffer_detach = mpi_buffer_detach_f
#pragma weak mpi_buffer_detach_ = mpi_buffer_detach_f
#pragma weak mpi_buffer_detach__ = mpi_buffer_detach_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_BUFFER_DETACH,
                           mpi_buffer_detach,
                           mpi_buffer_detach_,
                           mpi_buffer_detach__,
                           mpi_buffer_detach_f,
                           (char *buffer, MPI_Fint *size, MPI_Fint *ierr),
                           (buffer, size, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_buffer_detach_f(char *buffer, MPI_Fint *size, MPI_Fint *ierr)
{

    /* 
     * It does not make sense in fortran to return a pointer
     * here as the user may get a behavior that is unexpected.
     * Therefore, we use a dummy variable and leave the value
     * handed in alone.
     */
    void *dummy;
    OMPI_SINGLE_NAME_DECL(size);
    *ierr = OMPI_INT_2_FINT(MPI_Buffer_detach(&dummy, 
					      OMPI_SINGLE_NAME_CONVERT(size)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(size);
    }
}
