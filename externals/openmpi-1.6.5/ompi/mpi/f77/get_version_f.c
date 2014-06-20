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
#pragma weak PMPI_GET_VERSION = mpi_get_version_f
#pragma weak pmpi_get_version = mpi_get_version_f
#pragma weak pmpi_get_version_ = mpi_get_version_f
#pragma weak pmpi_get_version__ = mpi_get_version_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GET_VERSION,
                           pmpi_get_version,
                           pmpi_get_version_,
                           pmpi_get_version__,
                           pmpi_get_version_f,
                           (MPI_Fint *version, MPI_Fint *subversion, MPI_Fint *ierr),
                           (version, subversion, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GET_VERSION = mpi_get_version_f
#pragma weak mpi_get_version = mpi_get_version_f
#pragma weak mpi_get_version_ = mpi_get_version_f
#pragma weak mpi_get_version__ = mpi_get_version_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GET_VERSION,
                           mpi_get_version,
                           mpi_get_version_,
                           mpi_get_version__,
                           mpi_get_version_f,
                           (MPI_Fint *version, MPI_Fint *subversion, MPI_Fint *ierr),
                           (version, subversion, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_get_version_f(MPI_Fint *version, MPI_Fint *subversion, MPI_Fint *ierr)
{
    OMPI_SINGLE_NAME_DECL(version);
    OMPI_SINGLE_NAME_DECL(subversion);

    *ierr = OMPI_INT_2_FINT(MPI_Get_version(OMPI_SINGLE_NAME_CONVERT(version),
				    OMPI_SINGLE_NAME_CONVERT(subversion)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(version);
        OMPI_SINGLE_INT_2_FINT(subversion);
    }
}
