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
#pragma weak PMPI_INTERCOMM_MERGE = mpi_intercomm_merge_f
#pragma weak pmpi_intercomm_merge = mpi_intercomm_merge_f
#pragma weak pmpi_intercomm_merge_ = mpi_intercomm_merge_f
#pragma weak pmpi_intercomm_merge__ = mpi_intercomm_merge_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INTERCOMM_MERGE,
                           pmpi_intercomm_merge,
                           pmpi_intercomm_merge_,
                           pmpi_intercomm_merge__,
                           pmpi_intercomm_merge_f,
                           (MPI_Fint *intercomm, ompi_fortran_logical_t *high, MPI_Fint *newintercomm, MPI_Fint *ierr),
                           (intercomm, high, newintercomm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INTERCOMM_MERGE = mpi_intercomm_merge_f
#pragma weak mpi_intercomm_merge = mpi_intercomm_merge_f
#pragma weak mpi_intercomm_merge_ = mpi_intercomm_merge_f
#pragma weak mpi_intercomm_merge__ = mpi_intercomm_merge_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INTERCOMM_MERGE,
                           mpi_intercomm_merge,
                           mpi_intercomm_merge_,
                           mpi_intercomm_merge__,
                           mpi_intercomm_merge_f,
                           (MPI_Fint *intercomm, ompi_fortran_logical_t *high, MPI_Fint *newintercomm, MPI_Fint *ierr),
                           (intercomm, high, newintercomm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_intercomm_merge_f(MPI_Fint *intercomm, ompi_fortran_logical_t *high,
                           MPI_Fint *newintracomm, 
                           MPI_Fint *ierr)
{
    MPI_Comm c_newcomm;
    MPI_Comm c_intercomm = MPI_Comm_f2c(*intercomm);

    *ierr = MPI_Intercomm_merge (c_intercomm, OMPI_LOGICAL_2_INT(*high),
                                 &c_newcomm);
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *newintracomm = MPI_Comm_c2f (c_newcomm);
    }
}
