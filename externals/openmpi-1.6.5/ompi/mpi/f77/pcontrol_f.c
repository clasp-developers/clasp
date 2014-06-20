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
#pragma weak PMPI_PCONTROL = mpi_pcontrol_f
#pragma weak pmpi_pcontrol = mpi_pcontrol_f
#pragma weak pmpi_pcontrol_ = mpi_pcontrol_f
#pragma weak pmpi_pcontrol__ = mpi_pcontrol_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_PCONTROL,
                           pmpi_pcontrol,
                           pmpi_pcontrol_,
                           pmpi_pcontrol__,
                           pmpi_pcontrol_f,
                           (MPI_Fint *level),
                           (level) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_PCONTROL = mpi_pcontrol_f
#pragma weak mpi_pcontrol = mpi_pcontrol_f
#pragma weak mpi_pcontrol_ = mpi_pcontrol_f
#pragma weak mpi_pcontrol__ = mpi_pcontrol_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_PCONTROL,
                           mpi_pcontrol,
                           mpi_pcontrol_,
                           mpi_pcontrol__,
                           mpi_pcontrol_f,
                           (MPI_Fint *level),
                           (level) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_pcontrol_f(MPI_Fint *level)
{
    MPI_Pcontrol(OMPI_FINT_2_INT(*level));
}
