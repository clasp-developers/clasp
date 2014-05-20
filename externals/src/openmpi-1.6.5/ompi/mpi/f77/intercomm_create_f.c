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
#pragma weak PMPI_INTERCOMM_CREATE = mpi_intercomm_create_f
#pragma weak pmpi_intercomm_create = mpi_intercomm_create_f
#pragma weak pmpi_intercomm_create_ = mpi_intercomm_create_f
#pragma weak pmpi_intercomm_create__ = mpi_intercomm_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_INTERCOMM_CREATE,
                           pmpi_intercomm_create,
                           pmpi_intercomm_create_,
                           pmpi_intercomm_create__,
                           pmpi_intercomm_create_f,
                           (MPI_Fint *local_comm, MPI_Fint *local_leader, MPI_Fint *bridge_comm, MPI_Fint *remote_leader, MPI_Fint *tag, MPI_Fint *newintercomm, MPI_Fint *ierr),
                           (local_comm, local_leader, bridge_comm, remote_leader, tag, newintercomm, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INTERCOMM_CREATE = mpi_intercomm_create_f
#pragma weak mpi_intercomm_create = mpi_intercomm_create_f
#pragma weak mpi_intercomm_create_ = mpi_intercomm_create_f
#pragma weak mpi_intercomm_create__ = mpi_intercomm_create_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_INTERCOMM_CREATE,
                           mpi_intercomm_create,
                           mpi_intercomm_create_,
                           mpi_intercomm_create__,
                           mpi_intercomm_create_f,
                           (MPI_Fint *local_comm, MPI_Fint *local_leader, MPI_Fint *bridge_comm, MPI_Fint *remote_leader, MPI_Fint *tag, MPI_Fint *newintercomm, MPI_Fint *ierr),
                           (local_comm, local_leader, bridge_comm, remote_leader, tag, newintercomm, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_intercomm_create_f(MPI_Fint *local_comm, MPI_Fint *local_leader,
			    MPI_Fint *bridge_comm, 
                            MPI_Fint *remote_leader, MPI_Fint *tag,
			    MPI_Fint *newintercomm, 
                            MPI_Fint *ierr)
{
    MPI_Comm c_newcomm;
    MPI_Comm c_local_comm = MPI_Comm_f2c (*local_comm );
    MPI_Comm c_bridge_comm = MPI_Comm_f2c (*bridge_comm);

    *ierr = OMPI_INT_2_FINT(MPI_Intercomm_create(c_local_comm,
					 OMPI_FINT_2_INT(*local_leader), 
					 c_bridge_comm,
					 OMPI_FINT_2_INT(*remote_leader),
					 OMPI_FINT_2_INT(*tag),
					 &c_newcomm));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        *newintercomm = MPI_Comm_c2f (c_newcomm);
    }
}
