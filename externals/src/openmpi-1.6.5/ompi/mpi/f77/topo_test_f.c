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
#pragma weak PMPI_TOPO_TEST = mpi_topo_test_f
#pragma weak pmpi_topo_test = mpi_topo_test_f
#pragma weak pmpi_topo_test_ = mpi_topo_test_f
#pragma weak pmpi_topo_test__ = mpi_topo_test_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TOPO_TEST,
                           pmpi_topo_test,
                           pmpi_topo_test_,
                           pmpi_topo_test__,
                           pmpi_topo_test_f,
                           (MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (comm, status, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TOPO_TEST = mpi_topo_test_f
#pragma weak mpi_topo_test = mpi_topo_test_f
#pragma weak mpi_topo_test_ = mpi_topo_test_f
#pragma weak mpi_topo_test__ = mpi_topo_test_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TOPO_TEST,
                           mpi_topo_test,
                           mpi_topo_test_,
                           mpi_topo_test__,
                           mpi_topo_test_f,
                           (MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr),
                           (comm, status, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_topo_test_f(MPI_Fint *comm, MPI_Fint *status, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    OMPI_SINGLE_NAME_DECL(status);

    c_comm = MPI_Comm_f2c(*comm);
    
    *ierr = OMPI_INT_2_FINT(MPI_Topo_test(c_comm, 
					  OMPI_SINGLE_NAME_CONVERT(status)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(status);
    }
}
