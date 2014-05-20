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
#pragma weak PMPI_GRAPH_NEIGHBORS_COUNT = mpi_graph_neighbors_count_f
#pragma weak pmpi_graph_neighbors_count = mpi_graph_neighbors_count_f
#pragma weak pmpi_graph_neighbors_count_ = mpi_graph_neighbors_count_f
#pragma weak pmpi_graph_neighbors_count__ = mpi_graph_neighbors_count_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GRAPH_NEIGHBORS_COUNT,
                           pmpi_graph_neighbors_count,
                           pmpi_graph_neighbors_count_,
                           pmpi_graph_neighbors_count__,
                           pmpi_graph_neighbors_count_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *nneighbors, MPI_Fint *ierr),
                           (comm, rank, nneighbors, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPH_NEIGHBORS_COUNT = mpi_graph_neighbors_count_f
#pragma weak mpi_graph_neighbors_count = mpi_graph_neighbors_count_f
#pragma weak mpi_graph_neighbors_count_ = mpi_graph_neighbors_count_f
#pragma weak mpi_graph_neighbors_count__ = mpi_graph_neighbors_count_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GRAPH_NEIGHBORS_COUNT,
                           mpi_graph_neighbors_count,
                           mpi_graph_neighbors_count_,
                           mpi_graph_neighbors_count__,
                           mpi_graph_neighbors_count_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *nneighbors, MPI_Fint *ierr),
                           (comm, rank, nneighbors, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_graph_neighbors_count_f(MPI_Fint *comm, MPI_Fint *rank,
				 MPI_Fint *nneighbors, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    OMPI_SINGLE_NAME_DECL(nneighbors);

    c_comm = MPI_Comm_f2c(*comm);
    
    *ierr = OMPI_INT_2_FINT(MPI_Graph_neighbors_count(c_comm,
				      OMPI_FINT_2_INT(*rank),
				      OMPI_SINGLE_NAME_CONVERT(nneighbors)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_SINGLE_INT_2_FINT(nneighbors);
    }
}
