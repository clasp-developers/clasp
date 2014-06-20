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
#pragma weak PMPI_GRAPH_NEIGHBORS = mpi_graph_neighbors_f
#pragma weak pmpi_graph_neighbors = mpi_graph_neighbors_f
#pragma weak pmpi_graph_neighbors_ = mpi_graph_neighbors_f
#pragma weak pmpi_graph_neighbors__ = mpi_graph_neighbors_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GRAPH_NEIGHBORS,
                           pmpi_graph_neighbors,
                           pmpi_graph_neighbors_,
                           pmpi_graph_neighbors__,
                           pmpi_graph_neighbors_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxneighbors, MPI_Fint *neighbors, MPI_Fint *ierr),
                           (comm, rank, maxneighbors, neighbors, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPH_NEIGHBORS = mpi_graph_neighbors_f
#pragma weak mpi_graph_neighbors = mpi_graph_neighbors_f
#pragma weak mpi_graph_neighbors_ = mpi_graph_neighbors_f
#pragma weak mpi_graph_neighbors__ = mpi_graph_neighbors_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GRAPH_NEIGHBORS,
                           mpi_graph_neighbors,
                           mpi_graph_neighbors_,
                           mpi_graph_neighbors__,
                           mpi_graph_neighbors_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxneighbors, MPI_Fint *neighbors, MPI_Fint *ierr),
                           (comm, rank, maxneighbors, neighbors, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_graph_neighbors_f(MPI_Fint *comm, MPI_Fint *rank,
			   MPI_Fint *maxneighbors, MPI_Fint *neighbors, 
			   MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    OMPI_ARRAY_NAME_DECL(neighbors);

    c_comm = MPI_Comm_f2c(*comm);
    
    OMPI_ARRAY_FINT_2_INT_ALLOC(neighbors, *maxneighbors);
    
    *ierr = OMPI_INT_2_FINT(MPI_Graph_neighbors(c_comm, 
					OMPI_FINT_2_INT(*rank),
					OMPI_FINT_2_INT(*maxneighbors),
					OMPI_ARRAY_NAME_CONVERT(neighbors)
					));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_ARRAY_INT_2_FINT(neighbors, *maxneighbors);
    }
}
