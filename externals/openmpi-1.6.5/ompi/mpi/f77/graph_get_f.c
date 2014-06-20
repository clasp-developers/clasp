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
#pragma weak PMPI_GRAPH_GET = mpi_graph_get_f
#pragma weak pmpi_graph_get = mpi_graph_get_f
#pragma weak pmpi_graph_get_ = mpi_graph_get_f
#pragma weak pmpi_graph_get__ = mpi_graph_get_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_GRAPH_GET,
                           pmpi_graph_get,
                           pmpi_graph_get_,
                           pmpi_graph_get__,
                           pmpi_graph_get_f,
                           (MPI_Fint *comm, MPI_Fint *maxindex, MPI_Fint *maxedges, MPI_Fint *index, MPI_Fint *edges, MPI_Fint *ierr),
                           (comm, maxindex, maxedges, index, edges, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_GRAPH_GET = mpi_graph_get_f
#pragma weak mpi_graph_get = mpi_graph_get_f
#pragma weak mpi_graph_get_ = mpi_graph_get_f
#pragma weak mpi_graph_get__ = mpi_graph_get_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_GRAPH_GET,
                           mpi_graph_get,
                           mpi_graph_get_,
                           mpi_graph_get__,
                           mpi_graph_get_f,
                           (MPI_Fint *comm, MPI_Fint *maxindex, MPI_Fint *maxedges, MPI_Fint *index, MPI_Fint *edges, MPI_Fint *ierr),
                           (comm, maxindex, maxedges, index, edges, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/f77/profile/defines.h"
#endif

void mpi_graph_get_f(MPI_Fint *comm, MPI_Fint *maxindex, 
		     MPI_Fint *maxedges, MPI_Fint *index, 
		     MPI_Fint *edges, MPI_Fint *ierr)
{
    MPI_Comm c_comm;
    OMPI_ARRAY_NAME_DECL(index);
    OMPI_ARRAY_NAME_DECL(edges);

    c_comm = MPI_Comm_f2c(*comm);
    OMPI_ARRAY_FINT_2_INT_ALLOC(index, *maxindex);
    OMPI_ARRAY_FINT_2_INT_ALLOC(edges, *maxedges);

    *ierr = OMPI_INT_2_FINT(MPI_Graph_get(c_comm, 
					  OMPI_FINT_2_INT(*maxindex),
					  OMPI_FINT_2_INT(*maxedges),
					  OMPI_ARRAY_NAME_CONVERT(index),
					  OMPI_ARRAY_NAME_CONVERT(edges)));
    if (MPI_SUCCESS == OMPI_FINT_2_INT(*ierr)) {
        OMPI_ARRAY_INT_2_FINT(index, *maxindex);
        OMPI_ARRAY_INT_2_FINT(edges, *maxedges);
    }
}
