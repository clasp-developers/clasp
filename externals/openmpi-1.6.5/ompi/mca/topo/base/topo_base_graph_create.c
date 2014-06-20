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
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/topo/topo.h"

/*
 *
 * function - makes a new communicator to which topology information
 *            has been attached
 *
 * @param comm_old input communicator without topology (handle)
 * @param nnodes number of nodes in graph (integer)
 * @param index array of integers describing node degrees (see below)
 * @param edges array of integers describing graph edges (see below)
 * @param reorder ranking may be reordered (true) or not (false) (logical)
 * @param comm_graph communicator with graph topology added (handle)
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_OUT_OF_RESOURCE
 */

int mca_topo_base_graph_create (mca_topo_base_comm_t *topo_data,
                                int *proc_count,
                                ompi_proc_t **proc_pointers,
                                int *new_rank,
                                int nnodes,
                                int *index,
                                int *edges,
                                bool reorder)
{
    int  nedges;
    int  i;
    int  *p;

    /* check if the number of nodes is more than the number of procs */

    if (nnodes > *proc_count) {
        return MPI_ERR_DIMS;
    }

    /* Create and error check the topology information */

    nedges = topo_data->mtc_dims_or_index[nnodes-1];

    /* Check if there are any negative values on the edges */
    
    p = topo_data->mtc_periods_or_edges;

    for (i = 0; i < nedges; ++i, ++p) {
         if (*p < 0 || *p >= nnodes) {
            return MPI_ERR_TOPOLOGY;
         }
    }

    /* if the graph does not have to be trimmed, then nothing has to change */
    if (nnodes < *proc_count) {
        *proc_count = nnodes;
    }

    /* check if this rank makes the cut. if it does not return -1 */
    if (*new_rank > (nnodes-1)) {
        /* sorry but in our scheme, you are out */
        *new_rank = MPI_UNDEFINED;
        return MPI_SUCCESS;
    }

    return(MPI_SUCCESS);
}
