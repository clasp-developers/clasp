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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/communicator/communicator.h"

/*
 * function - Returns the shifted source and destination ranks, given a
 *            shift direction and amount
 *
 * @param comm communicator with cartesian structure (handle)
 * @param direction coordinate directionension of shift (integer)
 * @param disp displacement (> 0: upwards shift, < 0: downwards shift) (integer)
 * @param rank_source rank of source process (integer)
 * @param rank_dest rank of destination process (integer)
 *
 * The 'direction' argument is in the range '[0,n-1]' for an n-directionensional
 * Cartesian mesh.
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_TOPOLOGY
 * @retval MPI_ERR_DIMS
 * @retval MPI_ERR_COMM
 * @retval MPI_ERR_ARG
 */                  
int mca_topo_base_cart_shift (ompi_communicator_t* comm,
                          int direction,
                          int disp,
                          int *rank_source,
                          int *rank_dest){
    int factor;
    int thisdirection = 0;
    int thisperiod = 0;
    int ord;
    int srcord;
    int destord;
    int i;
    int *d, *q;

   /*
    * Handle the trivial case.
    */
    ord = ompi_comm_rank(comm);

    if (disp == 0) {
        *rank_dest = *rank_source = ord;
        return MPI_SUCCESS;
    }
   /*
    * Compute the rank factor and ordinate.
    */
    factor = ompi_comm_size(comm);
    d = comm->c_topo_comm->mtc_dims_or_index;
    q = comm->c_topo_comm->mtc_periods_or_edges;
    for (i = 0; (i < comm->c_topo_comm->mtc_ndims_or_nnodes) && (i <= direction); ++i, ++d, ++q) {
        thisdirection = *d;
        thisperiod = *q;

        ord %= factor;
        factor /= thisdirection;
     }

    ord /= factor;
    /*
     * Check the displacement value and compute the new ranks.
     */
    *rank_source = *rank_dest = MPI_UNDEFINED;

    srcord = ord - disp;
    destord = ord + disp;
    if ( ((destord < 0) || (destord >= thisdirection)) && (!thisperiod) ) {
         *rank_dest = MPI_PROC_NULL;
    } else {
       destord %= thisdirection;
       if (destord < 0) destord += thisdirection;
       *rank_dest = ompi_comm_rank(comm);
       *rank_dest += ((destord - ord) * factor);
    }
    if ( ((srcord < 0) || (srcord >= thisdirection)) && (!thisperiod) ) {
         *rank_source = MPI_PROC_NULL;
    } else {
       srcord %= thisdirection;
       if (srcord < 0) srcord += thisdirection;
       *rank_source= ompi_comm_rank(comm);
       *rank_source += ((srcord - ord) * factor);
    }

    return MPI_SUCCESS;
}
