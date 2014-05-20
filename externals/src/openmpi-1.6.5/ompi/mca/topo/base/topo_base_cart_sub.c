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
 * function - partitions a communicator into subgroups which
 *            form lower-dimensional cartesian subgrids
 *            
 * @param comm communicator with cartesian structure (handle)
 * @param remain_dims the 'i'th entry of 'remain_dims' specifies whether 
 *                the 'i'th dimension is kept in the subgrid (true) 
 *                or is dropped (false) (logical vector)
 * @param new_comm communicator containing the subgrid that includes the
 *                 calling process (handle)
 *
 * @retval MPI_SUCCESS
 * @retval MPI_ERR_TOPOLOGY
 * @retval MPI_ERR_COMM
 */                
int mca_topo_base_cart_sub (ompi_communicator_t* comm,
                        int *remain_dims,
                        ompi_communicator_t** new_comm){

     struct ompi_communicator_t *temp_comm;
     int errcode;
     int colour;
     int key;
     int colfactor;
     int keyfactor;
     int rank;
     int ndim;
     int dim;
     int i;
     int *d, *dold;
     int *c;
     int *r;
     int *p, *pold;

     *new_comm = MPI_COMM_NULL;

    /*
     * Compute colour and key used in splitting the communicator.
     */
     colour = key = 0;
     colfactor = keyfactor = 1;
     ndim = 0;

     i = comm->c_topo_comm->mtc_ndims_or_nnodes - 1;
     d = comm->c_topo_comm->mtc_dims_or_index + i;
     c = comm->c_topo_comm->mtc_coords + i;
     r = remain_dims + i;

     for (; i >= 0; --i, --d, --c, --r) {
        dim = *d;
        if (*r == 0) {
           colour += colfactor * (*c);
           colfactor *= dim;
        } else {
          ++ndim;
          key += keyfactor * (*c);
          keyfactor *= dim;
        }
     }
    /* Special case: if all of remain_dims were false, we need to make
       a 0-dimension cartesian communicator with just ourselves in it
       (you can't have a communicator unless you're in it). */
     if (0 == ndim) {
         colour = ompi_comm_rank (comm);
     }
    /*
     * Split the communicator.
     */
     errcode = ompi_comm_split (comm, colour, key, &temp_comm, true);
     if (errcode != MPI_SUCCESS) {
        return errcode;
     }
    /*
     * Fill the communicator with topology information.
     */
     if (temp_comm != MPI_COMM_NULL) {
        
        temp_comm->c_topo_comm->mtc_ndims_or_nnodes = ndim;

        if (ndim >= 1) {
            /* Copy the dimensions */
            d = temp_comm->c_topo_comm->mtc_dims_or_index;
            dold = comm->c_topo_comm->mtc_dims_or_index;
            r = remain_dims;
            for (i = 0; i < comm->c_topo_comm->mtc_ndims_or_nnodes; 
                 ++i, ++dold, ++r) {
                if (*r) {
                    *d++ = *dold;
                }
            }

            /* Copy the periods */
            p = temp_comm->c_topo_comm->mtc_periods_or_edges;
            pold = comm->c_topo_comm->mtc_periods_or_edges;
            r = remain_dims;
            for (i = 0; i < comm->c_topo_comm->mtc_ndims_or_nnodes; 
                 ++i, ++pold, ++r) {
                if (*r) {
                    *p++ = *pold;
                }
            }

            /*
             * Compute the caller's coordinates, if ndims>0
             */
            rank = ompi_comm_rank (temp_comm);
            if (MPI_SUCCESS != errcode) {
                OBJ_RELEASE(temp_comm);
                return errcode;
            }
            errcode = temp_comm->c_topo->topo_cart_coords (temp_comm, rank,
                                                           ndim, temp_comm->c_topo_comm->mtc_coords);
            if (MPI_SUCCESS != errcode) {
                OBJ_RELEASE(temp_comm);
                return errcode;
            }
        }
     }

      *new_comm = temp_comm;
      return MPI_SUCCESS;
}
