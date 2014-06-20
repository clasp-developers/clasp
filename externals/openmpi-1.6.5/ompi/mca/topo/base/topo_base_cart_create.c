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
#include "ompi/constants.h"

#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/topo/topo.h"

/*
 * function - makes a new communicator to which topology information
 *            has been attached
 *
 * @param comm input communicator (handle)
 * @param ndims number of dimensions of cartesian grid (integer)
 * @param dims integer array of size ndims specifying the number of processes in
 *             each dimension
 * @param periods logical array of size ndims specifying whether the grid is
 *                periodic (true) or not (false) in each dimension
 * @param reorder ranking may be reordered (true) or not (false) (logical)
 * @param comm_cart communicator with new cartesian topology (handle)
 *
 * Open MPI currently ignores the 'reorder' flag.
 *
 * @retval OMPI_SUCCESS
 */                       

int mca_topo_base_cart_create (mca_topo_base_comm_t *topo_data,
                               int *proc_count,
                               ompi_proc_t **proc_pointers,
                               int *new_rank,
                               int ndims,
                               int *dims,
                               int *periods,
                               bool reorder) {

   int nprocs;
   int dim;
   int i;
   int *p;
   int *coords = topo_data->mtc_coords;
   int dummy_rank;

   nprocs = 1;
   p = topo_data->mtc_dims_or_index;

   /* Calculate the number of processes in this grid */
   for (i = 0; i < topo_data->mtc_ndims_or_nnodes; ++i, ++p) {
      if(*p <= 0) {
          return OMPI_ERROR;
       }
       nprocs *= *p;
   }

   /* check for the error condition */

   if (*proc_count < nprocs) {
       return MPI_ERR_DIMS;
   }

   /* check if we have to trim the list of processes */
   if (nprocs < *proc_count) {
       *proc_count = nprocs;
   }
   
   if (*new_rank > (nprocs-1)) {
       /* sorry, but in our scheme this process is cut off */
       *new_rank = MPI_UNDEFINED;
       return OMPI_SUCCESS;
   }

   /* Have to replace this with the actual function body itself */
   p = topo_data->mtc_dims_or_index;
   coords =  topo_data->mtc_coords;
   dummy_rank = *new_rank;

   for (i=0; 
        (i < topo_data->mtc_ndims_or_nnodes && i < ndims); 
        ++i, ++p) {
        dim = *p;
        nprocs /= dim;
        *coords++ = dummy_rank / nprocs;
        dummy_rank %= nprocs;
    }

   /* end here */
   return OMPI_SUCCESS;
}
