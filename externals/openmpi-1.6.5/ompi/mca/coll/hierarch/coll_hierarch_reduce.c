/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "coll_hierarch.h"

#include <stdio.h>

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/coll/coll.h"


/*
 *	reduce_intra
 *
 *	Function:	- reduction using two level hierarchy algorithm
 *	Accepts:	- same as MPI_Reduce()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_hierarch_reduce_intra(void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype, 
                               struct ompi_op_t *op,
                               int root, struct ompi_communicator_t *comm, 
			       mca_coll_base_module_t *module)
{
    struct ompi_communicator_t *llcomm=NULL;
    struct ompi_communicator_t *lcomm=NULL;
    mca_coll_hierarch_module_t *hierarch_module = (mca_coll_hierarch_module_t *) module;
    int rank;
    int lroot, llroot;
    ptrdiff_t extent, true_extent, lb, true_lb;
    char *tmpbuf=NULL, *tbuf=NULL;
    int ret=OMPI_SUCCESS;

    rank   = ompi_comm_rank ( comm );
    lcomm  = hierarch_module->hier_lcomm;

    if ( mca_coll_hierarch_verbose_param ) {
      printf("%s:%d: executing hierarchical reduce with cnt=%d and root=%d\n",
	     comm->c_name, rank, count, root );
    }

    llcomm = mca_coll_hierarch_get_llcomm ( root, hierarch_module, &llroot, &lroot);

    if ( MPI_COMM_NULL != lcomm ) {
      ompi_datatype_get_extent(dtype, &lb, &extent);
      ompi_datatype_get_true_extent(dtype, &true_lb, &true_extent);
      
      tbuf = (char*)malloc(true_extent + (count - 1) * extent);
      if (NULL == tbuf) {
	return OMPI_ERR_OUT_OF_RESOURCE;
      }
      tmpbuf = tbuf - lb;
    

      if ( MPI_IN_PLACE != sbuf ) {
	ret = lcomm->c_coll.coll_reduce (sbuf, tmpbuf, count, dtype, 
					 op, lroot, lcomm, 
					 lcomm->c_coll.coll_reduce_module);
      }
      else {
	ret = lcomm->c_coll.coll_reduce (rbuf, tmpbuf, count, dtype, 
					 op, lroot, lcomm, 
					 lcomm->c_coll.coll_reduce_module);
      }
      if ( OMPI_SUCCESS != ret ) {
	goto exit;
      }
    }

    if ( MPI_UNDEFINED != llroot ) {
      if ( MPI_COMM_NULL != lcomm ) { 
	ret = llcomm->c_coll.coll_reduce (tmpbuf, rbuf, count, dtype,
					  op, llroot, llcomm, 
					  llcomm->c_coll.coll_reduce_module);
      }
      else {
	ret = llcomm->c_coll.coll_reduce (sbuf, rbuf, count, dtype,
					  op, llroot, llcomm, 
					  llcomm->c_coll.coll_reduce_module);
      }
    }

 exit:
    if ( NULL != tmpbuf ) {
	free ( tmpbuf );
    }

    return ret;
}
