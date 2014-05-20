/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Cart_rank = PMPI_Cart_rank
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Cart_rank";


int MPI_Cart_rank(MPI_Comm comm, int *coords, int *rank) 
{
    int i, err;
    mca_topo_base_module_cart_rank_fn_t func;

    MEMCHECKER(
        memchecker_comm(comm);
    );

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          FUNC_NAME);
        }
        if (OMPI_COMM_IS_INTER(comm)) { 
            return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_COMM,
                                          FUNC_NAME);
        }
        if (!OMPI_COMM_IS_CART(comm)) {
            return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_TOPOLOGY,
                                          FUNC_NAME);
        }
        /* Per MPI-2.1, coords is only relevant if the dimension of
           the cartesian comm is >0 */
        if ((NULL == coords && comm->c_topo_comm->mtc_ndims_or_nnodes >= 1) ||
            (NULL == rank)){
            return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_ARG,
                                          FUNC_NAME);
        }

        /* Check if coords[i] is within the acceptable range if
           dimension i is not periodic */
        for (i = 0; i < comm->c_topo_comm->mtc_ndims_or_nnodes; ++i) {
            if (!comm->c_topo_comm->mtc_periods_or_edges[i] &&
                (coords[i] < 0 || 
                 coords[i] >= comm->c_topo_comm->mtc_dims_or_index[i])) {
                return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
            }
        }
    }

    OPAL_CR_ENTER_LIBRARY();

    /* get the function pointer on this communicator */
    func = comm->c_topo->topo_cart_rank;

    /* call the function */
    err = func(comm, coords, rank);
    OPAL_CR_EXIT_LIBRARY();
    if ( MPI_SUCCESS != err ) {
        return OMPI_ERRHANDLER_INVOKE(comm, err, FUNC_NAME);
    }

    return MPI_SUCCESS;
}
