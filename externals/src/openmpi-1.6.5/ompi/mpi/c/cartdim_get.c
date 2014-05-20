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
#pragma weak MPI_Cartdim_get = PMPI_Cartdim_get
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif


static const char FUNC_NAME[] = "MPI_Cartdim_get";


int MPI_Cartdim_get(MPI_Comm comm, int *ndims) 
{
    mca_topo_base_module_cartdim_get_fn_t func;
    int err;

    MEMCHECKER(
        memchecker_comm(comm);
    );

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
       if (NULL == ndims) {
           return OMPI_ERRHANDLER_INVOKE (comm, MPI_ERR_ARG,
                                          FUNC_NAME);
       }
    }

    OPAL_CR_ENTER_LIBRARY();

    /* get the function pointer on this communicator */
    func = comm->c_topo->topo_cartdim_get;

    /* call the function */
    err = func(comm, ndims);
    OPAL_CR_EXIT_LIBRARY();
    if ( MPI_SUCCESS != err ) {
        return OMPI_ERRHANDLER_INVOKE(comm, err, FUNC_NAME);
    }

    return MPI_SUCCESS;
}
