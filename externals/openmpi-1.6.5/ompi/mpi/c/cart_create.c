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
#include "ompi/mca/topo/base/base.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Cart_create = PMPI_Cart_create
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Cart_create";


int MPI_Cart_create(MPI_Comm old_comm, int ndims, int *dims,
                    int *periods, int reorder, MPI_Comm *comm_cart) {

    int err;
    bool re_order = false;

    MEMCHECKER(
        memchecker_comm(old_comm);
    );

    /* check the arguments */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(old_comm)) {
            return OMPI_ERRHANDLER_INVOKE (MPI_COMM_WORLD, MPI_ERR_COMM,
                                          FUNC_NAME);
        }
        if (OMPI_COMM_IS_INTER(old_comm)) {
            return OMPI_ERRHANDLER_INVOKE (old_comm, MPI_ERR_COMM,
                                          FUNC_NAME);
        }
        if (ndims < 0) {
            return OMPI_ERRHANDLER_INVOKE (old_comm, MPI_ERR_ARG,
                                          FUNC_NAME);
        } else if (ndims >= 1 &&
                   (NULL == dims || NULL == periods || NULL == comm_cart)) {
            return OMPI_ERRHANDLER_INVOKE (old_comm, MPI_ERR_ARG,
                                          FUNC_NAME);
        }

        /* check if the number of processes on the grid are correct */
        {
           int i;
           int *p = dims;
           int count_nodes = 1;
           int parent_procs = ompi_comm_size(old_comm);

           for (i=0; i < ndims; i++) {
               count_nodes *= *p;
               p++;
           }

           if (parent_procs < count_nodes) {
               return OMPI_ERRHANDLER_INVOKE (old_comm, MPI_ERR_ARG,
                                              FUNC_NAME);
           }
        }
    }

    /*
     * Now we have to check if the topo module exists or not. This has been
     * removed from initialization since most of the MPI calls do not use 
     * this module 
     */
    if (!(mca_topo_base_components_opened_valid ||
          mca_topo_base_components_available_valid)) {
        if (OMPI_SUCCESS != (err = mca_topo_base_open())) {
            return OMPI_ERRHANDLER_INVOKE(old_comm, err, FUNC_NAME);
        }
        if (OMPI_SUCCESS != 
            (err = mca_topo_base_find_available(OPAL_ENABLE_PROGRESS_THREADS,
                                                OMPI_ENABLE_THREAD_MULTIPLE))) {
            return OMPI_ERRHANDLER_INVOKE(old_comm, err, FUNC_NAME);
        }
    }

    OPAL_CR_ENTER_LIBRARY();

    /* everything seems to be alright with the communicator, we can go 
     * ahead and select a topology module for this purpose and create 
     * the new cartesian communicator
     */

    re_order = (0 == reorder)? false : true;

    err = ompi_topo_create (old_comm,
                            ndims,
                            dims,
                            periods,
                            re_order,
                            comm_cart,
                            OMPI_COMM_CART);

    OPAL_CR_EXIT_LIBRARY();
    /* check the error status */
    if (MPI_SUCCESS != err) {
        return OMPI_ERRHANDLER_INVOKE(old_comm, err, FUNC_NAME);
    }
    
    /* All done */
    return MPI_SUCCESS;
}
