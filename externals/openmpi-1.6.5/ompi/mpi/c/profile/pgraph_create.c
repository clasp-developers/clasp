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
#pragma weak MPI_Graph_create = PMPI_Graph_create
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Graph_create";


int MPI_Graph_create(MPI_Comm old_comm, int nnodes, int *index,
                     int *edges, int reorder, MPI_Comm *comm_graph) 
{

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
        if (nnodes < 0) {
            return OMPI_ERRHANDLER_INVOKE (old_comm, MPI_ERR_ARG,
                                           FUNC_NAME);
        } else if (nnodes >= 1 && ((NULL == index) || (NULL == edges))) {
            return OMPI_ERRHANDLER_INVOKE (old_comm, MPI_ERR_ARG,
                                           FUNC_NAME);
        }

        if (nnodes > ompi_comm_size(old_comm)) {
            return OMPI_ERRHANDLER_INVOKE (old_comm, MPI_ERR_ARG,
                                           FUNC_NAME);
        }
    }

    /* MPI-2.1 7.5.3 states that if nnodes == 0, all processes should
       get MPI_COMM_NULL */
    if (0 == nnodes) {
        *comm_graph = MPI_COMM_NULL;
        return MPI_SUCCESS;
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

    /* 
     * everything seems to be alright with the communicator, we can go 
     * ahead and select a topology module for this purpose and create 
     * the new graph communicator
     */

    re_order = (0 == reorder) ? false : true;

    err = ompi_topo_create ((struct ompi_communicator_t *)old_comm,
                            nnodes,
                            index,
                            edges,
                            re_order,
                            (struct ompi_communicator_t **)comm_graph,
                            OMPI_COMM_GRAPH);

    OPAL_CR_EXIT_LIBRARY();
    /* check the error status */
    if (MPI_SUCCESS != err) {
        return OMPI_ERRHANDLER_INVOKE(old_comm, err, FUNC_NAME);
    }
    
    /* All done */
    return MPI_SUCCESS;
}
