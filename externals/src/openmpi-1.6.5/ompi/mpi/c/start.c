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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/mca/pml/pml.h"
#include "ompi/request/request.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Start = PMPI_Start
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Start";


int MPI_Start(MPI_Request *request) 
{
    int ret = OMPI_SUCCESS;

    MEMCHECKER(
        memchecker_request(request);
    );

    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (request == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    switch((*request)->req_type) {
    case OMPI_REQUEST_PML:
        OPAL_CR_ENTER_LIBRARY();

        ret = MCA_PML_CALL(start(1, request));

        OPAL_CR_EXIT_LIBRARY();
        return ret;
        break;

    case OMPI_REQUEST_NOOP:
        return MPI_SUCCESS;
        break;

    default:
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_REQUEST, FUNC_NAME);
    }
}

