/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
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
#include "ompi/request/request.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Testall = PMPI_Testall
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Testall";


int MPI_Testall(int count, MPI_Request requests[], int *flag,
                MPI_Status statuses[]) 
{
    MEMCHECKER(
        int j;
        for (j = 0; j < count; j++){
            memchecker_request(&requests[j]);
        }
    );

    if ( MPI_PARAM_CHECK ) {
        int i, rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if( (NULL == requests) && (0 != count) ) {
            rc = MPI_ERR_REQUEST;
        } else {
            for (i = 0; i < count; i++) {
                if (NULL == requests[i]) {
                    rc = MPI_ERR_REQUEST;
                    break;
                }
            }
        }
        if ((NULL == flag) || (count < 0)) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    if (OPAL_UNLIKELY(0 == count)) {
        *flag = true;
        return MPI_SUCCESS;
    }

    OPAL_CR_ENTER_LIBRARY();

    if (OMPI_SUCCESS == ompi_request_test_all(count, requests, flag, 
                                              statuses)) {
        OPAL_CR_EXIT_LIBRARY();
        return MPI_SUCCESS;
    }

    if (MPI_SUCCESS !=
        ompi_errhandler_request_invoke(count, requests, FUNC_NAME)) {
        OPAL_CR_EXIT_LIBRARY();
        return MPI_ERR_IN_STATUS;
    }

    OPAL_CR_EXIT_LIBRARY();
    return MPI_SUCCESS;
}
