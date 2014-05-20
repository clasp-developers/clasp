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
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_set_errhandler = PMPI_Comm_set_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_set_errhandler";


int MPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler) 
{
    MPI_Errhandler tmp;

    /* Error checking */
    MEMCHECKER(
        memchecker_comm(comm);
    );

    OPAL_CR_NOOP_PROGRESS();

    /* Error checking */

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM,
                                          FUNC_NAME);
        } else if (NULL == errhandler ||
                   MPI_ERRHANDLER_NULL == errhandler ||
                   ( OMPI_ERRHANDLER_TYPE_COMM != errhandler->eh_mpi_object_type &&
                     OMPI_ERRHANDLER_TYPE_PREDEFINED != errhandler->eh_mpi_object_type) ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
    }

    /* Prepare the new error handler */
    OBJ_RETAIN(errhandler);

    /* Ditch the old errhandler, and decrement its refcount.  On 64
       bits environments we have to make sure the reading of the
       error_handler became atomic. */
    do {
        tmp = comm->error_handler;
    } while (!OPAL_ATOMIC_CMPSET(&(comm->error_handler), tmp, errhandler));
    OBJ_RELEASE(tmp);

    /* All done */
    return MPI_SUCCESS;
}
