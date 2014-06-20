/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
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

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Get_version = PMPI_Get_version
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Get_version";


int MPI_Get_version(int *version, int *subversion) 
{
    MPI_Comm null = NULL;

    OPAL_CR_NOOP_PROGRESS();

    if (MPI_PARAM_CHECK) {
        /* Per MPI-2:3.1, this function can be invoked before
           MPI_INIT, so we don't invoke the normal
           MPI_ERR_INIT_FINALIZE() macro here */
        
        if (NULL == version || NULL == subversion) {
            /* Note that we have to check and see if we have
               previously called MPI_INIT or not.  If so, use the
               normal OMPI_ERRHANDLER_INVOKE, because the user may
               have changed the default errhandler on MPI_COMM_WORLD.
               If we have not invoked MPI_INIT, then just abort
               (i.e., use a NULL communicator, which will end up at the
               default errhandler, which is abort). */

            if (ompi_mpi_initialized && !ompi_mpi_finalized) {
                return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                              FUNC_NAME);
            } else {
                return OMPI_ERRHANDLER_INVOKE(null, MPI_ERR_ARG,
                                              FUNC_NAME);
            }
        }
    }

    /* According to the MPI-2 specification */

    *version = MPI_VERSION;
    *subversion = MPI_SUBVERSION;

    return MPI_SUCCESS;
}
