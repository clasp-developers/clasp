/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/win/win.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Win_get_errhandler = PMPI_Win_get_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Win_get_errhandler";


int MPI_Win_get_errhandler(MPI_Win win, MPI_Errhandler *errhandler) 
{
    MPI_Errhandler tmp;

    OPAL_CR_NOOP_PROGRESS();

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_win_invalid(win)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_WIN,
                                          FUNC_NAME);
        } else if (NULL == errhandler) {
            return OMPI_ERRHANDLER_INVOKE(win, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
    }

    /* On 64 bits environments we have to make sure the reading of the
       error_handler became atomic. */
    do {
        tmp = win->error_handler;
    } while (!OPAL_ATOMIC_CMPSET(&(win->error_handler), tmp, tmp));

    /* Retain the errhandler, corresponding to object refcount
       decrease in errhandler_free.c. */
    OBJ_RETAIN(win->error_handler);
    *errhandler = win->error_handler;

    /* All done */
    return MPI_SUCCESS;
}
