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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pubsub/pubsub.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Lookup_name = PMPI_Lookup_name
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Lookup_name";


int MPI_Lookup_name(char *service_name, MPI_Info info, char *port_name) 
{
    char *tmp;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME); 

        if ( NULL == port_name ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
        if ( NULL == service_name ) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
                                          FUNC_NAME);
        }
        if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                          FUNC_NAME);
        }
    }

    OPAL_CR_ENTER_LIBRARY();

    /* 
     * No predefined info-objects for this function in MPI-2,
     * therefore, we do not parse the info-object at the moment.
     */

    /*
     * if multiple entries found, this implementation uses
     * at the moment the first entry.
     */
    tmp = (char *) ompi_pubsub.lookup(service_name, info);
    if ( NULL == tmp ) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NAME,
                                      FUNC_NAME);
    }

    strncpy ( port_name, tmp, MPI_MAX_PORT_NAME );

    OPAL_CR_EXIT_LIBRARY();
    return MPI_SUCCESS;
}
