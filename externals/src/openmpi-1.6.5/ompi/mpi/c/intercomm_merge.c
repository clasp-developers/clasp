/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2009 University of Houston.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Intercomm_merge = PMPI_Intercomm_merge
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

#define INTERCOMM_MERGE_TAG 1010

static const char FUNC_NAME[] = "MPI_Intercomm_merge";


int MPI_Intercomm_merge(MPI_Comm intercomm, int high,
                        MPI_Comm *newcomm) 
{
    ompi_communicator_t *newcomp=MPI_COMM_NULL;
    ompi_proc_t **procs=NULL;
    int local_size, remote_size;
    int first;
    int total_size;
    int rc=MPI_SUCCESS;
    int thigh = high;
    ompi_group_t *new_group_pointer;
    

    MEMCHECKER(
        memchecker_comm(intercomm);
    );

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME); 

        if (ompi_comm_invalid ( intercomm ) ||
             !( intercomm->c_flags & OMPI_COMM_INTER ) ) 
            return OMPI_ERRHANDLER_INVOKE ( MPI_COMM_WORLD, MPI_ERR_COMM,
                                            FUNC_NAME);

        if ( NULL == newcomm )
            return OMPI_ERRHANDLER_INVOKE ( intercomm, MPI_ERR_ARG, 
                                            FUNC_NAME);
    }

    OPAL_CR_ENTER_LIBRARY();

    local_size  = ompi_comm_size ( intercomm );
    remote_size = ompi_comm_remote_size ( intercomm );
    total_size  = local_size + remote_size;
    procs = (ompi_proc_t **) malloc ( total_size * sizeof(ompi_proc_t *));
    if ( NULL == procs ) {
        rc = MPI_ERR_INTERN;
        goto exit;
    }
    
    first = ompi_comm_determine_first ( intercomm, thigh );
    if ( MPI_UNDEFINED == first ) {
        rc = MPI_ERR_INTERN;
        goto exit;
    }

    if ( first ) {
        ompi_group_union ( intercomm->c_local_group, intercomm->c_remote_group, &new_group_pointer );
    }
    else {
        ompi_group_union ( intercomm->c_remote_group, intercomm->c_local_group, &new_group_pointer );
    }

    rc = ompi_comm_set ( &newcomp,                 /* new comm */
                         intercomm,                /* old comm */
                         total_size,               /* local_size */
                         NULL,                     /* local_procs*/
                         0,                        /* remote_size */
                         NULL,                     /* remote_procs */
                         NULL,                     /* attrs */
                         intercomm->error_handler, /* error handler*/
                         NULL,                     /* topo mpodule */
                         new_group_pointer,        /* local group */
                         NULL                      /* remote group */
                         );
    if ( NULL == newcomp ) {
        rc = MPI_ERR_INTERN;
        goto exit;
    }
    if ( MPI_SUCCESS != rc ) {
        goto exit;
    }

    ompi_group_decrement_proc_count(new_group_pointer);
    OBJ_RELEASE(new_group_pointer);
    new_group_pointer = MPI_GROUP_NULL;

    /* Determine context id. It is identical to f_2_c_handle */
    rc = ompi_comm_nextcid ( newcomp,              /* new comm */ 
                             intercomm,            /* old comm */
                             NULL,                 /* bridge comm */
                             NULL,                 /* local leader */
                             NULL,                 /* remote_leader */
                             OMPI_COMM_CID_INTER,  /* mode */
                             -1 );                 /* send_first */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

    /* activate communicator and init coll-module */
    rc = ompi_comm_activate( &newcomp,             /* new comm */ 
                             intercomm,            /* old comm */
                             NULL,                 /* bridge comm */
                             NULL,                 /* local leader */
                             NULL,                 /* remote_leader */
                             OMPI_COMM_CID_INTER,  /* mode */
                             -1 );                 /* send_first */
    if ( OMPI_SUCCESS != rc ) {
        goto exit;
    }

 exit:
    OPAL_CR_EXIT_LIBRARY();

    if ( NULL != procs ) {
        free ( procs );
    }
    if ( MPI_SUCCESS != rc ) {
        if ( MPI_COMM_NULL != newcomp && NULL != newcomp ) {
            OBJ_RELEASE(newcomp);
        }
        *newcomm = MPI_COMM_NULL;
        return OMPI_ERRHANDLER_INVOKE(intercomm, rc,  FUNC_NAME);
    }

    *newcomm = newcomp;
    return MPI_SUCCESS;
}

