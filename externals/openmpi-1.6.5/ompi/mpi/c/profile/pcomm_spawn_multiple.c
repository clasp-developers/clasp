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
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
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
#include "ompi/info/info.h"
#include "ompi/mca/dpm/dpm.h"
#include "ompi/memchecker.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_spawn_multiple = PMPI_Comm_spawn_multiple
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Comm_spawn_multiple";


int MPI_Comm_spawn_multiple(int count, char **array_of_commands, char ***array_of_argv,
                            int *array_of_maxprocs, MPI_Info *array_of_info,
                            int root, MPI_Comm comm, MPI_Comm *intercomm,
                            int *array_of_errcodes) 
{
    int i=0, rc=0, rank=0, size=0, flag;
    ompi_communicator_t *newcomp=NULL;
    bool send_first=false; /* they are contacting us first */
    char port_name[MPI_MAX_PORT_NAME];
    bool non_mpi = false, cumulative = false;

    MEMCHECKER(
        memchecker_comm(comm);
    );
    
    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if ( ompi_comm_invalid (comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, 
                                          FUNC_NAME);
        }
        if ( OMPI_COMM_IS_INTER(comm)) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_COMM, FUNC_NAME);
        }
        if ( (0 > root) || (ompi_comm_size(comm) <= root) ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
        }
        if ( NULL == intercomm ) {
            return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
        }
    }
   
    rank = ompi_comm_rank ( comm );
    if ( MPI_PARAM_CHECK ) {
        if ( rank == root ) {
            if ( 0 > count ) {
                return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
            }
            if ( NULL == array_of_commands ) {
                return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
            }
            if ( NULL ==  array_of_maxprocs ) {
                return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
            }
            if ( NULL == array_of_info ) {
                return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_INFO, FUNC_NAME);
            }
            for (i = 0; i < count; ++i) {
                if (NULL == array_of_info[i] || 
                    ompi_info_is_freed(array_of_info[i])) {
                    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_INFO,
                                                  FUNC_NAME);
                }
                /* If ompi_non_mpi is set to true on any info, it must
                   be set to true on all of them.  Note that not
                   setting ompi_non_mpi is the same as setting it to
                   false. */
                ompi_info_get_bool(array_of_info[i], "ompi_non_mpi", &non_mpi,
                                   &flag);
                if (flag && 0 == i) {
                    /* If this is the first info, save its
                       ompi_non_mpi value */
                    cumulative = non_mpi;
                } else if (!flag) {
                    non_mpi = false;
                }
                /* If this info's effective value doesn't agree with
                   the rest of them, error */
                if (cumulative != non_mpi) {
                    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD,
                                                  MPI_ERR_INFO,
                                                  FUNC_NAME);
                }
            }
            for ( i=0; i<count; i++ ) {
                if ( NULL == array_of_commands[i] ) {
                    return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
                }
                if ( 0 > array_of_maxprocs[i] ) {
                    return OMPI_ERRHANDLER_INVOKE(comm, MPI_ERR_ARG, FUNC_NAME);
                }
            }  
        }
    }

    if (rank == root) {
        if (MPI_INFO_NULL == array_of_info[0]) {
            non_mpi = false;
        } else {
            ompi_info_get_bool(array_of_info[0], "ompi_non_mpi", &non_mpi,
                               &flag);
            if (!flag) {
                non_mpi = false;
            }
        }
    }

    /* initialize the port name to avoid problems */
    memset(port_name, 0, MPI_MAX_PORT_NAME);
    
    OPAL_CR_ENTER_LIBRARY();

    if ( rank == root ) {
        if (!non_mpi) {
            /* Open a port. The port_name is passed as an environment
               variable to the children. */
            if (OMPI_SUCCESS != (rc = ompi_dpm.open_port (port_name, OMPI_RML_TAG_INVALID))) {
                goto error;
            }
        } else if (1 < ompi_comm_size(comm)) {
            /* we do not support non_mpi spawns on comms this size */
            rc = OMPI_ERR_NOT_SUPPORTED;
            goto error;
        }
        if (OMPI_SUCCESS != (rc = ompi_dpm.spawn(count, array_of_commands,
                                                 array_of_argv, array_of_maxprocs,
                                                 array_of_info, port_name))) {
            goto error;
        }
    }

    if (non_mpi) {
        newcomp = MPI_COMM_NULL;
    } else {
        rc = ompi_dpm.connect_accept (comm, root, port_name, send_first, &newcomp);
    }

error:
    OPAL_CR_EXIT_LIBRARY();
    
    /* close the port */
    if (rank == root && !non_mpi) {
        ompi_dpm.close_port(port_name);
    }
    
    /* set array of errorcodes */
    if (MPI_ERRCODES_IGNORE != array_of_errcodes) {
        if (NULL != newcomp) {
            for ( i=0; i < newcomp->c_remote_group->grp_proc_count; i++ ) {
                array_of_errcodes[i]=rc;
            }
        } else {
            for ( i=0; i < count; i++) {
                size = size + array_of_maxprocs[i];
            }

            for ( i=0; i < size; i++) {
                array_of_errcodes[i]=rc;
            }
        }
    }

    *intercomm = newcomp;
    OMPI_ERRHANDLER_RETURN (rc, comm, rc, FUNC_NAME);
}

