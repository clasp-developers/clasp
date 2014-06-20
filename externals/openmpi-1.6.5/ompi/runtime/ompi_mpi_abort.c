/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "opal/mca/backtrace/backtrace.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/communicator/communicator.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"
#include "ompi/debuggers/debuggers.h"
#include "ompi/errhandler/errcode.h"

static bool have_been_invoked = false;

int
ompi_mpi_abort(struct ompi_communicator_t* comm,
               int errcode,
               bool kill_remote_of_intercomm)
{
    int count = 0, i;
    char *msg, *host, hostname[MAXHOSTNAMELEN];
    pid_t pid = 0;
    orte_process_name_t *abort_procs;
    orte_std_cntr_t nabort_procs;

    /* Protection for recursive invocation */
    if (have_been_invoked) {
        return OMPI_SUCCESS;
    }
    have_been_invoked = true;

    /* If ORTE is initialized, use its nodename.  Otherwise, call
       gethostname. */

    if (orte_initialized) {
        host = orte_process_info.nodename;
    } else {
        gethostname(hostname, sizeof(hostname));
        host = hostname;
    }
    pid = getpid();

    /* Should we print a stack trace?  Not aggregated because they
       might be different on all processes. */
    if (ompi_mpi_abort_print_stack) {
        char **messages;
        int len, i;

        if (OMPI_SUCCESS == opal_backtrace_buffer(&messages, &len)) {
            for (i = 0; i < len; ++i) {
                fprintf(stderr, "[%s:%d] [%d] func:%s\n", host, (int) pid, 
                        i, messages[i]);
                fflush(stderr);
            }
            free(messages);
        } else {
            /* This will print an message if it's unable to print the
               backtrace, so we don't need an additional "else" clause
               if opal_backtrace_print() is not supported. */
            opal_backtrace_print(stderr);
        }
    }

    /* Notify the debugger that we're about to abort */

    if (errcode < 0 ||
        asprintf(&msg, "[%s:%d] aborting with MPI error %s%s", 
                 host, (int) pid, ompi_mpi_errnum_get_string(errcode), 
                 ompi_mpi_abort_print_stack ? 
                 " (stack trace available on stderr)" : "") < 0) {
        msg = NULL;
    }
    ompi_debugger_notify_abort(msg);
    if (NULL != msg) {
        free(msg);
    }

    /* Should we wait for a while before aborting? */

    if (0 != ompi_mpi_abort_delay) {
        if (ompi_mpi_abort_delay < 0) {
            fprintf(stderr ,"[%s:%d] Looping forever (MCA parameter mpi_abort_delay is < 0)\n",
                    host, (int) pid);
            fflush(stderr);
            while (1) { 
                sleep(5); 
            }
        } else {
            fprintf(stderr, "[%s:%d] Delaying for %d seconds before aborting\n",
                    host, (int) pid, ompi_mpi_abort_delay);
            do {
                sleep(1);
            } while (--ompi_mpi_abort_delay > 0);
        }
    }

    /* If OMPI isn't setup yet/any more, then don't even try killing
       everyone.  Ditto for ORTE (e.g., ORTE may be initialized before
       MPI_INIT is over, but ompi_initialized will be false because
       communicators are not setup yet). Sorry, Charlie... */

    if (!orte_initialized || !ompi_mpi_initialized || ompi_mpi_finalized) {
        if (orte_show_help_is_available()) {
            orte_show_help("help-mpi-runtime.txt", 
                           "ompi mpi abort:cannot guarantee all killed",
                           true,
                           (ompi_mpi_finalized ? 
                            "After MPI_FINALIZE was invoked" :
                            (ompi_mpi_init_started ?
                             "Before MPI_INIT completed" : 
                             "Before MPI_INIT was invoked")),
                           host, (int) pid);
        } else {
            fprintf(stderr, "[%s:%d] Local abort %s completed successfully; not able to aggregate error messages, and not able to guarantee that all other processes were killed!\n",
                    host, (int) pid, ompi_mpi_finalized ? 
                    "after MPI_FINALIZE" : "before MPI_INIT");
        }
        exit(errcode);
    }

    /* abort local procs in the communicator.  If the communicator is
       an intercommunicator AND the abort has explicitly requested
       that we abort the remote procs, then do that as well. */
    nabort_procs = ompi_comm_size(comm);

    if (kill_remote_of_intercomm) {
        /* ompi_comm_remote_size() returns 0 if not an intercomm, so
           this is cool */
        nabort_procs += ompi_comm_remote_size(comm);
    }

    abort_procs = (orte_process_name_t*)malloc(sizeof(orte_process_name_t) * nabort_procs);
    if (NULL == abort_procs) {
        /* quick clean orte and get out */
        orte_errmgr.abort(errcode, "Abort unable to malloc memory to kill procs");
    }

    /* put all the local procs in the abort list */
    for (i = 0 ; i < ompi_comm_size(comm) ; ++i) {
        if (OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL, 
                                 &comm->c_local_group->grp_proc_pointers[i]->proc_name,
                                 ORTE_PROC_MY_NAME)) {
            assert(count <= nabort_procs);
            abort_procs[count++] = comm->c_local_group->grp_proc_pointers[i]->proc_name;
        } else {
            /* don't terminate me just yet */
            nabort_procs--;
        }
    }

    /* if requested, kill off remote procs too */
    if (kill_remote_of_intercomm) {
        for (i = 0 ; i < ompi_comm_remote_size(comm) ; ++i) {
            if (OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL, 
                                     &comm->c_remote_group->grp_proc_pointers[i]->proc_name,
                                     ORTE_PROC_MY_NAME)) {
                assert(count <= nabort_procs);
                abort_procs[count++] =
                    comm->c_remote_group->grp_proc_pointers[i]->proc_name;
            } else {
                /* don't terminate me just yet */
                nabort_procs--;
            }
        }
    }

    if (nabort_procs > 0) {
#if 0
        int ret = orte_errmgr.abort_procs_request(abort_procs, nabort_procs);
        if (OMPI_SUCCESS != ret) {
            orte_errmgr.abort(ret, "Open MPI failed to abort procs as requested (%d). Exiting.", ret);
        }
#endif
    }

    /* now that we've aborted everyone else, gracefully die. */
    orte_errmgr.abort(errcode, NULL);
    
    return OMPI_SUCCESS;
}
