/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 * Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      IBM Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <signal.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */

#if defined(HAVE_SCHED_YIELD)
/* Only if we have sched_yield() */
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif
#else
/* Only do these if we don't have <sched.h> */
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#endif /* HAVE_SCHED_YIELD */

#include "opal/mca/maffinity/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/util/opal_environ.h"

#include "orte/util/show_help.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/iof/base/iof_base_setup.h"
#include "orte/mca/plm/plm.h"
#include "orte/util/name_fns.h"

#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/odls/default/odls_default.h"

/*
 * External Interface
 */
static int orte_odls_default_launch_local_procs(opal_buffer_t *data);
static int orte_odls_default_kill_local_procs(opal_pointer_array_t *procs, bool set_state);
static int orte_odls_default_signal_local_procs(const orte_process_name_t *proc, int32_t signal);

static void set_handler_default(int sig);

orte_odls_base_module_t orte_odls_default_module = {
    orte_odls_base_default_get_add_procs_data,
    orte_odls_default_launch_local_procs,
    orte_odls_default_kill_local_procs,
    orte_odls_default_signal_local_procs,
    orte_odls_base_default_deliver_message,
    orte_odls_base_default_require_sync
};

/* convenience macro for erroring out */
#define ORTE_ODLS_ERROR_OUT(errval)     \
    do {                                \
        rc = (errval);                  \
        write(p[1], &rc, sizeof(int));  \
        exit(1);                        \
    } while(0);

/* convenience macro for checking binding requirements */
#define ORTE_ODLS_IF_BIND_NOT_REQD(n)                               \
    do {                                                            \
        if (ORTE_BINDING_NOT_REQUIRED(jobdat->policy)) {            \
            if (orte_report_bindings) {                             \
                orte_show_help("help-odls-default.txt",             \
                               "odls-default:binding-not-avail",    \
                               true, orte_process_info.nodename,    \
                               (n), context->app);                  \
            }                                                       \
            goto LAUNCH_PROCS;                                      \
        }                                                           \
    } while(0);

static bool odls_default_child_died(pid_t pid, unsigned int timeout, int *exit_status)
{
    time_t end;
    pid_t ret;
        
    /* Because of rounding in time (which returns whole seconds) we
     * have to add 1 to our wait number: this means that we wait
     * somewhere between (target) and (target)+1 seconds.  Otherwise,
     * the default 1s actually means 'somwhere between 0 and 1s'. */
    end = time(NULL) + timeout + 1;
    do {
        ret = waitpid(pid, exit_status, WNOHANG);
        if (pid == ret) {
            OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                 "%s odls:default:WAITPID INDICATES PROC %d IS DEAD",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)pid));
            /* It died -- return success */
            return true;
        } else if (0 == ret) {
            /* with NOHANG specified, if a process has already exited
             * while waitpid was registered, then waitpid returns 0
             * as there is no error - this is a race condition problem
             * that occasionally causes us to incorrectly report a proc
             * as refusing to die. Unfortunately, errno may not be reset
             * by waitpid in this case, so we cannot check it.
	     *
	     * (note the previous fix to this, to return 'process dead'
	     * here, fixes the race condition at the cost of reporting
	     * all live processes have immediately died!  Better to
	     * occasionally report a dead process as still living -
	     * which will occasionally trip the timeout for cases that
	     * are right on the edge.)
             */

	    /* Do nothing, process still alive */
        } else if (-1 == ret && ECHILD == errno) {
            /* The pid no longer exists, so we'll call this "good
               enough for government work" */
            OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                 "%s odls:default:WAITPID INDICATES PID %d NO LONGER EXISTS",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (int)pid));
            return true;
        }
        
        /* Bogus delay for 1 msec - let's actually give the CPU some time
         * to quit the other process (sched_yield() -- even if we have it
         * -- changed behavior in 2.6.3x Linux flavors to be undesirable)
         * Don't use select on a bogus file descriptor here as it has proven
         * unreliable and sometimes immediately returns - we really, really
         * -do- want to wait a bit!
         */
        usleep(1000);
    } while (time(NULL) < end);

    /* The child didn't die, so return false */
    return false;
}

static int odls_default_kill_local(pid_t pid, int signum)
{
    if (orte_forward_job_control) {
        pid = -pid;
    }
    if (0 != kill(pid, signum)) {
        if (ESRCH != errno) {
            OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                 "%s odls:default:SENT KILL %d TO PID %d GOT ERRNO %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), signum, (int)pid, errno));
            return errno;
        }
    }
    OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                         "%s odls:default:SENT KILL %d TO PID %d SUCCESS",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), signum, (int)pid));
    return 0;
}

int orte_odls_default_kill_local_procs(opal_pointer_array_t *procs, bool set_state)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_kill_local_procs(procs, set_state,
                                    odls_default_kill_local, odls_default_child_died))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}

/**
 *  Fork/exec the specified processes
 */

static int odls_default_fork_local_proc(orte_app_context_t* context,
                                        orte_odls_child_t *child,
                                        char **environ_copy,
                                        orte_odls_job_t *jobdat)
{
    orte_iof_base_io_conf_t opts;
    int rc;
    sigset_t sigs;
    int i, p[2];
    pid_t pid;
    bool paffinity_enabled = false;
    opal_paffinity_base_cpu_set_t mask;
    orte_node_rank_t nrank;
    int16_t n;
    orte_local_rank_t lrank;
    int target_socket, npersocket, logical_skt;
    int logical_cpu, phys_core, phys_cpu, ncpu;
    bool bound = false;
    char *param, *tmp;
    
    if (NULL != child) {
        /* should pull this information from MPIRUN instead of going with
         default */
        opts.usepty = OPAL_ENABLE_PTY_SUPPORT;
        
        /* do we want to setup stdin? */
        if (NULL != child &&
            (jobdat->stdin_target == ORTE_VPID_WILDCARD || child->name->vpid == jobdat->stdin_target)) {
            opts.connect_stdin = true;
        } else {
            opts.connect_stdin = false;
        }
        
        if (ORTE_SUCCESS != (rc = orte_iof_base_setup_prefork(&opts))) {
            ORTE_ERROR_LOG(rc);
            if (NULL != child) {
                child->state = ORTE_PROC_STATE_FAILED_TO_START;
                child->exit_code = rc;
            }
            return rc;
        }
    }
    
    /* A pipe is used to communicate between the parent and child to
     indicate whether the exec ultimately succeeded or failed.  The
     child sets the pipe to be close-on-exec; the child only ever
     writes anything to the pipe if there is an error (e.g.,
     executable not found, exec() fails, etc.).  The parent does a
     blocking read on the pipe; if the pipe closed with no data,
     then the exec() succeeded.  If the parent reads something from
     the pipe, then the child was letting us know that it failed. */
    if (pipe(p) < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_PIPES);
        if (NULL != child) {
            child->state = ORTE_PROC_STATE_FAILED_TO_START;
            child->exit_code = ORTE_ERR_SYS_LIMITS_PIPES;
        }
        return ORTE_ERR_SYS_LIMITS_PIPES;
    }
    
    /* Fork off the child */
    pid = fork();
    if (NULL != child) {
        child->pid = pid;
    }
    
    if(pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
        if (NULL != child) {
            child->state = ORTE_PROC_STATE_FAILED_TO_START;
            child->exit_code = ORTE_ERR_SYS_LIMITS_CHILDREN;
        }
        return ORTE_ERR_SYS_LIMITS_CHILDREN;
    }
    
    if (pid == 0) {
        long fd, fdmax = sysconf(_SC_OPEN_MAX);

        if (orte_forward_job_control) {
            /* Set a new process group for this child, so that a
               SIGSTOP can be sent to it without being sent to the
               orted. */
            setpgid(0, 0);
        }
        
        /* Setup the pipe to be close-on-exec */
        close(p[0]);
        fcntl(p[1], F_SETFD, FD_CLOEXEC);
        
        if (NULL != child) {
            /*  setup stdout/stderr so that any error messages that we may
             print out will get displayed back at orterun.
             
             NOTE: Definitely do this AFTER we check contexts so that any
             error message from those two functions doesn't come out to the
             user. IF we didn't do it in this order, THEN a user who gives
             us a bad executable name or working directory would get N
             error messages, where N=num_procs. This would be very annoying
             for large jobs, so instead we set things up so that orterun
             always outputs a nice, single message indicating what happened
             */
            if (ORTE_SUCCESS != (i = orte_iof_base_setup_child(&opts, &environ_copy))) {
                ORTE_ODLS_ERROR_OUT(i);
            }
            
            /* Setup process affinity.  First check to see if a slot list was
             * specified.  If so, use it.  If no slot list was specified,
             * that's not an error -- just fall through and try the next
             * paffinity scheme.
             */
            if (NULL != child->slot_list) {
                OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                     "%s odls:default:fork got slot_list %s for child %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     child->slot_list, ORTE_NAME_PRINT(child->name)));
                if (opal_paffinity_alone) {
                    /* It's an error if multiple paffinity schemes were specified */
                    orte_show_help("help-odls-default.txt",
                                   "odls-default:multiple-paffinity-schemes", true, child->slot_list);
                    ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                }
                if (ORTE_SUCCESS != (rc = opal_paffinity_base_slot_list_set((long)child->name->vpid, child->slot_list))) {
                    if (ORTE_ERR_NOT_SUPPORTED == rc) {
                        /* OS doesn't support providing topology information */
                        orte_show_help("help-odls-default.txt",
                                       "odls-default:topo-not-supported", 
                                       true, orte_process_info.nodename,  "rankfile containing a slot_list of ", 
                                       child->slot_list, context->app);
                        ORTE_ODLS_ERROR_OUT(rc);
                    }
                    
                    orte_show_help("help-odls-default.txt",
                                   "odls-default:slot-list-failed", true, child->slot_list, ORTE_ERROR_NAME(rc));
                    ORTE_ODLS_ERROR_OUT(rc);
                }
                if (orte_report_bindings) {
                    char tmp1[1024], tmp2[1024];
                    rc = opal_paffinity_base_get(&mask);
                    if (OPAL_SUCCESS == rc) {
                        opal_paffinity_base_cset2str(tmp1, sizeof(tmp1),
                                                     &mask);
                        opal_paffinity_base_cset2mapstr(tmp2, sizeof(tmp2),
                                                        &mask);

                        opal_output(0, "MCW rank %d bound to %s: %s (slot list %s)",
                                    child->name->vpid, 
                                    tmp1, tmp2, child->slot_list);
                    }
                }
            } else if (ORTE_BIND_TO_CORE & jobdat->policy) {
                /* we want to bind this proc to a specific core, or multiple cores
                 * if the cpus_per_rank is > 0
                 */
                OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                     "%s odls:default:fork binding child %s to core(s) cpus/rank %d stride %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(child->name),
                                     (int)jobdat->cpus_per_rank, (int)jobdat->stride));
                /* get the node rank */
                if (ORTE_NODE_RANK_INVALID == (nrank = orte_ess.get_node_rank(child->name))) {
                    orte_show_help("help-odls-default.txt",
                                   "odls-default:invalid-node-rank", true);
                    ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                }
                /* get the local rank */
                if (ORTE_LOCAL_RANK_INVALID == (lrank = orte_ess.get_local_rank(child->name))) {
                    orte_show_help("help-odls-default.txt",
                                   "odls-default:invalid-local-rank", true);
                    ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                }
                /* init the mask */
                OPAL_PAFFINITY_CPU_ZERO(mask);
                if (ORTE_MAPPING_NPERXXX & jobdat->policy) {
                    /* we need to balance the children from this job across the available sockets */
                    npersocket = jobdat->npersocket;
                    /* determine the socket to use based on those available */
                    if (npersocket < 2) {
                        /* if we only have 1/sock, or we have less procs than sockets,
                         * then just put it on the lrank socket
                         */
                        logical_skt = lrank;
                    } else if (ORTE_MAPPING_BYSOCKET & jobdat->policy) {
                        logical_skt = lrank % npersocket;
                    } else {
                        logical_skt = lrank / npersocket;
                    }
                    if (orte_odls_globals.bound) {
                        /* if we are bound, use this as an index into our available sockets */
                        for (n=target_socket=0; target_socket < opal_bitmap_size(&orte_odls_globals.sockets) && n < logical_skt; target_socket++) {
                            if (opal_bitmap_is_set_bit(&orte_odls_globals.sockets, target_socket)) {
                                n++;
                            }
                        }
                        /* if we don't have enough sockets, that is an error */
                        if (n < logical_skt) {
                            ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-core");
                            orte_show_help("help-odls-default.txt",
                                           "odls-default:not-enough-resources", true,
                                           "sockets", orte_process_info.nodename,
                                           "bind-to-core", context->app);
                            ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                        }
                    } else {
                        target_socket = opal_paffinity_base_get_physical_socket_id(logical_skt);
                        if (ORTE_ERR_NOT_SUPPORTED == target_socket) {
                            /* OS doesn't support providing topology information */
                            ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-core");
                            orte_show_help("help-odls-default.txt",
                                           "odls-default:topo-not-supported", 
                                           true, orte_process_info.nodename,  "bind-to-core", "",
                                           context->app);
                            ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                        }
                    }
                    OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                         "%s odls:default:fork child %s local rank %d npersocket %d logical socket %d target socket %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(child->name), lrank,
                                         npersocket, logical_skt, target_socket));
                    /* set the starting point */
                    logical_cpu = (lrank % npersocket) * jobdat->cpus_per_rank;
                    /* bind to this socket */
                    goto bind_socket;
                } else if (ORTE_MAPPING_BYSOCKET & jobdat->policy) {
                    /* this corresponds to a mapping policy where
                     * local rank 0 goes on socket 0, and local
                     * rank 1 goes on socket 1, etc. - round robin
                     * until all ranks are mapped
                     *
                     * NOTE: we already know our number of sockets
                     * from when we initialized
                     */
                    target_socket = opal_paffinity_base_get_physical_socket_id(lrank % orte_odls_globals.num_sockets);
                    if (ORTE_ERR_NOT_SUPPORTED == target_socket) {
                        /* OS does not support providing topology information */
                        ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-core");
                        orte_show_help("help-odls-default.txt",
                                       "odls-default:topo-not-supported", 
                                       true, orte_process_info.nodename,  "bind-to-core", "",
                                       context->app);
                        ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                    }
                    OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                         "bysocket lrank %d numsocks %d logical socket %d target socket %d", (int)lrank,
                                         (int)orte_odls_globals.num_sockets,
                                         (int)(lrank % orte_odls_globals.num_sockets),
                                         target_socket));
                    /* my starting core within this socket has to be offset by cpus_per_rank */
                    logical_cpu = (lrank / orte_odls_globals.num_sockets) * jobdat->cpus_per_rank;
                    
                bind_socket:
                    /* cycle across the cpus_per_rank */
                    for (n=0; n < jobdat->cpus_per_rank; n++) {
                        /* get the physical core within this target socket */
                        phys_core = opal_paffinity_base_get_physical_core_id(target_socket, logical_cpu);
                        if (0 > phys_core) {
                            ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-core");
                            orte_show_help("help-odls-default.txt",
                                           "odls-default:invalid-phys-cpu", true,
                                           orte_process_info.nodename);
                            ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                        }
                        /* map this to a physical cpu on this node */
                        if (ORTE_SUCCESS != opal_paffinity_base_get_map_to_processor_id(target_socket, phys_core, &phys_cpu)) {
                            ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-core");
                            orte_show_help("help-odls-default.txt",
                                           "odls-default:not-enough-resources", true,
                                           "processors", orte_process_info.nodename,
                                           "bind-to-core", context->app);
                            ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                        }
                        /* are we bound? */
                        if (orte_odls_globals.bound) {
                            /* see if this physical cpu is available to us */
                            if (!OPAL_PAFFINITY_CPU_ISSET(phys_cpu, orte_odls_globals.my_cores)) {
                                /* no it isn't - skip it */
                                continue;
                            }
                        }
                        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                             "%s odls:default:fork mapping phys socket %d core %d to phys_cpu %d",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             target_socket, phys_core, phys_cpu));
                        OPAL_PAFFINITY_CPU_SET(phys_cpu, mask);
                        /* increment logical cpu */
                        logical_cpu += jobdat->stride;
                    }
                    if (orte_report_bindings) {
                        char tmp1[1024], tmp2[1024];
                        opal_paffinity_base_cset2str(tmp1, sizeof(tmp1),
                                                       &mask);
                        opal_paffinity_base_cset2mapstr(tmp2, sizeof(tmp2),
                                                       &mask);
                        opal_output(0, "MCW rank %d bound to %s: %s",
                                    child->name->vpid, tmp1, tmp2);
                    }
                } else {
                    /* my starting core has to be offset by cpus_per_rank */
                    logical_cpu = nrank * jobdat->cpus_per_rank;
                    for (n=0; n < jobdat->cpus_per_rank; n++) {
                        /* are we bound? */
                        if (orte_odls_globals.bound) {
                            /* if we are bound, then use the logical_cpu as an index
                             * against our available cores
                             */
                            ncpu = 0;
                            for (i=0; i < OPAL_PAFFINITY_BITMASK_CPU_MAX && ncpu <= logical_cpu; i++) {
                                if (OPAL_PAFFINITY_CPU_ISSET(i, orte_odls_globals.my_cores)) {
                                    ncpu++;
                                    phys_cpu = i;
                                }
                            }
                            /* if we don't have enough processors, that is an error */
                            if (ncpu <= logical_cpu) {
                                ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-core");
                                orte_show_help("help-odls-default.txt",
                                               "odls-default:not-enough-resources", true,
                                               "processors", orte_process_info.nodename,
                                               "bind-to-core", context->app);
                                ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);                            
                            }
                        } else {
                            /* if we are not bound, then all processors are available
                             * to us, so index into the node's array to get the
                             * physical cpu
                             */
                            phys_cpu = opal_paffinity_base_get_physical_processor_id(logical_cpu);
                            if (OPAL_ERROR == phys_cpu){
                                /* No processor to bind to so error out */
                                ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-core");
                                orte_show_help("help-odls-default.txt",
                                               "odls-default:not-enough-resources", true,
                                               "processors", orte_process_info.nodename,
                                               "bind-to-core", context->app);
                                ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                            } else if (0 > phys_cpu) {
                                ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-core");
                                orte_show_help("help-odls-default.txt",
                                               "odls-default:invalid-phys-cpu", true,
                                               orte_process_info.nodename);
                                ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                            }
                        }
                        OPAL_PAFFINITY_CPU_SET(phys_cpu, mask);
                        /* increment logical cpu */
                        logical_cpu += jobdat->stride;
                    }
                    if (orte_report_bindings) {
                        char tmp1[1024], tmp2[1024];
                        opal_paffinity_base_cset2str(tmp1, sizeof(tmp1),
                                                     &mask);
                        opal_paffinity_base_cset2mapstr(tmp2, sizeof(tmp2),
                                                       &mask);
                        opal_output(0, "MCW rank %d bound to %s: %s",
                                    child->name->vpid, tmp1, tmp2);
                    }
                }
                if (ORTE_SUCCESS != (rc = opal_paffinity_base_set(mask))) {
                    ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-core");
                    orte_show_help("help-odls-default.txt",
                                   "odls-default:failed-set-paff", true);
                    ORTE_ODLS_ERROR_OUT(rc);                            
                }
                paffinity_enabled = true;
            } else if (ORTE_BIND_TO_SOCKET & jobdat->policy) {
                /* bind this proc to a socket */
                OPAL_OUTPUT_VERBOSE((5, orte_odls_globals.output,
                                     "%s odls:default:fork binding child %s to socket",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(child->name)));
                /* layout this process across the sockets based on
                 * the provided mapping policy
                 */
                if (ORTE_LOCAL_RANK_INVALID == (lrank = orte_ess.get_local_rank(child->name))) {
                    orte_show_help("help-odls-default.txt",
                                   "odls-default:invalid-local-rank", true);
                    ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                }
                if (ORTE_MAPPING_NPERXXX & jobdat->policy) {
                    /* we need to balance the children from this job across the available sockets */
                    npersocket = jobdat->npersocket;
                    /* determine the socket to use based on those available */
                    if (npersocket < 2) {
                        /* if we only have 1/sock, or we have less procs than sockets,
                         * then just put it on the lrank socket
                         */
                        logical_skt = lrank;
                    } else if (ORTE_MAPPING_BYSOCKET & jobdat->policy) {
                        logical_skt = lrank % npersocket;
                    } else {
                        logical_skt = lrank / npersocket;
                    }
                    if (orte_odls_globals.bound) {
                        /* if we are bound, use this as an index into our available sockets */
                        for (target_socket=0, n = 0; target_socket < opal_bitmap_size(&orte_odls_globals.sockets) && n < logical_skt; target_socket++) {
                            if (opal_bitmap_is_set_bit(&orte_odls_globals.sockets, target_socket)) {
                                n++;
                            }
                        }
                        /* if we don't have enough sockets, that is an error */
                        if (n < logical_skt) {
                            ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-socket");
                            orte_show_help("help-odls-default.txt",
                                           "odls-default:not-enough-resources", true,
                                           "sockets", orte_process_info.nodename,
                                           "bind-to-socket", context->app);
                            ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                        }
                    } else {
                        target_socket = opal_paffinity_base_get_physical_socket_id(logical_skt);
                        if (ORTE_ERR_NOT_SUPPORTED == target_socket) {
                            /* OS doesn't support providing topology information */
                            ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-socket");
                            orte_show_help("help-odls-default.txt",
                                           "odls-default:topo-not-supported", 
                                           true, orte_process_info.nodename,  "bind-to-socket", "",
                                           context->app);
                            ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                        }
                    }
                    OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                         "%s odls:default:fork child %s local rank %d npersocket %d logical socket %d target socket %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(child->name), lrank,
                                         npersocket, logical_skt, target_socket));
                } else if (ORTE_MAPPING_BYSOCKET & jobdat->policy) {
                    /* this corresponds to a mapping policy where
                     * local rank 0 goes on socket 0, and local
                     * rank 1 goes on socket 1, etc. - round robin
                     * until all ranks are mapped
                     *
                     * NOTE: we already know our number of sockets
                     * from when we initialized
                     */
                    target_socket = opal_paffinity_base_get_physical_socket_id(lrank % orte_odls_globals.num_sockets);
                    if (ORTE_ERR_NOT_SUPPORTED == target_socket) {
                        /* OS does not support providing topology information */
                        ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-socket");
                        orte_show_help("help-odls-default.txt",
                                       "odls-default:topo-not-supported", 
                                       true, orte_process_info.nodename,  "bind-to-socket", "",
                                       context->app);
                        ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                    }
                    OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                         "bysocket lrank %d numsocks %d logical socket %d target socket %d", (int)lrank,
                                         (int)orte_odls_globals.num_sockets,
                                         (int)(lrank % orte_odls_globals.num_sockets),
                                         target_socket));
                } else {
                    /* use a byslot-like policy where local rank 0 goes on
                     * socket 0, and local rank 1 goes on socket 0, etc.
                     * following round-robin until all ranks mapped
                     */
                    if (orte_odls_globals.bound) {
                        /* if we are bound, then we compute the logical socket id
                         * based on the number of available cores in each socket so
                         * that each rank gets its own core, adjusting for the cpus_per_task
                         */
                        /* Find the lrank available core, accounting for cpus_per_task */
                        logical_cpu = lrank * jobdat->cpus_per_rank;
                        /* use the logical_cpu as an index against our available cores */
                        ncpu = 0;
                        for (i=0; i < orte_odls_globals.num_processors && ncpu <= logical_cpu; i++) {
                            if (OPAL_PAFFINITY_CPU_ISSET(i, orte_odls_globals.my_cores)) {
                                ncpu++;
                                phys_cpu = i;
                            }
                        }
                        /* if we don't have enough processors, that is an error */
                        if (ncpu < logical_cpu) {
                            ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-socket");
                            orte_show_help("help-odls-default.txt",
                                           "odls-default:not-enough-resources", true,
                                           "processors", orte_process_info.nodename,
                                           "bind-to-socket", context->app);
                            ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                        }
                        /* get the physical socket of that cpu */
                        if (ORTE_SUCCESS != opal_paffinity_base_get_map_to_socket_core(phys_cpu, &target_socket, &phys_core)) {
                            if (ORTE_BINDING_NOT_REQUIRED(jobdat->policy)) {
                                goto LAUNCH_PROCS;
                            }
                            orte_show_help("help-odls-default.txt",
                                           "odls-default:topo-not-supported", 
                                           true, orte_process_info.nodename,  "bind-to-socket", "",
                                           context->app);
                            ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                        }
                    } else {
                        /* if we are not bound, then just use all sockets */
                        if (1 == orte_odls_globals.num_sockets) {
                            /* if we only have one socket, then just put it there */
                            target_socket = opal_paffinity_base_get_physical_socket_id(0);
                            if (ORTE_ERR_NOT_SUPPORTED == target_socket) {
                                /* OS doesn't support providing topology information */
                                ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-socket");
                                orte_show_help("help-odls-default.txt",
                                               "odls-default:topo-not-supported", 
                                               true, orte_process_info.nodename,  "bind-to-socket", "",
                                               context->app);
                                ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                            }
                        } else {
                            /* compute the logical socket, compensating for the number of cpus_per_rank */
                            logical_skt = lrank / (orte_odls_globals.num_cores_per_socket / jobdat->cpus_per_rank);
                            /* wrap that around the number of sockets so we round-robin */
                            logical_skt = logical_skt % orte_odls_globals.num_sockets;
                            /* now get the target physical socket */
                            target_socket = opal_paffinity_base_get_physical_socket_id(logical_skt);
                            if (ORTE_ERR_NOT_SUPPORTED == target_socket) {
                                /* OS doesn't support providing topology information */
                                ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-socket");
                                orte_show_help("help-odls-default.txt",
                                               "odls-default:topo-not-supported", 
                                               true, orte_process_info.nodename,  "bind-to-socket", "",
                                               context->app);
                                ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                            }
                        }
                        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                             "byslot lrank %d socket %d", (int)lrank, target_socket));
                    }
                }
                
                OPAL_PAFFINITY_CPU_ZERO(mask);
                
                for (n=0; n < orte_odls_globals.num_cores_per_socket; n++) {
                    /* get the physical core within this target socket */
                    phys_core = opal_paffinity_base_get_physical_core_id(target_socket, n);
                    if (0 > phys_core) {
                        ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-socket");
                        orte_show_help("help-odls-default.txt",
                                       "odls-default:invalid-phys-cpu", true,
                                       orte_process_info.nodename);
                        ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                    }
                    /* map this to a physical cpu on this node */
                    if (ORTE_SUCCESS != opal_paffinity_base_get_map_to_processor_id(target_socket, phys_core, &phys_cpu)) {
                        ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-socket");
                        orte_show_help("help-odls-default.txt",
                                       "odls-default:invalid-phys-cpu", true,
                                       orte_process_info.nodename);
                        ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                    }
                    /* are we bound? */
                    if (orte_odls_globals.bound) {
                        /* see if this physical cpu is available to us */
                        if (!OPAL_PAFFINITY_CPU_ISSET(phys_cpu, orte_odls_globals.my_cores)) {
                            /* no it isn't - skip it */
                            continue;
                        }
                    }
                    OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                         "%s odls:default:fork mapping phys socket %d core %d to phys_cpu %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         target_socket, phys_core, phys_cpu));
                    OPAL_PAFFINITY_CPU_SET(phys_cpu, mask);
                }
                /* if we did not bind it anywhere, then that is an error */
                OPAL_PAFFINITY_PROCESS_IS_BOUND(mask, &bound);
                if (!bound) {
                    /* If failed to bind, check to see if the reason was
                       just because we only have 1 socket.  If so,
                       that's not an error. */
                    int num_sockets;
                    if (OPAL_SUCCESS != opal_paffinity_base_get_socket_info(&num_sockets) ||
                        num_sockets > 1) {
                        orte_show_help("help-odls-default.txt",
                                       "odls-default:could-not-bind-to-socket", true,
                                       target_socket, orte_process_info.nodename);
                        ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
                    }
                }
                if (orte_report_bindings) {
                    char tmp1[1024], tmp2[1024];
                    opal_paffinity_base_cset2str(tmp1, sizeof(tmp1), &mask);
                    opal_paffinity_base_cset2mapstr(tmp2, sizeof(tmp2), &mask);
                    opal_output(0, "MCW rank %d bound to %s: %s",
                                child->name->vpid, tmp1, tmp2);
                }
                if (ORTE_SUCCESS != (rc = opal_paffinity_base_set(mask))) {
                    ORTE_ODLS_IF_BIND_NOT_REQD("bind-to-socket");
                    orte_show_help("help-odls-default.txt",
                                   "odls-default:failed-set-paff", true);
                    ORTE_ODLS_ERROR_OUT(rc);
                }
                paffinity_enabled = true;
            }
            
            /* If we were able to set processor affinity, try setting up
             * memory affinity
             */
            if (paffinity_enabled) {
                if (OPAL_SUCCESS == opal_maffinity_base_open() &&
                    OPAL_SUCCESS == opal_maffinity_base_select()) {
                    opal_maffinity_setup = true;
                }
            }
            
        } else if (!(ORTE_JOB_CONTROL_FORWARD_OUTPUT & jobdat->controls)) {
            /* tie stdin/out/err/internal to /dev/null */
            int fdnull;
            for (i=0; i < 3; i++) {
                fdnull = open("/dev/null", O_RDONLY, 0);
                if(fdnull > i) {
                    dup2(fdnull, i);
                }
                close(fdnull);
            }
            fdnull = open("/dev/null", O_RDONLY, 0);
            if(fdnull > opts.p_internal[1]) {
                dup2(fdnull, opts.p_internal[1]);
            }
            close(fdnull);
        }
        
LAUNCH_PROCS:
        /* if we are bound, report it */
        if (opal_paffinity_base_bound) {
            param = mca_base_param_environ_variable("paffinity","base","bound");
            opal_setenv(param, "1", true, &environ_copy);
            free (param);
            /* and provide a char representation of what we did */
            tmp = opal_paffinity_base_print_binding(mask);
            if (NULL != tmp) {
                param = mca_base_param_environ_variable("paffinity","base","applied_binding");
                opal_setenv(param, tmp, true, &environ_copy);
                free(tmp);
            }

            /* v1.6-specific comment: if OMPI is configured
               --without-hwloc, the hwloc base function called below
               will not exist (!).  Hence, we have to protect this
               call with the OPAL_HAVE_HWLOC macro.  

               That being said, I do not believe that this code path
               will ever be called if there's no hwloc (i.e., we'll
               fail earlier and this affinity-related block of code
               won't be invoked).  But we still have to protect linker
               semantics, lest this module fail because it can't find
               a symbol at linker time. */
#if OPAL_HAVE_HWLOC
            /* Also set the memory affininty policy */
            opal_hwloc_base_set_process_membind_policy();
#endif
        }

        /* close all file descriptors w/ exception of
         * stdin/stdout/stderr and the pipe used for the IOF INTERNAL
         * messages
         */
        for(fd=3; fd<fdmax; fd++) {
            if (fd != opts.p_internal[1]) {
                close(fd);
            }
        }
        
        if (context->argv == NULL) {
            context->argv = malloc(sizeof(char*)*2);
            context->argv[0] = strdup(context->app);
            context->argv[1] = NULL;
        }
        
        /* Set signal handlers back to the default.  Do this close to
         the exev() because the event library may (and likely will)
         reset them.  If we don't do this, the event library may
         have left some set that, at least on some OS's, don't get
         reset via fork() or exec().  Hence, the launched process
         could be unkillable (for example). */
        
        set_handler_default(SIGTERM);
        set_handler_default(SIGINT);
        set_handler_default(SIGHUP);
        set_handler_default(SIGPIPE);
        set_handler_default(SIGCHLD);
        
        /* Unblock all signals, for many of the same reasons that we
         set the default handlers, above.  This is noticable on
         Linux where the event library blocks SIGTERM, but we don't
         want that blocked by the launched process. */
        sigprocmask(0, 0, &sigs);
        sigprocmask(SIG_UNBLOCK, &sigs, 0);
        
        /* Exec the new executable */
        
        execve(context->app, context->argv, environ_copy);
        ORTE_ODLS_ERROR_OUT(ORTE_ERR_FATAL);
    } else {

        if (NULL != child && (ORTE_JOB_CONTROL_FORWARD_OUTPUT & jobdat->controls)) {
            /* connect endpoints IOF */
            rc = orte_iof_base_setup_parent(child->name, &opts);
            if(ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* Wait to read something from the pipe or close */
        close(p[1]);
        while (1) {
            rc = read(p[0], &i, sizeof(int));
            if (rc < 0) {
                /* Signal interrupts are ok */
                if (errno == EINTR) {
                    continue;
                }
                
                /* Other errno's are bad */
                if (NULL != child) {
                    child->state = ORTE_PROC_STATE_FAILED_TO_START;
                    child->exit_code = ORTE_ERR_PIPE_READ_FAILURE;
                }
                
                OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                     "%s odls:default:fork got code %d back from child",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), i));
                close(p[0]);
                return ORTE_ERR_PIPE_READ_FAILURE;
            } else if (0 == rc) {
                /* Child was successful in exec'ing! */
                break;
            } else {
                /*  Doh -- child failed.
                    Let the calling function
                    know about the failure.  The actual exit status of child proc
                    cannot be found here - all we can do is report the ORTE error
                    code that was reported back to us. The calling func needs to report the
                    failure to launch this process through the SMR or else
                    everyone else will hang.
                */
                if (NULL != child) {
                    child->state = ORTE_PROC_STATE_FAILED_TO_START;
                    child->exit_code = i;
                }
                
                OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                                     "%s odls:default:fork got code %d back from child",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), i));
                close(p[0]);
                return ORTE_ERR_FAILED_TO_START;
            }
        }

        if (NULL != child) {
            /* set the proc state to LAUNCHED */
            child->state = ORTE_PROC_STATE_LAUNCHED;
            child->alive = true;
        }
        close(p[0]);
    }
    
    return ORTE_SUCCESS;
}


/**
 * Launch all processes allocated to the current node.
 */

int orte_odls_default_launch_local_procs(opal_buffer_t *data)
{
    int rc;
    orte_jobid_t job;
    orte_job_t *jdata;

    /* construct the list of children we are to launch */
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_construct_child_list(data, &job))) {
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:default:launch:local failed to construct child list on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_ERROR_NAME(rc)));
        goto CLEANUP;
    }
    
    /* launch the local procs */
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_launch_local(job, odls_default_fork_local_proc))) {
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:default:launch:local failed to launch on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_ERROR_NAME(rc)));
        goto CLEANUP;
    }
    
    /* look up job data object */
    if (NULL != (jdata = orte_get_job_data_object(job))) {
        if (jdata->state & ORTE_JOB_STATE_SUSPENDED) {
            if (ORTE_PROC_IS_HNP) {
                /* Have the plm send the signal to all the nodes.
                   If the signal arrived before the orteds started,
                   then they won't know to suspend their procs.
                   The plm also arranges for any local procs to
                   be signaled.
                 */
                orte_plm.signal_job(jdata->jobid, SIGTSTP);
            } else {
                orte_odls_default_signal_local_procs(NULL, SIGTSTP);
            }
        }
    }

CLEANUP:
   
    return rc;
}


static void set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);

    sigaction(sig, &act, (struct sigaction *)0);
}

/**
 * Send a sigal to a pid.  Note that if we get an error, we set the
 * return value and let the upper layer print out the message.  
 */
static int send_signal(pid_t pid, int signal)
{
    int rc = ORTE_SUCCESS;
    
    OPAL_OUTPUT_VERBOSE((1, orte_odls_globals.output,
                         "%s sending signal %d to pid %ld",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         signal, (long)pid));

    if (orte_forward_job_control) {
        /* Send the signal to the process group rather than the
           process.  The child is the leader of its process group. */
        pid = -pid;
    }
    if (kill(pid, signal) != 0) {
        switch(errno) {
            case EINVAL:
                rc = ORTE_ERR_BAD_PARAM;
                break;
            case ESRCH:
                /* This case can occur when we deliver a signal to a
                   process that is no longer there.  This can happen if
                   we deliver a signal while the job is shutting down. 
                   This does not indicate a real problem, so just 
                   ignore the error.  */
                break;
            case EPERM:
                rc = ORTE_ERR_PERM;
                break;
            default:
                rc = ORTE_ERROR;
        }
    }
    
    return rc;
}

static int orte_odls_default_signal_local_procs(const orte_process_name_t *proc, int32_t signal)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_signal_local_procs(proc, signal, send_signal))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}
