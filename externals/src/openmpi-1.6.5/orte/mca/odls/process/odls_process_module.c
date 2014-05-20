/*
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#include <sys/stat.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#include <signal.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */

#include "orte/util/show_help.h"
#include "opal/util/sys_limits.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/iof/base/iof_base_setup.h"
#include "orte/util/name_fns.h"

#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/odls/process/odls_process.h"

static void set_handler_default(int sig);


static bool odls_process_child_died( pid_t pid, unsigned int timeout,
									 int* exit_status )
{
    int error;
    HANDLE handle = OpenProcess( PROCESS_TERMINATE | SYNCHRONIZE, FALSE,
                                 (DWORD)pid );
    if( 0 == pid || INVALID_HANDLE_VALUE == handle ) {
        error = GetLastError();
        /* Let's suppose that the process dissapear ... by now */
        return true;
    }
    CloseHandle(handle);
    /* The child didn't die, so return false */
    return false;
}

static int odls_process_kill_local( pid_t pid, int sig_num )
{
    if( 0 != pid && false == TerminateProcess( (HANDLE)pid, 1 ) ) {
        return (int)GetLastError();
    }
    return 0;
}

static int odls_process_kill_local_procs(opal_pointer_array_t *procs, bool set_state)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_kill_local_procs(procs, set_state,
                                    odls_process_kill_local, odls_process_child_died))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;

}


/**
 *  Fork/exec the specified processes
 */

static int odls_process_fork_local_proc(orte_app_context_t* context,
                                        orte_odls_child_t *child,
                                        char **environ_copy,
                                        orte_odls_job_t *jobdat)
{
    pid_t pid;
    orte_iof_base_io_conf_t opts;
    int rc;
    int i = 0;

    /* check the system limits - if we are at our max allowed children, then
     * we won't be allowed to do this anyway, so we may as well abort now.
     * According to the documentation, num_procs = 0 is equivalent to
     * to no limit, so treat it as unlimited here.
     */
    if (opal_sys_limits.initialized) {
        if (0 < opal_sys_limits.num_procs &&
            opal_sys_limits.num_procs <= (int)opal_list_get_size(&orte_local_children)) {
            /* at the system limit - abort */
            ORTE_ERROR_LOG(ORTE_ERR_SYS_LIMITS_CHILDREN);
            child->state = ORTE_PROC_STATE_FAILED_TO_START;
            child->exit_code = ORTE_ERR_SYS_LIMITS_CHILDREN;
            return ORTE_ERR_SYS_LIMITS_CHILDREN;
        }
    }

    /* should pull this information from MPIRUN instead of going with
       default */
    opts.usepty = OPAL_ENABLE_PTY_SUPPORT;

    /* do we want to setup stdin? */
    if (jobdat->stdin_target == ORTE_VPID_WILDCARD || child->name->vpid == jobdat->stdin_target) {
        opts.connect_stdin = true;
    } else {
        opts.connect_stdin = false;
    }

    if (ORTE_SUCCESS != (rc = orte_iof_base_setup_prefork(&opts))) {
        ORTE_ERROR_LOG(rc);
        child->state = ORTE_PROC_STATE_FAILED_TO_START;
        child->exit_code = rc;
        return rc;
    }

#if 0
    /* connect endpoints IOF */
    rc = orte_iof_base_setup_parent(child->name, &opts);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
#endif
    /* Flush all standard handles (stdin, stdout & stderr). */
    _flushall();

    {
        intptr_t handle = _spawnve( _P_NOWAIT, context->app, context->argv, environ_copy );

        if( -1 == handle ) {
            child->state = ORTE_PROC_STATE_FAILED_TO_START;
            child->exit_code = ORTE_ERR_PIPE_READ_FAILURE;
            orte_show_help("help-orted-launcher.txt", "orted-launcher:execv-error",
                           true, context->app, "TODO: some error");
            child->state = ORTE_PROC_STATE_ABORTED;
            return ORTE_ERROR;
        }
        pid = handle;
    }

    /* set the proc state to LAUNCHED and save the pid */
    child->state = ORTE_PROC_STATE_LAUNCHED;
    child->pid = pid;
    child->alive = true;
        
    /* Windows automatically forwards IO, so we don't need to do so here. However,
     * we need to flag that IO termination conditions are met so that the daemon
     * knows the proc is done
     */
    orte_odls_base_notify_iof_complete(child->name);
    
    return ORTE_SUCCESS;
}


/**
 * Launch all processes allocated to the current node.
 */

static int odls_process_launch_local_procs(opal_buffer_t *data)
{
    int rc;
    orte_jobid_t job;
    
    /* construct the list of children we are to launch */
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_construct_child_list(data, &job))) {
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:process:launch:local failed to construct child list on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_ERROR_NAME(rc)));
        goto CLEANUP;
    }
    
    /* launch the local procs */
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_launch_local(job, odls_process_fork_local_proc))) {
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:process:launch:local failed to launch on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_ERROR_NAME(rc)));
        goto CLEANUP;
    }
    
CLEANUP:

    return rc;
}

static int send_signal(pid_t pid, int signal)
{
    return ORTE_ERROR;
}

static int odls_process_signal_local_proc(const orte_process_name_t *proc, int32_t signal)
{
    int rc;
	
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_signal_local_procs(proc, signal, send_signal))) {
        ORTE_ERROR_LOG(rc);
    }
	return rc;
}


orte_odls_base_module_t orte_odls_process_module = {
    orte_odls_base_default_get_add_procs_data,    
    odls_process_launch_local_procs,
    odls_process_kill_local_procs,
    odls_process_signal_local_proc,
    orte_odls_base_default_deliver_message,
    orte_odls_base_default_require_sync
};
