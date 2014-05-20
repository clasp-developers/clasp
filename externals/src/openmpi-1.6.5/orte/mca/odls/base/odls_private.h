/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Oracle and/or its affiliates. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_ODLS_PRIVATE_H
#define MCA_ODLS_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/types.h"

#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_bitmap.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"
#include "opal/dss/dss_types.h"
#include "opal/mca/paffinity/paffinity.h"

#include "orte/mca/grpcomm/grpcomm_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/odls/odls_types.h"

BEGIN_C_DECLS

/*
 * General ODLS types
 */

typedef struct {
    /** Verbose/debug output stream */
    int output;
    /** Time to allow process to forcibly die */
    int timeout_before_sigkill;
    /* mutex */
    opal_mutex_t mutex;
    /* condition variable */
    opal_condition_t cond;
    /* byte object to store daemon map for later xmit to procs */
    opal_byte_object_t *dmap;
    /* any co-spawned debugger daemon */
    orte_odls_job_t *debugger;
    /* debugger launched */
    bool debugger_launched;
    /* list of ranks to be displayed on separate xterms */
    opal_list_t xterm_ranks;
    /* the xterm cmd to be used */
    char **xtermcmd;
    /* any externally provided bindings */
    opal_paffinity_base_cpu_set_t my_cores;
    /* flag whether or not we are bound */
    bool bound; 
    /* local number of processors */
    int num_processors;
    /* map of locally available sockets
     * as determined by external bindings
     */
    opal_bitmap_t sockets;
    /* number of sockets available to us */
    int num_sockets;
    /* number of cores/socket - assumes homogeneity */
    int num_cores_per_socket;
    /* system capabilities */
    opal_list_t sysinfo;
} orte_odls_globals_t;

ORTE_DECLSPEC extern orte_odls_globals_t orte_odls_globals;


/*
 * Default functions that are common to most environments - can
 * be overridden by specific environments if they need something
 * different (e.g., bproc)
 */
ORTE_DECLSPEC int
orte_odls_base_default_get_add_procs_data(opal_buffer_t *data,
                                          orte_jobid_t job);

ORTE_DECLSPEC int
orte_odls_base_default_update_daemon_info(opal_buffer_t *data);

ORTE_DECLSPEC int
orte_odls_base_default_construct_child_list(opal_buffer_t *data,
                                            orte_jobid_t *job);

/* define a function that will fork a local proc */
typedef int (*orte_odls_base_fork_local_proc_fn_t)(orte_app_context_t *context,
                                                   orte_odls_child_t *child,
                                                   char **environ_copy,
                                                   orte_odls_job_t *jobdat);

ORTE_DECLSPEC int
orte_odls_base_default_launch_local(orte_jobid_t job,
                                    orte_odls_base_fork_local_proc_fn_t fork_local);

ORTE_DECLSPEC int
orte_odls_base_default_deliver_message(orte_jobid_t job, opal_buffer_t *buffer, orte_rml_tag_t tag);

ORTE_DECLSPEC void odls_base_default_wait_local_proc(pid_t pid, int status, void* cbdata);

/* define a function type to signal a local proc */
typedef int (*orte_odls_base_signal_local_fn_t)(pid_t pid, int signum);

ORTE_DECLSPEC int
orte_odls_base_default_signal_local_procs(const orte_process_name_t *proc, int32_t signal,
                                          orte_odls_base_signal_local_fn_t signal_local);

/* define a function type for killing a local proc */
typedef int (*orte_odls_base_kill_local_fn_t)(pid_t pid, int signum);

/* define a function type to detect that a child died */
typedef bool (*orte_odls_base_child_died_fn_t)(pid_t pid, unsigned int timeout, int *exit_status);

ORTE_DECLSPEC int
orte_odls_base_default_kill_local_procs(opal_pointer_array_t *procs, bool set_state,
                                        orte_odls_base_kill_local_fn_t kill_local,
                                        orte_odls_base_child_died_fn_t child_died);

ORTE_DECLSPEC int orte_odls_base_default_require_sync(orte_process_name_t *proc,
                                                      opal_buffer_t *buffer,
                                                      bool drop_nidmap);

/*
 * Preload binary/files functions
 */
ORTE_DECLSPEC int orte_odls_base_preload_files_app_context(orte_app_context_t* context);

/*
 * Obtain process stats on a child proc
 */
ORTE_DECLSPEC int orte_odls_base_get_proc_stats(opal_buffer_t *answer, orte_process_name_t *proc);

END_C_DECLS

#endif
