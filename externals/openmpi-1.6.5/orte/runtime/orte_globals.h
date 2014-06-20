/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c)      2011 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Global params for OpenRTE
 */
#ifndef ORTE_RUNTIME_ORTE_GLOBALS_H
#define ORTE_RUNTIME_ORTE_GLOBALS_H

#include "orte_config.h"
#include "orte/types.h"

#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/class/opal_pointer_array.h"
#include "opal/class/opal_value_array.h"

#include "orte/mca/plm/plm_types.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"


BEGIN_C_DECLS

ORTE_DECLSPEC extern int orte_debug_verbosity;  /* instantiated in orte/runtime/orte_init.c */
ORTE_DECLSPEC extern char *orte_prohibited_session_dirs;  /* instantiated in orte/runtime/orte_init.c */
ORTE_DECLSPEC extern bool orte_xml_output;  /* instantiated in orte/runtime/orte_globals.c */
ORTE_DECLSPEC extern FILE *orte_xml_fp;   /* instantiated in orte/runtime/orte_globals.c */
ORTE_DECLSPEC extern bool orte_help_want_aggregate;  /* instantiated in orte/util/show_help.c */
ORTE_DECLSPEC extern char *orte_job_ident;  /* instantiated in orte/runtime/orte_globals.c */

/* Shortcut for some commonly used names */
#define ORTE_NAME_WILDCARD      (&orte_name_wildcard)
ORTE_DECLSPEC extern orte_process_name_t orte_name_wildcard;  /** instantiated in orte/runtime/orte_init.c */
#define ORTE_NAME_INVALID       (&orte_name_invalid)
ORTE_DECLSPEC extern orte_process_name_t orte_name_invalid;  /** instantiated in orte/runtime/orte_init.c */

#define ORTE_PROC_MY_NAME       (&orte_process_info.my_name)

/* define a special name that belongs to orterun */
#define ORTE_PROC_MY_HNP        (&orte_process_info.my_hnp)

/* define the name of my daemon */
#define ORTE_PROC_MY_DAEMON     (&orte_process_info.my_daemon)

/* See comment in orte/tools/orterun.c about this MCA
   param */
ORTE_DECLSPEC extern bool orte_in_parallel_debugger;

/* error manager callback function */
typedef void (*orte_err_cb_fn_t)(orte_process_name_t *proc, orte_proc_state_t state, void *cbdata);

typedef uint16_t orte_mapping_policy_t;

#if ORTE_DISABLE_FULL_SUPPORT

/* These types are used in interface functions that should never be
   used or implemented in the non-full interface, but need to be
   declared for various reasons.  So have a dummy type to keep things
   simple (and throw an error if someone does try to use them) */
struct orte_job_t;
typedef struct orte_job_t orte_job_t;

#else

#define ORTE_GLOBAL_ARRAY_BLOCK_SIZE    64
#define ORTE_GLOBAL_ARRAY_MAX_SIZE      INT_MAX

/* define a default error return code for ORTE */
#define ORTE_ERROR_DEFAULT_EXIT_CODE    1

/**
 * Define a macro for updating the orte_exit_status
 * The macro provides a convenient way of doing this
 * so that we can add thread locking at some point
 * since the orte_exit_status is a global variable.
 *
 * Ensure that we do not overwrite the exit status if it has
 * already been set to some non-zero value. If we don't make
 * this check, then different parts of the code could overwrite
 * each other's exit status in the case of abnormal termination.
 *
 * For example, if a process aborts, we would record the initial
 * exit code from the aborted process. However, subsequent processes
 * will have been aborted by signal as we kill the job. We don't want
 * the subsequent processes to overwrite the original exit code so
 * we can tell the user the exit code from the process that caused
 * the whole thing to happen.
 */
#define ORTE_UPDATE_EXIT_STATUS(newstatus)                                  \
    do {                                                                    \
        if (0 == orte_exit_status && 0 != newstatus) {                      \
            OPAL_OUTPUT_VERBOSE((1, orte_debug_output,                      \
                                 "%s:%s(%d) updating exit status to %d",    \
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),        \
                                 __FILE__, __LINE__, newstatus));           \
            orte_exit_status = newstatus;                                   \
        }                                                                   \
    } while(0);

/* sometimes we need to reset the exit status - for example, when we
 * are restarting a failed process
 */
#define ORTE_RESET_EXIT_STATUS()                                \
    do {                                                        \
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,              \
                            "%s:%s(%d) reseting exit status",   \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), \
                            __FILE__, __LINE__));               \
        orte_exit_status = 0;                                   \
    } while(0);


/* define a macro for computing time differences - used for timing tests
 * across the code base
 */
#define ORTE_COMPUTE_TIME_DIFF(r, ur, s1, us1, s2, us2)     \
    do {                                                    \
        (r) = (s2) - (s1);                                  \
        if ((us2) >= (us1)) {                               \
            (ur) = (us2) - (us1);                           \
        } else {                                            \
            (r)--;                                          \
            (ur) = 1000000 - (us1) + (us2);                 \
        }                                                   \
    } while(0);

        
/* global type definitions used by RTE - instanced in orte_globals.c */

/************
* Declare this to allow us to use it before fully
* defining it - resolves potential circular definition
*/
struct orte_proc_t;
struct orte_job_map_t;
/************/

/**
* Information about a specific application to be launched in the RTE.
 */
typedef struct {
    /** Parent object */
    opal_object_t super;
    /** Unique index when multiple apps per job */
    int32_t idx;
    /** Absolute pathname of argv[0] */
    char   *app;
    /** Number of copies of this process that are to be launched */
    orte_std_cntr_t num_procs;
    /** Standard argv-style array, including a final NULL pointer */
    char  **argv;
    /** Standard environ-style array, including a final NULL pointer */
    char  **env;
    /** Current working directory for this app */
    char   *cwd;
    /** Whether the cwd was set by the user or by the system */
    bool user_specified_cwd;
    /* Any hostfile that was specified */
    char *hostfile;
    /* Hostfile for adding hosts to an existing allocation */
    char *add_hostfile;
    /* Hosts to be added to an existing allocation - analagous to -host */
    char **add_host;
    /** argv of hosts passed in to -host */
    char ** dash_host;
    /** Prefix directory for this app (or NULL if no override necessary) */
    char *prefix_dir;
    /** Preload the binary on the remote machine (in PLM via FileM) */
    bool preload_binary;
    /** Preload the libraries on the remote machine (in PLM via FileM) */
    bool preload_libs;
    /** Preload the comma separated list of files to the remote machines cwd */
    char * preload_files;
    /** Destination directory for the preloaded files 
        * If NULL then the absolute and relative paths are obeyed */
    char *preload_files_dest_dir;
    /** Source directory for the preloaded files 
     * If NULL then the absolute and relative paths are obeyed */
    char *preload_files_src_dir;
    /* is being used on the local node */
    bool used_on_node;
} orte_app_context_t;

ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_app_context_t);


typedef struct {
    /** Base object so this can be put on a list */
    opal_list_item_t super;
    /* index of this node object in global array */
    orte_std_cntr_t index;
    /** String node name */
    char *name;
    /* argv-like array of aliases for this node */
    char **alias;
    /* daemon on this node */
    struct orte_proc_t *daemon;
    /* whether or not this daemon has been launched */
    bool daemon_launched;
    /** Launch id - needed by some systems to launch a proc on this node */
    int32_t launch_id;
    /** number of procs on this node */
    orte_vpid_t num_procs;
    /* array of pointers to procs on this node */
    opal_pointer_array_t *procs;
    /* next node rank on this node */
    orte_node_rank_t next_node_rank;
    /* whether or not we are oversubscribed */
    bool oversubscribed;
    /** State of this node */
    orte_node_state_t state;
    /** A "soft" limit on the number of slots available on the node.
        This will typically correspond to the number of physical CPUs
        that we have been allocated on this note and would be the
        "ideal" number of processes for us to launch. */
    orte_std_cntr_t slots;
    /** How many processes have already been launched, used by one or
        more jobs on this node. */
    orte_std_cntr_t slots_inuse;
    /** This represents the number of slots we (the allocator) are
        attempting to allocate to the current job - or the number of
        slots allocated to a specific job on a query for the jobs
        allocations */
    orte_std_cntr_t slots_alloc;
    /** A "hard" limit (if set -- a value of 0 implies no hard limit)
        on the number of slots that can be allocated on a given
        node. This is for some environments (e.g. grid) there may be
        fixed limits on the number of slots that can be used.
        
        This value also could have been a boolean - but we may want to
        allow the hard limit be different than the soft limit - in
        other words allow the node to be oversubscribed up to a
        specified limit.  For example, if we have two processors, we
        may want to allow up to four processes but no more. */
    orte_std_cntr_t slots_max;
    /* number of physical boards in the node - defaults to 1 */
    uint8_t boards;
    /* number of sockets on each board - defaults to 1 */
    uint8_t sockets_per_board;
    /* number of cores per socket - defaults to 1 */
    uint8_t cores_per_socket;
    /** Username on this node, if specified */
    char *username;
    /* list of known system resources for this node */
    opal_list_t resources;
} orte_node_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_node_t);

/* define a set of flags to control the launch of a job */
typedef uint8_t orte_job_controls_t;
#define ORTE_JOB_CONTROL    OPAL_UINT8

#define ORTE_JOB_CONTROL_LOCAL_SLAVE        0x01
#define ORTE_JOB_CONTROL_NON_ORTE_JOB       0x02
#define ORTE_JOB_CONTROL_DEBUGGER_DAEMON    0x04
#define ORTE_JOB_CONTROL_FORWARD_OUTPUT     0x08
#define ORTE_JOB_CONTROL_DO_NOT_MONITOR     0x10
#define ORTE_JOB_CONTROL_FORWARD_COMM       0x20
#define ORTE_JOB_CONTROL_CONTINUOUS_OP      0x40

#define ORTE_MAPPING_POLICY OPAL_UINT16
/* put the rank assignment method in the upper 8 bits */
#define ORTE_MAPPING_NOPOL          0x0100
#define ORTE_MAPPING_BYNODE         0x0200
#define ORTE_MAPPING_BYSLOT         0x0400
#define ORTE_MAPPING_BYSOCKET       0x0800
#define ORTE_MAPPING_BYBOARD        0x1000
#define ORTE_MAPPING_NO_USE_LOCAL   0x2000
#define ORTE_MAPPING_NPERXXX        0x4000
#define ORTE_MAPPING_BYUSER         0x8000
/* nice macro for setting these */
#define ORTE_SET_MAPPING_POLICY(pol) \
    orte_default_mapping_policy = (orte_default_mapping_policy & 0x00ff) | (pol);
/* macro to detect if some other policy has been set */
#define ORTE_XSET_MAPPING_POLICY(pol)                           \
    do {                                                        \
        orte_mapping_policy_t tmp;                              \
        tmp = (orte_default_mapping_policy & 0xff00) & ~(pol);  \
        if (0 == tmp) {                                         \
            ORTE_SET_MAPPING_POLICY((pol));                     \
        }                                                       \
    } while(0);
/* macro to add another mapping policy */
#define ORTE_ADD_MAPPING_POLICY(pol) \
    orte_default_mapping_policy |= (pol);

/* put the binding policy in the lower 8 bits, using the paffinity values */
#define ORTE_BIND_TO_NONE           (uint16_t)OPAL_PAFFINITY_DO_NOT_BIND
#define ORTE_BIND_TO_CORE           (uint16_t)OPAL_PAFFINITY_BIND_TO_CORE
#define ORTE_BIND_TO_SOCKET         (uint16_t)OPAL_PAFFINITY_BIND_TO_SOCKET
#define ORTE_BIND_TO_BOARD          (uint16_t)OPAL_PAFFINITY_BIND_TO_BOARD
#define ORTE_BIND_IF_SUPPORTED      (uint16_t)OPAL_PAFFINITY_BIND_IF_SUPPORTED
/* nice macro for setting these */
#define ORTE_SET_BINDING_POLICY(pol) \
    orte_default_mapping_policy = (orte_default_mapping_policy & 0xff00) | (pol);
/* macro to detect if some other policy has been set */
#define ORTE_XSET_BINDING_POLICY(pol)                           \
    do {                                                        \
        orte_mapping_policy_t tmp;                              \
        tmp = (orte_default_mapping_policy & 0x00ff) & ~(pol);  \
        if (0 == tmp) {                                         \
            ORTE_SET_BINDING_POLICY((pol));                     \
        }                                                       \
    } while(0);
/* macro to detect if binding was qualified */
#define ORTE_BINDING_NOT_REQUIRED(n) \
    (ORTE_BIND_IF_SUPPORTED & (n))

typedef struct {
    /** Base object so this can be put on a list */
    opal_list_item_t super;
    /* jobid for this job */
    orte_jobid_t jobid;
    /* app_context array for this job */
    opal_pointer_array_t *apps;
    /* number of app_contexts in the array */
    int32_t num_apps;
    /* flags to control the launch of this job - see above
     * for description of supported flags
     */
    orte_job_controls_t controls;
    /* rank desiring stdin - for now, either one rank, all ranks
     * (wildcard), or none (invalid)
     */
    orte_vpid_t stdin_target;
    /* total slots allocated to this job */
    orte_std_cntr_t total_slots_alloc;
    /* number of procs in this job */
    orte_vpid_t num_procs;
    /* array of pointers to procs in this job */
    opal_pointer_array_t *procs;
    /* map of the job */
    struct orte_job_map_t *map;
    /* bookmark for where we are in mapping - this
     * indicates the node where we stopped
     */
    orte_node_t *bookmark;
    /** Whether or not to override oversubscription based on local
     *  hardware - used to indicate uncertainty in number of
     *  actual processors available on this node
     */
    bool oversubscribe_override;
    /* state of the overall job */
    orte_job_state_t state;
    /* number of procs launched */
    orte_vpid_t num_launched;
    /* number of procs reporting contact info */
    orte_vpid_t num_reported;
    /* number of procs terminated */
    orte_vpid_t num_terminated;
    /* did this job abort? */
    bool abort;
    /* proc that caused that to happen */
    struct orte_proc_t *aborted_proc;
    /* errmgr callback function for this job, if any */
    orte_err_cb_fn_t err_cbfunc;
    /* states that will trigger callback */
    orte_proc_state_t err_cbstates;
    /* errmgr callback data */
    void *err_cbdata;
    /* max number of times a process can be restarted */
    int32_t max_restarts;
#if OPAL_ENABLE_FT_CR == 1
    /* ckpt state */
    size_t ckpt_state;
    /* snapshot reference */
    char *ckpt_snapshot_ref;
    /* snapshot location */
    char *ckpt_snapshot_loc;
#endif
} orte_job_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_job_t);

struct orte_proc_t {
    /** Base object so this can be put on a list */
    opal_list_item_t super;
    /* process name */
    orte_process_name_t name;
    /* pid */
    pid_t pid;
    /* local rank amongst my peers on the node
     * where this is running - this value is
     * needed by MPI procs so that the lowest
     * rank on a node can perform certain fns -
     * e.g., open an sm backing file
     */
    orte_local_rank_t local_rank;
    /* local rank on the node across all procs
     * and jobs known to this HNP - this is
     * needed so that procs can do things like
     * know which static IP port to use
     */
    orte_node_rank_t node_rank;
    /* process state */
    orte_proc_state_t state;
    /* exit code */
    orte_exit_code_t exit_code;
    /* the app_context that generated this proc */
    int32_t app_idx;
    /* a cpu list, if specified by the user */
    char *slot_list;
    /* pointer to the node where this proc is executing */
    orte_node_t *node;
    /* name of the node where this proc is executing - this
     * is used simply to pass that info to a calling
     * tool since it may not have a node array available
     */
    char *nodename;
    /* RML contact info */
    char *rml_uri;
    /* seconds when last heartbeat was detected */
    int beat;
    /* number of times this process has been restarted */
    int32_t restarts;
#if OPAL_ENABLE_FT_CR == 1
    /* ckpt state */
    size_t ckpt_state;
    /* snapshot reference */
    char *ckpt_snapshot_ref;
    /* snapshot location */
    char *ckpt_snapshot_loc;
#endif
};
typedef struct orte_proc_t orte_proc_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_proc_t);

typedef struct {
    opal_list_item_t super;
    char *name;
    int32_t size;
    uint8_t *bytes;
} orte_attr_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_attr_t);

typedef struct {
    /* base object */
    opal_object_t super;
    /* index in the array */
    int index;
    /* nodename */
    char *name;
    /* vpid of this job family's daemon on this node */
    orte_vpid_t daemon;
    /* whether or not this node is oversubscribed */
    bool oversubscribed;
    /* list of attributes */
    opal_list_t attrs;
} orte_nid_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_nid_t);

typedef struct {
    /* base object */
    opal_object_t super;
    /* index to node */
    int32_t node;
    /* local rank */
    orte_local_rank_t local_rank;
    /* node rank */
    orte_node_rank_t node_rank;
} orte_pmap_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_pmap_t);

typedef struct {
    /* base object */
    opal_object_t super;
    /* jobid */
    orte_jobid_t job;
    /* number of procs in this job */
    orte_vpid_t num_procs;
    /* array of data for procs */
    opal_pointer_array_t pmap;
} orte_jmap_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_jmap_t);

typedef struct {
    /* list object */
    opal_list_item_t super;
    char *prefix;
    opal_value_array_t suffix;
    opal_value_array_t nodes;
    opal_value_array_t cnt;
    opal_value_array_t starting_vpid;
    opal_value_array_t ppn;
    opal_value_array_t nrank;
} orte_regex_node_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_regex_node_t);

/**
* Get a job data object
 * We cannot just reference a job data object with its jobid as
 * the jobid is no longer an index into the array. This change
 * was necessitated by modification of the jobid to include
 * an mpirun-unique qualifer to eliminate any global name
 * service
 */
ORTE_DECLSPEC   orte_job_t* orte_get_job_data_object(orte_jobid_t job);

/* global variables used by RTE - instanced in orte_globals.c */
ORTE_DECLSPEC extern bool orte_timing;
ORTE_DECLSPEC extern FILE *orte_timing_output;
ORTE_DECLSPEC extern bool orte_timing_details;
ORTE_DECLSPEC extern bool orte_debug_daemons_flag;
ORTE_DECLSPEC extern bool orte_debug_daemons_file_flag;
ORTE_DECLSPEC extern bool orte_leave_session_attached;
ORTE_DECLSPEC extern bool orte_do_not_launch;
ORTE_DECLSPEC extern bool orted_spin_flag;
ORTE_DECLSPEC extern bool orte_daemon_bootstrap;
ORTE_DECLSPEC extern char *orte_local_cpu_type;
ORTE_DECLSPEC extern char *orte_local_cpu_model;

/* ORTE OOB port flags */
ORTE_DECLSPEC extern bool orte_static_ports;
ORTE_DECLSPEC extern char *orte_oob_static_ports;
ORTE_DECLSPEC extern bool orte_standalone_operation;

ORTE_DECLSPEC extern bool orte_keep_fqdn_hostnames;
ORTE_DECLSPEC extern bool orte_show_resolved_nodenames;
ORTE_DECLSPEC extern int orted_debug_failure;
ORTE_DECLSPEC extern int orted_debug_failure_delay;
ORTE_DECLSPEC extern bool orte_homogeneous_nodes;
ORTE_DECLSPEC extern bool orte_hetero_apps;
ORTE_DECLSPEC extern bool orte_never_launched;
ORTE_DECLSPEC extern bool orte_devel_level_output;

ORTE_DECLSPEC extern char **orte_launch_environ;

ORTE_DECLSPEC extern bool orte_hnp_is_allocated;
ORTE_DECLSPEC extern bool orte_allocation_required;
ORTE_DECLSPEC extern bool orte_managed_allocation;

ORTE_DECLSPEC extern char *orte_launch_agent;
ORTE_DECLSPEC extern char **orted_cmd_line;

/* debugger job */
ORTE_DECLSPEC extern orte_job_t *orte_debugger_daemon;
ORTE_DECLSPEC extern bool orte_debugger_dump_proctable;
ORTE_DECLSPEC extern char *orte_debugger_test_daemon;
ORTE_DECLSPEC extern bool orte_debugger_test_attach;
ORTE_DECLSPEC extern bool orte_debugger_enable_fifo_attach;
ORTE_DECLSPEC extern int orte_debugger_check_rate;

/* exit triggers and flags */
ORTE_DECLSPEC extern orte_trigger_event_t orte_exit;
ORTE_DECLSPEC extern orte_trigger_event_t orteds_exit;
ORTE_DECLSPEC extern int orte_exit_status;
ORTE_DECLSPEC extern bool orte_abnormal_term_ordered;
ORTE_DECLSPEC extern bool orte_routing_is_enabled;
ORTE_DECLSPEC extern bool orte_job_term_ordered;

ORTE_DECLSPEC extern int orte_heartbeat_rate;
ORTE_DECLSPEC extern int orte_startup_timeout;

ORTE_DECLSPEC extern int orte_timeout_usec_per_proc;
ORTE_DECLSPEC extern float orte_max_timeout;

ORTE_DECLSPEC extern opal_buffer_t *orte_tree_launch_cmd;

/* global arrays for data storage */
ORTE_DECLSPEC extern opal_pointer_array_t *orte_job_data;
ORTE_DECLSPEC extern opal_pointer_array_t *orte_node_pool;

/* a clean output channel without prefix */
ORTE_DECLSPEC extern int orte_clean_output;

/* RHC: FLAG TO SELECT WHETHER OR NOT TO SEND PROFILE FILE IN NIDMAP */
ORTE_DECLSPEC extern bool orte_send_profile;

/* Nidmap and job maps */
ORTE_DECLSPEC extern opal_pointer_array_t orte_nidmap;
ORTE_DECLSPEC extern opal_pointer_array_t orte_jobmap;
ORTE_DECLSPEC extern bool orte_use_regexp;
ORTE_DECLSPEC extern char *orted_launch_cmd;

/* list of local children on a daemon */
ORTE_DECLSPEC extern opal_list_t orte_local_children;
/* list of job data for local children on a daemon */
ORTE_DECLSPEC extern opal_list_t orte_local_jobdata;
/* whether or not to forward SIGTSTP and SIGCONT signals */
ORTE_DECLSPEC extern bool orte_forward_job_control;

/* IOF controls */
ORTE_DECLSPEC extern bool orte_tag_output;
ORTE_DECLSPEC extern bool orte_timestamp_output;
ORTE_DECLSPEC extern char *orte_output_filename;
/* generate new xterm windows to display output from specified ranks */
ORTE_DECLSPEC extern char *orte_xterm;

/* rsh support */
ORTE_DECLSPEC extern char *orte_rsh_agent;
ORTE_DECLSPEC extern bool orte_assume_same_shell;

/* whether or not to barrier the orteds upon exit */
ORTE_DECLSPEC extern bool orte_orted_exit_with_barrier;

/* whether or not to report launch progress */
ORTE_DECLSPEC extern bool orte_report_launch_progress;

/* cluster hardware info */
ORTE_DECLSPEC extern uint8_t orte_default_num_boards;
ORTE_DECLSPEC extern uint8_t orte_default_num_sockets_per_board;
ORTE_DECLSPEC extern uint8_t orte_default_num_cores_per_socket;

/* allocation specification */
ORTE_DECLSPEC extern char *orte_default_hostfile;
ORTE_DECLSPEC extern char *orte_rankfile;
#ifdef __WINDOWS__
ORTE_DECLSPEC extern char *orte_ccp_headnode;
#endif 

/* default rank assigment and binding policy */
ORTE_DECLSPEC extern orte_mapping_policy_t orte_default_mapping_policy;

/* tool communication controls */
ORTE_DECLSPEC extern bool orte_report_events;
ORTE_DECLSPEC extern char *orte_report_events_uri;

/* report bindings */
ORTE_DECLSPEC extern bool orte_report_bindings;

/* barrier control */
ORTE_DECLSPEC extern bool orte_do_not_barrier;

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif /* ORTE_RUNTIME_ORTE_GLOBALS_H */
