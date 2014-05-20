/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
/** @file:
 */

#ifndef MCA_PLM_PRIVATE_H
#define MCA_PLM_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/threads/condition.h"

#include "opal/dss/dss_types.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/runtime/orte_globals.h"


BEGIN_C_DECLS

/* types for use solely within PLM framework */
typedef struct {
    opal_list_item_t super;
    char *node;
    bool local;
    char *prefix;
    char *bootproxy;
    bool positioned;
    opal_pointer_array_t apps;
    opal_pointer_array_t files;
} orte_slave_files_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_slave_files_t);

/* globals for use solely within PLM framework */
typedef struct {
    /** Verbose/debug output stream */
    int output;
    /* orted cmd comm lock */
    opal_mutex_t orted_cmd_lock;
    /* orted cmd cond */
    opal_condition_t orted_cmd_cond;
    /* next jobid */
    uint16_t next_jobid;
    /* time when daemons started launch */
    struct timeval daemonlaunchstart;
    /* rsh launch agent path */
    char *rsh_agent_path;
    /* rsh launch agent argv */
    char **rsh_agent_argv;
    /* jobid for local slaves */
    orte_jobid_t local_slaves;
    /* list of local slave files */
    opal_list_t slave_files;
    /* spawn lock */
    opal_mutex_t spawn_lock;
    /* spawn cond */
    opal_condition_t spawn_cond;
    /* spawn status */
    int spawn_status;
    /* completion flag */
    bool spawn_complete;
    /* spawn in progress cond */
    opal_condition_t spawn_in_progress_cond;
    /* flag */
    bool spawn_in_progress;
} orte_plm_globals_t;
/**
 * Global instance of PLM framework data
 */
ORTE_DECLSPEC extern orte_plm_globals_t orte_plm_globals;


/**
 * Utility routine to set progress engine schedule
 */
ORTE_DECLSPEC int orte_plm_base_set_progress_sched(int sched);

/*
 * Launch support
 */
ORTE_DECLSPEC int orte_plm_base_setup_job(orte_job_t *jdata);
ORTE_DECLSPEC int orte_plm_base_launch_apps(orte_jobid_t job);
ORTE_DECLSPEC void orte_plm_base_launch_failed(orte_jobid_t job, pid_t pid, int status, orte_job_state_t state);
ORTE_DECLSPEC int orte_plm_base_report_launched(orte_jobid_t job);

ORTE_DECLSPEC int orte_plm_base_daemon_callback(orte_std_cntr_t num_daemons);

ORTE_DECLSPEC int orte_plm_base_set_hnp_name(void);

ORTE_DECLSPEC int orte_plm_base_create_jobid(orte_job_t *jdata);

ORTE_DECLSPEC void orte_plm_base_reset_job(orte_job_t *jdata);

ORTE_DECLSPEC int orte_plm_base_setup_orted_cmd(int *argc, char ***argv);

/**
 * Local slave launch
 */
ORTE_DECLSPEC int orte_plm_base_local_slave_launch(orte_job_t *jdata);
ORTE_DECLSPEC int orte_plm_base_rsh_launch_agent_setup(const char *agent_list, char *path);
ORTE_DECLSPEC int orte_plm_base_rsh_launch_agent_lookup(const char *agent_list, char *path);
ORTE_DECLSPEC void orte_plm_base_local_slave_finalize(void);
ORTE_DECLSPEC int orte_plm_base_setup_rsh_launch(char *nodename, orte_app_context_t *app,
                                                 char *rcmd, char ***argv, char **exec_path);
ORTE_DECLSPEC int orte_plm_base_append_bootproxy_args(orte_app_context_t *app, char ***argv,
                                                      orte_jobid_t jobid, orte_vpid_t vpid,
                                                      int num_nodes, orte_vpid_t num_procs,
                                                      orte_node_rank_t nrank, orte_local_rank_t lrank,
                                                      orte_vpid_t nlocal, int nslots, bool overwrite);

/**
 * Heartbeat support
 */
ORTE_DECLSPEC void orte_plm_base_heartbeat(int fd, short event, void *data);
ORTE_DECLSPEC void orte_plm_base_start_heart(void);

/**
 * Utilities for plm components that use proxy daemons
 */
ORTE_DECLSPEC int orte_plm_base_orted_exit(orte_daemon_cmd_flag_t command);
ORTE_DECLSPEC int orte_plm_base_orted_terminate_job(orte_jobid_t jobid);
ORTE_DECLSPEC int orte_plm_base_orted_kill_local_procs(opal_pointer_array_t *procs);
ORTE_DECLSPEC int orte_plm_base_orted_signal_local_procs(orte_jobid_t job, int32_t signal);

/*
 * communications utilities
 */
ORTE_DECLSPEC int orte_plm_base_comm_start(void);
ORTE_DECLSPEC int orte_plm_base_comm_stop(void);
ORTE_DECLSPEC void orte_plm_base_recv(int status, orte_process_name_t* sender,
                                      opal_buffer_t* buffer, orte_rml_tag_t tag,
                                      void* cbdata);

    
/**
 * Construct basic ORTE Daemon command line arguments
 */
ORTE_DECLSPEC int orte_plm_base_orted_append_basic_args(int *argc, char ***argv,
                                                        char *ess_module,
                                                        int *proc_vpid_index,
                                                        bool heartbeat, char *nodes);

/*
 * Proxy functions for use by daemons and application procs
 * needing dynamic operations
 */
ORTE_DECLSPEC int orte_plm_proxy_init(void);
ORTE_DECLSPEC int orte_plm_proxy_spawn(orte_job_t *jdata);
ORTE_DECLSPEC int orte_plm_proxy_finalize(void);

END_C_DECLS

#endif  /* MCA_PLS_PRIVATE_H */
