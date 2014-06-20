/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

/**
 * @file
 * 
 * FULL SNAPC component
 *
 * Simple, braindead implementation.
 */

#ifndef MCA_SNAPC_FULL_EXPORT_H
#define MCA_SNAPC_FULL_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "opal/event/event.h"

#include "orte/mca/filem/filem.h"
#include "orte/mca/snapc/snapc.h"

BEGIN_C_DECLS

/*
 * cmds for base receive
 */
typedef uint8_t orte_snapc_full_cmd_flag_t;
#define ORTE_SNAPC_FULL_CMD  OPAL_UINT8
#define ORTE_SNAPC_FULL_UPDATE_JOB_STATE_CMD         1
#define ORTE_SNAPC_FULL_UPDATE_JOB_STATE_QUICK_CMD   2
#define ORTE_SNAPC_FULL_UPDATE_ORTED_STATE_CMD       3
#define ORTE_SNAPC_FULL_UPDATE_ORTED_STATE_QUICK_CMD 4
#define ORTE_SNAPC_FULL_VPID_ASSOC_CMD               5
#define ORTE_SNAPC_FULL_ESTABLISH_DIR_CMD            6
#define ORTE_SNAPC_FULL_START_CKPT_CMD               7
#define ORTE_SNAPC_FULL_END_CKPT_CMD                 8
#define ORTE_SNAPC_FULL_MAX                          9

    /*
     * Local Component structures
     */
    struct orte_snapc_full_component_t {
        orte_snapc_base_component_t super;  /** Base SNAPC component */
    };
    typedef struct orte_snapc_full_component_t orte_snapc_full_component_t;
    OPAL_MODULE_DECLSPEC extern orte_snapc_full_component_t mca_snapc_full_component;

    /*
     * Global Coordinator per orted metadata
     */
    struct orte_snapc_full_orted_snapshot_t {
        /** Base SNAPC Global snapshot type */
        orte_snapc_base_global_snapshot_t super;

        /** ORTE Process name */
        orte_process_name_t process_name;

        /** State of the checkpoint */
        int state;

        /** OPAL CRS Component */
        char * opal_crs;

        /** Checkpoint Options */
        opal_crs_base_ckpt_options_t *options;

        /** FileM request */
        orte_filem_base_request_t *filem_request;
    };
    typedef struct orte_snapc_full_orted_snapshot_t orte_snapc_full_orted_snapshot_t;
    OBJ_CLASS_DECLARATION(orte_snapc_full_orted_snapshot_t);

    /*
     * Local Coordinator per app metadata
     */
    struct orte_snapc_full_app_snapshot_t {
        /** Base SNAPC Global snapshot type */
        orte_snapc_base_local_snapshot_t super;

        /** Named Pipe Read and Write */
        char * comm_pipe_r;
        char * comm_pipe_w;
        int    comm_pipe_r_fd;
        int    comm_pipe_w_fd;

        /* An opal event handle for the read pipe */
        struct opal_event comm_pipe_r_eh;
        bool is_eh_active;

        /** Process pid */
        pid_t process_pid;

        /** Options */
        opal_crs_base_ckpt_options_t *options;
    };
    typedef struct orte_snapc_full_app_snapshot_t orte_snapc_full_app_snapshot_t;
    OBJ_CLASS_DECLARATION(orte_snapc_full_app_snapshot_t);

    extern bool orte_snapc_full_skip_filem;
    extern bool orte_snapc_full_skip_app;
    extern bool orte_snapc_full_timing_enabled;
    extern int orte_snapc_full_max_wait_time;

    int orte_snapc_full_component_query(mca_base_module_t **module, int *priority);

    /*
     * Module functions
     */
    int orte_snapc_full_module_init(bool seed, bool app);
    int orte_snapc_full_module_finalize(void);

    int orte_snapc_full_setup_job(orte_jobid_t jobid);
    int orte_snapc_full_release_job(orte_jobid_t jobid);

    int orte_snapc_full_ft_event(int state);

    int orte_snapc_full_start_ckpt(orte_snapc_base_quiesce_t *datum);
    int orte_snapc_full_end_ckpt(orte_snapc_base_quiesce_t *datum);

    /*
     * Global Coordinator Functionality
     */
    int global_coord_init(void);
    int global_coord_finalize(void);
    int global_coord_setup_job(orte_jobid_t jobid);
    int global_coord_release_job(orte_jobid_t jobid);
    int global_coord_orted_state_update(orte_process_name_t proc_name,
                                        int    proc_ckpt_state,
                                        char **proc_ckpt_ref,
                                        char **proc_ckpt_loc,
                                        char **agent_ckpt);
    int global_coord_start_ckpt(orte_snapc_base_quiesce_t *datum);
    int global_coord_end_ckpt(orte_snapc_base_quiesce_t *datum);

    /*
     * Local Coordinator Functionality
     */
    int local_coord_init(void);
    int local_coord_finalize(void);
    int local_coord_setup_job(orte_jobid_t jobid);
    int local_coord_release_job(orte_jobid_t jobid);
    int local_coord_job_state_update(orte_jobid_t jobid,
                                     int    job_ckpt_state,
                                     char **job_ckpt_ref,
                                     char **job_ckpt_loc,
                                     opal_crs_base_ckpt_options_t *options);

    /*
     * Application Coordinator Functionality
     */
    int app_coord_init(void);
    int app_coord_finalize(void);
    int app_coord_ft_event(int state);
    int app_coord_start_ckpt(orte_snapc_base_quiesce_t *datum);
    int app_coord_end_ckpt(orte_snapc_base_quiesce_t *datum);

END_C_DECLS

#endif /* MCA_SNAPC_FULL_EXPORT_H */
