/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/basename.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/orte_globals.h"
#include "opal/dss/dss.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/filem/filem.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

#include "snapc_full.h"

#include MCA_timer_IMPLEMENTATION_HEADER

/************************************
 * Locally Global vars & functions :)
 ************************************/
#define INC_SEQ_NUM()                         \
 {                                            \
   if(orte_snapc_base_store_only_one_seq) {   \
     orte_snapc_base_snapshot_seq_number = 0; \
   } else {                                   \
     orte_snapc_base_snapshot_seq_number++;   \
   }                                          \
 }

static orte_jobid_t current_global_jobid = ORTE_JOBID_INVALID;
static orte_snapc_base_global_snapshot_t global_snapshot;
static bool updated_job_to_running;
static int current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;
static bool global_coord_has_local_children = false;
static bool wait_all_xfer = false;

static opal_crs_base_ckpt_options_t *current_options = NULL;

static double timer_start = 0;
static double timer_local_done = 0;
static double timer_xfer_done = 0;
static double timer_end   = 0;
static double get_time(void);
static void print_time(void);

static int global_init_job_structs(void);
static int global_refresh_job_structs(void);

static bool snapc_orted_recv_issued = false;
static bool is_orte_checkpoint_connected = false;
static int snapc_full_global_start_listener(void);
static int snapc_full_global_stop_listener(void);
static void snapc_full_global_orted_recv(int status,
                                         orte_process_name_t* sender,
                                         opal_buffer_t* buffer,
                                         orte_rml_tag_t tag,
                                         void* cbdata);
static void snapc_full_process_orted_request_cmd(int fd, short event, void *cbdata);

static void snapc_full_process_start_ckpt_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* buffer);
static void snapc_full_process_end_ckpt_cmd(orte_process_name_t* sender,
                                            opal_buffer_t* buffer);

/*** Command Line Interactions */
static orte_process_name_t orte_checkpoint_sender = {ORTE_JOBID_INVALID, ORTE_VPID_INVALID};
static bool snapc_cmdline_recv_issued = false;
static int snapc_full_global_start_cmdline_listener(void);
static int snapc_full_global_stop_cmdline_listener(void);
static void snapc_full_global_cmdline_recv(int status,
                                           orte_process_name_t* sender,
                                           opal_buffer_t* buffer,
                                           orte_rml_tag_t tag,
                                           void* cbdata);
static void snapc_full_process_cmdline_request_cmd(int fd, short event, void *cbdata);

static void snapc_full_process_filem_xfer(void);


static int snapc_full_establish_snapshot_dir(bool empty_metadata);

/*** */
static int snapc_full_global_checkpoint(opal_crs_base_ckpt_options_t *options);
static int snapc_full_global_notify_checkpoint(orte_jobid_t jobid,
                                               opal_crs_base_ckpt_options_t *options);
static int orte_snapc_full_global_set_job_ckpt_info( orte_jobid_t jobid,
                                                     int ckpt_state, 
                                                     char  *ckpt_snapshot_ref,
                                                     char  *ckpt_snapshot_loc,
                                                     bool quick,
                                                     opal_crs_base_ckpt_options_t *options);
int global_coord_job_state_update(orte_jobid_t jobid,
                                  int job_ckpt_state,
                                  char **job_ckpt_snapshot_ref,
                                  char **job_ckpt_snapshot_loc,
                                  opal_crs_base_ckpt_options_t *options);
static void snapc_full_process_job_update_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* buffer,
                                              bool quick);
static int snapc_full_process_orted_update_cmd(orte_process_name_t* sender,
                                               opal_buffer_t* buffer,
                                               bool quick);
static orte_snapc_full_orted_snapshot_t *find_orted_snapshot(orte_process_name_t *name );
static orte_snapc_base_local_snapshot_t *find_orted_app_snapshot(orte_snapc_full_orted_snapshot_t *orted_snapshot,
                                                                 orte_process_name_t *name);

static int snapc_full_start_filem(orte_snapc_full_orted_snapshot_t *orted_snapshot);
static int snapc_full_wait_filem(void);

static int snapc_full_global_get_min_state(void);
static int write_out_global_metadata(void);

/************************
 * Function Definitions
 ************************/
int global_coord_init(void) {

    current_global_jobid = ORTE_JOBID_INVALID;
    orte_snapc_base_snapshot_seq_number = -1;

    current_options = OBJ_NEW(opal_crs_base_ckpt_options_t);

    return ORTE_SUCCESS;
}

int global_coord_finalize(void) {

    current_global_jobid = ORTE_JOBID_INVALID;
    orte_snapc_base_snapshot_seq_number = -1;

    if( NULL != current_options ) {
        OBJ_RELEASE(current_options);
        current_options = NULL;
    }

    return ORTE_SUCCESS;
}

int global_coord_setup_job(orte_jobid_t jobid) {
    int ret, exit_status = ORTE_SUCCESS;

    /*
     * Only allow one job at a time.
     *
     * It is possible to pass through this function twice since HNP may also be
     * a local daemon. So it may be both a global and local coordinator.
     *  Global: orte_plm_base_setup_job()
     *  Local : odls_default_module.c
     */
    /* Global Coordinator pass */
    if( ORTE_JOBID_INVALID == current_global_jobid ) {
        current_global_jobid = jobid;
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Setup job %s as the Global Coordinator\n",
                             ORTE_JOBID_PRINT(jobid)));
    }
    /* Local Coordinator pass - Always happens after global coordinator pass */
    else if ( jobid == current_global_jobid ) {
        /* If there are no local children, do not become a local coordinator */
        if( !global_coord_has_local_children ) {
            return ORTE_SUCCESS;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Setup job %s as the Local Coordinator\n",
                             ORTE_JOBID_PRINT(jobid)));
        orte_snapc_coord_type |= ORTE_SNAPC_LOCAL_COORD_TYPE;
        return local_coord_setup_job(jobid);
    }
    /* Only allow one job at a time */
    else {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Global) Setup of job %s Failed! Already setup job %s\n",
                    ORTE_JOBID_PRINT(jobid), ORTE_JOBID_PRINT(current_global_jobid));
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }

    /*
     * Start out with a sequence number just below the first
     * This will be incremented when we checkpoint
     */
    orte_snapc_base_snapshot_seq_number = -1;

    /*
     * Allocate structure to track node status
     */
    if( ORTE_SUCCESS != (ret = global_init_job_structs()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup Global Coordinator command processing listener
     */
    if( ORTE_SUCCESS != (ret = snapc_full_global_start_listener()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Setup command line tool checkpoint request listener
     */
    if( ORTE_SUCCESS != (ret = snapc_full_global_start_cmdline_listener()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * If requested pre-establish the global snapshot directory
     */
    if(orte_snapc_base_establish_global_snapshot_dir) {
        opal_output(0, "Global) Error: Pre-establishment of snapshot directory currently not supported!");
        ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
#if 0
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Pre-establish the global snapshot directory\n"));
        if( ORTE_SUCCESS != (ret = snapc_full_establish_snapshot_dir(true))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
#endif
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Finished setup of job %s ",
                         ORTE_JOBID_PRINT(jobid)));

 cleanup:
    return exit_status;
}

int global_coord_release_job(orte_jobid_t jobid) {
    int ret, exit_status = ORTE_SUCCESS;

    /*
     * Make sure we are not waiting on a checkpoint to complete
     */

    /*
     * Clean up listeners
     */
    if( ORTE_SUCCESS != (ret = snapc_full_global_stop_cmdline_listener()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
    }

    if( ORTE_SUCCESS != (ret = snapc_full_global_stop_listener()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
    }

    OBJ_DESTRUCT(&global_snapshot);
    
    return exit_status;
}

int global_coord_start_ckpt(orte_snapc_base_quiesce_t *datum)
{
    int ret, exit_status = ORTE_SUCCESS;

    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;
    opal_list_item_t* orted_item = NULL;
    opal_list_item_t* app_item = NULL;
    orte_snapc_base_local_snapshot_t *vpid_snapshot = NULL;
    opal_crs_base_ckpt_options_t *options = NULL;

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Starting checkpoint (internally requested)"));

    orte_checkpoint_sender = orte_name_invalid;

    /* Save Options */
    options = OBJ_NEW(opal_crs_base_ckpt_options_t);
    opal_crs_base_copy_options(options, current_options);

    /*************************
     * Kick off the checkpoint (local coord will release the processes)
     *************************/
    if( ORTE_SUCCESS != (ret = snapc_full_global_checkpoint(options) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Wait for checkpoint to locally finish on all nodes
     */
    while(current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL &&
          current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_FINISHED &&
          current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_ERROR &&
          current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_NONE ) {
        opal_progress();
    }

    /*
     * Update the quiesce structure with the handle
     */
    datum->snapshot = OBJ_NEW(orte_snapc_base_global_snapshot_t);
    datum->snapshot->reference_name = strdup(global_snapshot.reference_name);
    datum->snapshot->local_location = strdup(global_snapshot.local_location);
    datum->snapshot->seq_num = orte_snapc_base_snapshot_seq_number;
    datum->epoch             = orte_snapc_base_snapshot_seq_number;

    /* Copy the snapshot information */
    for(orted_item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        orted_item != opal_list_get_end(&(global_snapshot.local_snapshots));
        orted_item  = opal_list_get_next(orted_item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)orted_item;

        if( ORTE_SNAPC_CKPT_STATE_ERROR == orted_snapshot->state ) {
            continue;
        }

        for(app_item  = opal_list_get_first(&(orted_snapshot->super.local_snapshots));
            app_item != opal_list_get_end(&(orted_snapshot->super.local_snapshots));
            app_item  = opal_list_get_next(app_item) ) {
            app_snapshot = (orte_snapc_base_local_snapshot_t*)app_item;

            vpid_snapshot = OBJ_NEW(orte_snapc_base_local_snapshot_t);
            vpid_snapshot->process_name.jobid = app_snapshot->process_name.jobid;
            vpid_snapshot->process_name.vpid  = app_snapshot->process_name.vpid;
            vpid_snapshot->reference_name     = strdup(app_snapshot->reference_name);
            vpid_snapshot->local_location     = strdup(app_snapshot->local_location);

            opal_list_append(&(datum->snapshot->local_snapshots), &(vpid_snapshot->super));
        }
    }
    
 cleanup:
    if( NULL != options ) {
        OBJ_RELEASE(options);
        options = NULL;
    }

    return exit_status;
}

int global_coord_end_ckpt(orte_snapc_base_quiesce_t *datum)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Finishing checkpoint (internally requested)"));

    while(current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_FINISHED &&
          current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_ERROR &&
          current_job_ckpt_state != ORTE_SNAPC_CKPT_STATE_NONE ) {
        opal_progress();
    }

    /*
     * Update the job structure since processes may have moved around
     */
    if( ORTE_SUCCESS != (ret = global_refresh_job_structs()) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Finished checkpoint (internally requested) [%d]",
                         current_job_ckpt_state));

 cleanup:
    return exit_status;
}

/******************
 * Local functions
 ******************/
static int global_init_job_structs(void)
{
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;
    orte_node_t **nodes = NULL;
    orte_job_map_t *map = NULL;
    orte_job_t *jdata = NULL;
    orte_proc_t **procs = NULL;
    orte_std_cntr_t i = 0;
    orte_vpid_t p = 0;

    /* look up job data object */
    if (NULL == (jdata = orte_get_job_data_object(current_global_jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    OBJ_CONSTRUCT(&global_snapshot, orte_snapc_base_global_snapshot_t);
    /* JJH XXX global_snapshot.component_name = strdup(mca_snapc_full_component.super.base_version.mca_component_name);*/

    map = jdata->map;
    nodes = (orte_node_t**)map->nodes->addr;

    for(i = 0; i < map->num_nodes; i++) {
        procs = (orte_proc_t**)nodes[i]->procs->addr;

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) [%d] Found Daemon %s with %d procs",
                             i, ORTE_NAME_PRINT(&(nodes[i]->daemon->name)), nodes[i]->num_procs));

        orted_snapshot = OBJ_NEW(orte_snapc_full_orted_snapshot_t);

        orted_snapshot->process_name.jobid  = nodes[i]->daemon->name.jobid;
        orted_snapshot->process_name.vpid   = nodes[i]->daemon->name.vpid;

        if( orted_snapshot->process_name.jobid == ORTE_PROC_MY_NAME->jobid &&
            orted_snapshot->process_name.vpid  == ORTE_PROC_MY_NAME->vpid ) {
            global_coord_has_local_children = true;
        }

        for(p = 0; p < nodes[i]->num_procs; ++p) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) \t [%d] Found Process %s on Daemon %s",
                                 p, ORTE_NAME_PRINT(&(procs[p]->name)), ORTE_NAME_PRINT(&(nodes[i]->daemon->name)) ));

            app_snapshot = OBJ_NEW(orte_snapc_base_local_snapshot_t);

            app_snapshot->process_name.jobid = procs[p]->name.jobid;
            app_snapshot->process_name.vpid = procs[p]->name.vpid;

            opal_list_append(&(orted_snapshot->super.local_snapshots), &(app_snapshot->super));
        }


        opal_list_append(&global_snapshot.local_snapshots, &(orted_snapshot->super.super));
    }

    return ORTE_SUCCESS;
}

static int global_refresh_job_structs(void)
{
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;
    opal_list_item_t* orted_item = NULL;
    orte_node_t **nodes = NULL;
    orte_job_map_t *map = NULL;
    orte_job_t *jdata = NULL;
    orte_proc_t **procs = NULL;
    orte_std_cntr_t i = 0;
    orte_vpid_t p = 0;
    bool found = false;

    /* look up job data object */
    if (NULL == (jdata = orte_get_job_data_object(current_global_jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    map = jdata->map;
    nodes = (orte_node_t**)map->nodes->addr;

    /*
     * Look for new nodes
     */
    for(i = 0; i < map->num_nodes; i++) {
        procs = (orte_proc_t**)nodes[i]->procs->addr;

        /*
         * See if we are already tracking it (if so skip)
         */
        found = false;
        for(orted_item  = opal_list_get_first(&(global_snapshot.local_snapshots));
            orted_item != opal_list_get_end(&(global_snapshot.local_snapshots));
            orted_item  = opal_list_get_next(orted_item) ) {
            orted_snapshot = (orte_snapc_full_orted_snapshot_t*)orted_item;

            if(OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL,
                                                           &(nodes[i]->daemon->name),
                                                           &(orted_snapshot->process_name) )) {
                found = true;
                break;
            }
        }
        if( found ) {
            continue;
        }

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) [%d] Found Daemon %s with %d procs",
                             i, ORTE_NAME_PRINT(&(nodes[i]->daemon->name)), nodes[i]->num_procs));

        orted_snapshot = OBJ_NEW(orte_snapc_full_orted_snapshot_t);

        orted_snapshot->process_name.jobid  = nodes[i]->daemon->name.jobid;
        orted_snapshot->process_name.vpid   = nodes[i]->daemon->name.vpid;

        if( orted_snapshot->process_name.jobid == ORTE_PROC_MY_NAME->jobid &&
            orted_snapshot->process_name.vpid  == ORTE_PROC_MY_NAME->vpid ) {
            global_coord_has_local_children = true;
        }
        for(p = 0; p < nodes[i]->num_procs; ++p) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) \t [%d] Found Process %s on Daemon %s",
                                 p, ORTE_NAME_PRINT(&(procs[p]->name)), ORTE_NAME_PRINT(&(nodes[i]->daemon->name)) ));

            app_snapshot = OBJ_NEW(orte_snapc_base_local_snapshot_t);

            app_snapshot->process_name.jobid = procs[p]->name.jobid;
            app_snapshot->process_name.vpid = procs[p]->name.vpid;

            opal_list_append(&(orted_snapshot->super.local_snapshots), &(app_snapshot->super));
        }


        opal_list_append(&global_snapshot.local_snapshots, &(orted_snapshot->super.super));
    }

    return ORTE_SUCCESS;
}

/*****************
 * Setup listeners
 *****************/
static int snapc_full_global_start_listener(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    if (snapc_orted_recv_issued && ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Startup Coordinator Channel"));

    /*
     * Coordinator command listener
     */
    if (ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                       ORTE_RML_TAG_SNAPC_FULL,
                                                       ORTE_RML_PERSISTENT,
                                                       snapc_full_global_orted_recv,
                                                       NULL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    snapc_orted_recv_issued = true;
    
 cleanup:
    return exit_status;
}

static int snapc_full_global_stop_listener(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    
    if (!snapc_orted_recv_issued && ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Shutdown Coordinator Channel"));
    
    if (ORTE_SUCCESS != (ret = orte_rml.recv_cancel(ORTE_NAME_WILDCARD,
                                                    ORTE_RML_TAG_SNAPC_FULL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    snapc_orted_recv_issued = false;
    
 cleanup:
    return exit_status;
}

static int snapc_full_global_start_cmdline_listener(void)
{
    int ret, exit_status = ORTE_SUCCESS;

    if (snapc_cmdline_recv_issued && ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Startup Command Line Channel"));

    /*
     * Coordinator command listener
     */
    if (ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                       ORTE_RML_TAG_CKPT,
                                                       0,
                                                       snapc_full_global_cmdline_recv,
                                                       NULL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    snapc_cmdline_recv_issued = true;

 cleanup:
    return exit_status;
}

static int snapc_full_global_stop_cmdline_listener(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    
    if (!snapc_cmdline_recv_issued && ORTE_PROC_IS_HNP) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Shutdown Command Line Channel"));
    
    if (ORTE_SUCCESS != (ret = orte_rml.recv_cancel(ORTE_NAME_WILDCARD,
                                                    ORTE_RML_TAG_CKPT))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    snapc_cmdline_recv_issued = false;
    
 cleanup:
    return exit_status;
}

/*****************
 * Listener Callbacks
 *****************/
static void snapc_full_global_cmdline_recv(int status,
                                           orte_process_name_t* sender,
                                           opal_buffer_t* buffer,
                                           orte_rml_tag_t tag,
                                           void* cbdata)
{
    if( ORTE_RML_TAG_CKPT != tag ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Global) Error: Unknown tag: Received a command message from %s (tag = %d).",
                    ORTE_NAME_PRINT(sender), tag);
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Command Line: Start a checkpoint operation [Sender = %s]",
                         ORTE_NAME_PRINT(sender)));

    snapc_cmdline_recv_issued = false; /* Not a persistent RML message */

    /*
     * Do not process this right away - we need to get out of the recv before
     * we process the message to avoid performing the rest of the job while
     * inside this receive! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     *
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, snapc_full_process_cmdline_request_cmd);

    return;
}

void snapc_full_global_orted_recv(int status,
                                  orte_process_name_t* sender,
                                  opal_buffer_t* buffer,
                                  orte_rml_tag_t tag,
                                  void* cbdata)
{
    if( ORTE_RML_TAG_SNAPC_FULL != tag ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Global) Error: Unknown tag: Received a command message from %s (tag = %d).",
                    ORTE_NAME_PRINT(sender), tag);
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return;
    }

    /*
     * This is a message from a Local Coordinator
     */
    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "Global) Receive a command message from %s.",
                         ORTE_NAME_PRINT(sender)));

    /*
     * Do not process this right away - we need to get out of the recv before
     * we process the message to avoid performing the rest of the job while
     * inside this receive! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     *
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, snapc_full_process_orted_request_cmd);

    return;
}

/************************************/
static void snapc_full_process_cmdline_request_cmd(int fd, short event, void *cbdata)
{
    int ret;
    orte_message_event_t *mev = (orte_message_event_t*)cbdata;
    orte_process_name_t *sender = NULL;
    orte_snapc_cmd_flag_t command;
    orte_std_cntr_t count = 1;
    orte_jobid_t jobid;
    opal_crs_base_ckpt_options_t *options = NULL;

    sender = &(mev->sender);

    options = OBJ_NEW(opal_crs_base_ckpt_options_t);

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(mev->buffer, &command, &count, ORTE_SNAPC_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    /*
     * orte_checkpoint has requested that a checkpoint be taken
     */
    if (ORTE_SNAPC_GLOBAL_INIT_CMD == command) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Command line requested a checkpoint [command %d]\n",
                             command));

        /*
         * Unpack the buffer from the orte_checkpoint command
         */
        if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_init_cmd(sender,
                                                                              mev->buffer,
                                                                              options,
                                                                              &jobid)) ) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }

        /* Save Options */
        opal_crs_base_copy_options(options, current_options);

        /*
         * If the jobid was specified, and does not match the current job, then fail
         */
        if( ORTE_JOBID_INVALID != jobid && jobid != current_global_jobid) {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "Global) Error: Jobid %s does not match the current jobid %s",
                        ORTE_JOBID_PRINT(jobid), ORTE_JOBID_PRINT(current_global_jobid));
            ORTE_ERROR_LOG(ORTE_ERROR);
            goto cleanup;
        }

        /*************************
         * Kick off the checkpoint
         *************************/
        orte_checkpoint_sender = *sender;
        is_orte_checkpoint_connected = true;
        if(orte_snapc_full_timing_enabled) {
            timer_start = get_time();
        }
        if( ORTE_SUCCESS != (ret = snapc_full_global_checkpoint(options) ) ) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
        
    }
    /*
     * Terminate the connection (Not currently implemented)
     */
    else if (ORTE_SNAPC_GLOBAL_TERM_CMD == command) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Command line requested to terminate connection (command %d)\n",
                             command));
        ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
        goto cleanup;
    }
    /*
     * Unknown command
     */
    else {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) Command line sent an unknown command (command %d)\n",
                             command));
        ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
        goto cleanup;
    }
    
 cleanup:
    if( NULL != options ) {
        OBJ_RELEASE(options);
        options = NULL;
    }

    /* release the message event */
    OBJ_RELEASE(mev);
    return;
}

static void snapc_full_process_orted_request_cmd(int fd, short event, void *cbdata)
{
    int ret;
    orte_message_event_t *mev = (orte_message_event_t*)cbdata;
    orte_snapc_full_cmd_flag_t command;
    orte_std_cntr_t count;
    static int num_inside = 0;

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(mev->buffer, &command, &count, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    ++num_inside;

    switch (command) {
        case ORTE_SNAPC_FULL_UPDATE_JOB_STATE_QUICK_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Job State Update (quick)"));

            snapc_full_process_job_update_cmd(&(mev->sender), mev->buffer, true);
            break;

        case ORTE_SNAPC_FULL_UPDATE_JOB_STATE_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Job State Update"));

            snapc_full_process_job_update_cmd(&(mev->sender), mev->buffer, false);
            break;

        case ORTE_SNAPC_FULL_UPDATE_ORTED_STATE_QUICK_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Daemon State Update (quick)"));

            snapc_full_process_orted_update_cmd(&(mev->sender), mev->buffer, true);
            break;

        case ORTE_SNAPC_FULL_UPDATE_ORTED_STATE_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Daemon State Update"));

            snapc_full_process_orted_update_cmd(&(mev->sender), mev->buffer, false);
            break;

        case ORTE_SNAPC_FULL_START_CKPT_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: Start Checkpoint"));

            snapc_full_process_start_ckpt_cmd(&(mev->sender), mev->buffer);
            break;

        case ORTE_SNAPC_FULL_END_CKPT_CMD:
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) Command: End Checkpoint"));

            snapc_full_process_end_ckpt_cmd(&(mev->sender), mev->buffer);
            break;

        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
    }

    /* We need to wait for the last notification to start the waiting loop
     * if we do not then we could get stuck in a recursive stack.
     */
    --num_inside;
    if( wait_all_xfer && num_inside <= 0) {
        wait_all_xfer = false;
        snapc_full_process_filem_xfer();
    }

 cleanup:
    /* release the message event */
    OBJ_RELEASE(mev);
    return;
}

static void snapc_full_process_start_ckpt_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* sbuffer)
{
    int ret;
    orte_std_cntr_t count = 1;
    orte_jobid_t jobid;
    opal_crs_base_ckpt_options_t *options = NULL;

    orte_checkpoint_sender = orte_name_invalid;

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &jobid, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    /* Save Options */
    options = OBJ_NEW(opal_crs_base_ckpt_options_t);
    opal_crs_base_copy_options(options, current_options);

    /*************************
     * Kick off the checkpoint (local coord will release the processes)
     *************************/
    if( ORTE_SUCCESS != (ret = snapc_full_global_checkpoint(options) ) ) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

 cleanup:
    if( NULL != options ) {
        OBJ_RELEASE(options);
        options = NULL;
    }

    return;
}

static void snapc_full_process_end_ckpt_cmd(orte_process_name_t* sender,
                                            opal_buffer_t* sbuffer)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count = 1;
    orte_jobid_t jobid;
    int local_epoch;

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &jobid, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(sbuffer, &local_epoch, &count, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return;
}

static int snapc_full_process_orted_update_cmd(orte_process_name_t* sender,
                                               opal_buffer_t* buffer,
                                               bool quick)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count;
    orte_process_name_t remote_proc;
    size_t num_procs, i;
    int remote_ckpt_state;
    char *remote_ckpt_ref = NULL, *remote_ckpt_loc = NULL;
    char *agent_crs = NULL;
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;
    int loc_min_state;
    char *state_str = NULL;

    orted_snapshot = find_orted_snapshot(sender);
    if( NULL == orted_snapshot ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "Global) Error: Unknown Daemon %s",
                    ORTE_NAME_PRINT(sender) );
        exit_status = ORTE_ERROR;
        ORTE_ERROR_LOG(ORTE_ERROR);
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) Daemon %s: Changed state to:\n",
                         ORTE_NAME_PRINT(&(orted_snapshot->process_name)) ));

    /*
     * Unpack the data (quick)
     * - state
     * Unpack the data (long)
     * - state
     * - CRS Component
     * - # procs
     * - Foreach proc
     *   - process name
     *   - ckpt_ref
     *   - ckpt_loc
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &remote_ckpt_state, &count, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    orted_snapshot->state = remote_ckpt_state;
    orte_snapc_ckpt_state_str(&state_str, orted_snapshot->state);
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global)   State:        %d (%s)\n",
                         (int)(orted_snapshot->state), state_str));
    free(state_str);
    state_str = NULL;

    if( quick ) {
        exit_status = ORTE_SUCCESS;
        goto post_process;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &agent_crs, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    if( NULL != orted_snapshot->opal_crs ) {
        free( orted_snapshot->opal_crs );
    }
    orted_snapshot->opal_crs = strdup(agent_crs);
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global)   CRS:        %s\n",
                         orted_snapshot->opal_crs));

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &num_procs, &count, OPAL_SIZE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    for(i = 0; i < num_procs; ++i ) {
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &remote_proc, &count, ORTE_NAME))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
        app_snapshot = find_orted_app_snapshot(orted_snapshot, &remote_proc);
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global)   Process:   %s\n",
                             ORTE_NAME_PRINT(&remote_proc) ));

        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &remote_ckpt_ref, &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
        if( NULL != app_snapshot->reference_name ) {
            free( app_snapshot->reference_name );
        }
        app_snapshot->reference_name = strdup(remote_ckpt_ref);
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global)      Ref:  %s\n",
                             app_snapshot->reference_name ));

        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &remote_ckpt_loc, &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
        if( NULL != app_snapshot->remote_location ) {
            free( app_snapshot->remote_location );
        }
        app_snapshot->remote_location = strdup(remote_ckpt_loc);
        if( NULL == app_snapshot->local_location ) {
            app_snapshot->local_location  = strdup(orte_snapc_base_global_snapshot_loc);
        }
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global)    R Loc:  %s\n",
                             app_snapshot->remote_location ));
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global)    L Loc:  %s\n",
                             app_snapshot->local_location ));

    }

 post_process:
    loc_min_state = snapc_full_global_get_min_state();

    /*
     * Notify the orte-checkpoint command once we have everyone running.
     * No need to broadcast this to everyone since they already know.
     */
    if( ORTE_SNAPC_CKPT_STATE_RUNNING == loc_min_state &&
        ORTE_SNAPC_CKPT_STATE_RUNNING != current_job_ckpt_state) {
        current_job_ckpt_state = loc_min_state;

        if( is_orte_checkpoint_connected &&
            ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender, 
                                                                                global_snapshot.reference_name,
                                                                                global_snapshot.seq_num,
                                                                                current_job_ckpt_state)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Notify the orte-checkpoint command once we have everyone stopped.
     * No need to broadcast this to everyone since they already know.
     */
    if( ORTE_SNAPC_CKPT_STATE_STOPPED == loc_min_state &&
        ORTE_SNAPC_CKPT_STATE_STOPPED > current_job_ckpt_state) {
        current_job_ckpt_state = loc_min_state;

        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global)    All Processes have been stopped!\n"));

        if( is_orte_checkpoint_connected &&
            ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender, 
                                                                                global_snapshot.reference_name,
                                                                                global_snapshot.seq_num,
                                                                                current_job_ckpt_state)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /* orte-checkpoint detaches at this point */
        is_orte_checkpoint_connected = false;

        /*
         * Write out metadata
         */
        write_out_global_metadata();
    }

    /*
     * if(all_orted == FINISHED_LOCAL) {
     *   xcast(FIN_LOCAL)
     *   if( !xfer ) {
     *     xcast(FIN) -- happens in job_state_update -- 
     *   }
     * }
     * if(orted == FINISHED_LOCAL && xfer) {
     *   start_filem_xfer();
     *   send(FIN) when finished with xfer
     * }
     */
    /*
     * If all daemons have finished
     */
    if( loc_min_state == ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL ) {
        if(orte_snapc_full_timing_enabled) {
            timer_local_done = get_time();
        }

        if( ORTE_SNAPC_CKPT_STATE_NONE != current_job_ckpt_state ) {
            if( loc_min_state == current_job_ckpt_state) {
                opal_output(0, "Global) JJH WARNING!!: (%d) == (%d)", loc_min_state, current_job_ckpt_state);
            }
        }

        /*
         * If we know that there is no file transfer, just fast path the
         * finished message, the local coordinator will know how to handle it.
         */
        if( orte_snapc_base_store_in_place || orte_snapc_full_skip_filem) {
            current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_FINISHED;
        } else {
            current_job_ckpt_state = loc_min_state;
        }

        if( NULL != state_str ) {
            free(state_str);
        }
        orte_snapc_ckpt_state_str(&state_str, current_job_ckpt_state);
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global) Job State Changed: %d (%s)\n",
                             (int)current_job_ckpt_state, state_str ));
        free(state_str);
        state_str = NULL;

        if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(current_global_jobid,
                                                                            current_job_ckpt_state,
                                                                            NULL, NULL, true,
                                                                            NULL) ) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * If the process has finished the local checkpoint, start any transfers
     * while the other daemons are reporting in.
     *
     * if(orted == FINISHED_LOCAL && xfer) {
     *   start_filem_xfer();
     *   send(FIN) when finished with xfer
     * }
     */
    if( ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL == orted_snapshot->state ) {
        if(!orte_snapc_base_store_in_place && !orte_snapc_full_skip_filem) {
            /* Start the transfer of files while other daemons are reporting in */
            orted_snapshot->state = ORTE_SNAPC_CKPT_STATE_FILE_XFER;

            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global) Starting FileM (%s)",
                                 ORTE_NAME_PRINT(&orted_snapshot->process_name)));
            if( ORTE_SUCCESS != (ret = snapc_full_start_filem(orted_snapshot) ) ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }
        }
    }

    /*
     * If all of the daemons are currently transferring data,
     * wait here until done. Then xcast(FIN)
     */
    loc_min_state = snapc_full_global_get_min_state();
    if( ORTE_SNAPC_CKPT_STATE_FILE_XFER == loc_min_state ) {
        wait_all_xfer = true;
    }

 cleanup:
    if( NULL != state_str ) {
        free(state_str);
        state_str = NULL;
    }

    return exit_status;
}

static void snapc_full_process_filem_xfer(void)
{
    int ret;
    char * state_str = NULL;

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) Wait for all FileM to complete"));
    if( ORTE_SUCCESS != (ret = snapc_full_wait_filem() ) ) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    if(orte_snapc_full_timing_enabled) {
        timer_xfer_done = get_time();
    }
    current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_FINISHED;

    orte_snapc_ckpt_state_str(&state_str, current_job_ckpt_state);
    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) Job State Changed: %d (%s) -- Done with Transfer of files\n",
                         (int)current_job_ckpt_state, state_str ));

    if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(current_global_jobid,
                                                                        current_job_ckpt_state,
                                                                        NULL, NULL, true,
                                                                        NULL) ) ) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

 cleanup:
    if(NULL != state_str ){
        free(state_str);
        state_str = NULL;
    }

    return;
}

static void snapc_full_process_job_update_cmd(orte_process_name_t* sender,
                                              opal_buffer_t* buffer,
                                              bool quick)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count;
    orte_jobid_t jobid;
    int   job_ckpt_state = ORTE_SNAPC_CKPT_STATE_NONE;
    char *job_ckpt_snapshot_ref = NULL;
    char *job_ckpt_snapshot_loc = NULL;
    size_t loc_seq_num = 0;
    opal_crs_base_ckpt_options_t *options = NULL;

    /*
     * Unpack the data (quick)
     * - jobid
     * - ckpt_state
     * Unpack the data (long)
     * - jobid
     * - ckpt_state
     * - snapshot reference
     * - snapshot_location
     * - local seq number
     * - ckpt_options
     */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &jobid, &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job_ckpt_state, &count, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( !quick ) {
        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job_ckpt_snapshot_ref, &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &job_ckpt_snapshot_loc, &count, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        count = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &loc_seq_num, &count, OPAL_SIZE))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        options = OBJ_NEW(opal_crs_base_ckpt_options_t);
        if( ORTE_SUCCESS != (ret = orte_snapc_base_unpack_options(buffer, options)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
        /* In this case we want to use the current_options that are cached
         * so that we do not have to send them every time.
         */
        opal_crs_base_copy_options(options, current_options);
    }

    if( ORTE_SUCCESS != (ret = global_coord_job_state_update(jobid,
                                                             job_ckpt_state,
                                                             &job_ckpt_snapshot_ref,
                                                             &job_ckpt_snapshot_loc,
                                                             current_options) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != options ) {
        OBJ_RELEASE(options);
        options = NULL;
    }

    return;
}

static int snapc_full_establish_snapshot_dir(bool empty_metadata)
{
    int ret;
    char * global_snapshot_handle = NULL;

    /*********************
     * Generate the global snapshot directory, and unique global snapshot handle
     *********************/
    INC_SEQ_NUM();
    if( NULL == global_snapshot_handle ) {
        orte_snapc_base_unique_global_snapshot_name(&global_snapshot_handle, getpid());
    }

    orte_snapc_base_get_global_snapshot_directory(&orte_snapc_base_global_snapshot_loc, global_snapshot_handle);

    global_snapshot.seq_num = orte_snapc_base_snapshot_seq_number;
    global_snapshot.reference_name = strdup(global_snapshot_handle);
    global_snapshot.local_location = opal_dirname(orte_snapc_base_global_snapshot_loc);

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) Setup Directory (seq = %d) (dir = %s)",
                         global_snapshot.seq_num, orte_snapc_base_global_snapshot_loc));

    /* Creates the directory (with metadata files):
     *   /tmp/ompi_global_snapshot_PID.ckpt/seq_num
     */
    if( ORTE_SUCCESS != (ret = orte_snapc_base_init_global_snapshot_directory(global_snapshot.reference_name, empty_metadata))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    if( NULL != global_snapshot_handle ) {
        free(global_snapshot_handle);
        global_snapshot_handle = NULL;
    }

    return ORTE_SUCCESS;
}

static int snapc_full_global_checkpoint(opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) Checkpoint of job %s has been requested\n",
                         ORTE_JOBID_PRINT(current_global_jobid)));

    current_job_ckpt_state = ORTE_SNAPC_CKPT_STATE_REQUEST;

    /*********************
     * Generate the global snapshot directory, and unique global snapshot handle
     *********************/
    if( ORTE_SUCCESS != (ret = snapc_full_establish_snapshot_dir(false))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /***********************************
     * Do an update handshake with the orte_checkpoint command
     ***********************************/
    updated_job_to_running = false;
    if( is_orte_checkpoint_connected &&
        ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender,
                                                                            global_snapshot.reference_name,
                                                                            global_snapshot.seq_num,
                                                                            ORTE_SNAPC_CKPT_STATE_REQUEST) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) Using the checkpoint directory (%s)\n",
                         global_snapshot.reference_name));

    /**********************
     * Notify the Local Snapshot Coordinators of the checkpoint request
     **********************/
    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Global) Notifying the Local Coordinators\n"));

    if( ORTE_SUCCESS != (ret = snapc_full_global_notify_checkpoint(current_global_jobid, options)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status; 
}

static int  snapc_full_global_notify_checkpoint(orte_jobid_t jobid,
                                                opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    opal_list_item_t* item = NULL;
    char * global_dir = NULL;
    int ckpt_state;

    orte_snapc_base_get_global_snapshot_directory(&global_dir, global_snapshot.reference_name);
    ckpt_state = ORTE_SNAPC_CKPT_STATE_PENDING;

    /*
     * Update the global structure
     */
    for(item  = opal_list_get_first(&global_snapshot.local_snapshots);
        item != opal_list_get_end(&global_snapshot.local_snapshots);
        item  = opal_list_get_next(item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;
        orted_snapshot->state   = ckpt_state;

        opal_crs_base_copy_options(options, orted_snapshot->options);
    }

    /*
     * Update the job state, and broadcast to all local daemons
     */
    orte_snapc_base_global_snapshot_loc = strdup(global_dir);
    if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(jobid,
                                                                        ckpt_state,
                                                                        global_snapshot.reference_name,
                                                                        global_dir,
                                                                        false,
                                                                        options) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != global_dir)
        free(global_dir);

    return exit_status;
}

/**********************************
 * Job/Proc State Set/Get Routines
 **********************************/
static int orte_snapc_full_global_set_job_ckpt_info( orte_jobid_t jobid,
                                                     int    ckpt_state, 
                                                     char  *ckpt_snapshot_ref,
                                                     char  *ckpt_snapshot_loc,
                                                     bool quick,
                                                     opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_cmd_flag_t command;
    opal_buffer_t buffer;
    char * state_str = NULL;

    /*
     * Update all Local Coordinators (broadcast operation)
     */
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if( quick ) {
        command = ORTE_SNAPC_FULL_UPDATE_JOB_STATE_QUICK_CMD;
    } else {
        command = ORTE_SNAPC_FULL_UPDATE_JOB_STATE_CMD;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &ckpt_state, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( quick ) {
        goto process_msg;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &ckpt_snapshot_ref, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &ckpt_snapshot_loc, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &orte_snapc_base_snapshot_seq_number, 1, OPAL_SIZE))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = orte_snapc_base_pack_options(&buffer, options)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 process_msg:
    orte_snapc_ckpt_state_str(&state_str, ckpt_state);
    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Global) Notify Local Coordinators of job %s state change to %d (%s)\n",
                         ORTE_JOBID_PRINT(jobid), (int)ckpt_state, state_str ));
    free(state_str);
    state_str = NULL;

    if( ORTE_SUCCESS != (ret = orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid, &buffer, ORTE_RML_TAG_SNAPC_FULL)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * We will also receive the job update, and process in the RML callback
     */

 cleanup:
    if( NULL != state_str ) {
        free(state_str);
        state_str = NULL;
    }

    OBJ_DESTRUCT(&buffer);
    return exit_status;
}

int global_coord_job_state_update(orte_jobid_t jobid,
                                  int    job_ckpt_state,
                                  char **job_ckpt_snapshot_ref,
                                  char **job_ckpt_snapshot_loc,
                                  opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;
    opal_list_item_t* item = NULL;
    opal_list_item_t* aitem = NULL;
    bool term_job  = false;
    char * state_str = NULL;

    orte_snapc_ckpt_state_str(&state_str, job_ckpt_state);
    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "Global) Job update command: jobid %s -> state %d (%s)\n",
                         ORTE_JOBID_PRINT(jobid), (int)job_ckpt_state, state_str ));
    free(state_str);
    state_str = NULL;

    /************************
     * Update the orte_checkpoint command
     ************************/
    current_job_ckpt_state = job_ckpt_state;
    if( is_orte_checkpoint_connected &&
        ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(&orte_checkpoint_sender, 
                                                                            global_snapshot.reference_name,
                                                                            global_snapshot.seq_num,
                                                                            current_job_ckpt_state)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Global Coordinator: If also a Local coordinator then act locally before globally
     */
    if( ORTE_SNAPC_LOCAL_COORD_TYPE == (orte_snapc_coord_type & ORTE_SNAPC_LOCAL_COORD_TYPE) ) {
        if( ORTE_SUCCESS != (ret = local_coord_job_state_update(jobid,
                                                                job_ckpt_state,
                                                                job_ckpt_snapshot_ref,
                                                                job_ckpt_snapshot_loc,
                                                                options)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * If we have completed locally, and not transfering files
     * then just finish the checkpoint operation.
     *
     * Otherwise the FIN is xcast'ed in process_orted_update_cmd()
     */
    if( ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL == job_ckpt_state ) {
        if( orte_snapc_base_store_in_place || orte_snapc_full_skip_filem) {
            if( ORTE_SUCCESS != (ret = orte_snapc_full_global_set_job_ckpt_info(current_global_jobid,
                                                                                ORTE_SNAPC_CKPT_STATE_FINISHED,
                                                                                NULL, NULL, true, options) ) ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }
        }
    }
    /*
     * Once finished, then cleanup and finalize the global snapshot
     */
    else if( ORTE_SNAPC_CKPT_STATE_FINISHED  == job_ckpt_state ||
             ORTE_SNAPC_CKPT_STATE_ERROR     == job_ckpt_state ) {
        /*
         * Write out metadata
         * if we are stopping then we have already written out this data.
         */
        if( ! (current_options->stop) ) {
            write_out_global_metadata();
        }

        /*
         * Clear globally cached options
         */
        opal_crs_base_clear_options(current_options);

        /*
         * Reset global data structures
         */
        for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
            item != opal_list_get_end(&(global_snapshot.local_snapshots));
            item  = opal_list_get_next(item) ) {
            orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

            orted_snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;

            if( orted_snapshot->options->term ) {
                term_job = true;
            }
            opal_crs_base_clear_options(orted_snapshot->options);

            for(aitem  = opal_list_get_first(&(orted_snapshot->super.local_snapshots));
                aitem != opal_list_get_end(&(orted_snapshot->super.local_snapshots));
                aitem  = opal_list_get_next(aitem) ) {
                app_snapshot = (orte_snapc_base_local_snapshot_t*)aitem;

                app_snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;
                if( NULL != app_snapshot->reference_name ) {
                    free(app_snapshot->reference_name);
                    app_snapshot->reference_name = NULL;
                }
                if( NULL != app_snapshot->local_location ) {
                    free(app_snapshot->local_location);
                    app_snapshot->local_location = NULL;
                }
                if( NULL != app_snapshot->remote_location ) {
                    free(app_snapshot->remote_location);
                    app_snapshot->remote_location = NULL;
                }
            }
        }

        if(orte_snapc_full_timing_enabled) {
            timer_end = get_time();
            print_time();
            timer_start = 0;
            timer_local_done = 0;
            timer_xfer_done = 0;
            timer_end = 0;
        }

        /************************
         * Set up the Command Line listener again
         *************************/
        is_orte_checkpoint_connected = false;
        if( ORTE_SUCCESS != (ret = snapc_full_global_start_cmdline_listener() ) ){
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
        }

        /********************************
         * Terminate the job if requested
         * At this point the application should have already exited, but do this
         * just to make doubly sure that the job is terminated.
         *********************************/
        if( term_job ) {
            orte_plm.terminate_job(jobid);
        }
    }
    /*
     * This should not happen, since this state is always handled locally
     */
    else if(ORTE_SNAPC_CKPT_STATE_STOPPED == job_ckpt_state ) {
        ;
    }
    /*
     * This should not happen, since this state is always handled locally
     */
    else if( ORTE_SNAPC_CKPT_STATE_FILE_XFER == job_ckpt_state ) {
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "Global) JJH WARNING: job state = %d (FILE_XFER)",
                             job_ckpt_state));
    }
    /*
     * This should not happen, since we do not handle this case
     */
    else if(ORTE_SNAPC_CKPT_STATE_REQUEST == job_ckpt_state ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "ERROR: Internal Checkpoint request not implemented.");
        ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
    }

 cleanup:
    if( NULL != state_str) {
        free(state_str);
        state_str = NULL;
    }

    return exit_status;
}

static int write_out_global_metadata(void)
{
    int ret;
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;
    opal_list_item_t* orted_item = NULL;
    opal_list_item_t* app_item = NULL;

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) Updating Metadata"));

    for(orted_item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        orted_item != opal_list_get_end(&(global_snapshot.local_snapshots));
        orted_item  = opal_list_get_next(orted_item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)orted_item;

        if( ORTE_SNAPC_CKPT_STATE_ERROR == orted_snapshot->state ) {
            return ORTE_ERROR;
        }

        for(app_item  = opal_list_get_first(&(orted_snapshot->super.local_snapshots));
            app_item != opal_list_get_end(&(orted_snapshot->super.local_snapshots));
            app_item  = opal_list_get_next(app_item) ) {
            app_snapshot = (orte_snapc_base_local_snapshot_t*)app_item;

            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global)   Process Name: %s\n",
                                 ORTE_NAME_PRINT(&app_snapshot->process_name) ));
            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global)     Reference : %s\n",
                                 app_snapshot->reference_name));
            OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                                 "Global)     Location  : %s\n",
                                 app_snapshot->local_location));

            if(ORTE_SUCCESS != (ret = orte_snapc_base_add_vpid_metadata(&app_snapshot->process_name,
                                                                        global_snapshot.reference_name,
                                                                        app_snapshot->reference_name,
                                                                        app_snapshot->local_location,
                                                                        orted_snapshot->opal_crs) ) ){
                ORTE_ERROR_LOG(ret);
                return ret;
            }
        }

    }

    orte_snapc_base_finalize_metadata(global_snapshot.reference_name);

    return ORTE_SUCCESS;
}

static orte_snapc_full_orted_snapshot_t *find_orted_snapshot(orte_process_name_t *name )
{
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    opal_list_item_t* item = NULL;

    for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        item != opal_list_get_end(&(global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

        if( name->jobid == orted_snapshot->process_name.jobid &&
            name->vpid  == orted_snapshot->process_name.vpid ) {
            return orted_snapshot;
        }
    }

    return NULL;
}

static orte_snapc_base_local_snapshot_t *find_orted_app_snapshot(orte_snapc_full_orted_snapshot_t *orted_snapshot,
                                                                 orte_process_name_t *name)
{
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;
    opal_list_item_t* item = NULL;

    for(item  = opal_list_get_first(&(orted_snapshot->super.local_snapshots));
        item != opal_list_get_end(&(orted_snapshot->super.local_snapshots));
        item  = opal_list_get_next(item) ) {
        app_snapshot = (orte_snapc_base_local_snapshot_t*)item;

        if( name->jobid == app_snapshot->process_name.jobid &&
            name->vpid  == app_snapshot->process_name.vpid ) {
            return app_snapshot;
        }
    }

    return NULL;
}
static int snapc_full_start_filem(orte_snapc_full_orted_snapshot_t *orted_snapshot)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_filem_base_process_set_t *p_set = NULL;
    orte_filem_base_file_set_t * f_set = NULL;
    opal_list_t all_filem_requests;
    orte_snapc_base_local_snapshot_t *app_snapshot = NULL;
    opal_list_item_t* item = NULL;

    OBJ_CONSTRUCT(&all_filem_requests, opal_list_t);

    /*
     * If we just want to pretend to do the filem
     */
    if(orte_snapc_full_skip_filem) {
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }
    /*
     * If it is stored in place, then we do not need to transfer anything
     * -- Should not have gotten here, so return an error --
     */
    else if( orte_snapc_base_store_in_place ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * Setup the FileM data structures to transfer the files
     */
    orted_snapshot->filem_request = OBJ_NEW(orte_filem_base_request_t);
    /*
     * Construct the process set
     */
    p_set = OBJ_NEW(orte_filem_base_process_set_t);

    p_set->source.jobid = orted_snapshot->process_name.jobid;
    p_set->source.vpid  = orted_snapshot->process_name.vpid;
    p_set->sink.jobid   = ORTE_PROC_MY_NAME->jobid;
    p_set->sink.vpid    = ORTE_PROC_MY_NAME->vpid;

    opal_list_append(&(orted_snapshot->filem_request->process_sets), &(p_set->super) );

    for(item  = opal_list_get_first(&(orted_snapshot->super.local_snapshots));
        item != opal_list_get_end(&(orted_snapshot->super.local_snapshots));
        item  = opal_list_get_next(item) ) {
        app_snapshot = (orte_snapc_base_local_snapshot_t*)item;

        /* If one of the checkpoints failed, we need to return an error */
        if( ORTE_SNAPC_CKPT_STATE_ERROR == app_snapshot->state ) {
            exit_status = ORTE_ERROR;
            ORTE_ERROR_LOG(ORTE_ERROR);
            goto cleanup;
        }

        /*
         * Construct the file set
         */
        f_set = OBJ_NEW(orte_filem_base_file_set_t);

        f_set->local_target  = strdup(orte_snapc_base_global_snapshot_loc);
        if( orte_snapc_base_is_global_dir_shared ) {
            f_set->local_hint = ORTE_FILEM_HINT_SHARED;
        }

        asprintf(&(f_set->remote_target), "%s/%s", app_snapshot->remote_location, app_snapshot->reference_name);

        f_set->target_flag   = ORTE_FILEM_TYPE_DIR;

        opal_list_append(&(orted_snapshot->filem_request->file_sets), &(f_set->super) );

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) ... FileM (%s) [%s] --> [%s]",
                             ORTE_NAME_PRINT(&orted_snapshot->process_name), f_set->remote_target, f_set->local_target));
    }

    /*
     * Start the transfer
     */
    if(ORTE_SUCCESS != (ret = orte_filem.get_nb(orted_snapshot->filem_request) ) ) {
        OBJ_RELEASE(orted_snapshot->filem_request);
        orted_snapshot->filem_request = NULL;
        exit_status = ret;
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

static int snapc_full_wait_filem(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_list_t all_filem_requests;
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    opal_list_item_t* item = NULL;

    OBJ_CONSTRUCT(&all_filem_requests, opal_list_t);

    /*
     * Construct a list for wait_all()
     */
    for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        item != opal_list_get_end(&(global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

        if( NULL != orted_snapshot->filem_request ) {
            opal_list_append(&all_filem_requests, &(orted_snapshot->filem_request->super));
        }
    }

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) FileM -- Enter wait_all() Get"));

    /*
     * Wait for all transfers to complete
     */
    if(ORTE_SUCCESS != (ret = orte_filem.wait_all(&all_filem_requests) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) FileM -- Setup removal()"));

    /*
     * Start removal of old data
     */
    for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        item != opal_list_get_end(&(global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

        if( NULL != orted_snapshot->filem_request ) {
            if(ORTE_SUCCESS != (ret = orte_filem.rm_nb(orted_snapshot->filem_request)) ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }
        }
    }

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "Global) FileM -- Enter wait_all() Remove"));

    /*
     * Wait for all removals to complete
     */
    if(ORTE_SUCCESS != (ret = orte_filem.wait_all(&all_filem_requests) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        item != opal_list_get_end(&(global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

        if( NULL != orted_snapshot->filem_request ) {
            /*OBJ_RELEASE(orted_snapshot->filem_request);*/
            orted_snapshot->filem_request = NULL;
        }
    }

    /* JJH I don't think this is needed (??) */
    while (NULL != (item = opal_list_remove_first(&all_filem_requests) ) ) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&all_filem_requests);
    return exit_status;
}

static int snapc_full_global_get_min_state(void)
{
    int min_state = ORTE_SNAPC_CKPT_MAX;
    orte_snapc_full_orted_snapshot_t *orted_snapshot = NULL;
    opal_list_item_t* item = NULL;
    char * state_str_a = NULL;
    char * state_str_b = NULL;

    for(item  = opal_list_get_first(&(global_snapshot.local_snapshots));
        item != opal_list_get_end(&(global_snapshot.local_snapshots));
        item  = opal_list_get_next(item) ) {
        orted_snapshot = (orte_snapc_full_orted_snapshot_t*)item;

        /* Ignore orteds with no processes */
        if( 0 >= opal_list_get_size(&(orted_snapshot->super.local_snapshots)) ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) ... Skipping - %s (no children)",
                                 ORTE_NAME_PRINT(&orted_snapshot->process_name) ));
            continue;
        }

        if( NULL != state_str_a ) {
            free(state_str_a);
            state_str_a = NULL;
        }
        if( NULL != state_str_b ) {
            free(state_str_b);
            state_str_b = NULL;
        }

        orte_snapc_ckpt_state_str(&state_str_a, orted_snapshot->state);
        orte_snapc_ckpt_state_str(&state_str_b, min_state);

        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "Global) ... Checking [%d %s] vs [%d %s]",
                             (int)orted_snapshot->state, state_str_a,
                             min_state, state_str_b ));

        if( (int)min_state > (int)orted_snapshot->state ) {
            min_state = orted_snapshot->state;

            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "Global) ... Update --> Min State [%d %s]",
                                 (int)min_state, state_str_a ));
        }
    }

    if( NULL != state_str_b ) {
        free(state_str_b);
        state_str_b = NULL;
    }
    orte_snapc_ckpt_state_str(&state_str_b, min_state);
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "Global) ... Min State [%d %s]",
                         (int)min_state, state_str_b ));

    if( NULL != state_str_a ) {
        free(state_str_a);
        state_str_a = NULL;
    }
    if( NULL != state_str_b ) {
        free(state_str_b);
        state_str_b = NULL;
    }

    return min_state;
}

static double get_time(void) {
    double wtime;

#if OPAL_TIMER_USEC_NATIVE
    wtime = (double)opal_timer_base_get_usec() / 1000000.0;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
#endif

    return wtime;
}

static void print_time(void) {
    double t_local, t_transfer, t_cleanup, t_total;

    if(!orte_snapc_full_timing_enabled) {
        return;
    }

    t_total = timer_end - timer_start;

    t_local = timer_local_done - timer_start;

    if(orte_snapc_base_store_in_place || orte_snapc_full_skip_filem) {
        t_transfer = 0;
        t_cleanup = timer_end - timer_local_done;
    } else {
        t_transfer = timer_xfer_done - timer_local_done;
        t_cleanup = timer_end - timer_xfer_done;
    }

    opal_output(0, "Checkpoint Time:");
    opal_output(0, "\tLocal   :  %10.2f s\n", t_local);
    opal_output(0, "\tTransfer:  %10.2f s\n", t_transfer);
    opal_output(0, "\tCleanup :  %10.2f s\n", t_cleanup);
    opal_output(0, "\tTotal   :  %10.2f s\n", t_total);

    return;
}
