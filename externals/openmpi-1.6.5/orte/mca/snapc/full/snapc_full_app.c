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

#include "orte_config.h"

#include <errno.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>  /* for mkfifo */
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/runtime/orte_cr.h"
#include "orte/runtime/orte_globals.h"
#include "opal/runtime/opal_cr.h"
#include "opal/util/output.h"
#include "opal/event/event.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/util/name_fns.h"
#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"

#include "snapc_full.h"

/************************************
 * Locally Global vars & functions :)
 ************************************/
static void snapc_full_app_signal_handler (int signo);
static int snapc_full_app_notify_response(opal_cr_ckpt_cmd_state_t resp);
static int app_notify_resp_stage_1(opal_cr_ckpt_cmd_state_t resp,
                                   opal_crs_base_ckpt_options_t *options);
static int app_notify_resp_stage_2(int cr_state );
static int app_notify_resp_stage_3(int cr_state);
static int snapc_full_app_notify_reopen_files(void);
static int snapc_full_app_ckpt_handshake_start(opal_crs_base_ckpt_options_t *options,
                                               opal_cr_ckpt_cmd_state_t resp);
static int snapc_full_app_ckpt_handshake_end(int cr_state);

static int snapc_full_app_ft_event_update_process_info(orte_process_name_t proc, pid_t pid);

static char *app_comm_pipe_r = NULL;
static char *app_comm_pipe_w = NULL;
static int   app_comm_pipe_r_fd = -1;
static int   app_comm_pipe_w_fd = -1;

static opal_crs_base_snapshot_t *local_snapshot = NULL;

static int app_cur_epoch  = -1;
static int app_last_epoch = -1;
static bool app_split_ckpt = false;
static bool app_notif_processed = false;

static char * app_cur_global_ref = NULL;

/************************
 * Function Definitions
 ************************/

int app_coord_init() {
    int exit_status  = ORTE_SUCCESS;
    opal_cr_notify_callback_fn_t prev_notify_func;
    char *tmp_pid = NULL;

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "App) Initalized for Application %s\n", 
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /*
     * Register the INC notification callback
     */
    opal_cr_reg_notify_callback(snapc_full_app_notify_response, &prev_notify_func);

    /* String representation of the PID */
    asprintf(&tmp_pid, "%d", getpid());

    asprintf(&app_comm_pipe_r, "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_R, tmp_pid);
    asprintf(&app_comm_pipe_w, "%s/%s.%s", opal_cr_pipe_dir, OPAL_CR_NAMED_PROG_W, tmp_pid);

    /*
     * Setup a signal handler to catch and start the proper thread
     * to handle the checkpoint
     */
    if( SIG_ERR == signal(opal_cr_entry_point_signal, snapc_full_app_signal_handler) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) init: Error: Failed to register signal %d\n",
                    opal_cr_entry_point_signal);
        ORTE_ERROR_LOG(OPAL_ERROR);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((15, mca_snapc_full_component.super.output_handle,
                         "app) Named Pipes (%s) (%s), Signal (%d)", 
                         app_comm_pipe_r, app_comm_pipe_w, opal_cr_entry_point_signal));

 cleanup:
    if( NULL != tmp_pid) {
        free(tmp_pid);
        tmp_pid = NULL;
    }

    return exit_status;
}

int app_coord_finalize() {
    /*
     * Cleanup named pipes
     */
    if( NULL != app_comm_pipe_r) {
        free(app_comm_pipe_r);
        app_comm_pipe_r = NULL;
    }

    if( NULL != app_comm_pipe_w) {
        free(app_comm_pipe_w);
        app_comm_pipe_w = NULL;
    }

    return ORTE_SUCCESS;
}

/******************
 * Local functions
 ******************/
static void snapc_full_app_signal_handler (int signo)
{
    if( opal_cr_entry_point_signal != signo ) {
        OPAL_OUTPUT_VERBOSE((1, mca_snapc_full_component.super.output_handle,
                             "App) signal_handler: Received unknown signal %d",
                             signo));
        /* Not our signal */
        return;
    }
    /*
     * Signal thread to start checkpoint handshake
     */
    opal_cr_checkpoint_request   = OPAL_CR_STATUS_REQUESTED;

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) signal_handler: Receive Checkpoint Request."));
}

/*
 * Respond to an asynchronous checkpoint request
 */
int snapc_full_app_notify_response(opal_cr_ckpt_cmd_state_t resp)
{
    static opal_crs_base_ckpt_options_t *options = NULL;
    static int cr_state;
    int app_pid;
    int ret, exit_status = ORTE_SUCCESS;

    if( NULL == options ) {
        options = OBJ_NEW(opal_crs_base_ckpt_options_t);
    }

    if( opal_cr_currently_stalled ) {
        goto STAGE_1;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Stage 1..."));
    if( ORTE_SUCCESS != (ret = app_notify_resp_stage_1(resp, options) ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto ckpt_cleanup;
    }

    /*
     * If this is a split checkpoint operation then we only need to do stage_1,
     * but we need to keep the name pipe open for the end();
     */
    if( app_split_ckpt ) {
        app_notif_processed = true;
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Start checkpoint..."));
 STAGE_1:
    opal_cr_currently_stalled = false;

    app_pid = getpid();
    if( orte_snapc_full_skip_app ) {
        OPAL_OUTPUT_VERBOSE((2, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Skipping App. (%d)\n",
                             getpid()));
        ret = ORTE_SUCCESS;
        cr_state = OPAL_CRS_CONTINUE;
    }
    else {
        /*
         * INC: Prepare stack using the registered coordination routine
         */
        if(OPAL_SUCCESS != (ret = opal_cr_inc_core_prep() ) ) {
            if( OPAL_EXISTS == ret ) {
                OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                                     "App) notify_response: Stalling the checkpoint progress until state is stable again (PID = %d)\n",
                                     getpid()));
                opal_cr_currently_stalled = true;
                return exit_status;
            }
            else {
                opal_output(mca_snapc_full_component.super.output_handle,
                            "App) notify_response: Error: checkpoint notification failed. %d\n", ret);
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto ckpt_cleanup;
            }
        }

        /*
         * INC: Take the checkpoint
         */
        ret = opal_cr_inc_core_ckpt(app_pid, local_snapshot, options, &cr_state);

        /*
         * Tell Local Coordinator that we are done with local checkpoint
         * (only if not restarting, on restart we are not attached to the Local
         *  Coordinator. )
         */
        if( OPAL_CRS_RESTART != cr_state ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "App) notify_response: Stage 2..."));
            if( ORTE_SUCCESS != (ret = app_notify_resp_stage_2(cr_state) ) ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto ckpt_cleanup;
            }
        }

        /*
         * INC: Recover stack using the registered coordination routine
         */
        if( OPAL_SUCCESS != (ret = opal_cr_inc_core_recover(cr_state)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto ckpt_cleanup;
        }
    }

    /* Don't stall any longer */
    opal_cr_stall_check = false;

    if(OPAL_CRS_RESTART == cr_state) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Restarting...(%d)\n",
                             getpid()));
        
        options->term = false;
        /* Do not respond to the non-existent command line tool */
        goto ckpt_cleanup;
    }
    else if(cr_state == OPAL_CRS_CONTINUE) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Continuing...(%d)\n",
                             getpid()));
        ; /* Don't need to do anything here */
    }
    else if(cr_state == OPAL_CRS_TERM ) {
        ; /* Don't need to do anything here */
    }
    else {
        OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Unknown cr_state(%d) [%d]",
                             cr_state, getpid()));
    }

 ckpt_cleanup:

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Stage 3..."));
    if( ORTE_SUCCESS != (ret = app_notify_resp_stage_3(cr_state) )) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto ckpt_cleanup;
    }
    
    if( options->term ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: User has asked to terminate the application"));
        exit(ORTE_SUCCESS);
    }

    if( NULL != options ) {
        OBJ_RELEASE(options);
        options = NULL;
    }

    return exit_status;
}

static int app_notify_resp_stage_1(opal_cr_ckpt_cmd_state_t resp,
                                   opal_crs_base_ckpt_options_t *options)
{
    int ret;

    OPAL_CR_CLEAR_TIMERS();
    opal_cr_timing_my_rank = ORTE_PROC_MY_NAME->vpid;
    OPAL_CR_SET_TIMER(OPAL_CR_TIMER_ENTRY0);

    /*
     * Open communication channels
     */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Open Communication Channels."));
    if (ORTE_SUCCESS != (ret = snapc_full_app_notify_reopen_files())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Initial Handshake
     */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Initial Handshake."));
    if( ORTE_SUCCESS != (ret = snapc_full_app_ckpt_handshake_start(options, resp) ) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    OPAL_CR_SET_TIMER(OPAL_CR_TIMER_ENTRY1);

    /*
     * Begin checkpoint
     * - Init the checkpoint metadata file
     */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Init checkpoint directory..."));
    if( OPAL_SUCCESS != (ret = opal_crs_base_init_snapshot_directory(local_snapshot) ) ) {
        opal_output(0, "App) Error: Unable to initalize the snapshot directory!\n");
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    OPAL_CR_SET_TIMER(OPAL_CR_TIMER_ENTRY2);

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Start checkpoint..."));

    return ORTE_SUCCESS;
}

static int app_notify_resp_stage_2(int cr_state )
{
    int ret;

    /*
     * Final Handshake
     */
    OPAL_CR_SET_TIMER(OPAL_CR_TIMER_ENTRY3);
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Waiting for final handshake."));
    if( ORTE_SUCCESS != (ret = snapc_full_app_ckpt_handshake_end(cr_state ) ) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Final Handshake complete."));

    return ORTE_SUCCESS;
}

static int app_notify_resp_stage_3(int cr_state)
{
    if( 0 <= app_comm_pipe_r_fd ) {
        close(app_comm_pipe_r_fd);
        app_comm_pipe_r_fd = -1;
    }
    if( 0 <= app_comm_pipe_w_fd ) {
        close(app_comm_pipe_w_fd);
        app_comm_pipe_w_fd = -1;
    }

    remove(app_comm_pipe_r);
    remove(app_comm_pipe_w);

    app_comm_pipe_r_fd = -1;
    app_comm_pipe_w_fd = -1;

    /* Prepare to wait for another checkpoint action */
    opal_cr_checkpointing_state = OPAL_CR_STATUS_NONE;
    opal_cr_currently_stalled   = false;

    OPAL_CR_SET_TIMER(OPAL_CR_TIMER_ENTRY4);
    if(OPAL_CRS_RESTART != cr_state) {
        OPAL_CR_DISPLAY_ALL_TIMERS();
    }

    return ORTE_SUCCESS;
}

static int snapc_full_app_notify_reopen_files(void)
{
    int ret = OPAL_ERR_NOT_IMPLEMENTED;

#ifndef HAVE_MKFIFO
    return ret;
#else
#ifdef __WINDOWS__
    return ret;
#else
    /*
     * Open up the read pipe
     */
    if( (ret = mkfifo(app_comm_pipe_r, 0660)) < 0) {
        if(EEXIST == ret || -1 == ret ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "App) notify_reopen_files: mkfifo failed because file (%s) already exists, attempting to use this pipe. (%d)",
                                 app_comm_pipe_r, ret));
        }
        else {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "App) notify_reopen_files: Error: mkfifo failed to make named pipe (%s). (%d)\n",
                        app_comm_pipe_r, ret);
            return ORTE_ERROR;
        }
    }
    
    app_comm_pipe_r_fd = open(app_comm_pipe_r, O_RDWR);
    if(app_comm_pipe_r_fd < 0) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) init: Error: open failed to open the named pipe (%s). %d\n",
                    app_comm_pipe_r, app_comm_pipe_r_fd);
        return ORTE_ERROR;
    }

    /*
     * Open up the write pipe
     */
    if( (ret = mkfifo(app_comm_pipe_w, 0660)) < 0) {
        if(EEXIST == ret || -1 == ret ) {
            OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                                 "App) notify_reopen_files: mkfifo failed because file (%s) already exists, attempting to use this pipe. (%d)",
                                 app_comm_pipe_w, ret));
        }
        else {
            opal_output(mca_snapc_full_component.super.output_handle,
                        "App) notify_reopen_files: Error: mkfifo failed to make named pipe (%s). (%d)\n",
                        app_comm_pipe_w, ret);
            return ORTE_ERROR;
        }
    }
    
    app_comm_pipe_w_fd = open(app_comm_pipe_w, O_WRONLY);
    if(app_comm_pipe_w_fd < 0) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_reopen_files: Error: open failed to open the named pipe (%s). (%d)\n",
                    app_comm_pipe_w, app_comm_pipe_w_fd);
        return ORTE_ERROR;
    }
    
    return ORTE_SUCCESS;
#endif  /* __WINDOWS__ */
#endif  /* HAVE_MKFIFO */
}

static int snapc_full_app_ckpt_handshake_start(opal_crs_base_ckpt_options_t *options,
                                               opal_cr_ckpt_cmd_state_t resp)
{
    int ret, exit_status = ORTE_SUCCESS;
    int len = 0, tmp_resp, opt_rep;
    char *tmp_str = NULL;
    ssize_t tmp_size = 0;

    /*
     * Get the initial handshake command:
     * - Term argument
     * - Stop argument
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the 'term' from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    options->term = OPAL_INT_TO_BOOL(opt_rep);

    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &opt_rep, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the 'stop' from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    options->stop = OPAL_INT_TO_BOOL(opt_rep);

    tmp_resp = (int)resp;
    if( sizeof(int) != (ret = write(app_comm_pipe_w_fd, &tmp_resp, sizeof(int)) ) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: %d: Error: Unable to write to pipe (%s) ret = %d [Line %d]\n",
                    tmp_resp, app_comm_pipe_w, ret, __LINE__);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    
    /*
     * Respond that the checkpoint is currently in progress
     */
    if( OPAL_CHECKPOINT_CMD_IN_PROGRESS == resp ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Checkpoint in progress, cannot start (%d)",
                             getpid()));
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    /*
     * Respond that the application is unable to be checkpointed
     */
    else if( OPAL_CHECKPOINT_CMD_NULL == resp ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Non-checkpointable application, cannot start (%d)", 
                             getpid()));
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    /*
     * Respond that some error has occurred such that the application is 
     * not able to be checkpointed
     */
    else if( OPAL_CHECKPOINT_CMD_ERROR == resp ) {
        OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                             "App) notify_response: Error generated, cannot start (%d)", 
                             getpid()));
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    /*
     * Respond signalng that we wish to respond to this request
     */
    OPAL_OUTPUT_VERBOSE((10, mca_snapc_full_component.super.output_handle,
                         "App) notify_response: Starting checkpoint request (%d)", 
                         getpid()));

    /*
     * Get Snapshot Handle argument
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &len, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the snapshot_handle len from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    
    tmp_size = sizeof(char) * len;
    tmp_str  = (char *) malloc(sizeof(char) * len);
    if( tmp_size != (ret = read(app_comm_pipe_r_fd, tmp_str, (sizeof(char) * len))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the snapshot_handle from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    
    /* 
     * If they didn't send anything of meaning then use the defaults 
     */
    local_snapshot = OBJ_NEW(opal_crs_base_snapshot_t);

    if( 1 < strlen(tmp_str) ) {
        if( NULL != local_snapshot->reference_name)
            free( local_snapshot->reference_name );
        local_snapshot->reference_name = strdup(tmp_str);
        
        if( NULL != local_snapshot->local_location )
            free( local_snapshot->local_location );
        local_snapshot->local_location = opal_crs_base_get_snapshot_directory(local_snapshot->reference_name);
        
        if( NULL != local_snapshot->remote_location )
            free( local_snapshot->remote_location );
        local_snapshot->remote_location = strdup(local_snapshot->local_location);
    }
    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }
    
    /*
     * Get Snapshot location argument
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &len, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the snapshot_location len from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        goto cleanup;
    }
    
    tmp_str = (char *) malloc(sizeof(char) * len);
    tmp_size = sizeof(char) * len;
    if( tmp_size != (ret = read(app_comm_pipe_r_fd, tmp_str, (sizeof(char) * len))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the snapshot_location from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        goto cleanup;
    }
    
    /* 
     * If they didn't send anything of meaning then use the defaults 
     */
    if( 1 < strlen(tmp_str) ) {
        if( NULL != local_snapshot->local_location)
            free( local_snapshot->local_location );
        asprintf(&(local_snapshot->local_location), "%s/%s", tmp_str, local_snapshot->reference_name);
        
        if( NULL != local_snapshot->remote_location)
            free( local_snapshot->remote_location );
        local_snapshot->remote_location = strdup(local_snapshot->local_location);
    }

    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

    /*
     * Get Global Snapshot Ref
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &len, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the global snapshot ref len from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    tmp_str = (char *) malloc(sizeof(char) * len);
    tmp_size = sizeof(char) * len;
    if( tmp_size != (ret = read(app_comm_pipe_r_fd, tmp_str, (sizeof(char) * len))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the global snapshot ref from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    if( NULL != app_cur_global_ref ) {
        free(app_cur_global_ref);
        app_cur_global_ref = NULL;
    }
    app_cur_global_ref = strdup(tmp_str);

    /*
     * Get the Seq. Number
     */
    if( sizeof(size_t) != (ret = read(app_comm_pipe_r_fd, &tmp_size, sizeof(size_t))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the global snapshot seq number from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }
    app_cur_epoch = (int)tmp_size;

 cleanup:
    if( NULL != tmp_str ) {
        free(tmp_str);
        tmp_str = NULL;
    }

    return exit_status;
}

static int snapc_full_app_ckpt_handshake_end(int cr_state)
{
    int ret, exit_status = ORTE_SUCCESS;
    int last_cmd = 0;
    int err;

    /*
     * Return the final checkpoint state to the local coordinator
     */
    if( sizeof(int) != (ret = write(app_comm_pipe_w_fd, &cr_state, sizeof(int))) ) {
        err = errno;
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to write cr_state to named pipe (%s). %d/%d/%s\n",
                    app_comm_pipe_w, ret, err, strerror(err));
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "App) handshake_end: Waiting for release (%d)",
                         getpid()));

    /*
     * Wait for the local coordinator to release us
     */
    if( sizeof(int) != (ret = read(app_comm_pipe_r_fd, &last_cmd, sizeof(int))) ) {
        opal_output(mca_snapc_full_component.super.output_handle,
                    "App) notify_response: Error: Unable to read the 'last_cmd' from named pipe (%s). %d\n",
                    app_comm_pipe_r, ret);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((5, mca_snapc_full_component.super.output_handle,
                         "App) handshake_end: Released... (%d)",
                         getpid()));

 cleanup:
    return exit_status;
}

int app_coord_ft_event(int state) {
    int ret, exit_status = ORTE_SUCCESS;

    OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                         "App) In ft_event(%d)", state));

    /******** Checkpoint Prep ********/
    if(OPAL_CRS_CHECKPOINT == state) {
        ; /* Nothing */
    }
    /******** Continue Recovery ********/
    else if (OPAL_CRS_CONTINUE == state ) {
        ; /* Nothing */
    }
    /******** Restart Pre-Recovery ********/
    else if (OPAL_CRS_RESTART_PRE == state ) {
        ;
    }
    /******** Restart Recovery ********/
    else if (OPAL_CRS_RESTART == state ) {
        OPAL_OUTPUT_VERBOSE((20, mca_snapc_full_component.super.output_handle,
                             "App) Initalized for Application %s (Restart)\n", 
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

        /*
         * Send new PID to HNP/daemon
         * The checkpointer could have used a proxy program to boot us
         * so the pid that the orted got from fork() may not be the
         * PID of this application.
         * - Note: BLCR does this because it tries to preseve the PID
         *         of the program across checkpointes
         */
        if( ORTE_SUCCESS != (ret = snapc_full_app_ft_event_update_process_info(orte_process_info.my_name, getpid())) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    /******** Termination ********/
    else if (OPAL_CRS_TERM == state ) {
        ; /* Nothing */
    }
    /******** Error State ********/
    else {
        ; /* Nothing */
    }

 cleanup:
    return exit_status;
}

static int snapc_full_app_ft_event_update_process_info(orte_process_name_t proc, pid_t proc_pid)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t buffer;
    orte_snapc_cmd_flag_t command = ORTE_SNAPC_LOCAL_UPDATE_CMD;

    OBJ_CONSTRUCT(&buffer, opal_buffer_t);

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_CMD )) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &proc, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &proc_pid, 1, OPAL_PID))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buffer, ORTE_RML_TAG_SNAPC, 0))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    OBJ_DESTRUCT(&buffer);

    return exit_status;
}

int app_coord_start_ckpt(orte_snapc_base_quiesce_t *datum)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_START_CKPT_CMD;
    opal_buffer_t buffer;

    /*
     * Identify this as a split checkpoint
     */
    app_split_ckpt = true;

    /*
     * Rank 0: Contact HNP to start checkpoint
     * Rank *: Wait for HNP to xcast epoch
     */
    if( 0 == ORTE_PROC_MY_NAME->vpid ) {
        /*
         * Send request to HNP
         */
        OBJ_CONSTRUCT(&buffer, opal_buffer_t);

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            return ORTE_ERROR;
        }
        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(ORTE_PROC_MY_NAME->jobid), 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            return ORTE_ERROR;
        }

        if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            return ORTE_ERROR;
        }

        OBJ_DESTRUCT(&buffer);
    }

    while( app_cur_epoch < 0 || !app_notif_processed ) {
        opal_progress();
        opal_event_loop(OPAL_EVLOOP_NONBLOCK);
        OPAL_CR_TEST_CHECKPOINT_READY();
    }

    datum->epoch = app_cur_epoch;
    asprintf(&(datum->handle), "[%s:%s:%d]", app_cur_global_ref, local_snapshot->reference_name, app_cur_epoch);
    datum->target_dir = strdup(local_snapshot->local_location);

    /*
     * INC: Prepare the stack
     */
    if(OPAL_SUCCESS != (ret = opal_cr_inc_core_prep() ) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
    }

    opal_cr_checkpointing_state = OPAL_CR_STATUS_RUNNING;

    return ORTE_SUCCESS;
}

int app_coord_end_ckpt(orte_snapc_base_quiesce_t *datum)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_full_cmd_flag_t command = ORTE_SNAPC_FULL_END_CKPT_CMD;
    opal_buffer_t buffer;

    if( datum->restarting ) {
        datum->cr_state = OPAL_CRS_RESTART;
    } else {
        datum->cr_state = OPAL_CRS_CONTINUE;
    }

    /*
     * INC: Recover the stack
     */
    if(OPAL_SUCCESS != (ret = opal_cr_inc_core_recover(datum->cr_state) ) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    if( datum->cr_state != OPAL_CRS_CONTINUE ) {
        if( ORTE_SUCCESS != (ret = app_notify_resp_stage_3(datum->cr_state) )) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        goto cleanup;
    }

    if( ORTE_SUCCESS != (ret = app_notify_resp_stage_2(datum->cr_state) ) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    if( ORTE_SUCCESS != (ret = app_notify_resp_stage_3(datum->cr_state) )) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Rank 0: Contact HNP to let them know we are done
     * Then return to application
     */
    if( 0 == ORTE_PROC_MY_NAME->vpid ) {
        /*
         * Send request to HNP
         */
        OBJ_CONSTRUCT(&buffer, opal_buffer_t);

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &command, 1, ORTE_SNAPC_FULL_CMD))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            return ORTE_ERROR;
        }

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(ORTE_PROC_MY_NAME->jobid), 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            return ORTE_ERROR;
        }

        if (ORTE_SUCCESS != (ret = opal_dss.pack(&buffer, &(datum->epoch), 1, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            return ORTE_ERROR;
        }

        if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &buffer, ORTE_RML_TAG_SNAPC_FULL, 0))) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            OBJ_DESTRUCT(&buffer);
            return ORTE_ERROR;
        }

        OBJ_DESTRUCT(&buffer);
    }

    app_last_epoch = datum->epoch;
    app_cur_epoch  = -1;
    if( NULL != app_cur_global_ref ) {
        free(app_cur_global_ref);
        app_cur_global_ref = NULL;
    }

 cleanup:
    /*
     * Split checkpoint complete
     */
    app_split_ckpt = false;
    app_notif_processed = false;

    return ORTE_SUCCESS;
}
