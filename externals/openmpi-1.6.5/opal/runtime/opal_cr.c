/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file 
 * 
 * OPAL Layer Checkpoint/Restart Runtime functions
 *
 */

#include "opal_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <errno.h>
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

#include "opal/class/opal_object.h"
#include "opal/util/opal_environ.h"
#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/malloc.h"
#include "opal/util/if.h"
#include "opal/util/keyval_parse.h"
#include "opal/util/opal_environ.h"
#include "opal/util/argv.h"
#include "opal/memoryhooks/memory.h"

#include "opal/mca/base/base.h"
#include "opal/runtime/opal_cr.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"

#include "opal/mca/memcpy/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/timer/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/paffinity/base/base.h"

#include "opal/threads/mutex.h"
#include "opal/threads/threads.h"
#include "opal/mca/crs/base/base.h"

/******************
 * Global Var Decls
 ******************/
bool opal_cr_stall_check       = false;
bool opal_cr_currently_stalled = false;
int  opal_cr_output;

static double opal_cr_get_time(void);
static void display_indv_timer_core(double diff, char *str);
static double timer_start[OPAL_CR_TIMER_MAX];
bool opal_cr_timing_barrier_enabled = false;
bool opal_cr_timing_enabled = false;
int  opal_cr_timing_my_rank = 0;
int  opal_cr_timing_target_rank = 0;

/******************
 * Local Functions & Var Decls
 ******************/
static int extract_env_vars(int prev_pid);

static void opal_cr_sigpipe_debug_signal_handler (int signo);

static opal_cr_coord_callback_fn_t  cur_coord_callback = NULL;
static opal_cr_notify_callback_fn_t cur_notify_callback = NULL;

static int core_prev_pid = 0;

/******************
 * Interface Functions & Vars
 ******************/
char * opal_cr_pipe_dir   = NULL;
int    opal_cr_entry_point_signal     = 0;
bool   opal_cr_is_enabled = true;
bool   opal_cr_is_tool    = false;

/* Current checkpoint state */
int    opal_cr_checkpointing_state = OPAL_CR_STATUS_NONE;

/* Current checkpoint request channel state */
int    opal_cr_checkpoint_request  = OPAL_CR_STATUS_NONE;

static bool   opal_cr_debug_sigpipe = false;

#if OPAL_ENABLE_FT_THREAD == 1
/*****************
 * Threading Functions and Variables
 *****************/
static void* opal_cr_thread_fn(opal_object_t *obj);
bool    opal_cr_thread_is_done    = false;
bool    opal_cr_thread_is_active  = false;
bool    opal_cr_thread_in_library = false;
bool    opal_cr_thread_use_if_avail = true;
int32_t opal_cr_thread_num_in_library = 0;
int     opal_cr_thread_sleep_check = 0;
int     opal_cr_thread_sleep_wait = 0;
opal_thread_t opal_cr_thread;
opal_mutex_t  opal_cr_thread_lock;
#if 0
#define OPAL_CR_LOCK()           opal_cr_thread_in_library = true;  opal_mutex_lock(&opal_cr_thread_lock);
#define OPAL_CR_UNLOCK()         opal_cr_thread_in_library = false; opal_mutex_unlock(&opal_cr_thread_lock);
#define OPAL_CR_THREAD_LOCK()    opal_mutex_lock(&opal_cr_thread_lock);
#define OPAL_CR_THREAD_UNLOCK()  opal_mutex_unlock(&opal_cr_thread_lock);
#else
/* This technique will potentially starve the thread, but that is OK since
 * it is only there as support for when the process is not in the MPI library
 */
static const uint32_t ThreadFlag = 0x1;
static const uint32_t ProcInc    = 0x2;

#define OPAL_CR_LOCK()                                            \
 {                                                                \
    opal_cr_thread_in_library = true;                             \
    OPAL_THREAD_ADD32(&opal_cr_thread_num_in_library, ProcInc);   \
    while( (opal_cr_thread_num_in_library & ThreadFlag ) != 0 ) { \
      sched_yield();                                              \
    }                                                             \
 }
#define OPAL_CR_UNLOCK()                                         \
 {                                                               \
    OPAL_THREAD_ADD32(&opal_cr_thread_num_in_library, -ProcInc); \
    if( opal_cr_thread_num_in_library <= 0 ) {                   \
      opal_cr_thread_in_library = false;                         \
    }                                                            \
 }
#define OPAL_CR_THREAD_LOCK()                                                      \
 {                                                                                 \
    while(!OPAL_ATOMIC_CMPSET_32(&opal_cr_thread_num_in_library, 0, ThreadFlag)) { \
      if( !opal_cr_thread_is_active && opal_cr_thread_is_done) {                   \
          break;                                                                   \
      }                                                                            \
      sched_yield();                                                               \
      usleep(opal_cr_thread_sleep_check);                                          \
    }                                                                              \
 }
#define OPAL_CR_THREAD_UNLOCK()                                     \
 {                                                                  \
    OPAL_THREAD_ADD32(&opal_cr_thread_num_in_library, -ThreadFlag); \
 }
#endif

#endif /* OPAL_ENABLE_FT_THREAD == 1 */

int opal_cr_set_enabled(bool en)
{
    opal_cr_is_enabled = en;
    return OPAL_SUCCESS;
}

int opal_cr_initalized = 0;

int opal_cr_init(void )
{
    int ret, exit_status = OPAL_SUCCESS;
    opal_cr_coord_callback_fn_t prev_coord_func;
    int val;

    if( ++opal_cr_initalized != 1 ) {
        if( opal_cr_initalized < 1 ) {
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
        exit_status = OPAL_SUCCESS;
        goto cleanup;
    }

    /*
     * Some startup MCA parameters
     */
    ret = mca_base_param_reg_int_name("opal_cr", "verbose",
                                      "Verbose output level for the runtime OPAL Checkpoint/Restart functionality",
                                      false, false,
                                      0,
                                      &val);
    if(0 != val) {
        opal_cr_output = opal_output_open(NULL);
    } else {
        opal_cr_output = -1;
    }
    opal_output_set_verbosity(opal_cr_output, val);

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: Verbose Level: %d",
                        val);

    mca_base_param_reg_int_name("ft", "cr_enabled",
                                "Enable fault tolerance for this program",
                                false, false,
                                0, &val);
    opal_cr_set_enabled(OPAL_INT_TO_BOOL(val));

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: FT Enabled: %d",
                        val);

    mca_base_param_reg_int_name("opal_cr", "enable_timer",
                                "Enable Checkpoint timer (Default: Disabled)",
                                false, false,
                                0, &val);
    opal_cr_timing_enabled = OPAL_INT_TO_BOOL(val);

    mca_base_param_reg_int_name("opal_cr", "enable_timer_barrier",
                                "Enable Checkpoint timer Barrier (Default: Disabled)",
                                false, false,
                                0, &val);
    if( opal_cr_timing_enabled ) {
        opal_cr_timing_barrier_enabled = OPAL_INT_TO_BOOL(val);
    } else {
        opal_cr_timing_barrier_enabled = false;
    }

    mca_base_param_reg_int_name("opal_cr", "timer_target_rank",
                                "Target Rank for the timer (Default: 0)",
                                false, false,
                                0, &val);
    opal_cr_timing_target_rank = val;

#if OPAL_ENABLE_FT_THREAD == 1
    mca_base_param_reg_int_name("opal_cr", "use_thread",
                                "Use an async thread to checkpoint this program (Default: Disabled)",
                                false, false,
                                0, &val);
    opal_cr_thread_use_if_avail = OPAL_INT_TO_BOOL(val);

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: FT Use thread: %d",
                        val);

    mca_base_param_reg_int_name("opal_cr", "thread_sleep_check",
                                "Time to sleep between checking for a checkpoint (Default: 0)",
                                false, false,
                                0, &val);
    opal_cr_thread_sleep_check = val;

    mca_base_param_reg_int_name("opal_cr", "thread_sleep_wait",
                                "Time to sleep waiting for process to exit MPI library (Default: 1000)",
                                false, false,
                                1000, &val);
    opal_cr_thread_sleep_wait = val;

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: FT thread sleep: check = %d, wait = %d",
                        opal_cr_thread_sleep_check, opal_cr_thread_sleep_wait);
#endif

    mca_base_param_reg_int_name("opal_cr", "is_tool",
                                "Is this a tool program, meaning does it require a fully operational OPAL or just enough to exec.",
                                false, false,
                                0,
                                &val);
    opal_cr_is_tool = OPAL_INT_TO_BOOL(val);

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: Is a tool program: %d",
                        val);
#ifndef __WINDOWS__
    mca_base_param_reg_int_name("opal_cr", "signal",
                                "Checkpoint/Restart signal used to initialize an OPAL Only checkpoint of a program",
                                false, false,
                                SIGUSR1,
                                &opal_cr_entry_point_signal);

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: Checkpoint Signal: %d",
                        opal_cr_entry_point_signal);

    mca_base_param_reg_int_name("opal_cr", "debug_sigpipe",
                                "Activate a signal handler for debugging SIGPIPE Errors that can happen on restart. (Default: Disabled)",
                                false, false,
                                0, &val);
    opal_cr_debug_sigpipe = OPAL_INT_TO_BOOL(val);

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: Debug SIGPIPE: %d (%s)",
                        val, (opal_cr_debug_sigpipe ? "True" : "False"));

#if OPAL_ENABLE_FT_THREAD == 1
    /* If we have a thread, then attach the SIGPIPE signal handler there since
     * it is most likely to be the one that needs it.
     */
    if( opal_cr_debug_sigpipe && !opal_cr_thread_use_if_avail ) {
        if( SIG_ERR == signal(SIGPIPE, opal_cr_sigpipe_debug_signal_handler) ) {
            ;
        }
    }
#else
    if( opal_cr_debug_sigpipe ) {
        if( SIG_ERR == signal(SIGPIPE, opal_cr_sigpipe_debug_signal_handler) ) {
            ;
        }
    }
#endif

#else
    opal_cr_is_tool = true;  /* no support for CR on Windows yet */ 
#endif  /* __WINDOWS__ */

    mca_base_param_reg_string_name("opal_cr", "tmp_dir",
                                   "Temporary directory to place rendezvous files for a checkpoint",
                                   false, false,
                                   "/tmp",
                                   &opal_cr_pipe_dir);

    opal_output_verbose(10, opal_cr_output,
                        "opal_cr: init: Temp Directory: %s",
                        opal_cr_pipe_dir);

    if( !opal_cr_is_tool ) {
        /* Register the OPAL interlevel coordination callback */
        opal_cr_reg_coord_callback(opal_cr_coord, &prev_coord_func);

        opal_cr_stall_check = false;
        opal_cr_currently_stalled = false;

    } /* End opal_cr_is_tool = true */

    /* 
     * If fault tolerance was not compiled in then
     * we need to make sure that the listener thread is active to tell
     * the tools that this is not a checkpointable job.
     * We don't need the CRS framework to be initalized.
     */
#if OPAL_ENABLE_FT_CR    == 1
    /*
     * Open the checkpoint / restart service components
     */
    if (OPAL_SUCCESS != (ret = opal_crs_base_open())) {
        opal_show_help( "help-opal-runtime.txt",
                        "opal_cr_init:no-crs", true,
                        "opal_crs_base_open", ret );
        exit_status = ret;
        goto cleanup;
    }
    
    if (OPAL_SUCCESS != (ret = opal_crs_base_select())) {
        opal_show_help( "help-opal-runtime.txt",
                        "opal_cr_init:no-crs", true,
                        "opal_crs_base_select", ret );
        exit_status = ret;
        goto cleanup;
    }
#endif

#if OPAL_ENABLE_FT_THREAD == 1
    if( !opal_cr_is_tool && opal_cr_thread_use_if_avail) {
        opal_output_verbose(10, opal_cr_output,
                            "opal_cr: init: starting the thread\n");

        opal_set_using_threads(true);
        /*
         * Start the thread
         */
        OBJ_CONSTRUCT(&opal_cr_thread,     opal_thread_t);
        OBJ_CONSTRUCT(&opal_cr_thread_lock, opal_mutex_t);

        opal_cr_thread_is_done    = false;
        opal_cr_thread_is_active  = false;
        opal_cr_thread_in_library = false;
        opal_cr_thread_num_in_library = 0;

        opal_cr_thread.t_run = opal_cr_thread_fn;
        opal_cr_thread.t_arg = NULL;
        opal_thread_start(&opal_cr_thread);

    } /* End opal_cr_is_tool = true */
    else {
        opal_output_verbose(10, opal_cr_output,
                            "opal_cr: init: *Not* Using C/R thread\n");
    }
#endif /* OPAL_ENABLE_FT_THREAD == 1 */

 cleanup:
    return exit_status;
}

int opal_cr_finalize(void)
{
    int exit_status = OPAL_SUCCESS;

    if( --opal_cr_initalized != 0 ) {
        if( opal_cr_initalized < 0 ) {
            return OPAL_ERROR;
        }
        return OPAL_SUCCESS;
    }

    if( !opal_cr_is_tool ) {
#if OPAL_ENABLE_FT_THREAD == 1
        if( opal_cr_thread_use_if_avail ) {
            void *data;
            /*
             * Stop the thread
             */
            opal_cr_thread_is_done    = true;
            opal_cr_thread_is_active  = false;
            opal_cr_thread_in_library = true;

            opal_thread_join(&opal_cr_thread, &data);
            OBJ_DESTRUCT(&opal_cr_thread);
            OBJ_DESTRUCT(&opal_cr_thread_lock);
        }
#endif /* OPAL_ENABLE_FT_THREAD == 1 */

        /* Nothing to do for just process notifications */
        opal_cr_checkpointing_state = OPAL_CR_STATUS_TERM;
        opal_cr_checkpoint_request  = OPAL_CR_STATUS_TERM;
    }

    if (NULL != opal_cr_pipe_dir) {
        free(opal_cr_pipe_dir);
        opal_cr_pipe_dir = NULL;
    }

#if OPAL_ENABLE_FT_CR    == 1
    /*
     * Close the checkpoint / restart service components
     */
    opal_crs_base_close();
#endif

    return exit_status;
}

/*
 * Check if a checkpoint request needs to be operated upon
 */
void opal_cr_test_if_checkpoint_ready(void)
{
    int ret;

    if( opal_cr_currently_stalled) {
        opal_output_verbose(20, opal_cr_output,
                            "opal_cr:opal_test_if_ready: JUMPING to Post Stall stage");
        goto STAGE_1;
    }

    /*
     * If there is no checkpoint request to act on 
     * then just return
     */
    if(OPAL_CR_STATUS_REQUESTED != opal_cr_checkpoint_request ) {
        return;
    }

    /*
     * If we are currently checkpointing:
     *  - If a request is pending then cancel it
     *  - o.w., skip it.
     */
    if(OPAL_CR_STATUS_RUNNING == opal_cr_checkpointing_state ) {
        if( OPAL_SUCCESS != (ret = cur_notify_callback(OPAL_CHECKPOINT_CMD_IN_PROGRESS) ) ) {
            opal_output(opal_cr_output,
                        "Error: opal_cr: test_if_checkpoint_ready: Respond [In Progress] Failed. (%d)",
                        ret);
        }
        opal_cr_checkpoint_request = OPAL_CR_STATUS_NONE;
        return;
    }

    /*
     * If no CRS module is loaded return an error
     */
    if (NULL == opal_crs.crs_checkpoint ) {
         if( OPAL_SUCCESS != (ret = cur_notify_callback(OPAL_CHECKPOINT_CMD_NULL) ) ) {
             opal_output(opal_cr_output,
                         "Error: opal_cr: test_if_checkpoint_ready: Respond [Not Able/NULL] Failed. (%d)",
                         ret);
         }
         opal_cr_checkpoint_request = OPAL_CR_STATUS_NONE;
         return;
    }
   
    /* 
     * Start the checkpoint
     */
    opal_cr_checkpointing_state = OPAL_CR_STATUS_RUNNING;
    opal_cr_checkpoint_request  = OPAL_CR_STATUS_NONE;

 STAGE_1:
    if( OPAL_SUCCESS != (ret = cur_notify_callback(OPAL_CHECKPOINT_CMD_START) ) ) {
        opal_output(opal_cr_output,
                    "Error: opal_cr: test_if_checkpoint_ready: Respond [Start Ckpt] Failed. (%d)",
                    ret);
    }

    return;
}

/*******************************
 * Notification Routines
 *******************************/
int opal_cr_inc_core_prep(void)
{
    int ret;

    /*
     * Use the registered coordination routine
     */
    if(OPAL_SUCCESS != (ret = cur_coord_callback(OPAL_CRS_CHECKPOINT)) ) {
        if ( OPAL_EXISTS != ret ) {
            opal_output(opal_cr_output, 
                        "opal_cr: inc_core: Error: cur_coord_callback(%d) failed! %d\n",
                        OPAL_CRS_CHECKPOINT, ret);
        }
        return ret;
    }

    core_prev_pid = getpid();

    return OPAL_SUCCESS;
}

int opal_cr_inc_core_ckpt(pid_t pid,
                          opal_crs_base_snapshot_t *snapshot,
                          opal_crs_base_ckpt_options_t *options,
                          int *state)
{
    int ret, exit_status = OPAL_SUCCESS;

    OPAL_CR_SET_TIMER(OPAL_CR_TIMER_CORE0);
    if(OPAL_SUCCESS != (ret = opal_crs.crs_checkpoint(pid,
                                                      snapshot,
                                                      options,
                                                      (opal_crs_state_type_t *)state))) {
        opal_output(opal_cr_output,
                    "opal_cr: inc_core: Error: The checkpoint failed. %d\n", ret);
        exit_status = ret;
    }

    if(*state == OPAL_CRS_CONTINUE) {
        OPAL_CR_SET_TIMER(OPAL_CR_TIMER_CORE1);

        if(options->term) {
            *state = OPAL_CRS_TERM;
            opal_cr_checkpointing_state  = OPAL_CR_STATUS_TERM;
        } else {
            opal_cr_checkpointing_state  = OPAL_CR_STATUS_CONTINUE;
        }
    }
    else {
        options->term = false;
    }

    /*
     * If restarting read environment stuff that opal-restart left us.
     */
    if(*state == OPAL_CRS_RESTART) {
        extract_env_vars(core_prev_pid);
        opal_cr_checkpointing_state  = OPAL_CR_STATUS_RESTART_PRE;
    }

    return exit_status;
}

int opal_cr_inc_core_recover(int state)
{
    int ret;

    if( opal_cr_checkpointing_state != OPAL_CR_STATUS_TERM && 
        opal_cr_checkpointing_state != OPAL_CR_STATUS_CONTINUE && 
        opal_cr_checkpointing_state != OPAL_CR_STATUS_RESTART_PRE && 
        opal_cr_checkpointing_state != OPAL_CR_STATUS_RESTART_POST ) {

        if(state == OPAL_CRS_CONTINUE) {
            OPAL_CR_SET_TIMER(OPAL_CR_TIMER_CORE1);
            opal_cr_checkpointing_state  = OPAL_CR_STATUS_CONTINUE;
        }
        /*
         * If restarting read environment stuff that opal-restart left us.
         */
        else if(state == OPAL_CRS_RESTART) {
            extract_env_vars(core_prev_pid);
            opal_cr_checkpointing_state  = OPAL_CR_STATUS_RESTART_PRE;
        }
    }

    /*
     * Use the registered coordination routine
     */
    if(OPAL_SUCCESS != (ret = cur_coord_callback(state)) ) {
        if ( OPAL_EXISTS != ret ) {
            opal_output(opal_cr_output,
                        "opal_cr: inc_core: Error: cur_coord_callback(%d) failed! %d\n",
                        state, ret);
        }
        return ret;
    }

    return OPAL_SUCCESS;
}

int opal_cr_inc_core(pid_t pid,
                     opal_crs_base_snapshot_t *snapshot,
                     opal_crs_base_ckpt_options_t *options,
                     int *state)
{
    int ret, exit_status = OPAL_SUCCESS;

    /*
     * INC: Prepare stack using the registered coordination routine
     */
    if(OPAL_SUCCESS != (ret = opal_cr_inc_core_prep() ) ) {
        return ret;
    }
     
    /*
     * INC: Take the checkpoint
     */
    if(OPAL_SUCCESS != (ret = opal_cr_inc_core_ckpt(pid, snapshot, options, state) ) ) {
        exit_status = ret;
        /* Don't return here since we want to restart the OPAL level stuff */
    }

    /*
     * INC: Recover stack using the registered coordination routine
     */
    if(OPAL_SUCCESS != (ret = opal_cr_inc_core_recover(*state) ) ) {
        return ret;
    }

    return exit_status;
}

/*******************************
 * Coordination Routines
 *******************************/
/**
 * Current Coordination callback routines
 */
int opal_cr_coord(int state) 
{
    if(OPAL_CRS_CHECKPOINT == state) {
        /* Do Checkpoint Phase work */
    }
    else if (OPAL_CRS_CONTINUE == state ) {
        /* Do Continue Phase work */
    }
    else if (OPAL_CRS_RESTART == state ) {
        /* Do Restart Phase work */

        /*
         * Flush if() functionality, since it caches system specific info.
         */
        opal_iffinalize();
        /* Since opal_ifinit() is not exposed, the necessary
         * functions will call it when needed. Just make sure we
         * finalized this code so we don't get old socket addrs.
         */
        opal_output_reopen_all();
    }
    else if (OPAL_CRS_TERM == state ) {
        /* Do Continue Phase work in prep to terminate the application */
    }
    else {
        /* We must have been in an error state from the checkpoint
         * recreate everything, as in the Continue Phase
         */
    }

    /*
     * Here we are returning to either:
     *  - [orte | ompi]_notify()
     */
    opal_cr_checkpointing_state  = OPAL_CR_STATUS_RESTART_POST;

    return OPAL_SUCCESS;
}

int opal_cr_reg_notify_callback(opal_cr_notify_callback_fn_t  new_func,
                                opal_cr_notify_callback_fn_t *prev_func)
{
    /*
     * Preserve the previous callback
     */
    if( NULL != cur_notify_callback) {
        *prev_func = cur_notify_callback;
    }
    else {
        *prev_func = NULL;
    }

    /*
     * Update the callbacks
     */
    cur_notify_callback     = new_func;

    return OPAL_SUCCESS;
}

int opal_cr_reg_coord_callback(opal_cr_coord_callback_fn_t  new_func,
                               opal_cr_coord_callback_fn_t *prev_func)
{
    /*
     * Preserve the previous callback
     */
    if( NULL != cur_coord_callback) {
        *prev_func = cur_coord_callback;
    }
    else {
        *prev_func = NULL;
    }

    /*
     * Update the callbacks
     */
    cur_coord_callback     = new_func;

    return OPAL_SUCCESS;
}

/*
 * Extract environment variables from a saved file
 * and place them in the environment.
 */
static int extract_env_vars(int prev_pid)
{
    int exit_status = OPAL_SUCCESS;
    char *file_name = NULL;
    FILE *env_data = NULL;
    int len = OPAL_PATH_MAX;
    char * tmp_str = NULL;

    if( 0 >= prev_pid ) {
        opal_output(opal_cr_output,
                    "opal_cr: extract_env_vars: Invalid PID (%d)\n",
                    prev_pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * JJH: Hardcode /tmp here, really only need an agreed upon file to 
     * transfer the environment variables.
     */
    asprintf(&file_name, "/tmp/%s-%d", OPAL_CR_BASE_ENV_NAME, prev_pid);

    if (NULL == (env_data = fopen(file_name, "r")) ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* Extract an env var */
    while(!feof(env_data) ) {
        char **t_set = NULL;
        len = OPAL_PATH_MAX;

        tmp_str = (char *) malloc(sizeof(char) * len);
        if( NULL == tmp_str) {
            exit_status = OPAL_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        if( NULL == fgets(tmp_str, len, env_data) ) {
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
        len = strlen(tmp_str);
        if(tmp_str[len - 1] == '\n') {
            tmp_str[len - 1] = '\0';
        } else {
            opal_output(opal_cr_output,
                        "opal_cr: extract_env_vars: Error: Parameter too long (%s)\n",
                        tmp_str);
            free(tmp_str);
            tmp_str = NULL;
            continue;
        }

        if( NULL == (t_set = opal_argv_split(tmp_str, '=')) ) {
            break;
        }
        
        opal_setenv(t_set[0], t_set[1], true, &environ);

        free(tmp_str);
        tmp_str = NULL;
    }

    
 cleanup:
    if( NULL != env_data ) {
        fclose(env_data);
    }
    unlink(file_name);

    if( NULL != file_name ){
        free(file_name);
    }

    if( NULL != tmp_str ){
        free(tmp_str);
    }
    
    return exit_status;
}

/*****************************************
 * OPAL CR Entry Point Functionality
*****************************************/
/*
 * Used only for debugging SIGPIPE problems
 */
static void opal_cr_sigpipe_debug_signal_handler (int signo)
{
    int sleeper = 1;

    if( !opal_cr_debug_sigpipe ) {
        opal_output_verbose(10, opal_cr_output,
                            "opal_cr: sigpipe_debug: Debug SIGPIPE Not enabled :(\n");
        return;
    }

    opal_output(0,
                "opal_cr: sigpipe_debug: Debug SIGPIPE [%d]: PID (%d)\n",
                signo, getpid());
    while(sleeper == 1 ) {
        sleep(1);
    }
}

#if OPAL_ENABLE_FT_THREAD == 1
static void* opal_cr_thread_fn(opal_object_t *obj)
{
    /* Sanity Check */
    if( !opal_cr_thread_use_if_avail ) {
        return NULL;
    }

    if( opal_cr_debug_sigpipe ) {
        if( SIG_ERR == signal(SIGPIPE, opal_cr_sigpipe_debug_signal_handler) ) {
            ;
        }
    }

    /*
     * Register this thread with the OPAL CRS
     */
    if( NULL != opal_crs.crs_reg_thread ) {
        if( OPAL_SUCCESS != opal_crs.crs_reg_thread() ) {
            opal_output(0, "Error: Thread registration failed\n");
            return NULL;
        }
    }

    /*
     * Wait to become active
     */
    while( !opal_cr_thread_is_active && !opal_cr_thread_is_done) {
        sched_yield();
    }

    if( opal_cr_thread_is_done ) {
        return NULL;
    }

    /*
     * While active
     */
    while( opal_cr_thread_is_active && !opal_cr_thread_is_done) {
        /*
         * While no threads are in the MPI library then try to process
         * checkpoint requests.
         */
        OPAL_CR_THREAD_LOCK();

        while ( !opal_cr_thread_in_library ) {
            sched_yield();
            usleep(opal_cr_thread_sleep_check);

            OPAL_CR_TEST_CHECKPOINT_READY();
            /* Sanity check */
            if( OPAL_UNLIKELY(opal_cr_currently_stalled) ) {
                OPAL_CR_TEST_CHECKPOINT_READY();
            }
        }

        /*
         * While they are in the MPI library yield
         */
        OPAL_CR_THREAD_UNLOCK();

        while ( opal_cr_thread_in_library && opal_cr_thread_is_active ) {
            usleep(opal_cr_thread_sleep_wait);
        }
    }

    return NULL;
}

void opal_cr_thread_init_library(void)
{
    if( !opal_cr_thread_use_if_avail ) {
        OPAL_CR_TEST_CHECKPOINT_READY();
    } else {
        /* Activate the CR Thread */
        opal_cr_thread_in_library = false;
        opal_cr_thread_is_done    = false;
        opal_cr_thread_is_active  = true;
    }
}

void opal_cr_thread_finalize_library(void)
{
    if( !opal_cr_thread_use_if_avail ) {
        OPAL_CR_TEST_CHECKPOINT_READY();
    } else {
        /* Deactivate the CR Thread */
        opal_cr_thread_is_done    = true;
        opal_cr_thread_is_active  = false;
        OPAL_CR_LOCK();
        opal_cr_thread_in_library = true;
    }
}

void opal_cr_thread_abort_library(void)
{
    if( !opal_cr_thread_use_if_avail ) {
        OPAL_CR_TEST_CHECKPOINT_READY();
    } else {
        /* Deactivate the CR Thread */
        opal_cr_thread_is_done    = true;
        opal_cr_thread_is_active  = false;
        OPAL_CR_LOCK();
        opal_cr_thread_in_library = true;
    }
}

void opal_cr_thread_enter_library(void)
{
    if( !opal_cr_thread_use_if_avail ) {
        OPAL_CR_TEST_CHECKPOINT_READY();
    } else {
        /* Lock out the CR Thread */
        OPAL_CR_LOCK();
    }
}

void opal_cr_thread_exit_library(void)
{
    if( !opal_cr_thread_use_if_avail ) {
        OPAL_CR_TEST_CHECKPOINT_READY();
    } else {
        /* Allow CR Thread to continue */
        OPAL_CR_UNLOCK();
    }
}

void opal_cr_thread_noop_progress(void)
{
    if( !opal_cr_thread_use_if_avail ) {
        OPAL_CR_TEST_CHECKPOINT_READY();
    }
}

#endif /* OPAL_ENABLE_FT_THREAD == 1 */

static double opal_cr_get_time() {
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

void opal_cr_set_time(int idx)
{
    if(idx < OPAL_CR_TIMER_MAX ) {
        if( timer_start[idx] <= 0.0 ) {
            timer_start[idx] = opal_cr_get_time();
        }
    }
}

void opal_cr_clear_timers(void)
{
    int i;
    for(i = 0; i < OPAL_CR_TIMER_MAX; ++i) {
        timer_start[i] = 0.0;
    }
}

static void display_indv_timer_core(double diff, char *str) {
    double total = 0;
    double perc  = 0;

    total = timer_start[OPAL_CR_TIMER_MAX-1] - timer_start[OPAL_CR_TIMER_ENTRY0];
    perc = (diff/total) * 100;

    opal_output(0,
                "opal_cr: timing: %-20s = %10.2f s\t%10.2f s\t%6.2f\n",
                str,
                diff,
                total,
                perc);
    return;
}

void opal_cr_display_all_timers(void)
{
    double diff = 0.0;
    char * label = NULL;

    if( opal_cr_timing_target_rank != opal_cr_timing_my_rank ) {
        return;
    }

    opal_output(0, "OPAL CR Timing: ******************** Summary Begin\n");

    /********** Entry into the system **********/
    label = strdup("Start Entry Point");
    if( opal_cr_timing_barrier_enabled ) {
        diff = timer_start[OPAL_CR_TIMER_CRCPBR0] - timer_start[OPAL_CR_TIMER_ENTRY0];
    } else {
        diff = timer_start[OPAL_CR_TIMER_CRCP0]   - timer_start[OPAL_CR_TIMER_ENTRY0];
    }
    display_indv_timer_core(diff, label);
    free(label);

    /********** CRCP Protocol **********/
    label = strdup("CRCP Protocol");
    if( opal_cr_timing_barrier_enabled ) {
        diff = timer_start[OPAL_CR_TIMER_CRCPBR1] - timer_start[OPAL_CR_TIMER_CRCP0];
    } else {
        diff = timer_start[OPAL_CR_TIMER_P2P0]    - timer_start[OPAL_CR_TIMER_CRCP0];
    }
    display_indv_timer_core(diff, label);
    free(label);

    /********** P2P Suspend **********/
    label = strdup("P2P Suspend");
    if( opal_cr_timing_barrier_enabled ) {
        diff = timer_start[OPAL_CR_TIMER_P2PBR0]     - timer_start[OPAL_CR_TIMER_P2P0];
    } else {
        diff = timer_start[OPAL_CR_TIMER_CORE0]     - timer_start[OPAL_CR_TIMER_P2P0];
    }
    display_indv_timer_core(diff, label);
    free(label);

    /********** Checkpoint to Disk  **********/
    label = strdup("Checkpoint");
    diff = timer_start[OPAL_CR_TIMER_CORE1]    - timer_start[OPAL_CR_TIMER_CORE0];
    display_indv_timer_core(diff, label);
    free(label);

    /********** P2P Reactivation **********/
    label = strdup("P2P Reactivation");
    if( opal_cr_timing_barrier_enabled ) {
        diff = timer_start[OPAL_CR_TIMER_P2PBR2] - timer_start[OPAL_CR_TIMER_CORE1];
    } else {
        diff = timer_start[OPAL_CR_TIMER_CRCP1]  - timer_start[OPAL_CR_TIMER_CORE1];
    }
    display_indv_timer_core(diff, label);
    free(label);

    /********** CRCP Protocol Finalize **********/
    label = strdup("CRCP Cleanup");
    if( opal_cr_timing_barrier_enabled ) {
        diff = timer_start[OPAL_CR_TIMER_COREBR1] - timer_start[OPAL_CR_TIMER_CRCP1];
    } else {
        diff = timer_start[OPAL_CR_TIMER_CORE2]   - timer_start[OPAL_CR_TIMER_CRCP1];
    }
    display_indv_timer_core(diff, label);
    free(label);

    /********** Exit the system **********/
    label = strdup("Finish Entry Point");
    diff = timer_start[OPAL_CR_TIMER_ENTRY4] - timer_start[OPAL_CR_TIMER_CORE2];
    display_indv_timer_core(diff, label);
    free(label);

    opal_output(0, "OPAL CR Timing: ******************** Summary End\n");
}
