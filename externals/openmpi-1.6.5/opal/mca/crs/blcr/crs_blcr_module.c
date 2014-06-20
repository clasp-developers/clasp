/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <sched.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/constants.h"

#include "opal/mca/base/mca_base_param.h"

#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "opal/event/event.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "crs_blcr.h"

/*
 * Blcr module
 */
static opal_crs_base_module_t blcr_module = {
    /** Initialization Function */
    opal_crs_blcr_module_init,
    /** Finalization Function */
    opal_crs_blcr_module_finalize,

    /** Checkpoint interface */
    opal_crs_blcr_checkpoint,

    /** Restart Command Access */
    opal_crs_blcr_restart,

    /** Disable checkpoints */
    opal_crs_blcr_disable_checkpoint,
    /** Enable checkpoints */
    opal_crs_blcr_enable_checkpoint,

    /** Prelaunch */
    opal_crs_blcr_prelaunch,

    /** Register Thread */
    opal_crs_blcr_reg_thread
};

/***************************
 * Snapshot Class Functions
 ***************************/
OBJ_CLASS_DECLARATION(opal_crs_blcr_snapshot_t);

struct opal_crs_blcr_snapshot_t {
    /** Base CRS snapshot type */
    opal_crs_base_snapshot_t super;
    char * context_filename;
};
typedef struct opal_crs_blcr_snapshot_t opal_crs_blcr_snapshot_t;

void opal_crs_blcr_construct(opal_crs_blcr_snapshot_t *obj);
void opal_crs_blcr_destruct( opal_crs_blcr_snapshot_t *obj);

OBJ_CLASS_INSTANCE(opal_crs_blcr_snapshot_t,
                   opal_crs_base_snapshot_t,
                   opal_crs_blcr_construct,
                   opal_crs_blcr_destruct);

/******************
 * Local Functions
 ******************/
static int blcr_checkpoint_peer(pid_t pid, char * local_dir, char ** fname);
static int blcr_get_checkpoint_filename(char **fname, pid_t pid);
static int opal_crs_blcr_thread_callback(void *arg);
static int opal_crs_blcr_signal_callback(void *arg);

static int opal_crs_blcr_checkpoint_cmd(pid_t pid, char * local_dir, char **fname, char **cmd);
static int opal_crs_blcr_restart_cmd(char *fname, char **cmd);

static int blcr_update_snapshot_metadata(opal_crs_blcr_snapshot_t *snapshot);
static int blcr_cold_start(opal_crs_blcr_snapshot_t *snapshot);

/*************************
 * Local Global Variables
 *************************/
static cr_client_id_t client_id;
static cr_callback_id_t cr_thread_callback_id;
static cr_callback_id_t cr_signal_callback_id;
static int blcr_current_state = OPAL_CRS_RUNNING;

static char *blcr_restart_cmd = NULL;
static char *blcr_checkpoint_cmd = NULL;

static opal_condition_t blcr_cond;
static opal_mutex_t     blcr_lock;

static pid_t my_pid = -1;

void opal_crs_blcr_construct(opal_crs_blcr_snapshot_t *snapshot) {
    snapshot->context_filename = NULL;
    snapshot->super.component_name = strdup(mca_crs_blcr_component.super.base_version.mca_component_name);
}

void opal_crs_blcr_destruct( opal_crs_blcr_snapshot_t *snapshot) {
    if(NULL != snapshot->context_filename)
        free(snapshot->context_filename);
}

/*****************
 * MCA Functions
 *****************/ 
int opal_crs_blcr_component_query(mca_base_module_t **module, int *priority)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: component_query()");

    *priority = mca_crs_blcr_component.super.priority;
    *module = (mca_base_module_t *)&blcr_module;

    return OPAL_SUCCESS;
}

int opal_crs_blcr_module_init(void)
{
    void *crs_blcr_thread_callback_arg = NULL;
    void *crs_blcr_signal_callback_arg = NULL;

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: module_init()");

    my_pid = getpid();

    if( !opal_cr_is_tool ) {
        /*
         * Initialize BLCR
         */
        client_id = cr_init();
        if (0 > client_id) {
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "Error: crs:blcr: module_init: cr_init failed (%d)\n", client_id);
            return OPAL_ERROR;
        }
    }

    blcr_restart_cmd    = strdup("cr_restart");
    blcr_checkpoint_cmd = strdup("cr_checkpoint");
    
    if( !opal_cr_is_tool ) {
        /* We need to make the lock and condition variable before
         * starting the thread, since the thread uses these vars.
         */
        OBJ_CONSTRUCT(&blcr_lock, opal_mutex_t);
        OBJ_CONSTRUCT(&blcr_cond, opal_condition_t);
        
        /*
         * Register the thread handler
         */
        cr_thread_callback_id = cr_register_callback(opal_crs_blcr_thread_callback,
                                                     crs_blcr_thread_callback_arg,
                                                     CR_THREAD_CONTEXT);
        /*
         * Register the signal handler
         *  - even though we do not use it
         */
        cr_signal_callback_id = cr_register_callback(opal_crs_blcr_signal_callback,
                                                     crs_blcr_signal_callback_arg,
                                                     CR_SIGNAL_CONTEXT);
    }

    /*
     * Now that we are done with init, set the state to running
     */
    blcr_current_state = OPAL_CRS_RUNNING;

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: module_init() --> Finished [%d]",
                        opal_cr_is_tool);
    
    return OPAL_SUCCESS;
}

int opal_crs_blcr_prelaunch(int32_t rank,
                            char *base_snapshot_dir,
                            char **app,
                            char **cwd,
                            char ***argv,
                            char ***env)
{
    char * tmp_env_var = NULL;

    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_setenv(tmp_env_var,
                "0", true, env);
    free(tmp_env_var);
    tmp_env_var = NULL;

    return OPAL_SUCCESS;
}

int opal_crs_blcr_reg_thread(void)
{
    cr_client_id_t loc_client_id;

    /*
     * Initialize BLCR
     */
    loc_client_id = cr_init();
    if (0 > loc_client_id) {
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "Error: crs:blcr: reg_thread: cr_init failed (%d)\n", loc_client_id);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

int opal_crs_blcr_module_finalize(void)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: module_finalize()");

    /* Cleanup some memory */
    if( NULL != blcr_restart_cmd ) {
        free(blcr_restart_cmd);
        blcr_restart_cmd = NULL;
    }
    if( NULL != blcr_checkpoint_cmd ) {
        free(blcr_checkpoint_cmd);
        blcr_checkpoint_cmd = NULL;
    }

    if( !opal_cr_is_tool ) {
        OBJ_DESTRUCT(&blcr_lock);
        OBJ_DESTRUCT(&blcr_cond);

        /* Unload the thread callback */
        cr_replace_callback(cr_thread_callback_id, NULL, NULL, CR_THREAD_CONTEXT);
        /* Unload the signal callback */
        cr_replace_callback(cr_signal_callback_id, NULL, NULL, CR_SIGNAL_CONTEXT);
    }

    /* BLCR does not have a finalization routine */

    return OPAL_SUCCESS;
}

int opal_crs_blcr_checkpoint(pid_t pid,
                             opal_crs_base_snapshot_t *base_snapshot,
                             opal_crs_base_ckpt_options_t *options,
                             opal_crs_state_type_t *state)
{
    int ret, exit_status = OPAL_SUCCESS;
    opal_crs_blcr_snapshot_t *snapshot = OBJ_NEW(opal_crs_blcr_snapshot_t);
#if CRS_BLCR_HAVE_CR_REQUEST_CHECKPOINT == 1
    cr_checkpoint_args_t cr_args;
    static cr_checkpoint_handle_t cr_handle = (cr_checkpoint_handle_t)(-1);
#endif

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: checkpoint(%d, ---)", pid);

    if(NULL != snapshot->super.reference_name)
        free(snapshot->super.reference_name);
    snapshot->super.reference_name = strdup(base_snapshot->reference_name);

    if(NULL != snapshot->super.local_location)
        free(snapshot->super.local_location);
    snapshot->super.local_location  = strdup(base_snapshot->local_location);

    if(NULL != snapshot->super.remote_location)
        free(snapshot->super.remote_location);
    snapshot->super.remote_location  = strdup(base_snapshot->remote_location);

    /*
     * Update the snapshot metadata
     */
    snapshot->super.component_name = strdup(mca_crs_blcr_component.super.base_version.mca_component_name);
    if( OPAL_SUCCESS != (ret = opal_crs_base_metadata_write_token(NULL, CRS_METADATA_COMP, snapshot->super.component_name) ) ) {
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: checkpoint(): Error: Unable to write component name to the directory for (%s).",
                    snapshot->super.reference_name);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * If we can checkpointing ourselves do so:
     * use cr_request_checkpoint() if available, and cr_request_file() if not
     */
#if CRS_BLCR_HAVE_CR_REQUEST_CHECKPOINT == 1 || CRS_BLCR_HAVE_CR_REQUEST == 1
    if( pid == my_pid ) {
        char *loc_fname = NULL;

        blcr_get_checkpoint_filename(&(snapshot->context_filename), pid);
        if( opal_crs_blcr_dev_null ) {
            loc_fname = strdup("/dev/null");
        } else {
            asprintf(&loc_fname, "%s/%s", snapshot->super.local_location, snapshot->context_filename);
        }

        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: checkpoint SELF <%s>",
                            loc_fname);

#if CRS_BLCR_HAVE_CR_REQUEST_CHECKPOINT == 1
        {
            int fd = 0;
            fd = open(loc_fname,
                       O_WRONLY | O_CREAT | O_TRUNC | O_LARGEFILE,
                       S_IRUSR | S_IWUSR);
            if( fd < 0 ) {
                *state = OPAL_CRS_ERROR;
                opal_output(mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: checkpoint(): Error: Unable to open checkpoint file (%s) for pid (%d)",
                            loc_fname, pid);
                exit_status = OPAL_ERROR;
                goto cleanup;
            }

            cr_initialize_checkpoint_args_t(&cr_args);
            cr_args.cr_scope = CR_SCOPE_PROC;
            cr_args.cr_fd    = fd;
            if( options->stop ) {
                cr_args.cr_signal = SIGSTOP;
            }

            ret = cr_request_checkpoint(&cr_args, &cr_handle);
            if( ret < 0 ) {
                close(cr_args.cr_fd);
                *state = OPAL_CRS_ERROR;
                opal_output(mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: checkpoint(): Error: Unable to checkpoint pid (%d) to file (%s)",
                            pid, loc_fname);
                exit_status = ret;
                goto cleanup;
            }

            /* Wait for checkpoint to finish */
            do {
                ret = cr_poll_checkpoint(&cr_handle, NULL);
                if( ret < 0 ) {
                    /* Check if restarting. This is not an error. */
                    if( (ret == CR_POLL_CHKPT_ERR_POST) && (errno == CR_ERESTARTED) ) {
                        ret = 0;
                        break;
                    }
                    /* If Call was interrupted by a signal, retry the call */
                    else if (errno == EINTR) {
                        ;
                    }
                    /* Otherwise this is a real error that we need to deal with */
                    else {
                        *state = OPAL_CRS_ERROR;
                        opal_output(mca_crs_blcr_component.super.output_handle,
                                    "crs:blcr: checkpoint(): Error: Unable to checkpoint pid (%d) to file (%s) - poll failed with (%d)",
                                    pid, loc_fname, ret);
                        exit_status = ret;
                        goto cleanup;
                    }
                }
            } while( ret < 0 );

            /* Close the file */
            close(cr_args.cr_fd);
        }
#else
        /* Request a checkpoint be taken of the current process.
         * Since we are not guaranteed to finish the checkpoint before this
         * returns, we also need to wait for it.
         */
        cr_request_file(loc_fname);
        
        /* Wait for checkpoint to finish */
        do {
            usleep(1000); /* JJH Do we really want to sleep? */
        } while(CR_STATE_IDLE != cr_status());
#endif

        *state = blcr_current_state;
        free(loc_fname);
    }
    /*
     * Checkpointing another process
     */
    else 
#endif
    {
        ret = blcr_checkpoint_peer(pid, snapshot->super.local_location, &(snapshot->context_filename));

        if(OPAL_SUCCESS != ret) {
            *state = OPAL_CRS_ERROR;
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: checkpoint(): Error: Unable to checkpoint pid (%d)",
                        pid);
            exit_status = ret;
            goto cleanup;
        }

        *state = blcr_current_state;
    }
    
    if(*state == OPAL_CRS_CONTINUE) {
        /*
         * Update the metadata file
         */
        if( OPAL_SUCCESS != (ret = blcr_update_snapshot_metadata(snapshot)) ) {
            *state = OPAL_CRS_ERROR;
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: checkpoint(): Error: Unable to update metadata for snapshot (%s).", 
                        snapshot->super.reference_name);
            exit_status = ret;
            goto cleanup;
        }
    }

    /*
     * Return to the caller
     */
    base_snapshot = &(snapshot->super);

 cleanup:
    return exit_status;
}

int opal_crs_blcr_restart(opal_crs_base_snapshot_t *base_snapshot, bool spawn_child, pid_t *child_pid)
{
    opal_crs_blcr_snapshot_t *snapshot = OBJ_NEW(opal_crs_blcr_snapshot_t);
    char **cr_argv = NULL;
    char *cr_cmd = NULL;
    char *cr_full_cmd = NULL;
    int ret;
    int exit_status = OPAL_SUCCESS;
    int status;

    snapshot->super = *base_snapshot;

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: restart(%s, %d)", snapshot->super.reference_name, spawn_child);

    /*
     * If we need to reconstruct the snapshot,
     */
    if(snapshot->super.cold_start) {
        if( OPAL_SUCCESS != (ret = blcr_cold_start(snapshot)) ) {
            exit_status = OPAL_ERROR;
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: blcr_restart: Unable to reconstruct the snapshot.");
            goto cleanup;
        }
    }
    

    /*
     * Get the restart command
     */
    if ( OPAL_SUCCESS != (ret = opal_crs_blcr_restart_cmd(snapshot->context_filename, &cr_cmd)) ) {
        exit_status = ret;
        goto cleanup;
    }
    if ( NULL == (cr_argv = opal_argv_split(cr_cmd, ' ')) ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }


    /*
     * Restart by replacing this process
     */
    /* Need to shutdown the event engine before this.
     * for some reason the BLCR checkpointer and our event engine don't get
     * along very well.
     */
    opal_progress_finalize();
    opal_event_fini();

    if (!spawn_child) {
        cr_full_cmd = opal_argv_join(cr_argv, ' ');
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: blcr_restart: SELF: exec :(%s, %s):", 
                            blcr_restart_cmd, cr_full_cmd);

        status = execvp(strdup(blcr_restart_cmd), cr_argv);

        if(status < 0) {
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: blcr_restart: SELF: Child failed to execute :(%d):", status);
        }
        opal_show_help("help-opal-crs-blcr.txt", "blcr:restart_failed_exec", true,
                       status,
                       blcr_restart_cmd,
                       cr_full_cmd);

        exit_status = status;
        goto cleanup;
    }
    /*
     * Restart by starting a new process
     */
    else {
        *child_pid = fork();

        if( 0 == *child_pid) {
            /* Child Process */
            opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                                "crs:blcr: blcr_restart: CHILD: exec :(%s, %s):", 
                                strdup(blcr_restart_cmd),
                                opal_argv_join(cr_argv, ' '));
            
            status = execvp(strdup(blcr_restart_cmd), cr_argv);

            if(status < 0) {
                opal_output(mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: blcr_restart: CHILD: Child failed to execute :(%d):", status);
            }
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: blcr_restart: CHILD: execvp returned %d", status);

            exit_status = status;
            goto cleanup;
        }
        else if(*child_pid > 0) {
            /* Parent is done once it is started. */
            ;
        }
        else {
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: blcr_restart: CHILD: fork failed :(%d):", *child_pid);
        }
    }

 cleanup:
    if(NULL != cr_cmd)
        free(cr_cmd);
    if(NULL != cr_argv)
        opal_argv_free(cr_argv);
    if(NULL != cr_full_cmd)
        free(cr_full_cmd);

    return exit_status;
}
    
int opal_crs_blcr_disable_checkpoint(void)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: disable_checkpoint()");
    /*
     * Enter the BLCR Critical Section
     */
    cr_enter_cs(client_id);

    return OPAL_SUCCESS;
}

int opal_crs_blcr_enable_checkpoint(void)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: enable_checkpoint()");
    /*
     * Leave the BLCR Critical Section
     */
    cr_leave_cs(client_id);

    return OPAL_SUCCESS;
}

/*****************************
 * Local Function Definitions
 *****************************/
static int blcr_checkpoint_peer(pid_t pid, char * local_dir, char ** fname) 
{
    char **cr_argv = NULL;
    char *cr_cmd = NULL;
    int ret;
    pid_t child_pid;
    int exit_status = OPAL_SUCCESS;
    int status, child_status;

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: checkpoint_peer(%d, --)", pid);

    /*
     * Get the checkpoint command
     */
    if ( OPAL_SUCCESS != (ret = opal_crs_blcr_checkpoint_cmd(pid, local_dir, fname, &cr_cmd)) ) {
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: checkpoint_peer: Failed to generate checkpoint command :(%d):", ret);
        exit_status = ret;
        goto cleanup;
    }
    if ( NULL == (cr_argv = opal_argv_split(cr_cmd, ' ')) ) {
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: checkpoint_peer: Failed to opal_argv_split :(%d):", ret);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Fork a child to do the checkpoint
     */
    blcr_current_state = OPAL_CRS_CHECKPOINT;
 
    child_pid = fork();

    if(0 == child_pid) {
        /* Child Process */
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: blcr_checkpoint_peer: exec :(%s, %s):", 
                            strdup(blcr_checkpoint_cmd),
                            opal_argv_join(cr_argv, ' '));
        
        status = execvp(strdup(blcr_checkpoint_cmd), cr_argv);

        if(status < 0) {
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: blcr_checkpoint_peer: Child failed to execute :(%d):", status);
        }
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: blcr_checkpoint_peer: execvp returned %d", status);
    }
    else if(child_pid > 0) {
        /* Don't waitpid here since we don't really want to restart from inside waitpid ;) */
        while(OPAL_CRS_RESTART  != blcr_current_state  &&
              OPAL_CRS_CONTINUE != blcr_current_state ) {
            OPAL_THREAD_LOCK(&blcr_lock);
            opal_condition_wait(&blcr_cond, &blcr_lock);
            OPAL_THREAD_UNLOCK(&blcr_lock);
        }

        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: blcr_checkpoint_peer: Thread finished with status %d", blcr_current_state);

        if(OPAL_CRS_CONTINUE == blcr_current_state) {
            /* Wait for the child only if we are continuing */
            if( 0 > waitpid(child_pid, &child_status, 0) ) {
                opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: blcr_checkpoint_peer: waitpid returned %d", child_status);
            }
        }
    }
    else {
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: blcr_checkpoint_peer: fork failed :(%d):", child_pid);
    }

    /*
     * Cleanup
     */
cleanup:
    if(NULL != cr_cmd)
        free(cr_cmd);
    if(NULL != cr_argv)
        opal_argv_free(cr_argv);

    return exit_status;
}

static int opal_crs_blcr_thread_callback(void *arg) {
    const struct cr_checkpoint_info *ckpt_info = cr_get_checkpoint_info();
    int ret;
    
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: thread_callback()");

    OPAL_THREAD_LOCK(&blcr_lock);
    blcr_current_state = OPAL_CRS_CHECKPOINT;

    /*
     * Allow the checkpoint to be taken, if we requested it
     */
#if CRS_BLCR_HAVE_INFO_REQUESTER == 1
    if( ckpt_info->requester != my_pid ) {
        ret = cr_checkpoint(CR_CHECKPOINT_OMIT);
        blcr_current_state = OPAL_CRS_RUNNING;
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: thread_callback(); WARNING: An external agent attempted to checkpoint this process "
                            "when it did not expect to be checkpointed. Skipping this checkpoint request."
                            " [%d != %d].", ckpt_info->requester, my_pid);
        return 0;
    }
    else
#endif
    {
        ret = cr_checkpoint(0);
    }
    
    /*
     * Restarting
     */
    if ( 0 < ret ) {
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: thread_callback: Restarting.");
        blcr_current_state = OPAL_CRS_RESTART;
    }
    /*
     * Continuing
     */
    else {
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: thread_callback: Continue.");
        blcr_current_state = OPAL_CRS_CONTINUE;
    }

    OPAL_THREAD_UNLOCK(&blcr_lock);
    opal_condition_signal(&blcr_cond);

    return 0;
}

static int opal_crs_blcr_signal_callback(void *arg) {
    const struct cr_checkpoint_info *ckpt_info = cr_get_checkpoint_info();
    int ret;

    /*
     * Allow the checkpoint to be taken, if we requested it
     */
#if CRS_BLCR_HAVE_INFO_REQUESTER == 1
    if( ckpt_info->requester != my_pid ) {
        ret = cr_checkpoint(CR_CHECKPOINT_OMIT);
        return 0;
    }
    else
#endif
    {
        ret = cr_checkpoint(0);
    }

    return 0;
}

static int opal_crs_blcr_checkpoint_cmd(pid_t pid, char * local_dir, char **fname, char **cmd)
{
    char **cr_argv = NULL;
    int argc = 0, ret;
    char * pid_str;
    int exit_status = OPAL_SUCCESS;
    char * loc_fname = NULL;

    blcr_get_checkpoint_filename(fname, pid);

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: checkpoint_cmd(%d)", pid);

    asprintf(&loc_fname, "%s/%s", local_dir, *fname);

    /*
     * Build the command
     */
    if (OPAL_SUCCESS != (ret = opal_argv_append(&argc, &cr_argv, strdup(blcr_checkpoint_cmd)))) {
        exit_status = ret;
        goto cleanup;
    }

    if (OPAL_SUCCESS != (ret = opal_argv_append(&argc, &cr_argv, strdup("--pid")))) {
        exit_status = ret;
        goto cleanup;
    }

    asprintf(&pid_str, "%d", pid);
    if (OPAL_SUCCESS != (ret = opal_argv_append(&argc, &cr_argv, strdup(pid_str)))) {
        exit_status = ret;
        goto cleanup;
    }

    if (OPAL_SUCCESS != (ret = opal_argv_append(&argc, &cr_argv, strdup("--file")))) {
        exit_status = ret;
        goto cleanup;
    }

    if (OPAL_SUCCESS != (ret = opal_argv_append(&argc, &cr_argv, strdup(loc_fname)))) {
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if(exit_status != OPAL_SUCCESS)
        *cmd = NULL;
    else 
        *cmd = opal_argv_join(cr_argv, ' ');
    
    if(NULL != pid_str) 
        free(pid_str);
    if( NULL != cr_argv)
        opal_argv_free(cr_argv);
    if(NULL != loc_fname) 
        free(loc_fname);

    return exit_status;
}

static int opal_crs_blcr_restart_cmd(char *fname, char **cmd)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: restart_cmd(%s, ---)", fname);

    if (NULL == fname) {
        opal_output_verbose(10, opal_crs_base_output, 
                            "crs:blcr: restart_cmd: Error: filename is NULL!");
        return OPAL_CRS_ERROR;
    }

    asprintf(cmd, "%s %s", blcr_restart_cmd, fname);

    return OPAL_SUCCESS;
}

static int blcr_get_checkpoint_filename(char **fname, pid_t pid)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: get_checkpoint_filename(--, %d)", pid);

    asprintf(fname, "ompi_blcr_context.%d", pid);
    
    return OPAL_SUCCESS;
}

static int blcr_update_snapshot_metadata(opal_crs_blcr_snapshot_t *snapshot) {
    int exit_status  = OPAL_SUCCESS;

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: update_snapshot_metadata(%s)", snapshot->super.reference_name);

    /* Bozo check to make sure this snapshot is ours */
    if ( 0 != strncmp(mca_crs_blcr_component.super.base_version.mca_component_name, 
                      snapshot->super.component_name, 
                      strlen(snapshot->super.component_name)) ) {
        exit_status = OPAL_ERROR;
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: blcr_update_snapshot_metadata: Error: This snapshot (%s) is not intended for us (%s)\n", 
                    snapshot->super.component_name, mca_crs_blcr_component.super.base_version.mca_component_name);
        goto cleanup;
    }

    /*
     * Append to the metadata file the context filename
     */
    opal_crs_base_metadata_write_token(snapshot->super.local_location, CRS_METADATA_CONTEXT, snapshot->context_filename);
    
 cleanup:
    return exit_status;
}

static int blcr_cold_start(opal_crs_blcr_snapshot_t *snapshot) {
    int ret, exit_status = OPAL_SUCCESS;
    char **tmp_argv = NULL;
    char * component_name = NULL;
    int prev_pid;

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: cold_start(%s)", snapshot->super.reference_name);

    /*
     * Find the snapshot directory, read the metadata file
     */
    if( OPAL_SUCCESS != (ret = opal_crs_base_extract_expected_component(snapshot->super.local_location, 
                                                                        &component_name, &prev_pid) ) ) {
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: blcr_cold_start: Error: Failed to extract the metadata from the local snapshot (%s). Returned %d.",
                    snapshot->super.local_location, ret);
        exit_status = ret;
        goto cleanup;
    }

    snapshot->super.component_name = strdup(component_name);

    /* Compare the component strings to make sure this is our snapshot before going further */
    if ( 0 != strncmp(mca_crs_blcr_component.super.base_version.mca_component_name,
                      component_name, strlen(component_name)) ) {
        exit_status = OPAL_ERROR;
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: blcr_cold_start: Error: This snapshot (%s) is not intended for us (%s)\n", 
                    component_name, mca_crs_blcr_component.super.base_version.mca_component_name);
        goto cleanup;
    }

    /*
     * Context Filename
     */
    opal_crs_base_metadata_read_token(snapshot->super.local_location, CRS_METADATA_CONTEXT, &tmp_argv);
    if( NULL == tmp_argv ) {
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: blcr_cold_start: Error: Failed to read the %s token from the local checkpoint in %s",
                    CRS_METADATA_CONTEXT, snapshot->super.local_location);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    asprintf(&snapshot->context_filename, "%s/%s", snapshot->super.local_location, tmp_argv[0]);

    /*
     * Reset the cold_start flag
     */
    snapshot->super.cold_start = false;

 cleanup:
    if(NULL != tmp_argv) {
        opal_argv_free(tmp_argv);
        tmp_argv = NULL;
    }

    return exit_status;
}
