/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"
#include "opal/runtime/opal_cr.h"

#include "crs_self.h"

/*
 * Self module
 */
static opal_crs_base_module_t loc_module = {
    /** Initialization Function */
    opal_crs_self_module_init,
    /** Finalization Function */
    opal_crs_self_module_finalize,

    /** Checkpoint interface */
    opal_crs_self_checkpoint,

    /** Restart Command Access */
    opal_crs_self_restart,

    /** Disable checkpoints */
    opal_crs_self_disable_checkpoint,
    /** Enable checkpoints */
    opal_crs_self_enable_checkpoint,

    /** Prelaunch */
    opal_crs_self_prelaunch,

    /** Register Thread */
    opal_crs_self_reg_thread
};

/*
 * Snapshot structure
 */
OBJ_CLASS_DECLARATION(opal_crs_self_snapshot_t);

struct opal_crs_self_snapshot_t {
    /** Base CRS snapshot type */
    opal_crs_base_snapshot_t super;
    /** Command Line used to restart the app */
    char * cmd_line;
};
typedef struct opal_crs_self_snapshot_t opal_crs_self_snapshot_t;

static void opal_crs_self_construct(opal_crs_self_snapshot_t *obj);
static void opal_crs_self_destruct( opal_crs_self_snapshot_t *obj);

OBJ_CLASS_INSTANCE(opal_crs_self_snapshot_t,
                   opal_crs_base_snapshot_t,
                   opal_crs_self_construct,
                   opal_crs_self_destruct);


typedef void (*opal_crs_self_dlsym_dummy_fn_t)(void);

/************************************
 * Locally Global vars & functions :)
 ************************************/
static int crs_self_find_function(char *prefix, char *suffix,
                                  opal_crs_self_dlsym_dummy_fn_t *fn_ptr);

static int self_update_snapshot_metadata(opal_crs_self_snapshot_t *snapshot);

static int opal_crs_self_restart_cmd(opal_crs_self_snapshot_t *snapshot, char **cmd);
static int self_cold_start(opal_crs_self_snapshot_t *snapshot);

void opal_crs_self_construct(opal_crs_self_snapshot_t *snapshot)
{
    snapshot->cmd_line = NULL;
}

void opal_crs_self_destruct( opal_crs_self_snapshot_t *snapshot)
{
    if(NULL != snapshot->cmd_line)
        free(snapshot->cmd_line);
}

static int opal_crs_self_extract_callbacks(void);

/*
 * MCA Functions
 */
int opal_crs_self_component_query(mca_base_module_t **module, int *priority)
{
    int ret;

    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: component_query()");

    /*
     * If this is a tool, then return a module with the lowest priority.
     * This allows 'mpirun' to select the 'none' component since it has
     * a priority higher than 0.
     * But also allows 'opal-restart' to select this component if needed
     * since it only ever requests that a specific component be opened
     * that is defined in the snapshot metadata file.
     */
    if( opal_cr_is_tool ) {
        *priority = 0;
        *module = (mca_base_module_t *)&loc_module;
        return OPAL_SUCCESS;
    }

    /*
     * Extract the user level callbacks if they exist
     */
    ret = opal_crs_self_extract_callbacks();

    if( OPAL_SUCCESS != ret || 
        !mca_crs_self_component.can_checkpoint ) {
        *priority = -1;
        *module = NULL;
        return OPAL_ERROR;
    }
    else {
        *priority = mca_crs_self_component.super.priority;
        *module = (mca_base_module_t *)&loc_module;
        return OPAL_SUCCESS;
    }
}

static int opal_crs_self_extract_callbacks(void)
{
    bool callback_matched = true;
    opal_crs_self_dlsym_dummy_fn_t loc_fn;

    /*
     * Find the function names
     */
    crs_self_find_function(mca_crs_self_component.prefix,
                           SUFFIX_CHECKPOINT,
                           &loc_fn);
    mca_crs_self_component.ucb_checkpoint_fn = (opal_crs_self_checkpoint_callback_fn_t)loc_fn;

    crs_self_find_function(mca_crs_self_component.prefix,
                           SUFFIX_CONTINUE,
                           &loc_fn);
    mca_crs_self_component.ucb_continue_fn = (opal_crs_self_continue_callback_fn_t)loc_fn;

    crs_self_find_function(mca_crs_self_component.prefix,
                           SUFFIX_RESTART,
                           &loc_fn);
    mca_crs_self_component.ucb_restart_fn = (opal_crs_self_restart_callback_fn_t)loc_fn;

    /*
     * Sanity check
     */
    mca_crs_self_component.can_checkpoint = true;

    if(NULL == mca_crs_self_component.ucb_checkpoint_fn) {
        callback_matched = false;
        mca_crs_self_component.can_checkpoint = false;
    }
    if(NULL == mca_crs_self_component.ucb_continue_fn) {
        callback_matched = false;
    }
    if(NULL == mca_crs_self_component.ucb_restart_fn) {
        callback_matched = false;
    }

    return OPAL_SUCCESS;
}

int opal_crs_self_module_init(void)
{
    bool callback_matched = true;

    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: module_init()");

    if( opal_cr_is_tool ) {
        return OPAL_SUCCESS;
    }
    
    /*
     * Sanity check
     */
    if(NULL == mca_crs_self_component.ucb_checkpoint_fn) {
        callback_matched = false;
        mca_crs_self_component.can_checkpoint = false;
    }
    if(NULL == mca_crs_self_component.ucb_continue_fn) {
        callback_matched = false;
    }
    if(NULL == mca_crs_self_component.ucb_restart_fn) {
        callback_matched = false;
    }
    if( !callback_matched ) {
        if( 1 <= mca_crs_self_component.super.verbose ) {
            opal_show_help("help-opal-crs-self.txt", "self:no_callback", false,
                           "checkpoint", mca_crs_self_component.prefix, SUFFIX_CHECKPOINT,
                           "continue  ", mca_crs_self_component.prefix, SUFFIX_CONTINUE,
                           "restart   ", mca_crs_self_component.prefix, SUFFIX_RESTART,
                           PREFIX_DEFAULT);
        }
    }

    /*
     * If the user requested that we do_restart, then call their callback
     */
    if(mca_crs_self_component.do_restart) {
        opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                            "crs:self: module_init: Call their restart function");
        if( NULL != mca_crs_self_component.ucb_restart_fn) 
            mca_crs_self_component.ucb_restart_fn();
    }

    return OPAL_SUCCESS;
}

int opal_crs_self_module_finalize(void)
{
    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: module_finalize()");

    return OPAL_SUCCESS;
}


int opal_crs_self_checkpoint(pid_t pid,
                             opal_crs_base_snapshot_t *base_snapshot,
                             opal_crs_base_ckpt_options_t *options,
                             opal_crs_state_type_t *state)
{
    opal_crs_self_snapshot_t *snapshot = OBJ_NEW(opal_crs_self_snapshot_t);
    int ret, exit_status = OPAL_SUCCESS;
    char * restart_cmd = NULL;

    /*
     * This function should never be called by a tool
     */
    if( opal_cr_is_tool ) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    if( options->stop ) {
        opal_output(0,
                    "crs:self: checkpoint(): Error: SIGSTOP Not currently supported!");
    }

    /*
     * Setup for snapshot directory creation
     */
    if(NULL != snapshot->super.reference_name)
        free(snapshot->super.reference_name);
    snapshot->super.reference_name = strdup(base_snapshot->reference_name);

    if(NULL != snapshot->super.local_location)
        free(snapshot->super.local_location);
    snapshot->super.local_location  = strdup(base_snapshot->local_location);

    if(NULL != snapshot->super.remote_location)
        free(snapshot->super.remote_location);
    snapshot->super.remote_location  = strdup(base_snapshot->remote_location);

    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: checkpoint(%d, ---)", pid);

    if(!mca_crs_self_component.can_checkpoint) {
        opal_show_help("help-opal-crs-self.txt", "self:ckpt_disabled", false);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Update the snapshot metadata
     */
    snapshot->super.component_name = strdup(mca_crs_self_component.super.base_version.mca_component_name);
    if( OPAL_SUCCESS != (ret = opal_crs_base_metadata_write_token(NULL, CRS_METADATA_COMP, snapshot->super.component_name) ) ) {
        opal_output(mca_crs_self_component.super.output_handle,
                    "crs:self: checkpoint(): Error: Unable to write component name to the directory for (%s).",
                    snapshot->super.reference_name);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Call the user callback function
     */
    if(NULL != mca_crs_self_component.ucb_checkpoint_fn) {
        mca_crs_self_component.ucb_checkpoint_fn(&restart_cmd);
    }

    /*
     * Save the restart command
     */
    if( NULL == restart_cmd) {
        *state = OPAL_CRS_ERROR;
        opal_show_help("help-opal-crs-self.txt", "self:no-restart-cmd",
                       true);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    else {
        snapshot->cmd_line = strdup(restart_cmd);

        opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                            "crs:self: checkpoint: Restart Command (%s)", snapshot->cmd_line);
    }

    /*
     * The best we can do is update the metadata file with the
     * application argv and argc we started with.
     */
    if( OPAL_SUCCESS != (ret = self_update_snapshot_metadata(snapshot)) ) {
        *state = OPAL_CRS_ERROR;
        opal_output(mca_crs_self_component.super.output_handle,
                    "crs:self: checkpoint(): Error: Unable to update metadata for snapshot (%s).",
                    snapshot->super.reference_name);
        exit_status = ret;
        goto cleanup;
    }


    *state = OPAL_CRS_CONTINUE;
    
    /*
     * Call their continue routine for completeness
     */
    if(NULL != mca_crs_self_component.ucb_continue_fn) {
        mca_crs_self_component.ucb_continue_fn();
    }

    base_snapshot = &(snapshot->super);

 cleanup:
    if( NULL != restart_cmd) {
        free(restart_cmd);
        restart_cmd = NULL;
    }

    return exit_status;
}

/*
 * Notice that the user restart callback is not called here, but always from 
 *  opal_init for the self module.
 */
int opal_crs_self_restart(opal_crs_base_snapshot_t *base_snapshot, bool spawn_child, pid_t *child_pid)
{
    opal_crs_self_snapshot_t *snapshot = OBJ_NEW(opal_crs_self_snapshot_t);
    char **cr_argv = NULL;
    char * cr_cmd = NULL;
    int ret;
    int exit_status = OPAL_SUCCESS;
    int status;

    snapshot->super = *base_snapshot;

    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: restart(%s, %d)", snapshot->super.reference_name, spawn_child);

    /*
     * If we need to reconstruct the snapshot
     */
    if(snapshot->super.cold_start) {
        if( OPAL_SUCCESS != (ret = self_cold_start(snapshot)) ){
            exit_status = ret;
            opal_output(mca_crs_self_component.super.output_handle,
                        "crs:blcr: blcr_restart: Unable to reconstruct the snapshot.");
            goto cleanup;
        }
    }

    /*
     * JJH: Check to make sure the application exists?
     */

    /*
     * Get the restart command
     */
    if ( OPAL_SUCCESS != (ret = opal_crs_self_restart_cmd(snapshot, &cr_cmd)) ) {
        exit_status = ret;
        goto cleanup;
    }
    if ( NULL == (cr_argv = opal_argv_split(cr_cmd, ' ')) ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }


    if (!spawn_child) {
        opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                            "crs:self: self_restart: SELF: exec :(%s, %s):",
                            strdup(cr_argv[0]),
                            opal_argv_join(cr_argv, ' '));

        status = execvp(strdup(cr_argv[0]), cr_argv);

        if(status < 0) {
            opal_output(mca_crs_self_component.super.output_handle,
                        "crs:self: self_restart: SELF: Child failed to execute :(%d):", status);
        }
        opal_output(mca_crs_self_component.super.output_handle,
                    "crs:self: self_restart: SELF: execvp returned %d", status);
        exit_status = status;
        goto cleanup;
    }
    else {
        *child_pid = fork();
        if( *child_pid == 0) {
            /* Child Process */
            opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                                "crs:self: self_restart: CHILD: exec :(%s, %s):",
                                strdup(cr_argv[0]),
                                opal_argv_join(cr_argv, ' '));

            status = execvp(strdup(cr_argv[0]), cr_argv);

            if(status < 0) {
                opal_output(mca_crs_self_component.super.output_handle,
                            "crs:self: self_restart: CHILD: Child failed to execute :(%d):", status);
            }
            opal_output(mca_crs_self_component.super.output_handle,
                        "crs:self: self_restart: CHILD: execvp returned %d", status);
            exit_status = status;
            goto cleanup;
        }
        else if(*child_pid > 0) {
            /* Parent is done once it is started. */
            ;
        }
        else {
            opal_output(mca_crs_self_component.super.output_handle,
                        "crs:self: self_restart: CHILD: fork failed :(%d):", *child_pid);
        }
    }

 cleanup:
    if( NULL != cr_cmd)
        free(cr_cmd);
    if( NULL != cr_argv) 
        opal_argv_free(cr_argv);

    return exit_status;
}

int opal_crs_self_disable_checkpoint(void)
{
    /*
     * This function should never be called by a tool
     */
    if( opal_cr_is_tool ) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: disable_checkpoint()");

    mca_crs_self_component.can_checkpoint = false;

    return OPAL_SUCCESS;
}

int opal_crs_self_enable_checkpoint(void)
{
    /*
     * This function should never be called by a tool
     */
    if( opal_cr_is_tool ) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: enable_checkpoint()");

    mca_crs_self_component.can_checkpoint = true;

    return OPAL_SUCCESS;
}

int opal_crs_self_prelaunch(int32_t rank,
                            char *base_snapshot_dir,
                            char **app,
                            char **cwd,
                            char ***argv,
                            char ***env)
{
    char * tmp_env_var = NULL;

    /*
     * This function should never be called by a tool
     */
    if( opal_cr_is_tool ) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    tmp_env_var = mca_base_param_env_var("opal_cr_is_tool");
    opal_setenv(tmp_env_var,
                "0", true, env);
    free(tmp_env_var);
    tmp_env_var = NULL;

    return OPAL_SUCCESS;
}

int opal_crs_self_reg_thread(void)
{
    /*
     * This function should never be called by a tool
     */
    if( opal_cr_is_tool ) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    return OPAL_SUCCESS;
}

/******************
 * Local functions
 ******************/
static int crs_self_find_function(char *prefix, char *suffix,
                                  opal_crs_self_dlsym_dummy_fn_t *fn_ptr) {
    char *func_to_find = NULL;

    if( NULL == prefix || 0 >= strlen(prefix) ) {
        opal_output(mca_crs_self_component.super.output_handle,
                    "crs:self: crs_self_find_function: Error: prefix is NULL or empty string!");
        *fn_ptr = NULL;
        return OPAL_ERROR;
    }
    if( NULL == suffix || 0 >= strlen(suffix) ) {
        opal_output(mca_crs_self_component.super.output_handle,
                    "crs:self: crs_self_find_function: Error: suffix is NULL or empty string!");
        *fn_ptr = NULL;
        return OPAL_ERROR;
    }

    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: crs_self_find_function(--, %s, %s)", 
                        prefix, suffix);

    asprintf(&func_to_find, "%s_%s", prefix, suffix);

    /* The RTLD_DEFAULT is a special handle that searches the default libraries
     * including the current application for the indicated symbol. This allows
     * us to not have to dlopen/dlclose the executable. A bit of short hand
     * really.
     */
    *((void**) fn_ptr) = dlsym(RTLD_DEFAULT, func_to_find);
    if( NULL == fn_ptr) {
        opal_output_verbose(12, mca_crs_self_component.super.output_handle,
                            "crs:self: crs_self_find_function: WARNING: Function \"%s\" not found",
                            func_to_find);
    }
    else {
        opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                            "crs:self: crs_self_find_function: Found function \"%s\"",
                            func_to_find);
    }

    if( NULL == func_to_find) {
        free(func_to_find);
    }

    return OPAL_SUCCESS;
}

/*
 * Self is a special case. The 'fname' here is the command line that the user
 * wishes to execute. This function takes this command line and adds
 *   -mca crs_self_do_restart 1
 * Which will trigger the restart callback once the program has been run.
 *
 * For example, The user starts their program with:
 *   $ my_prog arg1 arg2
 *
 * They checkpoint it:
 *   $ opal_checkpoint -mca crs self 1234
 *
 * They restart it:
 *   $ opal_restart -mca crs self my_prog arg1 arg2
 *
 * fname is then:
 *   fname = "my_prog arg1 arg2"
 *
 * This funciton translates that to the command:
 *   cmd = "my_prog arg1 arg2 -mca crs self -mca crs_self_do_restart 1"
 *
 * Which will cause the program "my_prog" to call their restart function 
 * upon opal_init time.
 *
 * Note: The user could bypass the opal_restart routine safely by simply calling
 *   $ my_prog arg1 arg2 -mca crs self -mca crs_self_do_restart 1
 * However, for consistency sake, we should not encourage this as it won't work for 
 * all of the other checkpointers.
 */
static int opal_crs_self_restart_cmd(opal_crs_self_snapshot_t *snapshot, char **cmd)
{
    char * tmp_env_var = NULL;

    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: restart_cmd(%s, ---)", snapshot->cmd_line);

    tmp_env_var = mca_base_param_env_var("crs");
    opal_setenv(tmp_env_var,
                "self", 
                true, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    tmp_env_var = mca_base_param_env_var("crs_self_do_restart");
    opal_setenv(tmp_env_var,
                "1", 
                true, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    tmp_env_var = mca_base_param_env_var("crs_self_prefix");
    opal_setenv(tmp_env_var,
                mca_crs_self_component.prefix, 
                true, &environ);
    free(tmp_env_var);
    tmp_env_var = NULL;

    /* Instead of adding it to the command line, we should use the environment
     * to pass the values. This allow sthe OPAL application to be braindead 
     * WRT MCA parameters
     *   add_args = strdup("-mca crs self -mca crs_self_do_restart 1");
     */

    asprintf(cmd, "%s", snapshot->cmd_line);

    return OPAL_SUCCESS;
}

static int self_cold_start(opal_crs_self_snapshot_t *snapshot) {
    int ret, exit_status = OPAL_SUCCESS;
    char **tmp_argv = NULL;
    char * component_name = NULL;
    int prev_pid;

    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: cold_start(%s)", snapshot->super.reference_name);

    /*
     * Find the snapshot directory, read the metadata file
     */
    if( OPAL_SUCCESS != (ret = opal_crs_base_extract_expected_component(snapshot->super.local_location, 
                                                                        &component_name, &prev_pid) ) ) {
        opal_output(mca_crs_self_component.super.output_handle,
                    "crs:self: self_cold_start: Error: Failed to extract the metadata from the local snapshot (%s). Returned %d.",
                    snapshot->super.local_location, ret);
        exit_status = ret;
        goto cleanup;
    }

    snapshot->super.component_name = strdup(component_name);

    /* Compare the strings to make sure this is our snapshot before going further */
    if ( 0 != strncmp(mca_crs_self_component.super.base_version.mca_component_name, 
                      component_name, strlen(component_name)) ) {
        exit_status = OPAL_ERROR;
        opal_output(mca_crs_self_component.super.output_handle,
                    "crs:self: self_cold_start: Error: This snapshot (%s) is not intended for us (%s)\n", 
                    component_name, mca_crs_self_component.super.base_version.mca_component_name);
        goto cleanup;
    }

    /*
     * Restart command
     * JJH: Command lines limited to 256 chars.
     */
    opal_crs_base_metadata_read_token(snapshot->super.local_location, CRS_METADATA_CONTEXT, &tmp_argv);
    if( NULL == tmp_argv ) {
        opal_output(mca_crs_self_component.super.output_handle,
                    "crs:self: self_cold_start: Error: Failed to read the %s token from the local checkpoint in %s",
                    CRS_METADATA_CONTEXT, snapshot->super.local_location);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    asprintf(&snapshot->cmd_line, "%s", tmp_argv[0]);

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

static int self_update_snapshot_metadata(opal_crs_self_snapshot_t *snapshot) {
    int exit_status = OPAL_SUCCESS;
    
    if(NULL == snapshot->cmd_line) {
        opal_show_help("help-opal-crs-self.txt", "self:no-restart-cmd",
                       true);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opal_output_verbose(10, mca_crs_self_component.super.output_handle,
                        "crs:self: update_snapshot_metadata(%s)",
                        snapshot->super.reference_name);
    
    /*
     * Append to the metadata file the command line to restart with
     *  - How user wants us to restart
     */
    opal_crs_base_metadata_write_token(snapshot->super.local_location, CRS_METADATA_CONTEXT, snapshot->cmd_line);

 cleanup:
    return exit_status;
}
