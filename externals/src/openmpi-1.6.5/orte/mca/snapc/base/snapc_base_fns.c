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
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif /* HAVE_DIRENT_H */
#include <time.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/basename.h"
#include "opal/util/argv.h"
#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/mca/notifier/notifier.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"

/******************
 * Local Functions
 ******************/
/* Some local strings to use genericly with the global metadata file */
#define SNAPC_METADATA_SEQ      ("# Seq: ")
#define SNAPC_METADATA_DONE_SEQ ("# Finished Seq: ")
#define SNAPC_METADATA_TIME     ("# Timestamp: ")
#define SNAPC_METADATA_PROCESS  ("# Process: ")
#define SNAPC_METADATA_CRS_COMP ("# OPAL CRS Component: ")
#define SNAPC_METADATA_SNAP_REF ("# Snapshot Reference: ")
#define SNAPC_METADATA_SNAP_LOC ("# Snapshot Location: ")

static int get_next_seq_number(FILE *file);
static int get_next_valid_seq_number(FILE *file);
static int metadata_extract_next_token(FILE *file, char **token, char **value);

size_t orte_snapc_base_snapshot_seq_number = 0;

/******************
 * Object stuff
 ******************/
OBJ_CLASS_INSTANCE(orte_snapc_base_local_snapshot_t,
                   opal_list_item_t,
                   orte_snapc_base_local_snapshot_construct,
                   orte_snapc_base_local_snapshot_destruct);

void orte_snapc_base_local_snapshot_construct(orte_snapc_base_local_snapshot_t *snapshot)
{
    snapshot->process_name.jobid  = 0;
    snapshot->process_name.vpid   = 0;

    snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;

    snapshot->reference_name  = NULL;
    snapshot->local_location  = NULL;
    snapshot->remote_location = NULL;

    snapshot->opal_crs      = NULL;
}

void orte_snapc_base_local_snapshot_destruct( orte_snapc_base_local_snapshot_t *snapshot)
{
    snapshot->process_name.jobid  = 0;
    snapshot->process_name.vpid   = 0;

    snapshot->state = ORTE_SNAPC_CKPT_STATE_NONE;

    if( NULL != snapshot->reference_name ) {
        free(snapshot->reference_name);
        snapshot->reference_name = NULL;
    }

    if( NULL != snapshot->local_location ) {
        free(snapshot->local_location);
        snapshot->local_location = NULL;
    }

    if( NULL != snapshot->remote_location ) {
        free(snapshot->remote_location);
        snapshot->remote_location = NULL;
    }

    if( NULL != snapshot->opal_crs ) {
        free(snapshot->opal_crs);
        snapshot->opal_crs = NULL;
    }
}

/****/
OBJ_CLASS_INSTANCE(orte_snapc_base_global_snapshot_t,
                   opal_list_item_t,
                   orte_snapc_base_global_snapshot_construct,
                   orte_snapc_base_global_snapshot_destruct);

void orte_snapc_base_global_snapshot_construct(orte_snapc_base_global_snapshot_t *snapshot)
{
    char *tmp_dir = NULL;

    OBJ_CONSTRUCT(&(snapshot->local_snapshots), opal_list_t);

    orte_snapc_base_unique_global_snapshot_name(&(snapshot->reference_name), getpid());

    orte_snapc_base_get_global_snapshot_directory(&tmp_dir, snapshot->reference_name);
    snapshot->local_location = opal_dirname(tmp_dir);
    free(tmp_dir);
    
    snapshot->seq_num    = 0;

    snapshot->start_time = NULL;
    snapshot->end_time   = NULL;
}

void orte_snapc_base_global_snapshot_destruct( orte_snapc_base_global_snapshot_t *snapshot)
{
    opal_list_item_t* item = NULL;

    while (NULL != (item = opal_list_remove_first(&snapshot->local_snapshots))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&(snapshot->local_snapshots));

    if(NULL != snapshot->reference_name) {
        free(snapshot->reference_name);
        snapshot->reference_name = NULL;
    }

    if(NULL != snapshot->local_location) {
        free(snapshot->local_location);
        snapshot->local_location = NULL;
    }

    if(NULL != snapshot->start_time) {
        free(snapshot->start_time);
        snapshot->start_time = NULL;
    }

    if(NULL != snapshot->end_time) {
        free(snapshot->end_time);
        snapshot->end_time = NULL;
    }

    snapshot->seq_num = 0;
}

OBJ_CLASS_INSTANCE(orte_snapc_base_quiesce_t,
                   opal_object_t,
                   orte_snapc_base_quiesce_construct,
                   orte_snapc_base_quiesce_destruct);

void orte_snapc_base_quiesce_construct(orte_snapc_base_quiesce_t *quiesce)
{
    quiesce->epoch         = -1;
    quiesce->snapshot      = NULL;
    quiesce->handle        = NULL;
    quiesce->target_dir    = NULL;
    quiesce->crs_name      = NULL;
    quiesce->cmdline       = NULL;
    quiesce->cr_state      = OPAL_CRS_NONE;
    quiesce->checkpointing = false;
    quiesce->restarting    = false;

}

void orte_snapc_base_quiesce_destruct( orte_snapc_base_quiesce_t *quiesce)
{
    quiesce->epoch = -1;

    if( NULL != quiesce->snapshot ) {
        OBJ_RELEASE(quiesce->snapshot);
        quiesce->snapshot      = NULL;
    }

    if( NULL != quiesce->handle ) {
        free(quiesce->handle);
        quiesce->handle = NULL;
    }
    if( NULL != quiesce->target_dir ) {
        free(quiesce->target_dir);
        quiesce->target_dir = NULL;
    }
    if( NULL != quiesce->crs_name ) {
        free(quiesce->crs_name);
        quiesce->crs_name = NULL;
    }
    if( NULL != quiesce->cmdline ) {
        free(quiesce->cmdline);
        quiesce->cmdline = NULL;
    }

    quiesce->cr_state      = OPAL_CRS_NONE;
    quiesce->checkpointing = false;
    quiesce->restarting    = false;

}


/***********************
 * None component stuff
 ************************/
int orte_snapc_base_none_open(void)
{
    return ORTE_SUCCESS;
}

int orte_snapc_base_none_close(void)
{
    return ORTE_SUCCESS;
}

int orte_snapc_base_none_query(mca_base_module_t **module, int *priority)
{
    *module = NULL;
    *priority = 0;

    return OPAL_SUCCESS;
}

int orte_snapc_base_module_init(bool seed, bool app)
{
    return ORTE_SUCCESS;
}

int orte_snapc_base_module_finalize(void)
{
    return ORTE_SUCCESS;
}

/* None RML command line response callback */
static void snapc_none_global_cmdline_request(int status,
                                              orte_process_name_t* sender,
                                              opal_buffer_t *buffer,
                                              orte_rml_tag_t tag,
                                              void* cbdata);
int orte_snapc_base_none_setup_job(orte_jobid_t jobid)
{
    int exit_status = ORTE_SUCCESS;
    int rc;

    /*
     * Coordinator command listener
     */
    orte_snapc_base_snapshot_seq_number = -1;
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_CKPT,
                                                      ORTE_RML_PERSISTENT,
                                                      snapc_none_global_cmdline_request,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
        exit_status = rc;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

int orte_snapc_base_none_release_job(orte_jobid_t jobid)
{
    /*
     * Remove the checkpoint request callback
     */

    return ORTE_SUCCESS;
}

int orte_snapc_base_none_ft_event(int state)
{
    return ORTE_SUCCESS;
}

int orte_snapc_base_none_start_ckpt(orte_snapc_base_quiesce_t *datum)
{
    return ORTE_SUCCESS;
}

int orte_snapc_base_none_end_ckpt(orte_snapc_base_quiesce_t *datum)
{
    return ORTE_SUCCESS;
}


/********************
 * Local Functions
 ********************/
/* None RML response callback */
static void snapc_none_global_cmdline_request(int status,
                                              orte_process_name_t* sender,
                                              opal_buffer_t *buffer,
                                              orte_rml_tag_t tag,
                                              void* cbdata)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_snapc_cmd_flag_t command;
    orte_std_cntr_t n = 1;
    opal_crs_base_ckpt_options_t *options = NULL;
    orte_jobid_t jobid;

    options = OBJ_NEW(opal_crs_base_ckpt_options_t);

    n = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &command, &n, ORTE_SNAPC_CMD))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * orte_checkpoint has requested that a checkpoint be taken
     * Respond that a checkpoint cannot be taken at this time
     */
    if (ORTE_SNAPC_GLOBAL_INIT_CMD == command) {
        /*
         * Do the basic handshake with the orte_checkpoint command
         */
        if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_init_cmd(sender, buffer, options, &jobid)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        /*
         * Respond with an invalid response
         */
        if( ORTE_SUCCESS != (ret = orte_snapc_base_global_coord_ckpt_update_cmd(sender, NULL, -1, ORTE_SNAPC_CKPT_STATE_NO_CKPT)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    /*
     * Unknown command
     */
    else {
        goto cleanup;
    }
    
 cleanup:
    if( NULL != options ) {
        OBJ_RELEASE(options);
        options = NULL;
    }

    return;
}

/********************
 * Utility functions
 ********************/

/* Report the checkpoint status over the notifier interface */
void orte_snapc_ckpt_state_notify(int state)
{
    switch(state) {
    case ORTE_SNAPC_CKPT_STATE_FINISHED:
	    orte_notifier.log(ORTE_NOTIFIER_INFO, ORTE_SNAPC_CKPT_NOTIFY(state),
                          "%d: Checkpoint established for process %s.",
			  orte_process_info.pid, ORTE_JOBID_PRINT(ORTE_PROC_MY_NAME->jobid));
        break;
    case ORTE_SNAPC_CKPT_STATE_NO_CKPT:
        orte_notifier.log(ORTE_NOTIFIER_WARN, ORTE_SNAPC_CKPT_NOTIFY(state),
                          "%d: Process %s is not checkpointable.",
                          orte_process_info.pid, ORTE_JOBID_PRINT(ORTE_PROC_MY_NAME->jobid));
        break;
    case ORTE_SNAPC_CKPT_STATE_ERROR:
        orte_notifier.log(ORTE_NOTIFIER_WARN, ORTE_SNAPC_CKPT_NOTIFY(state),
                          "%d: Failed to checkpoint process %s.",
                          orte_process_info.pid, ORTE_JOBID_PRINT(ORTE_PROC_MY_NAME->jobid));
        break;
    /* ADK: We currently do not notify for these states, but good to
     * have them around anyways. */
    case ORTE_SNAPC_CKPT_STATE_NONE:
    case ORTE_SNAPC_CKPT_STATE_REQUEST:
    case ORTE_SNAPC_CKPT_STATE_PENDING:
    case ORTE_SNAPC_CKPT_STATE_RUNNING:
    case ORTE_SNAPC_CKPT_STATE_STOPPED:
    case ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL:
    default:
        break;
    }
}

int orte_snapc_base_global_coord_ckpt_init_cmd(orte_process_name_t* peer,
                                               opal_buffer_t* buffer,
                                               opal_crs_base_ckpt_options_t *options,
                                               orte_jobid_t *jobid)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count = 1;

    /*
     * Do not send to self, as that is silly.
     */
    if (peer->jobid == ORTE_PROC_MY_HNP->jobid &&
        peer->vpid  == ORTE_PROC_MY_HNP->vpid ) {
        OPAL_OUTPUT_VERBOSE((10, orte_snapc_base_output,
                             "%s) base:ckpt_init_cmd: Error: Do not send to self!\n",
                             ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type)));
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((10, orte_snapc_base_output,
                         "%s) base:ckpt_init_cmd: Receiving commands\n",
                         ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type)));

    /********************
     * Receive command line checkpoint request:
     * - Command (already received)
     * - options
     * - jobid
     ********************/
    if( ORTE_SUCCESS != (ret = orte_snapc_base_unpack_options(buffer, options)) ) {
        opal_output(orte_snapc_base_output,
                    "%s) base:ckpt_init_cmd: Error: Unpack (options) Failure (ret = %d)\n",
                    ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type), ret );
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    count = 1;
    if ( ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, jobid, &count, ORTE_JOBID)) ) {
        opal_output(orte_snapc_base_output,
                    "%s) base:ckpt_init_cmd: Error: DSS Unpack (jobid) Failure (ret = %d) (LINE = %d)\n",
                    ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type),
                    ret, __LINE__);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((10, orte_snapc_base_output,
                         "%s) base:ckpt_init_cmd: Received [%d, %d, %s]\n",
                         ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type),
                         (int)(options->term),
                         (int)(options->stop),
                         ORTE_JOBID_PRINT(*jobid)));

 cleanup:
    return exit_status;
}

int orte_snapc_base_unpack_options(opal_buffer_t* buffer,
                                   opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_std_cntr_t count = 1;

    count = 1;
    if ( ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(options->term), &count, OPAL_BOOL)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base:unpack_options: Error: Unpack (term) Failure (ret = %d)\n",
                    ret);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    count = 1;
    if ( ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &(options->stop), &count, OPAL_BOOL)) ) {
        opal_output(orte_snapc_base_output,
                    "snapc:base:unpack_options: Error: Unpack (stop) Failure (ret = %d)\n",
                    ret);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    
 cleanup:
    return exit_status;
}

int orte_snapc_base_pack_options(opal_buffer_t* buffer,
                                 opal_crs_base_ckpt_options_t *options)
{
    int ret, exit_status = ORTE_SUCCESS;

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(options->term), 1, OPAL_BOOL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(buffer, &(options->stop), 1, OPAL_BOOL))) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

int orte_snapc_base_global_coord_ckpt_update_cmd(orte_process_name_t* peer,
                                                 char *global_snapshot_handle,
                                                 int seq_num,
                                                 int ckpt_status)
{
    int ret, exit_status = ORTE_SUCCESS;
    opal_buffer_t *loc_buffer = NULL;
    orte_snapc_cmd_flag_t command = ORTE_SNAPC_GLOBAL_UPDATE_CMD;

    /*
     * Noop if invalid peer, or peer not specified (JJH Double check this)
     */
    if( NULL == peer ||
        OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_NAME_INVALID, peer) ) {
        /*return ORTE_ERR_BAD_PARAM;*/
        return ORTE_SUCCESS;
    }

    /*
     * Do not send to self, as that is silly.
     */
    if (peer->jobid == ORTE_PROC_MY_HNP->jobid &&
        peer->vpid  == ORTE_PROC_MY_HNP->vpid ) {
        OPAL_OUTPUT_VERBOSE((10, orte_snapc_base_output,
                             "%s) base:ckpt_update_cmd: Error: Do not send to self!\n",
                             ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type)));
        return ORTE_SUCCESS;
    }

    /*
     * Pass on the checkpoint state over the notifier interface.
     */
    orte_snapc_ckpt_state_notify(ckpt_status);

    OPAL_OUTPUT_VERBOSE((10, orte_snapc_base_output,
                         "%s) base:ckpt_update_cmd: Sending update command <%s> <seq %d> <status %d>\n",
                         ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type),
                         global_snapshot_handle, seq_num, ckpt_status));

    /********************
     * Send over the status of the checkpoint
     * - ckpt_state
     * - global snapshot handle (upon finish only)
     * - sequence number        (upon finish only)
     ********************/
    if (NULL == (loc_buffer = OBJ_NEW(opal_buffer_t))) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(loc_buffer, &command, 1, ORTE_SNAPC_CMD)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(loc_buffer, &ckpt_status, 1, OPAL_INT))) {
        opal_output(orte_snapc_base_output,
                    "%s) base:ckpt_update_cmd: Error: DSS Pack (ckpt_status) Failure (ret = %d) (LINE = %d)\n",
                    ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type),
                    ret, __LINE__);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    if( ORTE_SNAPC_CKPT_STATE_FINISHED == ckpt_status ||
        ORTE_SNAPC_CKPT_STATE_STOPPED  == ckpt_status ||
        ORTE_SNAPC_CKPT_STATE_ERROR    == ckpt_status ) {
        if (ORTE_SUCCESS != (ret = opal_dss.pack(loc_buffer, &global_snapshot_handle, 1, OPAL_STRING))) {
            opal_output(orte_snapc_base_output,
                        "%s) base:ckpt_update_cmd: Error: DSS Pack (snapshot handle) Failure (ret = %d) (LINE = %d)\n",
                        ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type),
                        ret, __LINE__);
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
        if (ORTE_SUCCESS != (ret = opal_dss.pack(loc_buffer, &seq_num, 1, OPAL_INT))) {
            opal_output(orte_snapc_base_output,
                        "%s) base:ckpt_update_cmd: Error: DSS Pack (seq number) Failure (ret = %d) (LINE = %d)\n",
                        ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type),
                        ret, __LINE__);
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }

    if (0 > (ret = orte_rml.send_buffer(peer, loc_buffer, ORTE_RML_TAG_CKPT, 0))) {
        opal_output(orte_snapc_base_output,
                    "%s) base:ckpt_update_cmd: Error: Send (ckpt_status) Failure (ret = %d) (LINE = %d)\n",
                    ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type),
                    ret, __LINE__);
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }


 cleanup:
    if(NULL != loc_buffer) {
        OBJ_RELEASE(loc_buffer);
        loc_buffer = NULL;
    }

    return exit_status;
}

/****************************
 * Command line tool request functions
 ****************************/
/* JJH TODO - Move the command line functions here ? */

/*****************************
 * Snapshot metadata functions
 *****************************/
int orte_snapc_base_get_all_snapshot_refs(char *base_dir, int *num_refs, char ***snapshot_refs)
{
#ifndef HAVE_DIRENT_H
    return ORTE_ERR_NOT_SUPPORTED;
#else
    int ret, exit_status = ORTE_SUCCESS;
    char * tmp_str = NULL, * metadata_file = NULL;
    DIR *dirp = NULL;
    struct dirent *dir_entp = NULL;
    struct stat file_status;

    if( NULL == base_dir ) {
        if( NULL == orte_snapc_base_global_snapshot_dir ) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
        base_dir = strdup(orte_snapc_base_global_snapshot_dir);
    }

    /*
     * Get all subdirectories under the base directory
     */
    dirp = opendir(base_dir);
    while( NULL != (dir_entp = readdir(dirp))) {
        /* Skip "." and ".." if they are in the list */
        if( 0 == strncmp("..", dir_entp->d_name, strlen("..") ) ||
            0 == strncmp(".",  dir_entp->d_name, strlen(".")  ) ) {
            continue;
        }

        /* Add the full path */
        asprintf(&tmp_str, "%s/%s", base_dir, dir_entp->d_name);
        if(0 != (ret = stat(tmp_str, &file_status) ) ){
            free( tmp_str);
            tmp_str = NULL;
            continue;
        } else {
            /* Is it a directory? */
            if(S_ISDIR(file_status.st_mode) ) {
                asprintf(&metadata_file, "%s/%s",
                         tmp_str,
                         orte_snapc_base_metadata_filename);
                if(0 != (ret = stat(metadata_file, &file_status) ) ){
                    free( tmp_str);
                    tmp_str = NULL;
                    free( metadata_file);
                    metadata_file = NULL;
                    continue;
                } else {
                    if(S_ISREG(file_status.st_mode) ) {
                        opal_argv_append(num_refs, snapshot_refs, dir_entp->d_name);
                    }
                }
                free( metadata_file);
                metadata_file = NULL;
            }
        }

        free( tmp_str);
        tmp_str = NULL;
    }
    
    closedir(dirp);

 cleanup:
    if( NULL != tmp_str) {
        free( tmp_str);
        tmp_str = NULL;
    }

    return exit_status;
#endif /* HAVE_DIRENT_H */
}

int orte_snapc_base_get_all_snapshot_ref_seqs(char *base_dir, char *snapshot_name, int *num_seqs, int **snapshot_ref_seqs)
{
    int exit_status = ORTE_SUCCESS;
    char * metadata_file = NULL;
    FILE * meta_data = NULL;
    int s, next_seq_int;

    if( NULL == base_dir ) {
        if( NULL == orte_snapc_base_global_snapshot_dir ) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
        base_dir = strdup(orte_snapc_base_global_snapshot_dir);
    }

    asprintf(&metadata_file, "%s/%s/%s",
             base_dir,
             snapshot_name,
             orte_snapc_base_metadata_filename);


    if (NULL == (meta_data = fopen(metadata_file, "r")) ) {
        opal_output(0, "Error: Unable to open the file <%s>\n", metadata_file);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /* First pass to count the number of sequence numbers */
    *num_seqs = 0;
    while(0 <= (next_seq_int = get_next_valid_seq_number(meta_data)) ){
        *num_seqs += 1;
    }

    /* If there are no valid seq numbers then just return here */
    if( 0 == *num_seqs ) {
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    rewind(meta_data);

    /* Second pass to add them to the list */
    (*snapshot_ref_seqs) = (int *) malloc(sizeof(int) * (*num_seqs));
    s = 0;
    while(0 <= (next_seq_int = get_next_valid_seq_number(meta_data)) ){
        (*snapshot_ref_seqs)[s] = next_seq_int;
        ++s;
    }

 cleanup:
    if(NULL != meta_data) {
        fclose(meta_data);
        meta_data = NULL;
    }
    if(NULL != metadata_file) {
        free(metadata_file);
        metadata_file = NULL;
    }

    return exit_status;
}

int orte_snapc_base_unique_global_snapshot_name(char **name_str, pid_t pid)
{
    if( NULL == orte_snapc_base_global_snapshot_ref ) {
        asprintf(name_str, "ompi_global_snapshot_%d.ckpt", pid);
    }
    else {
        *name_str = strdup(orte_snapc_base_global_snapshot_ref);
    }
    
    return ORTE_SUCCESS;
}

int orte_snapc_base_get_global_snapshot_metadata_file(char **file_name, char *uniq_snapshot_name)
{
    asprintf(file_name, "%s/%s/%s",
             orte_snapc_base_global_snapshot_dir,
             uniq_snapshot_name,
             orte_snapc_base_metadata_filename);
    
    return ORTE_SUCCESS;
}

int orte_snapc_base_get_global_snapshot_directory(char **dir_name, char *uniq_snapshot_name)
{
    asprintf(dir_name, "%s/%s/%d", 
             orte_snapc_base_global_snapshot_dir, 
             uniq_snapshot_name,
             (int)orte_snapc_base_snapshot_seq_number);

    return ORTE_SUCCESS;
}

int orte_snapc_base_init_global_snapshot_directory(char *uniq_global_snapshot_name, bool empty_metadata)
{
    char * dir_name = NULL, *meta_data_fname = NULL;
    mode_t my_mode = S_IRWXU;
    int ret;
    int exit_status = ORTE_SUCCESS;
    FILE * meta_data = NULL;

    /*
     * Make the snapshot directory from the uniq_global_snapshot_name
     */
    orte_snapc_base_get_global_snapshot_directory(&dir_name, uniq_global_snapshot_name);
    if(OPAL_SUCCESS != (ret = opal_os_dirpath_create(dir_name, my_mode)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * Initialize the metadata file at the top of that directory.
     */
    orte_snapc_base_get_global_snapshot_metadata_file(&meta_data_fname, uniq_global_snapshot_name);

    if (NULL == (meta_data = fopen(meta_data_fname, "a")) ) {
        opal_output(orte_snapc_base_output,
                    "%s) base:init_global_snapshot_directory: Error: Unable to open the file (%s)\n",
                    ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type),
                    meta_data_fname);
        ORTE_ERROR_LOG(ORTE_ERROR);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    /*
     * Put in the checkpoint sequence number
     */
    if( empty_metadata ) {
        fprintf(meta_data, "#\n");
    }
    else {
        /*
         * Put in the checkpoint sequence number
         */
        fprintf(meta_data, "#\n%s%d\n", SNAPC_METADATA_SEQ, (int)orte_snapc_base_snapshot_seq_number);

        fclose(meta_data);
        meta_data = NULL;

        /* Add timestamp */
        orte_snapc_base_add_timestamp(uniq_global_snapshot_name);
    }

 cleanup:
    if(NULL != meta_data)
        fclose(meta_data);
    if(NULL != dir_name)
        free(dir_name);
    if(NULL != meta_data_fname)
        free(meta_data_fname);

    return ORTE_SUCCESS;
}

/*
 * Metadata file handling functions
 * File is of the form:
 *
 * #
 * # Checkpoint Sequence #
 * # Begin Timestamp
 * # Process ID
 * # OPAL CRS
 * opal_restart ----mca crs_base_snapshot_dir SNAPSHOT_LOC SNAPSHOT_REF
 * ...
 * # End Timestamp
 *
 * E.g.,
 #
 # Seq: 0
 # Timestamp:          Mon Jun  5 18:32:08 2006
 # Process:            0.1.0
 # OPAL CRS Component: blcr
 opal_restart --mca crs_base_snapshot_dir /tmp/ompi_global_snapshot_32535.ckpt/0 opal_snapshot_0.ckpt
 # Process:            0.1.1
 # OPAL CRS Component: blcr
 opal_restart --mca crs_base_snapshot_dir /tmp/ompi_global_snapshot_32535.ckpt/0 opal_snapshot_1.ckpt
 # Timestamp:          Mon Jun  5 18:32:10 2006
 #
 # Seq: 1
 # Timestamp:          Mon Jun  5 18:32:12 2006
 # Process:            0.1.0
 # OPAL CRS Component: blcr
 opal_restart --mca crs_base_snapshot_dir /tmp/ompi_global_snapshot_32535.ckpt/1 opal_snapshot_0.ckpt
 # Process:            0.1.1
 # OPAL CRS Component: blcr
 opal_restart --mca crs_base_snapshot_dir /tmp/ompi_global_snapshot_32535.ckpt/1 opal_snapshot_1.ckpt
 # Timestamp:          Mon Jun  5 18:32:13 2006
 *
 */
int orte_snapc_base_add_timestamp(char * global_snapshot_ref)
{
    int exit_status = ORTE_SUCCESS;
    FILE * meta_data = NULL;
    char * meta_data_fname = NULL;
    time_t timestamp;

    orte_snapc_base_get_global_snapshot_metadata_file(&meta_data_fname, global_snapshot_ref);

    if (NULL == (meta_data = fopen(meta_data_fname, "a")) ) {
        opal_output(orte_snapc_base_output,
                    "%s) base:add_timestamp: Error: Unable to open the file (%s)\n",
                    ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type),
                    meta_data_fname);
        ORTE_ERROR_LOG(ORTE_ERROR);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    timestamp = time(NULL);
    fprintf(meta_data, "%s%s", SNAPC_METADATA_TIME, ctime(&timestamp));

 cleanup:
    if( NULL != meta_data )
        fclose(meta_data);
    if( NULL != meta_data_fname)
        free(meta_data_fname);
    
    return exit_status;
}

int orte_snapc_base_finalize_metadata(char * global_snapshot_ref)
{
    int exit_status = ORTE_SUCCESS;
    FILE * meta_data = NULL;
    char * meta_data_fname = NULL;

    /* Add the final timestamp */
    orte_snapc_base_add_timestamp(global_snapshot_ref);

    orte_snapc_base_get_global_snapshot_metadata_file(&meta_data_fname, global_snapshot_ref);

    if (NULL == (meta_data = fopen(meta_data_fname, "a")) ) {
        opal_output(orte_snapc_base_output,
                    "%s) base:add_timestamp: Error: Unable to open the file (%s)\n",
                    ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type),
                    meta_data_fname);
        ORTE_ERROR_LOG(ORTE_ERROR);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    fprintf(meta_data, "%s%d\n", SNAPC_METADATA_DONE_SEQ, (int)orte_snapc_base_snapshot_seq_number);

 cleanup:
    if( NULL != meta_data )
        fclose(meta_data);
    if( NULL != meta_data_fname)
        free(meta_data_fname);
    
    return exit_status;
}


int orte_snapc_base_add_vpid_metadata( orte_process_name_t *proc,
                                       char * global_snapshot_ref,
                                       char *snapshot_ref,
                                       char *snapshot_location,
                                       char *crs_agent)
{
    int ret, exit_status = ORTE_SUCCESS;
    FILE * meta_data = NULL;
    char * meta_data_fname = NULL;
    char * crs_comp = NULL;
    char * proc_name = NULL;
    char * local_snapshot = NULL;
    int prev_pid = 0;

    if( NULL == snapshot_location ) {
        return ORTE_ERROR;
    }

    orte_snapc_base_get_global_snapshot_metadata_file(&meta_data_fname, global_snapshot_ref);

    if (NULL == (meta_data = fopen(meta_data_fname, "a")) ) {
        opal_output(orte_snapc_base_output,
                    "%s) base:add_metadata: Error: Unable to open the file (%s)\n",
                    ORTE_SNAPC_COORD_NAME_PRINT(orte_snapc_coord_type),
                    meta_data_fname);
        ORTE_ERROR_LOG(ORTE_ERROR);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    
    /* 
     * Something of the form:
     * 0.1.0 opal_snapshot_0.ckpt /tmp/ompi_global_snapshot_8827.ckpt/1/opal_snapshot_0.ckpt BLCR 
     * or better yet start to create the proper app schema:
     * orte_restart --mca crs_base_snapshot_dir /tmp/ompi_global_snapshot_8827.ckpt/1 opal_snapshot_0.ckpt
     */
    orte_util_convert_process_name_to_string(&proc_name, proc);

    /* Extract the checkpointer */
    if( NULL == crs_agent ) {
        asprintf(&local_snapshot, "%s/%s", snapshot_location, snapshot_ref);
        if( OPAL_SUCCESS != (ret = opal_crs_base_extract_expected_component(local_snapshot, &crs_comp, &prev_pid)) ) {
            opal_show_help("help-orte-snapc-base.txt", "invalid_metadata", true,
                           proc_name, opal_crs_base_metadata_filename, local_snapshot);
            exit_status = ret;
            goto cleanup;
        }
    } else {
        crs_comp = strdup(crs_agent);
    }

    /* Write the string */
    fprintf(meta_data, "%s%s\n", SNAPC_METADATA_PROCESS,   proc_name);
    fprintf(meta_data, "%s%s\n", SNAPC_METADATA_CRS_COMP,  crs_comp);
    fprintf(meta_data, "%s%s\n", SNAPC_METADATA_SNAP_REF,  snapshot_ref);
    fprintf(meta_data, "%s%s\n", SNAPC_METADATA_SNAP_LOC,  snapshot_location);

 cleanup:
    if( NULL != meta_data ) {
        fclose(meta_data);
        meta_data = NULL;
    }
    if( NULL != meta_data_fname) {
        free(meta_data_fname);
        meta_data_fname = NULL;
    }
    if( NULL != local_snapshot ) {
        free( local_snapshot );
        local_snapshot = NULL;
    }
    
    return exit_status;
}

int orte_snapc_base_extract_metadata(orte_snapc_base_global_snapshot_t *global_snapshot)
{
    int exit_status = ORTE_SUCCESS;
    FILE * meta_data = NULL;
    char * meta_data_fname = NULL;
    int    next_seq_int;
    char * token = NULL;
    char * value = NULL;
    orte_snapc_base_local_snapshot_t *vpid_snapshot = NULL;

    /*
     * Open the metadata file
     */
    orte_snapc_base_get_global_snapshot_metadata_file(&meta_data_fname, global_snapshot->reference_name);
    if (NULL == (meta_data = fopen(meta_data_fname, "r")) ) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /* 
     * If we were not given a sequence number, first find the largest valid seq number
     */
    if(0 > global_snapshot->seq_num ) {
        while(0 <= (next_seq_int = get_next_valid_seq_number(meta_data)) ){
            global_snapshot->seq_num = next_seq_int;
        }
        rewind(meta_data);
    }

    /*
     * Find the requested sequence number, 
     */
    while( global_snapshot->seq_num != (next_seq_int = get_next_seq_number(meta_data)) ) {
        /* We didn't find the requested seq */
        if(0 > next_seq_int) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
    }

    /*
     * Extract each token and make the records
     */
    do {
        if( ORTE_SUCCESS != metadata_extract_next_token(meta_data, &token, &value) ) {
            break;
        }
        
        if(0 == strncmp(SNAPC_METADATA_SEQ, token, strlen(SNAPC_METADATA_SEQ)) ) {
            break;
        }
        else if(0 == strncmp(SNAPC_METADATA_TIME, token, strlen(SNAPC_METADATA_TIME)) ) {
            if( NULL == global_snapshot->start_time) {
                global_snapshot->start_time = strdup(value);
            }
            else {
                global_snapshot->end_time = strdup(value);
            }
        }
        else if(0 == strncmp(SNAPC_METADATA_PROCESS, token, strlen(SNAPC_METADATA_PROCESS)) ) {
            orte_process_name_t proc;

            orte_util_convert_string_to_process_name(&proc, value);

            /* Not the first process, so append it to the list */
            if( NULL != vpid_snapshot) {
                opal_list_append(&global_snapshot->local_snapshots, &(vpid_snapshot->super));
            }

            vpid_snapshot = OBJ_NEW(orte_snapc_base_local_snapshot_t);

            vpid_snapshot->process_name.jobid  = proc.jobid;
            vpid_snapshot->process_name.vpid   = proc.vpid;
        }
        else if(0 == strncmp(SNAPC_METADATA_CRS_COMP, token, strlen(SNAPC_METADATA_CRS_COMP)) ) {
            vpid_snapshot->opal_crs = strdup(value);
        }
        else if(0 == strncmp(SNAPC_METADATA_SNAP_REF, token, strlen(SNAPC_METADATA_SNAP_REF)) ) {
            vpid_snapshot->reference_name = strdup(value);
        }
        else if(0 == strncmp(SNAPC_METADATA_SNAP_LOC, token, strlen(SNAPC_METADATA_SNAP_LOC)) ) {
            vpid_snapshot->local_location  = strdup(value);
            vpid_snapshot->remote_location = strdup(value);
        }
    } while(0 == feof(meta_data) );
    
    /* Append the last item */
    if( NULL != vpid_snapshot) {
        opal_list_append(&global_snapshot->local_snapshots, &(vpid_snapshot->super));
    }
    
 cleanup:
    if(NULL != meta_data)
        fclose(meta_data);
    if(NULL != meta_data_fname)
        free(meta_data_fname);

    return exit_status;
}

/*
 * Extract the next sequence number from the file
 */
static int get_next_seq_number(FILE *file)
{
    char *token = NULL;
    char *value = NULL;
    int seq_int = -1;

    do {
        if( ORTE_SUCCESS != metadata_extract_next_token(file, &token, &value) ) {
            seq_int = -1;
            goto cleanup;
        }
    } while(0 != strncmp(token, SNAPC_METADATA_SEQ, strlen(SNAPC_METADATA_SEQ)) );

    seq_int = atoi(value);

 cleanup:
    if( NULL != token)
        free(token);
    if( NULL != value)
        free(value);

    return seq_int;
}

/*
 * Extract the next Valid sequence number from the file
 */
static int get_next_valid_seq_number(FILE *file)
{
    char *token = NULL;
    char *value = NULL;
    int seq_int = -1;

    do {
        if( ORTE_SUCCESS != metadata_extract_next_token(file, &token, &value) ) {
            seq_int = -1;
            goto cleanup;
        }
    } while(0 != strncmp(token, SNAPC_METADATA_DONE_SEQ, strlen(SNAPC_METADATA_DONE_SEQ)) );

    seq_int = atoi(value);

 cleanup:
    if( NULL != token)
        free(token);
    if( NULL != value)
        free(value);

    return seq_int;
}

static int metadata_extract_next_token(FILE *file, char **token, char **value)
{
    int exit_status = ORTE_SUCCESS;
    int max_len = 256;
    char * line = NULL;
    int line_len = 0;
    int c = 0, s = 0, v = 0;
    char *local_token = NULL;
    char *local_value = NULL;
    bool end_of_line = false;

    line = (char *) malloc(sizeof(char) * max_len);

 try_again:
    /*
     * If we are at the end of the file, then just return
     */
    if(0 != feof(file) ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * Other wise grab the next token/value pair
     */
    if (NULL == fgets(line, max_len, file) ) {
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    line_len = strlen(line);
    /* Strip off the new line if it it there */
    if('\n' == line[line_len-1]) {
        line[line_len-1] = '\0';
        line_len--;
        end_of_line = true;
    }
    else {
        end_of_line = false;
    }

    /* Ignore lines with just '#' too */
    if(2 >= line_len)
        goto try_again;
    
    /*
     * Extract the token from the set
     */
    for(c = 0; 
        line[c] != ':' && 
            c < line_len;
        ++c) {
        ;
    }
    c += 2; /* For the ' ' and the '\0' */
    local_token = (char *)malloc(sizeof(char) * (c + 1));

    for(s = 0; s < c; ++s) {
        local_token[s] = line[s];
    }

    local_token[s] = '\0';
    *token = strdup(local_token);

    if( NULL != local_token) {
        free(local_token);
        local_token = NULL;
    }

    /*
     * Extract the value from the set
     */
    local_value = (char *)malloc(sizeof(char) * (line_len - c + 1));
    for(v = 0, s = c; 
        s < line_len;
        ++s, ++v) {
        local_value[v] = line[s];
    }

    while(!end_of_line) {
        if (NULL == fgets(line, max_len, file) ) {
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
        line_len = strlen(line);
        /* Strip off the new line if it it there */
        if('\n' == line[line_len-1]) {
            line[line_len-1] = '\0';
            line_len--;
            end_of_line = true;
        }
        else {
            end_of_line = false;
        }
        
        local_value = (char *)realloc(local_value, sizeof(char) * line_len);
        for(s = 0;
            s < line_len;
            ++s, ++v) {
            local_value[v] = line[s];
        }
    }

    local_value[v] = '\0';
    *value = strdup(local_value);

 cleanup:
    if( NULL != local_token)
        free(local_token);
    if( NULL != local_value)
        free(local_value);
    if( NULL != line)
        free(line);

    return exit_status;
}

int orte_snapc_ckpt_state_str(char ** state_str, int state)
{
    switch(state) {
    case ORTE_SNAPC_CKPT_STATE_NONE:
        *state_str = strdup(" -- ");
        break;
    case ORTE_SNAPC_CKPT_STATE_REQUEST:
        *state_str = strdup("Requested");
        break;
    case ORTE_SNAPC_CKPT_STATE_PENDING:
        *state_str = strdup("Pending");
        break;
    case ORTE_SNAPC_CKPT_STATE_RUNNING:
        *state_str = strdup("Running");
        break;
    case ORTE_SNAPC_CKPT_STATE_STOPPED:
        *state_str = strdup("Stopped");
        break;
    case ORTE_SNAPC_CKPT_STATE_FILE_XFER:
        *state_str = strdup("File Transfer");
        break;
    case ORTE_SNAPC_CKPT_STATE_FINISHED:
        *state_str = strdup("Finished");
        break;
    case ORTE_SNAPC_CKPT_STATE_FINISHED_LOCAL:
        *state_str = strdup("Locally Finished");
        break;
    case ORTE_SNAPC_CKPT_STATE_ERROR:
        *state_str = strdup("Error");
        break;
    default:
        asprintf(state_str, "Unknown %d", state);
        break;
    }

    return ORTE_SUCCESS;
}
