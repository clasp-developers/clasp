/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "opal/util/argv.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "opal/util/basename.h"

#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"

#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"


/*
 * Preload all files for a single app context
 */
static int orte_odls_base_preload_append_binary(orte_app_context_t* context,
                                               orte_filem_base_request_t *filem_request);
static int orte_odls_base_preload_append_files(orte_app_context_t* context,
                                              orte_filem_base_request_t *filem_request);
static bool orte_odls_base_is_preload_local_dup(char *local_ref,
                                                orte_filem_base_request_t *filem_request);

int orte_odls_base_preload_files_app_context(orte_app_context_t* app_context)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_filem_base_request_t *filem_request;
    orte_filem_base_process_set_t *p_set = NULL;

    /* Sanity Check - Make sure there are files to preload */
    if(!app_context->preload_binary &&
       NULL == app_context->preload_files) {
        return exit_status;
    }

    filem_request = OBJ_NEW(orte_filem_base_request_t);

    /* Define the process set */
    p_set = OBJ_NEW(orte_filem_base_process_set_t);
    if( ORTE_PROC_IS_HNP ) {
        /* if I am the HNP, then use me as the source */
        p_set->source.jobid = ORTE_PROC_MY_NAME->jobid;
        p_set->source.vpid  = ORTE_PROC_MY_NAME->vpid;
    }
    else {
        /* otherwise, set the HNP as the source */
        p_set->source.jobid = ORTE_PROC_MY_HNP->jobid;
        p_set->source.vpid  = ORTE_PROC_MY_HNP->vpid;
    }
    p_set->sink.jobid   = ORTE_PROC_MY_NAME->jobid;
    p_set->sink.vpid    = ORTE_PROC_MY_NAME->vpid;

    opal_list_append(&(filem_request->process_sets), &(p_set->super) );

    if(app_context->preload_binary) {
        OPAL_OUTPUT_VERBOSE((1, orte_odls_globals.output,
                             "%s) Preload Binary...",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        if( ORTE_SUCCESS != (ret = orte_odls_base_preload_append_binary(app_context, 
                                                                        filem_request) ) ){
            orte_show_help("help-orte-odls-base.txt",
                           "orte-odls-base:could-not-preload-binary",
                           true, app_context->app);
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            /* Keep accumulating files anyway */
        }
    }
    if( NULL != app_context->preload_files) {
        OPAL_OUTPUT_VERBOSE((1, orte_odls_globals.output,
                             "%s) Preload Files... [%s]",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             app_context->preload_files));
        if( ORTE_SUCCESS != (ret = orte_odls_base_preload_append_files(app_context, 
                                                                      filem_request) ) ){
            orte_show_help("help-orte-odls-base.txt",
                           "orte-odls-base:could-not-preload-files",
                           true, app_context->preload_files);
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            /* Keep accumulating files anyway */
        }
    }
    /* Actually bring over the files - One app context at a time 
     * JJH: This could be improved for multiple app contexts by making
     *      this a non-blocking filem get and then waiting on all of
     *      the requests for all app contexts.
     */
    if( ORTE_SUCCESS != (ret = orte_filem.get(filem_request)) ) {
        orte_show_help("help-orte-odls-base.txt",
                       "orte-odls-base:could-not-preload",
                       true,
                       (app_context->preload_binary ? app_context->app : ""),
                       (NULL != app_context->preload_files ? app_context->preload_files : ""));
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

 cleanup:
    if( NULL != filem_request ) {
        OBJ_RELEASE(filem_request);
        filem_request = NULL;
    }

    return exit_status;
}

/*
 * The difference between preloading a file, and a binary file is that 
 * we may need to update the app_context to reflect the placement of the binary file
 * on the local machine.
 */
static int orte_odls_base_preload_append_binary(orte_app_context_t* context, 
                                               orte_filem_base_request_t *filem_request) {
    char * local_bin = NULL;
    orte_filem_base_file_set_t * f_set = NULL;

    f_set = OBJ_NEW(orte_filem_base_file_set_t);

    /* Local Placement */
    asprintf(&local_bin, "%s/%s", orte_process_info.job_session_dir, opal_basename(context->app));
    if(orte_odls_base_is_preload_local_dup(local_bin, filem_request) ) {
        goto cleanup;
    }
    f_set->local_target = strdup(local_bin);

    /* Remote reference */
    f_set->remote_target = strdup(context->app);

    /* Flag as a single file */
    f_set->target_flag = ORTE_FILEM_TYPE_FILE;

    /* Add to the request list */
    opal_list_append(&(filem_request->file_sets), &(f_set->super) );

 cleanup:
    /*
     * Adjust the process name to point to the new local version (and argv[0])
     */
    if( NULL != local_bin ) {
        if(NULL != context->app) {
            free(context->app);
            context->app = NULL;
        }
        if(NULL != context->argv[0]) {
            free(context->argv[0]);
            context->argv[0] = NULL;
        }

        context->app     = strdup(local_bin);
        context->argv[0] = strdup(local_bin);

        free(local_bin);
    }
    
    return ORTE_SUCCESS;
}


static int orte_odls_base_preload_append_files(orte_app_context_t* context, 
                                              orte_filem_base_request_t *filem_request) {
    char * local_ref = NULL;
    int i, remote_argc = 0;
    char **remote_targets = NULL;
    orte_filem_base_file_set_t * f_set = NULL;

    remote_targets = opal_argv_split(context->preload_files, ',');
    remote_argc  = opal_argv_count(remote_targets);

    for(i = 0; i < remote_argc; ++i) {
        if(NULL != context->preload_files_dest_dir) {
            if(context->preload_files_dest_dir[0] == '.') {
                asprintf(&local_ref, "%s/%s/%s", context->cwd, context->preload_files_dest_dir, opal_basename(remote_targets[i]) );
            }
            else {
                asprintf(&local_ref, "%s/%s", context->preload_files_dest_dir, opal_basename(remote_targets[i]) );
            }
        }
        else {
            /* 
             * If the preload_files_dest_dir is not specified
             * If this is an absolute path, copy it to that path. Otherwise copy it to the cwd.
             */
            if('/' == remote_targets[i][0]) {
                asprintf(&local_ref, "%s", remote_targets[i]);
            } else {
                asprintf(&local_ref, "%s/%s", context->cwd, remote_targets[i] );
            }

            /* If this is the HNP, then source = sink, so use the same path for each local and remote */
            if( ORTE_PROC_IS_HNP ) {
                free(remote_targets[i]);
                remote_targets[i] = strdup(local_ref);
            }
        }

        /*
         * Is this a duplicate
         */
        if(orte_odls_base_is_preload_local_dup(local_ref, filem_request) ) {
            free(local_ref);
            local_ref = NULL;
            continue;
        }

        f_set = OBJ_NEW(orte_filem_base_file_set_t);

        /* Local Placement */
        f_set->local_target = strdup(local_ref);

        /* Remote reference */
        f_set->remote_target = strdup(remote_targets[i]);

        /* Flag as unknown, let FileM figure it out */
        f_set->target_flag = ORTE_FILEM_TYPE_UNKNOWN;

        /* Add to the request list */
        opal_list_append(&(filem_request->file_sets), &(f_set->super) );

        free(local_ref);
        local_ref = NULL;
    }

    if(NULL != local_ref)
        free(local_ref);
    if(NULL != remote_targets)
        opal_argv_free(remote_targets);

    return ORTE_SUCCESS;
}

/*
 * Keeps us from transfering the same file more than once.
 */
static bool orte_odls_base_is_preload_local_dup(char *local_ref,
                                                orte_filem_base_request_t *filem_request) {
    opal_list_item_t *item = NULL;

    for (item  = opal_list_get_first( &filem_request->file_sets);
         item != opal_list_get_end(   &filem_request->file_sets);
         item  = opal_list_get_next(   item) ) {
        orte_filem_base_file_set_t * f_set = (orte_filem_base_file_set_t*)item;

        if(0 == strncmp(local_ref, f_set->local_target, strlen(local_ref)+1) ) {
            return true;
        }
    }

    return false;
}
