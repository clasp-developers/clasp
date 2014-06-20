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

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <time.h>

#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/proc_info.h"

#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"

/******************
 * Local Functions
 ******************/

/******************
 * Object Stuff
 ******************/
ORTE_DECLSPEC OBJ_CLASS_INSTANCE(orte_filem_base_process_set_t,
                                 opal_list_item_t,
                                 orte_filem_base_process_set_construct,
                                 orte_filem_base_process_set_destruct);

ORTE_DECLSPEC void orte_filem_base_process_set_construct(orte_filem_base_process_set_t *req) {
    req->source = *ORTE_NAME_INVALID;
    req->sink   = *ORTE_NAME_INVALID;
}

ORTE_DECLSPEC void orte_filem_base_process_set_destruct( orte_filem_base_process_set_t *req) {
    req->source = *ORTE_NAME_INVALID;
    req->sink   = *ORTE_NAME_INVALID;
}

ORTE_DECLSPEC OBJ_CLASS_INSTANCE(orte_filem_base_file_set_t,
                                 opal_list_item_t,
                                 orte_filem_base_file_set_construct,
                                 orte_filem_base_file_set_destruct);

ORTE_DECLSPEC void orte_filem_base_file_set_construct(orte_filem_base_file_set_t *req) {
    req->local_target  = NULL;
    req->local_hint    = ORTE_FILEM_HINT_NONE;

    req->remote_target = NULL;
    req->remote_hint   = ORTE_FILEM_HINT_NONE;

    req->target_flag   = ORTE_FILEM_TYPE_UNKNOWN;

}

ORTE_DECLSPEC void orte_filem_base_file_set_destruct( orte_filem_base_file_set_t *req) {
    if( NULL != req->local_target ) {
        free(req->local_target);
        req->local_target = NULL;
    }
    req->local_hint    = ORTE_FILEM_HINT_NONE;

    if( NULL != req->remote_target ) {
        free(req->remote_target);
        req->remote_target = NULL;
    }
    req->remote_hint   = ORTE_FILEM_HINT_NONE;

    req->target_flag   = ORTE_FILEM_TYPE_UNKNOWN;
}

ORTE_DECLSPEC OBJ_CLASS_INSTANCE(orte_filem_base_request_t,
                                 opal_list_item_t,
                                 orte_filem_base_construct,
                                 orte_filem_base_destruct);

void orte_filem_base_construct(orte_filem_base_request_t *req) {
    OBJ_CONSTRUCT(&req->process_sets,  opal_list_t);
    OBJ_CONSTRUCT(&req->file_sets,     opal_list_t);

    req->num_mv = 0;

    req->is_done = NULL;
    req->is_active = NULL;

    req->exit_status = NULL;

    req->movement_type = ORTE_FILEM_MOVE_TYPE_UNKNOWN;
}

void orte_filem_base_destruct( orte_filem_base_request_t *req) {
    opal_list_item_t* item = NULL;

    while( NULL != (item = opal_list_remove_first(&req->process_sets)) ) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&req->process_sets);

    while( NULL != (item = opal_list_remove_first(&req->file_sets)) ) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&req->file_sets);

    req->num_mv = 0;

    if( NULL != req->is_done ) {
        free(req->is_done);
        req->is_done = NULL;
    }

    if( NULL != req->is_active ) {
        free(req->is_active);
        req->is_active = NULL;
    }

    if( NULL != req->exit_status ) {
        free(req->exit_status);
        req->exit_status = NULL;
    }

    req->movement_type = ORTE_FILEM_MOVE_TYPE_UNKNOWN;
}

/***********************
 * None component stuff
 ************************/
int orte_filem_base_none_open(void)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_close(void)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_query(mca_base_module_t **module, int *priority)
{
    *module = NULL;
    *priority = 0;

    return OPAL_SUCCESS;
}

int orte_filem_base_module_init(void)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_module_finalize(void)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_put(orte_filem_base_request_t *request )
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_put_nb(orte_filem_base_request_t *request )
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_get(orte_filem_base_request_t *request)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_get_nb(orte_filem_base_request_t *request)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_rm(orte_filem_base_request_t *request)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_rm_nb(orte_filem_base_request_t *request)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_wait(orte_filem_base_request_t *request)
{
    return ORTE_SUCCESS;
}

int orte_filem_base_none_wait_all(opal_list_t *request_list)
{
    return ORTE_SUCCESS;
}

/********************
 * Utility functions
 ********************/
int orte_filem_base_get_proc_node_name(orte_process_name_t *proc, char **machine_name) {
    int ret;
    orte_std_cntr_t count;
    opal_buffer_t request, answer;
    orte_filem_cmd_flag_t command=ORTE_FILEM_GET_PROC_NODE_NAME_CMD;

    /* set default answer */
    *machine_name = NULL;

    if (ORTE_PROC_IS_HNP) {
        /* if I am the HNP, then all the data structures are local to me - no
         * need to send messages around to get the info
         */
        orte_job_t *jdata;
        orte_proc_t **procs;

        /* get the job data object for this proc */
        if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        /* get the proc object for it */
        procs = (orte_proc_t**)jdata->procs->addr;
        if (NULL == procs[proc->vpid] || NULL == procs[proc->vpid]->node) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        *machine_name = strdup(procs[proc->vpid]->node->name);
        return ORTE_SUCCESS;
    }
    
    /* if I am not the HNP, then I have to send a request to the HNP
     * for the information
     */
    OBJ_CONSTRUCT(&request, opal_buffer_t);
    OBJ_CONSTRUCT(&answer, opal_buffer_t);
 
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&request, &command, 1, ORTE_FILEM_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    if (ORTE_SUCCESS != (ret = opal_dss.pack(&request, proc, 1, ORTE_NAME))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    
    if (0 > (ret = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &request, ORTE_RML_TAG_FILEM_BASE, 0))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }

    /* wait for answer */
    if (0 > (ret = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &answer, ORTE_RML_TAG_FILEM_BASE_RESP, 0))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }
    
    /* unpack the machine name */
    count = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&answer, machine_name, &count, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        goto CLEANUP;
    }

 CLEANUP:
    OBJ_DESTRUCT(&answer);
    OBJ_DESTRUCT(&request);

    return ret;
}


/*
 * This function is paired with the filem_base_process_get_remote_path_cmd() function on the remote machine
 */
int orte_filem_base_get_remote_path(char **remote_ref, orte_process_name_t *peer, int *flag) {
    int ret, exit_status = ORTE_SUCCESS;
    char *tmp_ref = NULL;
    orte_std_cntr_t n;
    opal_buffer_t request, answer;
    int tmp_flag;
    orte_filem_cmd_flag_t command=ORTE_FILEM_GET_REMOTE_PATH_CMD;

    /*
     * Ask for remote file information from the HNP
     */
    OBJ_CONSTRUCT(&request, opal_buffer_t);
    OBJ_CONSTRUCT(&answer, opal_buffer_t);

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&request, &command, 1, ORTE_FILEM_CMD))) {
        ORTE_ERROR_LOG(ret);
        goto cleanup;
    }

    if (ORTE_SUCCESS != (ret = opal_dss.pack(&request, remote_ref, 1, OPAL_STRING))) {
        exit_status = ret;
        goto cleanup;
    }

    if (0 > (ret = orte_rml.send_buffer(peer, &request, ORTE_RML_TAG_FILEM_BASE, 0))) {
        exit_status = ret;
        goto cleanup;
    }
    
    /*
     * Get the response
     */
    if( 0 > (ret = orte_rml.recv_buffer(peer, &answer, ORTE_RML_TAG_FILEM_BASE_RESP, 0)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * The absolute path for the remote file
     */
    n = 1;
    if ( ORTE_SUCCESS != (ret = opal_dss.unpack(&answer, &tmp_ref, &n, OPAL_STRING)) ) {
        exit_status = ret;
        goto cleanup;
    }

    /*
     * The file type on the remote machine
     */
    n = 1;
    if ( ORTE_SUCCESS != (ret = opal_dss.unpack(&answer, &tmp_flag, &n, OPAL_INT)) ) {
        exit_status = ret;
        goto cleanup;
    }

    if(NULL != *remote_ref)
        free(*remote_ref);
    
    *remote_ref = strdup(tmp_ref);
    *flag = tmp_flag;

 cleanup:
    OBJ_DESTRUCT(&answer);
    OBJ_DESTRUCT(&request);

    if( NULL != tmp_ref)
        free(tmp_ref);

    return exit_status;
}

int orte_filem_base_prepare_request(orte_filem_base_request_t *request, int move_type)
{
    int num_reqs = 0, i = 0;

    if( ORTE_FILEM_MOVE_TYPE_RM == move_type ) {
        num_reqs = opal_list_get_size(&request->process_sets);
    }
    else {
        num_reqs = opal_list_get_size(&request->process_sets) * opal_list_get_size(&request->file_sets);
    }

    if( 0 >= num_reqs ) {
        return ORTE_ERROR;
    }
    else {
        if( NULL != request->is_done ) {
            free(request->is_done);
            request->is_done = NULL;
        }

        if( NULL != request->is_active ) {
            free(request->is_active);
            request->is_active = NULL;
        }

        if( NULL != request->exit_status ) {
            free(request->exit_status);
            request->exit_status = NULL;
        }

        request->num_mv      = num_reqs;
        request->is_done     = (bool*) malloc(sizeof(bool) * num_reqs);
        request->is_active   = (bool*) malloc(sizeof(bool) * num_reqs);
        request->exit_status = (int32_t*) malloc(sizeof(int32_t) * num_reqs);
        for( i = 0; i < num_reqs; ++i) {
            request->is_done[i]     = false;
            request->is_active[i]   = false;
            request->exit_status[i] = 0;
        }
    }

    request->movement_type = move_type;

    return ORTE_SUCCESS;
}
