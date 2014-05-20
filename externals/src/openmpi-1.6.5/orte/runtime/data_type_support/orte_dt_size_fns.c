/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
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

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal/util/argv.h"

#include "opal/dss/dss.h"
#include "orte/constants.h"
#include "orte/types.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/data_type_support/orte_dt_support.h"


/*
 * STANDARD SIZE FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int orte_dt_std_size(size_t *size, void *src, opal_data_type_t type)
{
    switch(type) {
        case ORTE_STD_CNTR:
            *size = sizeof(orte_std_cntr_t);
            break;
            
        case ORTE_VPID:
            *size = sizeof(orte_vpid_t);
            break;
            
        case ORTE_JOBID:
            *size = sizeof(orte_jobid_t);
            break;
            
        case ORTE_NAME:
            *size = sizeof(orte_process_name_t);
            break;
            
#if !ORTE_DISABLE_FULL_SUPPORT
        case ORTE_DAEMON_CMD:
            *size = sizeof(orte_daemon_cmd_flag_t);
            break;
            
        case ORTE_PROC_STATE:
            *size = sizeof(orte_proc_state_t);
            break;
            
        case ORTE_JOB_STATE:
            *size = sizeof(orte_job_state_t);
            break;
            
        case ORTE_NODE_STATE:
            *size = sizeof(orte_node_state_t);
            break;
            
        case ORTE_EXIT_CODE:
            *size = sizeof(orte_exit_code_t);
            break;
            
        case ORTE_RML_TAG:
            *size = sizeof(orte_rml_tag_t);
            break;

        case ORTE_GRPCOMM_MODE:
            *size = sizeof(orte_grpcomm_mode_t);
            break;
            
        case ORTE_IOF_TAG:
            *size = sizeof(orte_iof_tag_t);
            break;
#endif
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_UNKNOWN_DATA_TYPE);
            return ORTE_ERR_UNKNOWN_DATA_TYPE;
    }
    
    return ORTE_SUCCESS;
}

#if !ORTE_DISABLE_FULL_SUPPORT
/*
 * JOB
 */
int orte_dt_size_job(size_t *size, orte_job_t *src, opal_data_type_t type)
{
    size_t sz;
    int32_t i;
    orte_app_context_t **apps;
    
    /* account for the object itself */
    *size = sizeof(orte_job_t);

    /* if src is NULL, then that's all we wanted */
    if (NULL == src) return ORTE_SUCCESS;

    apps = (orte_app_context_t**)src->apps->addr;
    for (i=0; i < src->num_apps; i++) {
        opal_dss.size(&sz, apps[i], ORTE_APP_CONTEXT);
        *size += sz;
    }
    
    opal_dss.size(&sz, src->map, ORTE_JOB_MAP);
    *size += sz;
    
    for (i=0; i < src->procs->size; i++) {
        if (NULL != src->procs->addr[i]) {
            orte_dt_size_proc(&sz, (orte_proc_t *) src->procs->addr[i], ORTE_PROC);
            *size += sz;
        }
    }

#if OPAL_ENABLE_FT_CR == 1
    if (NULL != src->ckpt_snapshot_ref) {
        *size += strlen(src->ckpt_snapshot_ref);
    }
    if (NULL != src->ckpt_snapshot_loc) {
        *size += strlen(src->ckpt_snapshot_loc);
    }
#endif

    return ORTE_SUCCESS;
}

/*
 * NODE
 */
int orte_dt_size_node(size_t *size, orte_node_t *src, opal_data_type_t type)
{
    size_t sz;
    int32_t i;
    
    /* account for the object itself */
    *size = sizeof(orte_node_t);
    
    /* if src is NULL, then that's all we wanted */
    if (NULL == src) return ORTE_SUCCESS;
    
    if (NULL != src->name) {
        *size += strlen(src->name);
    }
    
    if (NULL != src->username) {
        *size += strlen(src->username);
    }
    
    for (i=0; i < src->procs->size; i++) {
        if (NULL != src->procs->addr[i]) {
            orte_dt_size_proc(&sz, (orte_proc_t *) src->procs->addr[i], ORTE_PROC);
            *size += sz;
        }
    }
    
    return ORTE_SUCCESS;
}

/*
 * PROC
 */
int orte_dt_size_proc(size_t *size, orte_proc_t *src, opal_data_type_t type)
{
    /* account for the object itself */
    *size = sizeof(orte_proc_t);
    
    /* if src is NULL, then that's all we wanted */
    if (NULL == src) return ORTE_SUCCESS;
    
    if (NULL != src->slot_list) {
        *size += strlen(src->slot_list);
    }
    
#if OPAL_ENABLE_FT_CR == 1
    if (NULL != src->ckpt_snapshot_ref) {
        *size += strlen(src->ckpt_snapshot_ref);
    }
    if (NULL != src->ckpt_snapshot_loc) {
        *size += strlen(src->ckpt_snapshot_loc);
    }
#endif

    return ORTE_SUCCESS;
}

/*
 * APP CONTEXT
 */
int orte_dt_size_app_context(size_t *size, orte_app_context_t *src, opal_data_type_t type)
{
    /* account for the object itself */
    *size = sizeof(orte_app_context_t);
    
    /* if src is NULL, then that's all we wanted */
    if (NULL == src) return ORTE_SUCCESS;
    
    if (NULL != src->app) {
        *size += strlen(src->app);
    }
    
    *size += opal_argv_len(src->argv);

    *size += opal_argv_len(src->env);
    
    if (NULL != src->cwd) {
        *size += strlen(src->cwd);  /* working directory */
    }
    
    if (NULL != src->hostfile) {
        *size += strlen(src->hostfile);  /* hostfile name */
    }
    
    if (NULL != src->add_hostfile) {
        *size += strlen(src->add_hostfile);  /* add_hostfile name */
    }
    
    *size += opal_argv_len(src->add_host);

    *size += opal_argv_len(src->dash_host);
    
    if (NULL != src->prefix_dir) {
        *size += strlen(src->prefix_dir);
    }
    
    return ORTE_SUCCESS;
}

/*
 * JOB_MAP
 */
int orte_dt_size_map(size_t *size, orte_job_map_t *src, opal_data_type_t type)
{
    size_t sz;
    int32_t i;
    int rc;
    
    /* account for the object itself */
    *size = sizeof(orte_job_map_t);
    
    /* if src is NULL, then that's all we wanted */
    if (NULL == src) return ORTE_SUCCESS;
    
    for (i=0; i < src->nodes->size; i++) {
        if (NULL == src->nodes->addr[i]) {
            *size += sizeof(void*);
        } else {
            if (ORTE_SUCCESS != (rc = opal_dss.size(&sz, src->nodes->addr[i], ORTE_NODE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            *size += sz;
        }
    }
    return ORTE_SUCCESS;
}

#endif
