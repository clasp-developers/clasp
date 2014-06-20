/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/plm/base/base.h"

#include "orte/mca/routed/base/base.h"

static bool sync_recvd;

static void report_sync(int status, orte_process_name_t* sender,
                        opal_buffer_t *buffer,
                        orte_rml_tag_t tag, void *cbdata)
{
    /* just copy the payload to the sync_buf */
    opal_dss.copy_payload(orte_process_info.sync_buf, buffer);
    /* flag as complete */
    sync_recvd = true;
}

int orte_routed_base_register_sync(bool setup)
{
    opal_buffer_t buffer;
    int rc;
    orte_daemon_cmd_flag_t command=ORTE_DAEMON_SYNC_BY_PROC;
    char *rml_uri;
    
    /* we need to get the oob to establish
     * the connection - the oob will leave the connection "alive"
     * thereafter so we can communicate readily
     */
    
    OBJ_CONSTRUCT(&buffer, opal_buffer_t);
    
    /* if we are setting up, tell the daemon to send back a nidmap */
    if (setup) {
        command = ORTE_DAEMON_SYNC_WANT_NIDMAP;
    }


    /* tell the daemon to sync */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
    }
    
    /* add our contact info to the buffer so the daemon can explicitly
     * store it
     */
    rml_uri = orte_rml.get_contact_info();
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buffer, &rml_uri, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        free(rml_uri);
        return rc;
    }
    if (NULL != rml_uri) free(rml_uri);
    
    /* send the sync command to our daemon */
    if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_DAEMON, &buffer, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        return rc;
    }
    OBJ_DESTRUCT(&buffer);
    
    /* get the ack - need this to ensure that the sync communication
     * gets serviced by the event library on the orted prior to the
     * process exiting
     */
    sync_recvd = false;
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_SYNC,
                                 ORTE_RML_NON_PERSISTENT, report_sync, NULL);
    if (rc != ORTE_SUCCESS && rc != ORTE_ERR_NOT_IMPLEMENTED) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    ORTE_PROGRESSED_WAIT(sync_recvd, 0, 1);
    
    return ORTE_SUCCESS;
}

int orte_routed_base_process_callback(orte_jobid_t job, opal_buffer_t *buffer)
{
    orte_proc_t *proc;
    orte_job_t *jdata;
    orte_std_cntr_t cnt;
    char *rml_uri;
    orte_vpid_t vpid;
    int rc;
    
    /* lookup the job object for this process */
    if (NULL == (jdata = orte_get_job_data_object(job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* unpack the data for each entry */
    cnt = 1;
    while (ORTE_SUCCESS == (rc = opal_dss.unpack(buffer, &vpid, &cnt, ORTE_VPID))) {
        
        cnt = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buffer, &rml_uri, &cnt, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            continue;
            
        }
        OPAL_OUTPUT_VERBOSE((2, orte_routed_base_output,
                             "%s routed_binomial:callback got uri %s for job %s rank %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             (NULL == rml_uri) ? "NULL" : rml_uri,
                             ORTE_JOBID_PRINT(job), ORTE_VPID_PRINT(vpid)));
        
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, vpid))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            continue;
        }
        
        if (rml_uri == NULL) {
            /* if the rml_uri is NULL, then that means this process
             * terminated without calling orte_init. However, the only
             * reason we would be getting called here is if other
             * processes local to that daemon -did- call orte_init.
             * This is considered an "abnormal termination" mode per
             * community discussion, and must generate a corresponding
             * response, so declare the proc abnormally terminated
             */
            proc->state = ORTE_PROC_STATE_TERM_WO_SYNC;
            /* increment the number of procs that have terminated */
            jdata->num_terminated++;
            /* let the normal code path declare the job aborted */
            orte_plm_base_check_job_completed(jdata);
            continue;
        }
        
        /* update the record */
        proc->rml_uri = strdup(rml_uri);
        free(rml_uri);
        
        /* update the proc state */
        if (proc->state < ORTE_PROC_STATE_RUNNING) {
            proc->state = ORTE_PROC_STATE_RUNNING;
        }
        
        ++jdata->num_reported;
        cnt = 1;
    }
    if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }    
    
    /* if all procs have reported, update our job state */
    if (jdata->num_reported == jdata->num_procs) {
        /* update the job state */
        if (jdata->state < ORTE_JOB_STATE_RUNNING) {
            jdata->state = ORTE_JOB_STATE_RUNNING;
        }
    }
    
    return ORTE_SUCCESS;    
}
