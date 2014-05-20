/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "orte/types.h"
#include "orte/constants.h"

#include <stdio.h>
#include <string.h>

#include "opal/util/output.h"
#include "opal/threads/tsd.h"

#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/util/comm/comm.h"

/* internal communication handshake */
/* quick timeout loop */
static bool timer_fired;
static opal_buffer_t answer;
static opal_event_t *quicktime=NULL;
static int error_exit;

static void quicktime_cb(int fd, short event, void *cbdata)
{
    if (NULL != quicktime) {
        free(quicktime);
        quicktime = NULL;
    }
    error_exit = ORTE_ERR_SILENT;
    /* declare it fired */
    timer_fired = true;
}

static void send_cbfunc(int status, orte_process_name_t* sender,
                        opal_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    /* cancel the timer */
    if (NULL != quicktime) {
        opal_evtimer_del(quicktime);
        free(quicktime);
        quicktime = NULL;
    }
    /* declare the work done */
    timer_fired = true;
}

static void recv_info(int status, orte_process_name_t* sender,
                      opal_buffer_t* buffer, orte_rml_tag_t tag,
                      void* cbdata)
{
    int rc;
    
    /* cancel the timer */
    if (NULL != quicktime) {
        opal_evtimer_del(quicktime);
        free(quicktime);
        quicktime = NULL;
    }
    /* xfer the answer */
    if (ORTE_SUCCESS != (rc = opal_dss.copy_payload(&answer, buffer))) {
        ORTE_ERROR_LOG(rc);
    }
    /* declare the work done */
    timer_fired = true;
}


/* name of attached tool */
static orte_process_name_t tool;
static bool tool_connected = false;

/* connect a tool to us so we can send reports */
int orte_util_comm_connect_tool(char *uri)
{
    int rc;
    
    /* set the contact info into the comm hash tables*/
    if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(uri))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    
    /* extract the tool's name and store it */
    if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(uri, &tool, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* set the route to be direct */
    if (ORTE_SUCCESS != (rc = orte_routed.update_route(&tool, &tool))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tool_connected = true;
    return ORTE_SUCCESS;
}

/* whether we are in step mode */
static bool step=false;

/* report an event to a connected tool */
int orte_util_comm_report_event(orte_comm_event_t ev)
{
    int rc, i;
    opal_buffer_t buf;
    orte_node_t *node;
    
    /* if nothing is connected, ignore this */
    if (!tool_connected) {
        return ORTE_SUCCESS;
    }
    
    /* init a buffer for the data */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    /* flag the type of event */
    opal_dss.pack(&buf, &ev, 1, ORTE_COMM_EVENT);

    switch (ev) {
        case ORTE_COMM_EVENT_ALLOCATE:
            /* loop through nodes, storing just node names */
            for (i=0; i < orte_node_pool->size; i++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                    continue;
                }
                opal_dss.pack(&buf, &node->name, 1, OPAL_STRING);
            }
            break;
        
        case ORTE_COMM_EVENT_MAP:
            break;
        
        case ORTE_COMM_EVENT_LAUNCH:
            break;
        
        default:
            ORTE_ERROR_LOG(ORTE_ERROR);
            OBJ_DESTRUCT(&buf);
            return ORTE_ERROR;
            break;
    }
    
    /* do the send */
    if (0 > (rc = orte_rml.send_buffer(&tool, &buf, ORTE_RML_TAG_TOOL, 0))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    
    if (step) {
        /* the caller wants to wait until an ack is received -
         * define a max time to wait for an answer
         */
        OBJ_CONSTRUCT(&answer, opal_buffer_t);
        timer_fired = false;
        error_exit = ORTE_SUCCESS;
        ORTE_DETECT_TIMEOUT(&quicktime, 100, 1000, 100000, quicktime_cb);
        
        /* get the answer */
        if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                           ORTE_RML_TAG_TOOL,
                                                           ORTE_RML_NON_PERSISTENT,
                                                           recv_info,
                                                           NULL))) {
            /* cancel the timer */
            if (NULL != quicktime) {
                opal_evtimer_del(quicktime);
                free(quicktime);
                quicktime = NULL;
            }
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&answer);
            return rc;
        }
        
        ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);
        
        /* cleanup */
        OBJ_DESTRUCT(&answer);

        if (ORTE_SUCCESS != error_exit) {
            return error_exit;
        }
    }
    
    return ORTE_SUCCESS;
}


int orte_util_comm_query_job_info(const orte_process_name_t *hnp, orte_jobid_t job,
                                  int *num_jobs, orte_job_t ***job_info_array)
{
    int ret;
    int32_t cnt, cnt_jobs, n;
    opal_buffer_t *cmd;
    orte_daemon_cmd_flag_t command = ORTE_DAEMON_REPORT_JOB_INFO_CMD;
    orte_job_t **job_info;

    /* set default response */
    *num_jobs = 0;
    *job_info_array = NULL;
    
    /* send query to HNP */
    cmd = OBJ_NEW(opal_buffer_t);
    if (ORTE_SUCCESS != (ret = opal_dss.pack(cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cmd);
        return ret;
    }
    if (ORTE_SUCCESS != (ret = opal_dss.pack(cmd, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cmd);
        return ret;
    }
    /* define a max time to wait for send to complete */
    timer_fired = false;
    error_exit = ORTE_SUCCESS;
    ORTE_DETECT_TIMEOUT(&quicktime, 100, 1000, 100000, quicktime_cb);
    
    /* do the send */
    if (0 > (ret = orte_rml.send_buffer_nb((orte_process_name_t*)hnp, cmd, ORTE_RML_TAG_DAEMON, 0,
                                           send_cbfunc, NULL))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cmd);
        return ret;
    }
    
    /* wait for send to complete */
    ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);

    /* release the buffer */
    OBJ_RELEASE(cmd);
    
    /* did it succeed? */
    if (ORTE_SUCCESS != error_exit) {
        return error_exit;
    }
    
    /* setup for answer */
    OBJ_CONSTRUCT(&answer, opal_buffer_t);

    /* define a max time to wait for an answer */
    timer_fired = false;
    error_exit = ORTE_SUCCESS;
    ORTE_DETECT_TIMEOUT(&quicktime, 100, 1000, 100000, quicktime_cb);
    
    /* get the answer */
    if (ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_TOOL,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      recv_info,
                                                      NULL))) {
        /* cancel the timer */
        if (NULL != quicktime) {
            opal_evtimer_del(quicktime);
            free(quicktime);
            quicktime = NULL;
        }
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&answer);
        return ret;
    }
    
    ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);

    if (ORTE_SUCCESS != error_exit) {
        OBJ_DESTRUCT(&answer);
        return error_exit;
    }
    
    cnt = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&answer, &cnt_jobs, &cnt, OPAL_INT32))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&answer);
        return ret;
    }
    
    /* allocate the required memory */
    if (0 < cnt_jobs) {
        job_info = (orte_job_t**)malloc(cnt_jobs * sizeof(orte_job_t*));
        /* unpack the job data */
        for (n=0; n < cnt_jobs; n++) {
            cnt = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(&answer, &job_info[n], &cnt, ORTE_JOB))) {
                ORTE_ERROR_LOG(ret);
                OBJ_DESTRUCT(&answer);
                free(job_info);
                return ret;
            }
        }
        *job_info_array = job_info;
        *num_jobs = cnt_jobs;
    }
    OBJ_DESTRUCT(&answer);
    
    return ORTE_SUCCESS;
}

int orte_util_comm_query_node_info(const orte_process_name_t *hnp, char *node,
                                   int *num_nodes, orte_node_t ***node_info_array)
{
    int ret;
    int32_t cnt, cnt_nodes, n;
    opal_buffer_t *cmd;
    orte_daemon_cmd_flag_t command = ORTE_DAEMON_REPORT_NODE_INFO_CMD;
    orte_node_t **node_info;
    
    /* set default response */
    *num_nodes = 0;
    *node_info_array = NULL;
    
    /* query the HNP for node info */
    cmd = OBJ_NEW(opal_buffer_t);
    if (ORTE_SUCCESS != (ret = opal_dss.pack(cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cmd);
        return ret;
    }
    if (ORTE_SUCCESS != (ret = opal_dss.pack(cmd, &node, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cmd);
        return ret;
    }
    /* define a max time to wait for send to complete */
    timer_fired = false;
    error_exit = ORTE_SUCCESS;
    ORTE_DETECT_TIMEOUT(&quicktime, 100, 1000, 100000, quicktime_cb);
    
    /* do the send */
    if (0 > (ret = orte_rml.send_buffer_nb((orte_process_name_t*)hnp, cmd, ORTE_RML_TAG_DAEMON, 0,
                                           send_cbfunc, NULL))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cmd);
        return ret;
    }
    
    /* wait for send to complete */
    ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);
    
    /* release the buffer */
    OBJ_RELEASE(cmd);
    
    /* did it succeed? */
    if (ORTE_SUCCESS != error_exit) {
        return error_exit;
    }

    /* define a max time to wait for an answer */
    timer_fired = false;
    error_exit = ORTE_SUCCESS;
    ORTE_DETECT_TIMEOUT(&quicktime, 10, 1000, 10000, quicktime_cb);
    
    /* get the answer */
    OBJ_CONSTRUCT(&answer, opal_buffer_t);
    if (ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_TOOL,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      recv_info,
                                                      NULL))) {
        /* cancel the timer */
        if (NULL != quicktime) {
            opal_evtimer_del(quicktime);
            free(quicktime);
            quicktime = NULL;
        }
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&answer);
        return ret;
    }
    
    ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);
    
    if (ORTE_SUCCESS != error_exit) {
        OBJ_DESTRUCT(&answer);
        return error_exit;
    }
    
    cnt = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&answer, &cnt_nodes, &cnt, OPAL_INT32))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&answer);
        return ret;
    }
    
    /* allocate the required memory */
    if (0 < cnt_nodes) {
        node_info = (orte_node_t**)malloc(cnt_nodes * sizeof(orte_node_t*));
        /* unpack the node data */
        for (n=0; n < cnt_nodes; n++) {
            cnt = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(&answer, &node_info[n], &cnt, ORTE_NODE))) {
                ORTE_ERROR_LOG(ret);
                OBJ_DESTRUCT(&answer);
                free(node_info);
                return ret;
            }
        }
        *node_info_array = node_info;
        *num_nodes = cnt_nodes;
    }
    OBJ_DESTRUCT(&answer);
    
    return ORTE_SUCCESS;
}

int orte_util_comm_query_proc_info(const orte_process_name_t *hnp, orte_jobid_t job, orte_vpid_t vpid,
                                   int *num_procs, orte_proc_t ***proc_info_array)
{
    int ret;
    int32_t cnt, cnt_procs, n;
    opal_buffer_t *cmd;
    orte_daemon_cmd_flag_t command = ORTE_DAEMON_REPORT_PROC_INFO_CMD;
    orte_proc_t **proc_info;

    /* set default response */
    *num_procs = 0;
    *proc_info_array = NULL;
    
    /* query the HNP for info on the procs in this job */
    cmd = OBJ_NEW(opal_buffer_t);
    if (ORTE_SUCCESS != (ret = opal_dss.pack(cmd, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cmd);
        return ret;
    }
    if (ORTE_SUCCESS != (ret = opal_dss.pack(cmd, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cmd);
        return ret;
    }
    if (ORTE_SUCCESS != (ret = opal_dss.pack(cmd, &vpid, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cmd);
        return ret;
    }
    /* define a max time to wait for send to complete */
    timer_fired = false;
    error_exit = ORTE_SUCCESS;
    ORTE_DETECT_TIMEOUT(&quicktime, 100, 1000, 100000, quicktime_cb);
    
    /* do the send */
    if (0 > (ret = orte_rml.send_buffer_nb((orte_process_name_t*)hnp, cmd, ORTE_RML_TAG_DAEMON, 0,
                                           send_cbfunc, NULL))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(cmd);
        return ret;
    }
    
    /* wait for send to complete */
    ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);
    
    /* release the buffer */
    OBJ_RELEASE(cmd);
    
    /* did it succeed? */
    if (ORTE_SUCCESS != error_exit) {
        return error_exit;
    }

    /* define a max time to wait for an answer */
    timer_fired = false;
    error_exit = ORTE_SUCCESS;
    ORTE_DETECT_TIMEOUT(&quicktime, 10, 1000, 10000, quicktime_cb);
    
    /* get the answer */
    OBJ_CONSTRUCT(&answer, opal_buffer_t);
    if (ORTE_SUCCESS != (ret = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_TOOL,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      recv_info,
                                                      NULL))) {
        /* cancel the timer */
        if (NULL != quicktime) {
            opal_evtimer_del(quicktime);
            free(quicktime);
            quicktime = NULL;
        }
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&answer);
        return ret;
    }
    
    ORTE_PROGRESSED_WAIT(timer_fired, 0, 1);
    
    if (ORTE_SUCCESS != error_exit) {
        OBJ_DESTRUCT(&answer);
        return error_exit;
    }
    
    cnt = 1;
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(&answer, &cnt_procs, &cnt, OPAL_INT32))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&answer);
        return ret;
    }
    
    /* allocate the required memory */
    if (0 < cnt_procs) {
        proc_info = (orte_proc_t**)malloc(cnt_procs * sizeof(orte_proc_t*));
        /* unpack the procs */
        for (n=0; n < cnt_procs; n++) {
            cnt = 1;
            if (ORTE_SUCCESS != (ret = opal_dss.unpack(&answer, &proc_info[n], &cnt, ORTE_PROC))) {
                ORTE_ERROR_LOG(ret);
                OBJ_DESTRUCT(&answer);
                free(proc_info);
                return ret;
            }
        }
        *proc_info_array = proc_info;
        *num_procs = (int)cnt_procs;
    }
    OBJ_DESTRUCT(&answer);

    return ORTE_SUCCESS;
}

/* The spawn function cannot just call the plm.proxy since that won't
 * necessarily be open. Likewise, we can't just send the launch request
 * to the HNP's plm_receive as that function would return the response
 * to the plm_proxy tag! So we have to go another route to get this
 * request processed
 */
int orte_util_comm_spawn_job(const orte_process_name_t *hnp, orte_job_t *jdata)
{
    opal_buffer_t buf;
    orte_daemon_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                         "%s util_comm_spawn_job: requesting HNP %s spawn new job",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(hnp)));
    
    /* setup the buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* tell the HNP we are sending a launch request */
    command = ORTE_DAEMON_SPAWN_JOB_CMD;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* pack the jdata object */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &jdata, 1, ORTE_JOB))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
        
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                         "%s util_comm_spawn_job: sending spawn cmd to HNP %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(hnp)));
    
    /* tell the target HNP to launch the job */
    if (0 > (rc = orte_rml.send_buffer((orte_process_name_t*)hnp, &buf, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    OBJ_DESTRUCT(&buf);
    
    
    OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                         "%s util_comm_spawn_job: waiting for response",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* wait for the target's response */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (0 > (rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &buf, ORTE_RML_TAG_TOOL, 0))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* get the new jobid back in case the caller wants it */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &(jdata->jobid), &count, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    if (ORTE_JOBID_INVALID == jdata->jobid) {
        /* something went wrong on far end - go no further */
        rc = ORTE_ERR_FAILED_TO_START;
        goto CLEANUP;
    }
    
    /* good to go! */
    
CLEANUP:
    OBJ_DESTRUCT(&buf);
    
    return rc;
}


int orte_util_comm_terminate_job(const orte_process_name_t *hnp, orte_jobid_t job)
{
    opal_buffer_t buf;
    orte_daemon_cmd_flag_t command;
    orte_std_cntr_t count;
    int rc, ret = ORTE_ERROR;
    
    OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                         "%s util_comm_spawn_job: requesting HNP %s terminate job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(hnp),
                         ORTE_JOBID_PRINT(job)));
    
    /* setup the buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* tell the HNP we are sending a terminate request */
    command = ORTE_DAEMON_TERMINATE_JOB_CMD;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto CLEANUP;
    }
    
    /* pack the jobid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto CLEANUP;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                         "%s util_comm_spawn_job: sending terminate cmd to HNP %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(hnp)));
    
    /* tell the target HNP to terminate the job */
    if (0 > (rc = orte_rml.send_buffer((orte_process_name_t*)hnp, &buf, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto CLEANUP;
    }
    OBJ_DESTRUCT(&buf);
    
    
    OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                         "%s util_comm_terminate_job: waiting for response",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* wait for the target's response */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (0 > (rc = orte_rml.recv_buffer(ORTE_NAME_WILDCARD, &buf, ORTE_RML_TAG_TOOL, 0))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto CLEANUP;
    }
    
    /* get the status code */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(&buf, &ret, &count, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        ret = rc;
        goto CLEANUP;
    }
    
CLEANUP:
    OBJ_DESTRUCT(&buf);
    
    return ret;
}

int orte_util_comm_halt_vm(const orte_process_name_t *hnp)
{
    opal_buffer_t buf;
    orte_daemon_cmd_flag_t command;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_debug_output,
                         "%s util_comm_halt_vm: ordering HNP %s terminate",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(hnp)));
    
    /* setup the buffer */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    
    /* tell the HNP to die */
    command = ORTE_DAEMON_HALT_VM_CMD;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(&buf, &command, 1, ORTE_DAEMON_CMD))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    /* send the order */
    if (0 > (rc = orte_rml.send_buffer((orte_process_name_t*)hnp, &buf, ORTE_RML_TAG_DAEMON, 0))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    OBJ_DESTRUCT(&buf);
    
    /* don't bother waiting around */
CLEANUP:
    OBJ_DESTRUCT(&buf);
    
    return rc;
}

