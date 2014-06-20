/* -*- C -*-
 *
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
 * Copyright (c)      2011 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/dss/dss.h"

#include "orte/constants.h"
#include "orte/types.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/ras/base/base.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/plm/plm_types.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/base.h"

static bool recv_issued=false;
static opal_mutex_t lock;
static opal_list_t recvs;
static opal_event_t ready;
static int ready_fd[2];
static bool processing;

static void process_msg(int fd, short event, void *data);

int orte_plm_base_comm_start(void)
{
    int rc;

    if (recv_issued) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive start comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    processing = false;
    OBJ_CONSTRUCT(&lock, opal_mutex_t);
    OBJ_CONSTRUCT(&recvs, opal_list_t);
#ifndef __WINDOWS__
    pipe(ready_fd);
#else
    if (evutil_socketpair(AF_UNIX, SOCK_STREAM, 0, ready_fd) == -1) {
        return ORTE_ERROR;
    }
#endif

    opal_event_set(&ready, ready_fd[0], OPAL_EV_READ, process_msg, NULL);
    opal_event_add(&ready, 0);
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_PLM,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_plm_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = true;
    
    return rc;
}


int orte_plm_base_comm_stop(void)
{
    if (!recv_issued) {
        return ORTE_SUCCESS;
    }
    
    OBJ_DESTRUCT(&recvs);
    opal_event_del(&ready);
#ifndef __WINDOWS__
    close(ready_fd[0]);
#else
    closesocket(ready_fd[0]);
#endif
    processing = false;
    OBJ_DESTRUCT(&lock);
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive stop comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_PLM);
    recv_issued = false;
    
    return ORTE_SUCCESS;
}


/* process incoming messages in order of receipt */
static void process_msg(int fd, short event, void *data)
{
    orte_msg_packet_t *msgpkt;
    orte_plm_cmd_flag_t command;
    orte_std_cntr_t count;
    orte_jobid_t job;
    orte_job_t *jdata, *parent;
    opal_buffer_t answer;
    orte_vpid_t vpid;
    orte_proc_t *proc;
    orte_proc_state_t state;
    orte_exit_code_t exit_code;
    int rc=ORTE_SUCCESS, ret;
    struct timeval beat;
    orte_app_context_t *app, *child_app;
    opal_list_item_t *item;
    int dump[128];
    
    OPAL_THREAD_LOCK(&lock);
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive processing msg",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* tag that we are processing the list */
    processing = true;

    /* clear the file descriptor to stop the event from refiring */
#ifndef __WINDOWS__
    read(fd, &dump, sizeof(dump));
#else
    recv(fd, (char *) &dump, sizeof(dump), 0);
#endif
    
    while (NULL != (item = opal_list_remove_first(&recvs))) {
        msgpkt = (orte_msg_packet_t*)item;

        /* setup a default response */
        OBJ_CONSTRUCT(&answer, opal_buffer_t);
        job = ORTE_JOBID_INVALID;
        
        count = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &command, &count, ORTE_PLM_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        
        switch (command) {
            case ORTE_PLM_LAUNCH_JOB_CMD:
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:receive job launch command",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                
                /* unpack the job object */
                count = 1;
                if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &jdata, &count, ORTE_JOB))) {
                    ORTE_ERROR_LOG(rc);
                    goto ANSWER_LAUNCH;
                }
                
                /* if is a LOCAL slave cmd */
                if (jdata->controls & ORTE_JOB_CONTROL_LOCAL_SLAVE) {
                    /* In this case, I cannot lookup job info. All I do is pass
                     * this along to the local launcher, IF it is available
                     */
                    if (NULL == orte_plm.spawn) {
                        /* can't do this operation */
                        ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
                        rc = ORTE_ERR_NOT_SUPPORTED;
                        goto ANSWER_LAUNCH;
                    }
                    if (ORTE_SUCCESS != (rc = orte_plm.spawn(jdata))) {
                        ORTE_ERROR_LOG(rc);
                        goto ANSWER_LAUNCH;
                    }
                    job = jdata->jobid;
                } else {  /* this is a GLOBAL launch cmd */
                    /* get the parent's job object */
                    if (NULL == (parent = orte_get_job_data_object(msgpkt->sender.jobid))) {
                        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                        goto ANSWER_LAUNCH;
                    }
                    
                    /* if the prefix was set in the parent's job, we need to transfer
                     * that prefix to the child's app_context so any further launch of
                     * orteds can find the correct binary. There always has to be at
                     * least one app_context in both parent and child, so we don't
                     * need to check that here. However, be sure not to overwrite
                     * the prefix if the user already provided it!
                     */
                    app = (orte_app_context_t*)opal_pointer_array_get_item(parent->apps, 0);
                    child_app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0);
                    if (NULL != app->prefix_dir &&
                        NULL == child_app->prefix_dir) {
                        child_app->prefix_dir = strdup(app->prefix_dir);
                    }
                    
                    /* process any add-hostfile and add-host options that were provided */
                    if (ORTE_SUCCESS != (rc = orte_ras_base_add_hosts(jdata))) {
                        ORTE_ERROR_LOG(rc);
                        goto ANSWER_LAUNCH;
                    }
                    
                    if( NULL == parent->bookmark ) {
                        /* find the sender's node in the job map */
                        if (NULL != (proc = (orte_proc_t*)opal_pointer_array_get_item(parent->procs, msgpkt->sender.vpid))) {
                            /* set the bookmark so the child starts from that place - this means
                             * that the first child process could be co-located with the proc
                             * that called comm_spawn, assuming slots remain on that node. Otherwise,
                             * the procs will start on the next available node
                             */
                            jdata->bookmark = proc->node;
                        }
                    } else {
                        jdata->bookmark = parent->bookmark;
                    }
                    
                    /* launch it */
                    if (ORTE_SUCCESS != (rc = orte_plm.spawn(jdata))) {
                        ORTE_ERROR_LOG(rc);
                        goto ANSWER_LAUNCH;
                    }
                    job = jdata->jobid;

		    /* output debugger proctable, if requested */
		    if (orte_debugger_dump_proctable) {
		      char *output;
		      opal_dss.print(&output, NULL, jdata->map, ORTE_JOB_MAP);
		      if (orte_xml_output) {
			fprintf(orte_xml_fp, "%s\n", output);
			fflush(orte_xml_fp);
		      } else {
			opal_output(orte_clean_output, "%s", output);
		      }
		      free(output);
		    }

                    /* return the favor so that any repetitive comm_spawns track each other */
                    parent->bookmark = jdata->bookmark;
                }
                
                /* if the child is an ORTE job, wait for the procs to report they are alive */
                if (!(jdata->controls & ORTE_JOB_CONTROL_NON_ORTE_JOB)) {
                    ORTE_PROGRESSED_WAIT(false, jdata->num_reported, jdata->num_procs);
                }
                
            ANSWER_LAUNCH:
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:receive job %s launched",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_JOBID_PRINT(job)));
                
                /* pack the jobid to be returned */
                if (ORTE_SUCCESS != (ret = opal_dss.pack(&answer, &job, 1, ORTE_JOBID))) {
                    ORTE_ERROR_LOG(ret);
                }
                
                /* send the response back to the sender */
                if (0 > (ret = orte_rml.send_buffer(&msgpkt->sender, &answer, ORTE_RML_TAG_PLM_PROXY, 0))) {
                    ORTE_ERROR_LOG(ret);
                }
                break;
                
            case ORTE_PLM_UPDATE_PROC_STATE:
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:receive update proc state command",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                count = 1;
                jdata = NULL;
                while (ORTE_SUCCESS == (rc = opal_dss.unpack(msgpkt->buffer, &job, &count, ORTE_JOBID))) {
                    
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                         "%s plm:base:receive got update_proc_state for job %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_JOBID_PRINT(job)));
                    
                    /* lookup the job object */
                    if (NULL == (jdata = orte_get_job_data_object(job))) {
                        /* this job may already have been removed from the array, so just cleanly
                         * ignore this request
                         */
                        goto CLEANUP;
                    }
                    count = 1;
                    while (ORTE_SUCCESS == (rc = opal_dss.unpack(msgpkt->buffer, &vpid, &count, ORTE_VPID))) {
                        if (ORTE_VPID_INVALID == vpid) {
                            /* flag indicates that this job is complete - move on */
                            break;
                        }
                        /* unpack the state */
                        count = 1;
                        if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &state, &count, ORTE_PROC_STATE))) {
                            ORTE_ERROR_LOG(rc);
                            goto CLEANUP;
                        }
                        /* unpack the exit code */
                        count = 1;
                        if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &exit_code, &count, ORTE_EXIT_CODE))) {
                            ORTE_ERROR_LOG(rc);
                            goto CLEANUP;
                        }
                        
                        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                             "%s plm:base:receive got update_proc_state for vpid %lu state %x exit_code %d",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             (unsigned long)vpid, (unsigned int)state, (int)exit_code));
                        
                        /* retrieve the proc object */
                        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, vpid))) {
                            /* this proc is no longer in table - skip it */
                            OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                                 "%s plm:base:receive proc %s is not in proc table",
                                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                                 ORTE_VPID_PRINT(vpid)));
                            continue;
                        }
                        
                        OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                             "%s plm:base:receive updating state for proc %s current state %x new state %x",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             ORTE_NAME_PRINT(&proc->name),
                                             (unsigned int)proc->state, (unsigned int)state));
                        
                        /* update the termination counter IFF the state is changing to something
                         * indicating terminated
                         */
                        if (ORTE_PROC_STATE_UNTERMINATED < state &&
                            ORTE_PROC_STATE_UNTERMINATED > proc->state) {
                            ++jdata->num_terminated;
                        }
                        /* update the data */
                        proc->state = state;
                        proc->exit_code = exit_code;
                        
                        /* update orte's exit status if it is non-zero */
                        ORTE_UPDATE_EXIT_STATUS(exit_code);
                        
                    }
                    count = 1;
                }
                if (ORTE_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
                    ORTE_ERROR_LOG(rc);
                } else {
                    rc = ORTE_SUCCESS;
                }
                /* NOTE: jdata CAN BE NULL. This is caused by an orted
                 * being ordered to kill all its procs, but there are no
                 * procs left alive on that node. This can happen, for example,
                 * when a proc aborts somewhere, but the procs on this node
                 * have completed.
                 * So check job has to know how to handle a NULL pointer
                 */
                orte_plm_base_check_job_completed(jdata);
                break;
                
            case ORTE_PLM_HEARTBEAT_CMD:
                OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                     "%s plm:base:receive got heartbeat from %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&msgpkt->sender)));
                /* lookup the daemon object */
                if (NULL == (jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
                    /* this job can not possibly have been removed, so this is an error */
                    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                    goto CLEANUP;
                }
                gettimeofday(&beat, NULL);
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, msgpkt->sender.vpid))) {
                    /* this proc is no longer in table - skip it */
                    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                                         "%s plm:base:receive daemon %s is not in proc table",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_VPID_PRINT(msgpkt->sender.vpid)));
                    break;
                }
                proc->beat = beat.tv_sec; 
                break;
                
            default:
                ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
                rc = ORTE_ERR_VALUE_OUT_OF_BOUNDS;
                break;
        }
        
    CLEANUP:
        /* release the message */
        OBJ_RELEASE(msgpkt);
        OBJ_DESTRUCT(&answer);
        if (ORTE_SUCCESS != rc) {
            goto DEPART;
        }
    }
        
    /* reset the event */
    processing = false;
    opal_event_add(&ready, 0);
    
DEPART:
    /* release the thread */
    OPAL_THREAD_UNLOCK(&lock);
    
    /* see if an error occurred - if so, wakeup the HNP so we can exit */
    if (ORTE_PROC_IS_HNP && ORTE_SUCCESS != rc) {
        orte_trigger_event(&orte_exit);
    }

}

/*
 * handle message from proxies
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

void orte_plm_base_recv(int status, orte_process_name_t* sender,
                        opal_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:base:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));

    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_PROCESS_MESSAGE(&recvs, &lock, processing, ready_fd[1], true, sender, &buffer);
    
    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_PLM,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_plm_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return;
}

/* where HNP messages come */
void orte_plm_base_receive_process_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;

    ORTE_PROCESS_MESSAGE(&recvs, &lock, processing, ready_fd[1], false, &mev->sender, &mev->buffer);
    OBJ_RELEASE(mev);
}
