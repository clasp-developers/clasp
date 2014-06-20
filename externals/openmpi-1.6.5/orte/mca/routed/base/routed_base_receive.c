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
#include "orte/constants.h"
#include "orte/types.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/routed/base/base.h"

static bool recv_issued=false;
static opal_mutex_t lock;
static opal_list_t recvs;
static opal_event_t ready;
static int ready_fd[2];
static bool processing;

static void process_msg(int fd, short event, void *data);

int orte_routed_base_comm_start(void)
{
    int rc;

    if (recv_issued) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s routed:base: Receive: Start command recv",
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
                                                      ORTE_RML_TAG_INIT_ROUTES,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_routed_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }

    recv_issued = true;
    
    return rc;
}


int orte_routed_base_comm_stop(void)
{
    if (!recv_issued) {
        return ORTE_SUCCESS;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s routed:base:receive stop comm",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    OBJ_DESTRUCT(&recvs);
    opal_event_del(&ready);
#ifndef __WINDOWS__
    close(ready_fd[0]);
#else
    closesocket(ready_fd[0]);
#endif
    processing = false;
    OBJ_DESTRUCT(&lock);
    
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_INIT_ROUTES);
    recv_issued = false;
    
    return ORTE_SUCCESS;
}

static void process_msg(int fd, short event, void *data)
{
    orte_msg_packet_t *msgpkt;
    orte_jobid_t job;
    int rc;
    orte_std_cntr_t cnt;
    opal_list_item_t *item;
    int dump[128];

    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s routed:base:receive processing msg",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    OPAL_THREAD_LOCK(&lock);
    
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
        
        /* unpack the jobid this is for */
        cnt=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(msgpkt->buffer, &job, &cnt, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(msgpkt);
            continue;
        }
        
        /* pass the remainder of the buffer to the active module's
         * init_routes API
         */
        if (ORTE_SUCCESS != (rc = orte_routed.init_routes(job, msgpkt->buffer))) {
            ORTE_ERROR_LOG(rc);
        }
        OBJ_RELEASE(msgpkt);
    }
    
    /* reset the event */
    processing = false;
    opal_event_add(&ready, 0);
    
    /* release the thread */
    OPAL_THREAD_UNLOCK(&lock);
}


/*
 * handle init routes requests from non-HNP-local procs
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */
void orte_routed_base_recv(int status, orte_process_name_t* sender,
                           opal_buffer_t* buffer, orte_rml_tag_t tag,
                           void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_routed_base_output,
                         "%s routed:base:receive got message from %s",
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
                                                      ORTE_RML_TAG_INIT_ROUTES,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_routed_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    return;
}

/* where HNP messages come */
void orte_routed_base_process_msg(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    
    ORTE_PROCESS_MESSAGE(&recvs, &lock, processing, ready_fd[1], false, &mev->sender, &mev->buffer);
    OBJ_RELEASE(mev);
}
