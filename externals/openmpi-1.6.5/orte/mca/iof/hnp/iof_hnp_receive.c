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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#else
#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#endif


#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"

#include "iof_hnp.h"


static void process_msg(int fd, short event, void *cbdata)
{
    orte_message_event_t *mev = (orte_message_event_t*)cbdata;
    orte_process_name_t origin;
    unsigned char data[ORTE_IOF_BASE_MSG_MAX];
    orte_iof_tag_t stream;
    int32_t count, numbytes;
    orte_iof_sink_t *sink;
    opal_list_item_t *item, *next;
    int rc;

    
    /* unpack the stream first as this may be flow control info */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &stream, &count, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    
    if (ORTE_IOF_XON & stream) {
        /* re-start the stdin read event */
        if (NULL != mca_iof_hnp_component.stdinev &&
            !orte_job_term_ordered &&
            !mca_iof_hnp_component.stdinev->active) {
            mca_iof_hnp_component.stdinev->active = true;
            opal_event_add(&(mca_iof_hnp_component.stdinev->ev), 0);
        }
        goto CLEAN_RETURN;
    } else if (ORTE_IOF_XOFF & stream) {
        /* stop the stdin read event */
        if (NULL != mca_iof_hnp_component.stdinev &&
            !mca_iof_hnp_component.stdinev->active) {
            opal_event_del(&(mca_iof_hnp_component.stdinev->ev));
            mca_iof_hnp_component.stdinev->active = false;
        }
        goto CLEAN_RETURN;
    }
    
    /* get name of the process whose io we are discussing */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &origin, &count, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    
    /* check to see if a tool has requested something */
    if (ORTE_IOF_PULL & stream) {
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                             "%s received pull cmd from remote tool %s for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&mev->sender),
                             ORTE_NAME_PRINT(&origin)));
        /* a tool is requesting that we send it a copy of the specified stream(s)
         * from the specified process(es), so create a sink for it
         */
        if (ORTE_IOF_STDOUT & stream) {
            ORTE_IOF_SINK_DEFINE(&sink, &origin, -1, ORTE_IOF_STDOUT,
                                 NULL, &mca_iof_hnp_component.sinks);
            sink->daemon.jobid = mev->sender.jobid;
            sink->daemon.vpid = mev->sender.vpid;
        }
        if (ORTE_IOF_STDERR & stream) {
            ORTE_IOF_SINK_DEFINE(&sink, &origin, -1, ORTE_IOF_STDERR,
                                 NULL, &mca_iof_hnp_component.sinks);
            sink->daemon.jobid = mev->sender.jobid;
            sink->daemon.vpid = mev->sender.vpid;
        }
        if (ORTE_IOF_STDDIAG & stream) {
            ORTE_IOF_SINK_DEFINE(&sink, &origin, -1, ORTE_IOF_STDDIAG,
                                 NULL, &mca_iof_hnp_component.sinks);
            sink->daemon.jobid = mev->sender.jobid;
            sink->daemon.vpid = mev->sender.vpid;
        }
        goto CLEAN_RETURN;
    }
    
    if (ORTE_IOF_CLOSE & stream) {
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                             "%s received close cmd from remote tool %s for proc %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&mev->sender),
                             ORTE_NAME_PRINT(&origin)));
        /* a tool is requesting that we no longer forward a copy of the
         * specified stream(s) from the specified process(es) - remove the sink
         */
        item = opal_list_get_first(&mca_iof_hnp_component.sinks);
        while (item != opal_list_get_end(&mca_iof_hnp_component.sinks)) {
            next = opal_list_get_next(item);
            sink = (orte_iof_sink_t*)item;
            /* if the target isn't set, then this sink is for another purpose - ignore it */
            if (ORTE_JOBID_INVALID == sink->daemon.jobid) {
                continue;
            }
            /* if this sink is the designated one, then remove it from list */
            if ((stream & sink->tag) &&
                sink->name.jobid == origin.jobid &&
                (ORTE_VPID_WILDCARD == sink->name.vpid ||
                 ORTE_VPID_WILDCARD == origin.vpid ||
                 sink->name.vpid == origin.vpid)) {
                /* send an ack message to the requestor - this ensures that the RML has
                 * completed sending anything to that requestor before it exits
                 */
                orte_iof_hnp_send_data_to_endpoint(&sink->daemon, &origin, ORTE_IOF_CLOSE, NULL, 0);
                opal_list_remove_item(&mca_iof_hnp_component.sinks, item);
                OBJ_RELEASE(item);
            }
            item = next;
        }
        goto CLEAN_RETURN;
    }
    
    /* this must have come from a daemon forwarding output - unpack the data */
    numbytes=ORTE_IOF_BASE_MSG_MAX;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, data, &numbytes, OPAL_BYTE))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    /* numbytes will contain the actual #bytes that were sent */
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s unpacked %d bytes from remote proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                         ORTE_NAME_PRINT(&origin)));
    
    /* output this to our local output */
    if (ORTE_IOF_STDOUT & stream || orte_xml_output) {
        orte_iof_base_write_output(&origin, stream, data, numbytes, orte_iof_base.iof_write_stdout->wev);
    } else {
        orte_iof_base_write_output(&origin, stream, data, numbytes, orte_iof_base.iof_write_stderr->wev);
    }
    
    /* cycle through the endpoints to see if someone else wants a copy */
    for (item = opal_list_get_first(&mca_iof_hnp_component.sinks);
         item != opal_list_get_end(&mca_iof_hnp_component.sinks);
         item = opal_list_get_next(item)) {
        sink = (orte_iof_sink_t*)item;
        /* if the target isn't set, then this sink is for another purpose - ignore it */
        if (ORTE_JOBID_INVALID == sink->daemon.jobid) {
            continue;
        }
        if ((stream & sink->tag) &&
            sink->name.jobid == origin.jobid &&
            (ORTE_VPID_WILDCARD == sink->name.vpid ||
             ORTE_VPID_WILDCARD == origin.vpid ||
             sink->name.vpid == origin.vpid)) {
            /* send the data to the tool */
            orte_iof_hnp_send_data_to_endpoint(&sink->daemon, &origin, stream, data, numbytes);
        }
    }
    
CLEAN_RETURN:
    /* release the message event */
    OBJ_RELEASE(mev);
    return;
}

void orte_iof_hnp_recv(int status, orte_process_name_t* sender,
                       opal_buffer_t* buffer, orte_rml_tag_t tag,
                       void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_iof_base.iof_output,
                         "%s iof:hnp:receive got message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message to avoid performing the rest of the job while
     * inside this receive! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_msg);
    
    /* reissue the recv */
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_IOF_HNP,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_iof_hnp_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    return;
}
