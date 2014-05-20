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


#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/iof/iof_types.h"
#include "orte/mca/iof/base/base.h"

#include "iof_orted.h"

static void send_cb(int status, orte_process_name_t *peer,
                    opal_buffer_t *buf, orte_rml_tag_t tag,
                    void *cbdata)
{
    /* nothing to do here - just release buffer and return */
    OBJ_RELEASE(buf);
}

void orte_iof_orted_send_xonxoff(orte_iof_tag_t tag)
{
    opal_buffer_t *buf;
    int rc;
    
    buf = OBJ_NEW(opal_buffer_t);
    
    /* pack the tag - we do this first so that flow control messages can
     * consist solely of the tag
     */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &tag, 1, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        return;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s sending %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (ORTE_IOF_XON == tag) ? "xon" : "xoff"));

    /* send the buffer to the HNP */
    orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buf, ORTE_RML_TAG_IOF_HNP,
                            0, send_cb, NULL);
}

/*
 * The only messages coming to an orted are either:
 *
 * (a) stdin, which is to be copied to whichever local
 *     procs "pull'd" a copy
 *
 * (b) flow control messages
 */
static void process_msg(int fd, short event, void *cbdata)
{
    orte_message_event_t *mev = (orte_message_event_t*)cbdata;
    unsigned char data[ORTE_IOF_BASE_MSG_MAX];
    orte_iof_tag_t stream;
    int32_t count, numbytes;
    orte_process_name_t target;
    opal_list_item_t *item;
    int rc;
    
    /* see what stream generated this data */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &stream, &count, ORTE_IOF_TAG))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    
    /* if this isn't stdin, then we have an error */
    if (ORTE_IOF_STDIN != stream) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        goto CLEAN_RETURN;
    }
    
    /* unpack the intended target */
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &target, &count, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }

    /* unpack the data */
    numbytes=ORTE_IOF_BASE_MSG_MAX;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, data, &numbytes, OPAL_BYTE))) {
        ORTE_ERROR_LOG(rc);
        goto CLEAN_RETURN;
    }
    /* numbytes will contain the actual #bytes that were sent */
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s unpacked %d bytes for local proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), numbytes,
                         ORTE_NAME_PRINT(&target)));
    
    /* cycle through our list of sinks */
    for (item = opal_list_get_first(&mca_iof_orted_component.sinks);
         item != opal_list_get_end(&mca_iof_orted_component.sinks);
         item = opal_list_get_next(item)) {
        orte_iof_sink_t* sink = (orte_iof_sink_t*)item;
        
        /* is this intended for this jobid? */
        if (target.jobid == sink->name.jobid) {
            /* yes - is this intended for all vpids or this vpid? */
            if (ORTE_VPID_WILDCARD == target.vpid ||
                sink->name.vpid == target.vpid) {
                OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                                     "%s writing data to local proc %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&sink->name)));
                if (NULL == sink->wev || sink->wev->fd < 0) {
                    /* this sink was already closed - ignore this data */
                    goto CLEAN_RETURN;
                }
                /* send the bytes down the pipe - we even send 0 byte events
                 * down the pipe so it forces out any preceding data before
                 * closing the output stream
                 */
                if (ORTE_IOF_MAX_INPUT_BUFFERS < orte_iof_base_write_output(&target, stream, data, numbytes, sink->wev)) {
                    /* getting too backed up - tell the HNP to hold off any more input if we
                     * haven't already told it
                     */
                    if (!mca_iof_orted_component.xoff) {
                        mca_iof_orted_component.xoff = true;
                        orte_iof_orted_send_xonxoff(ORTE_IOF_XOFF);
                    }
                }
            }
        }
    }

CLEAN_RETURN:
    /* release the message event */
    OBJ_RELEASE(mev);
    return;
}


void orte_iof_orted_recv(int status, orte_process_name_t* sender,
                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                         void* cbdata)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_iof_base.iof_output,
                         "%s iof:orted:receive got message from %s",
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
                                                      ORTE_RML_TAG_IOF_PROXY,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_iof_orted_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    return;
}
