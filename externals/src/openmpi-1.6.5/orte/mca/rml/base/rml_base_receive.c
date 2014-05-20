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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
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

#include "orte/constants.h"
#include "orte/types.h"

#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"

static bool recv_issued=false;
static void orte_rml_base_recv(int status, orte_process_name_t* sender,
                               opal_buffer_t* buffer, orte_rml_tag_t tag,
                               void* cbdata);

int orte_rml_base_comm_start(void)
{
    int rc;

    if (recv_issued) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_RML_INFO_UPDATE,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_rml_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = true;
    
    return rc;
}


int orte_rml_base_comm_stop(void)
{
    int rc;
    
    if (!recv_issued) {
        return ORTE_SUCCESS;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_RML_INFO_UPDATE))) {
        ORTE_ERROR_LOG(rc);
    }
    recv_issued = false;
    
    return rc;
}

static void process_message(int fd, short event, void *data)
{
    orte_message_event_t *mev = (orte_message_event_t*)data;
    orte_rml_cmd_flag_t command;
    orte_std_cntr_t count;
    opal_buffer_t buf;
    int rc;
    

    OPAL_OUTPUT_VERBOSE((5, orte_rml_base_output,
                         "%s rml:base:recv: processing message from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&mev->sender)));
    
    count = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(mev->buffer, &command, &count, ORTE_RML_CMD))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    switch (command) {
        case ORTE_RML_UPDATE_CMD:
            if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(mev->buffer))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
            break;
            
        default:
            ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
    }
    
    /* send an ack back - this is REQUIRED to ensure that the routing
     * info gets updated -before- a message intending to use that info
     * arrives. Because message ordering is NOT preserved in the OOB, it
     * is possible for code that updates our contact info and then sends
     * a message to fail because the update contact info message is
     * processed too late
     */
    OPAL_OUTPUT_VERBOSE((5, orte_rml_base_output,
                         "%s rml:base:recv: sending ack to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&mev->sender)));

    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    if (0 > (rc = orte_rml.send_buffer(&mev->sender, &buf, ORTE_RML_TAG_UPDATE_ROUTE_ACK, 0))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&buf);
    
    OBJ_RELEASE(mev);
}

/*
 * handle message from proxies
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

static void
orte_rml_base_recv(int status, orte_process_name_t* sender,
                   opal_buffer_t* buffer, orte_rml_tag_t tag,
                   void* cbdata)
{
    int rc;
    
    /* don't process this right away - we need to get out of the recv before
     * we process the message as it may ask us to do something that involves
     * more messaging! Instead, setup an event so that the message gets processed
     * as soon as we leave the recv.
     *
     * The macro makes a copy of the buffer, which we release above - the incoming
     * buffer, however, is NOT released here, although its payload IS transferred
     * to the message buffer for later processing
     */
    ORTE_MESSAGE_EVENT(sender, buffer, tag, process_message);
   
    if (ORTE_SUCCESS != (rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                                                      ORTE_RML_TAG_RML_INFO_UPDATE,
                                                      ORTE_RML_NON_PERSISTENT,
                                                      orte_rml_base_recv,
                                                      NULL))) {
        ORTE_ERROR_LOG(rc);
    }
}

