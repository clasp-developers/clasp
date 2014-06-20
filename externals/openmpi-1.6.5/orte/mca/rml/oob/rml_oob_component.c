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

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#include "opal/mca/base/base.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/backtrace/backtrace.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "rml_oob.h"

static orte_rml_module_t* rml_oob_init(int* priority);
static int rml_oob_open(void);
static int rml_oob_close(void);
static void rml_oob_recv_route_callback(int status,
                                        struct orte_process_name_t* peer,
                                        struct iovec* iov,
                                        int count,
                                        orte_rml_tag_t tag,
                                        void *cbdata);
static void rml_oob_queued_progress(int fd, short event, void *arg);

/**
 * component definition
 */
orte_rml_component_t mca_rml_oob_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        ORTE_RML_BASE_VERSION_2_0_0,

        "oob", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        rml_oob_open,  /* component open */
        rml_oob_close, /* component close */
      },
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },
      rml_oob_init
};

orte_rml_oob_module_t orte_rml_oob_module = {
    {
        orte_rml_oob_init,
        orte_rml_oob_fini,

        orte_rml_oob_get_uri,
        orte_rml_oob_set_uri,

        orte_rml_oob_get_new_name,
        orte_rml_oob_ping,

        orte_rml_oob_send,
        orte_rml_oob_send_nb,
        orte_rml_oob_send_buffer,
        orte_rml_oob_send_buffer_nb,

        orte_rml_oob_recv,
        orte_rml_oob_recv_nb,
        orte_rml_oob_recv_buffer,
        orte_rml_oob_recv_buffer_nb,
        orte_rml_oob_recv_cancel,

        orte_rml_oob_add_exception,
        orte_rml_oob_del_exception,

        orte_rml_oob_ft_event
    }
};


static int
rml_oob_open(void)
{
    int rc;

    if (ORTE_SUCCESS != (rc = mca_oob_base_open())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return rc;
}


static int
rml_oob_close(void)
{
    int rc;

    if (ORTE_SUCCESS != (rc = mca_oob_base_close())) {
        return rc;
    }

    return rc;
}

static orte_rml_module_t*
rml_oob_init(int* priority)
{
    if (mca_oob_base_init() != ORTE_SUCCESS)
        return NULL;
    *priority = 1;
    
    OBJ_CONSTRUCT(&orte_rml_oob_module.exceptions, opal_list_t);
    OBJ_CONSTRUCT(&orte_rml_oob_module.exceptions_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&orte_rml_oob_module.queued_routing_messages, opal_list_t);
    OBJ_CONSTRUCT(&orte_rml_oob_module.queued_lock, opal_mutex_t);
    /* Set default timeout for queued messages to be 1/2 second */
    orte_rml_oob_module.timeout.tv_sec = 0;
    orte_rml_oob_module.timeout.tv_usec = 500000;
    orte_rml_oob_module.timer_event = (opal_event_t *) malloc(sizeof(opal_event_t));
    if (NULL == orte_rml_oob_module.timer_event) {
        return NULL;
    }
    opal_evtimer_set(orte_rml_oob_module.timer_event, rml_oob_queued_progress,
                     NULL);

    orte_rml_oob_module.active_oob = &mca_oob;
    orte_rml_oob_module.active_oob->oob_exception_callback = 
        orte_rml_oob_exception_callback;

    return &orte_rml_oob_module.super;
}

static struct iovec route_recv_iov[1];

int
orte_rml_oob_init(void)
{
    int ret;

    ret = orte_rml_oob_module.active_oob->oob_init();

    route_recv_iov[0].iov_base = NULL;
    route_recv_iov[0].iov_len = 0;

    ret = orte_rml_oob_module.active_oob->oob_recv_nb(ORTE_NAME_WILDCARD,
                                                      route_recv_iov, 1,
                                                      ORTE_RML_TAG_RML_ROUTE,
                                                      ORTE_RML_ALLOC|ORTE_RML_PERSISTENT,
                                                      rml_oob_recv_route_callback,
                                                      NULL);

    /* enable the base receive to get updates on contact info */
    orte_rml_base_comm_start();
    
    return ret;
}


int
orte_rml_oob_fini(void)
{
    opal_list_item_t *item;

    while (NULL != 
           (item = opal_list_remove_first(&orte_rml_oob_module.exceptions))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_rml_oob_module.exceptions);
    OBJ_DESTRUCT(&orte_rml_oob_module.exceptions_lock);
    OBJ_DESTRUCT(&orte_rml_oob_module.queued_routing_messages);
    OBJ_DESTRUCT(&orte_rml_oob_module.queued_lock);
    orte_rml_oob_module.active_oob->oob_exception_callback = NULL;

    /* clear the base receive */
    orte_rml_base_comm_stop();
    
    return ORTE_SUCCESS;
}


int
orte_rml_oob_ft_event(int state) {
    int exit_status = ORTE_SUCCESS;
    int ret;

    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        ;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    if( ORTE_SUCCESS != 
        (ret = orte_rml_oob_module.active_oob->oob_ft_event(state)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }


    if(OPAL_CRS_CHECKPOINT == state) {
        ;
    }
    else if(OPAL_CRS_CONTINUE == state) {
        ;
    }
    else if(OPAL_CRS_RESTART == state) {
        if( ORTE_SUCCESS != (ret = mca_oob_base_close())) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        if( ORTE_SUCCESS != (ret = mca_oob_base_open())) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }

        if( ORTE_SUCCESS != (ret = mca_oob_base_init())) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

 cleanup:
    return exit_status;
}


static void
msg_construct(orte_rml_oob_msg_t *msg)
{
    OBJ_CONSTRUCT(&msg->msg_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&msg->msg_cond, opal_condition_t);
    msg->msg_status = 0;
    msg->msg_complete = false;
    msg->msg_persistent = false;
    OBJ_CONSTRUCT(&msg->msg_recv_buffer, opal_buffer_t);
    msg->msg_data = NULL;
}

static void
msg_destruct(orte_rml_oob_msg_t *msg)
{
    if (NULL != msg->msg_data) free(msg->msg_data);
    OBJ_DESTRUCT(&msg->msg_recv_buffer);
    OBJ_DESTRUCT(&msg->msg_lock);
    OBJ_DESTRUCT(&msg->msg_cond);
}

OBJ_CLASS_INSTANCE(orte_rml_oob_msg_t, opal_object_t,
                   msg_construct, msg_destruct);

static void
queued_msg_construct(orte_rml_oob_queued_msg_t *msg)
{
    msg->payload[0].iov_base = NULL;
    msg->payload[0].iov_len = 0;
}

static void
queued_msg_destruct(orte_rml_oob_queued_msg_t *msg)
{
    if (NULL != msg->payload[0].iov_base) free(msg->payload[0].iov_base);
}

OBJ_CLASS_INSTANCE(orte_rml_oob_queued_msg_t, opal_list_item_t,
                   queued_msg_construct, queued_msg_destruct);

static void
rml_oob_recv_route_queued_send_callback(int status,
                                        struct orte_process_name_t* peer,
                                        struct iovec* iov,
                                        int count,
                                        orte_rml_tag_t tag,
                                        void* cbdata)
{
    orte_rml_oob_queued_msg_t *qmsg = (orte_rml_oob_queued_msg_t*) cbdata;
    OBJ_RELEASE(qmsg);
}


static void 
rml_oob_queued_progress(int fd, short event, void *arg)
{
    orte_rml_oob_queued_msg_t *qmsg;
    orte_rml_oob_msg_header_t *hdr;
    int real_tag;
    int ret;
    orte_process_name_t next, origin;

    while (true) {
        OPAL_THREAD_LOCK(&orte_rml_oob_module.queued_lock);
        qmsg = (orte_rml_oob_queued_msg_t*) opal_list_remove_first(&orte_rml_oob_module.queued_routing_messages);
        OPAL_THREAD_UNLOCK(&orte_rml_oob_module.queued_lock);
        if (NULL == qmsg) break;

        hdr = (orte_rml_oob_msg_header_t*) qmsg->payload[0].iov_base;
        origin = hdr->origin;

        next = orte_routed.get_route(&hdr->destination);
        if (next.vpid == ORTE_VPID_INVALID) {
            opal_output(0,
                        "%s:queued progress tried routing message from %s to %s:%d, can't find route",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&hdr->origin),
                        ORTE_NAME_PRINT(&hdr->destination),
                        hdr->tag);
            opal_backtrace_print(stderr);
            orte_errmgr.abort(ORTE_ERROR_DEFAULT_EXIT_CODE, NULL);
        }

        if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &next, ORTE_PROC_MY_NAME)) {
            opal_output(0, "%s:queued progress trying to get message from %s to %s:%d, routing loop",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&hdr->origin),
                        ORTE_NAME_PRINT(&hdr->destination),
                        hdr->tag);
            opal_backtrace_print(stderr);
            orte_errmgr.abort(ORTE_ERROR_DEFAULT_EXIT_CODE, NULL);
        }

        if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &next, &hdr->destination)) {
            real_tag = hdr->tag;
        } else {
            real_tag = ORTE_RML_TAG_RML_ROUTE;
        }

        OPAL_OUTPUT_VERBOSE((1, orte_rml_base_output,
                             "%s routing message from %s for %s to %s (tag: %d)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&hdr->origin),
                             ORTE_NAME_PRINT(&hdr->destination),
                             ORTE_NAME_PRINT(&next),
                             hdr->tag));

        ORTE_RML_OOB_MSG_HEADER_HTON(*hdr);

        ret = orte_rml_oob_module.active_oob->oob_send_nb(&next,
                                                          &origin,
                                                          qmsg->payload,
                                                          1,
                                                          real_tag,
                                                          0,
                                                          rml_oob_recv_route_queued_send_callback,
                                                          qmsg);

        if (ORTE_SUCCESS != ret) {
            if (ORTE_ERR_ADDRESSEE_UNKNOWN == ret) {
                /* still no route -- try again */
                ORTE_RML_OOB_MSG_HEADER_NTOH(*hdr);
                OPAL_THREAD_LOCK(&orte_rml_oob_module.queued_lock);
                opal_list_append(&orte_rml_oob_module.queued_routing_messages,
                                 &qmsg->super);
                if (1 == opal_list_get_size(&orte_rml_oob_module.queued_routing_messages)) {
                    opal_evtimer_add(orte_rml_oob_module.timer_event,
                                     &orte_rml_oob_module.timeout);
                }
                OPAL_THREAD_UNLOCK(&orte_rml_oob_module.queued_lock);
            } else {
                opal_output(0,
                            "%s failed to send message from %s to %s:%d %s (rc = %d)",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&next),
                            ORTE_NAME_PRINT(&origin),
                            real_tag,
                            ORTE_ERROR_NAME(ret),
                            ret);
                abort();
            }
        }
    }
}

static void
rml_oob_recv_route_send_callback(int status,
                                 struct orte_process_name_t* peer,
                                 struct iovec* iov,
                                 int count,
                                 orte_rml_tag_t tag,
                                 void* cbdata)
{
}


static void
rml_oob_recv_route_callback(int status,
                            struct orte_process_name_t* peer,
                            struct iovec* iov,
                            int count,
                            orte_rml_tag_t tag,
                            void *cbdata)
{
    orte_rml_oob_msg_header_t *hdr = 
        (orte_rml_oob_msg_header_t*) iov[0].iov_base;
    int real_tag;
    int ret;
    orte_process_name_t next, origin;

    /* BWB -- propogate errors here... */
    assert(status >= 0);

    ORTE_RML_OOB_MSG_HEADER_NTOH(*hdr);

    origin = hdr->origin;

    next = orte_routed.get_route(&hdr->destination);
    if (next.vpid == ORTE_VPID_INVALID) {
        opal_output(0, "%s:route_callback tried routing message from %s to %s:%d, can't find route",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&origin),
                    ORTE_NAME_PRINT(&hdr->destination),
                    hdr->tag);
        opal_backtrace_print(stderr);
        orte_errmgr.abort(ORTE_ERROR_DEFAULT_EXIT_CODE, NULL);
    }

    if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &next, ORTE_PROC_MY_NAME)) {
        opal_output(0, "%s:route_callback trying to get message from %s to %s:%d, routing loop",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&origin),
                    ORTE_NAME_PRINT(&hdr->destination),
                    hdr->tag);
        opal_backtrace_print(stderr);
        orte_errmgr.abort(ORTE_ERROR_DEFAULT_EXIT_CODE, NULL);
    }

    if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &next, &hdr->destination)) {
        real_tag = hdr->tag;
    } else {
        real_tag = ORTE_RML_TAG_RML_ROUTE;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_output,
                         "%s routing message from %s for %s to %s (tag: %d)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&hdr->origin),
                         ORTE_NAME_PRINT(&hdr->destination),
                         ORTE_NAME_PRINT(&next),
                         hdr->tag));

    ORTE_RML_OOB_MSG_HEADER_HTON(*hdr);

    ret = orte_rml_oob_module.active_oob->oob_send_nb(&next,
                                                      &origin,
                                                      iov,
                                                      count,
                                                      real_tag,
                                                      0,
                                                      rml_oob_recv_route_send_callback,
                                                      NULL);

    if (ORTE_SUCCESS != ret) {
        if (ORTE_ERR_ADDRESSEE_UNKNOWN == ret) {
            /* no route -- queue and hope we find a route */
            orte_rml_oob_queued_msg_t *qmsg = OBJ_NEW(orte_rml_oob_queued_msg_t);
            OPAL_OUTPUT_VERBOSE((1, orte_rml_base_output,
                                 "%s: no OOB information for %s.  Queuing for later.",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&next)));
            ORTE_RML_OOB_MSG_HEADER_NTOH(*hdr);
            qmsg->payload[0].iov_base = (IOVBASE_TYPE*) malloc(iov[0].iov_len);
            if (NULL == qmsg->payload[0].iov_base) abort();
            qmsg->payload[0].iov_len = iov[0].iov_len;
            memcpy(qmsg->payload[0].iov_base, iov[0].iov_base, iov[0].iov_len);
            OPAL_THREAD_LOCK(&orte_rml_oob_module.queued_lock);
            opal_list_append(&orte_rml_oob_module.queued_routing_messages,
                             &qmsg->super);
            if (1 == opal_list_get_size(&orte_rml_oob_module.queued_routing_messages)) {
                opal_evtimer_add(orte_rml_oob_module.timer_event,
                                 &orte_rml_oob_module.timeout);
            }
            OPAL_THREAD_UNLOCK(&orte_rml_oob_module.queued_lock);
        } else {
            opal_output(0,
                        "%s failed to send message to %s: %s (rc = %d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&next),
                        opal_strerror(ret),
                        ret);
            orte_errmgr.abort(ORTE_ERROR_DEFAULT_EXIT_CODE, NULL);
        }
    }
}
