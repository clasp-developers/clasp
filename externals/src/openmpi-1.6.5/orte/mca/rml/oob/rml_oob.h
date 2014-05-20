/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef MCA_RML_OOB_RML_OOB_H
#define MCA_RML_OOB_RML_OOB_H

#include "orte_config.h"
#include "opal/threads/condition.h"
#include "opal/threads/mutex.h"
#include "opal/event/event.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/oob/oob.h"
#include "opal/dss/dss_types.h"

BEGIN_C_DECLS

struct orte_rml_oob_module_t {
    struct orte_rml_module_t super;
    mca_oob_t               *active_oob;
    opal_list_t              exceptions;
    opal_mutex_t             exceptions_lock;
    opal_list_t              queued_routing_messages;
    opal_mutex_t             queued_lock;
    opal_event_t            *timer_event;
    struct timeval           timeout;
};
typedef struct orte_rml_oob_module_t orte_rml_oob_module_t;

ORTE_MODULE_DECLSPEC extern orte_rml_component_t mca_rml_oob_component;
extern orte_rml_oob_module_t orte_rml_oob_module;


typedef enum {
    ORTE_RML_BLOCKING_SEND,
    ORTE_RML_NONBLOCKING_IOV_SEND,
    ORTE_RML_NONBLOCKING_BUFFER_SEND,
    ORTE_RML_BLOCKING_RECV,
    ORTE_RML_NONBLOCKING_IOV_RECV,
    ORTE_RML_NONBLOCKING_BUFFER_RECV
} orte_rml_oob_msg_type_t;

struct orte_rml_oob_msg_header_t {
    orte_process_name_t origin;
    orte_process_name_t destination;
    int tag;
};
typedef struct orte_rml_oob_msg_header_t orte_rml_oob_msg_header_t;

#define ORTE_RML_OOB_MSG_HEADER_HTON(hdr)               \
    do {                                                \
        ORTE_PROCESS_NAME_HTON((hdr).origin);           \
        ORTE_PROCESS_NAME_HTON((hdr).destination);      \
        (hdr).tag = htonl((hdr).tag);                   \
    } while (0)


#define ORTE_RML_OOB_MSG_HEADER_NTOH(hdr)               \
    do {                                                \
        ORTE_PROCESS_NAME_NTOH((hdr).origin);           \
        ORTE_PROCESS_NAME_NTOH((hdr).destination);      \
        (hdr).tag = ntohl((hdr).tag);                   \
    } while (0)


struct orte_rml_oob_msg_t {
    opal_object_t             super;

    opal_mutex_t                      msg_lock;
    opal_condition_t                  msg_cond;

    orte_rml_oob_msg_type_t           msg_type;
    int                               msg_status;
    volatile bool                     msg_complete;
    bool                              msg_persistent;

    union {
        orte_rml_callback_fn_t        iov;
        orte_rml_buffer_callback_fn_t buffer;
    }                                 msg_cbfunc;
    void                             *msg_cbdata;

    struct iovec                     *msg_data;

    /** buffer for non-blocking buffer sends */
    opal_buffer_t                     msg_recv_buffer;
    /** pointer to user buffer for buffered sends */
    opal_buffer_t                    *user_buffer;
  
    orte_rml_oob_msg_header_t         msg_header;  
};
typedef struct orte_rml_oob_msg_t orte_rml_oob_msg_t;
OBJ_CLASS_DECLARATION(orte_rml_oob_msg_t);

struct orte_rml_oob_queued_msg_t {
    opal_list_item_t super;
    struct iovec payload[1];
};
typedef struct orte_rml_oob_queued_msg_t orte_rml_oob_queued_msg_t;
OBJ_CLASS_DECLARATION(orte_rml_oob_queued_msg_t);

int orte_rml_oob_init(void);
int orte_rml_oob_fini(void);
int orte_rml_oob_ft_event(int state);

int orte_rml_oob_send(orte_process_name_t* peer,
                      struct iovec *msg,
                      int count,
                      int tag,
                      int flags);
int orte_rml_oob_send_nb(orte_process_name_t* peer,
                         struct iovec* msg,
                         int count,
                         orte_rml_tag_t tag,
                         int flags,
                         orte_rml_callback_fn_t cbfunc,
                         void* cbdata);
int orte_rml_oob_send_buffer(orte_process_name_t* peer,
                             opal_buffer_t* buffer,
                             orte_rml_tag_t tag,
                             int flags);

int orte_rml_oob_send_buffer_nb(orte_process_name_t* peer,
                                opal_buffer_t* buffer,
                                orte_rml_tag_t tag,
                                int flags,
                                orte_rml_buffer_callback_fn_t cbfunc,
                                void* cbdata);

int orte_rml_oob_recv(orte_process_name_t* peer,
                      struct iovec *msg,
                      int count,
                      orte_rml_tag_t tag,
                      int flags);
int orte_rml_oob_recv_nb(orte_process_name_t* peer,
                         struct iovec* msg,
                         int count,
                         orte_rml_tag_t tag,
                         int flags,
                         orte_rml_callback_fn_t cbfunc,
                         void* cbdata);
int orte_rml_oob_recv_buffer(orte_process_name_t* peer,
                             opal_buffer_t *buf,
                             orte_rml_tag_t tag,
                             int flags);
int orte_rml_oob_recv_buffer_nb(orte_process_name_t* peer,
                                orte_rml_tag_t tag,
                                int flags,
                                orte_rml_buffer_callback_fn_t cbfunc,
                                void* cbdata);
int orte_rml_oob_recv_cancel(orte_process_name_t* peer, 
                                orte_rml_tag_t tag);

int orte_rml_oob_ping(const char* uri, 
                      const struct timeval* tv);

char* orte_rml_oob_get_uri(void);
int orte_rml_oob_set_uri(const char*);
int orte_rml_oob_get_new_name(orte_process_name_t *name);

int orte_rml_oob_add_exception(orte_rml_exception_callback_t cbfunc);
int orte_rml_oob_del_exception(orte_rml_exception_callback_t cbfunc);
void orte_rml_oob_exception_callback(const orte_process_name_t *peer,
                                    orte_rml_exception_t exception);


END_C_DECLS

#endif
