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

#ifndef MCA_BTL_SCTP_ENDPOINT_H
#define MCA_BTL_SCTP_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "btl_sctp_frag.h"
#include "btl_sctp.h"
BEGIN_C_DECLS

#define MCA_BTL_SCTP_ENDPOINT_CACHE 1

/**
 * State of SCTP endpoint connection.
 */

typedef enum {
    MCA_BTL_SCTP_CONNECTING = 0,
    MCA_BTL_SCTP_CONNECT_ACK,
    MCA_BTL_SCTP_CLOSED,
    MCA_BTL_SCTP_FAILED,
    MCA_BTL_SCTP_CONNECTED,
    MCA_BTL_SCTP_SHUTDOWN
} mca_btl_sctp_state_t;

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t                super;
    struct mca_btl_sctp_module_t*    endpoint_btl;          /**< BTL instance that created this connection */
    struct mca_btl_sctp_proc_t*      endpoint_proc;         /**< proc structure corresponding to endpoint */
    struct mca_btl_sctp_addr_t*      endpoint_addr;         /**< address of endpoint */
    int                             endpoint_sd;           /**< socket connection to endpoint */
#if MCA_BTL_SCTP_ENDPOINT_CACHE
    char*                           endpoint_cache;        /**< cache for the recv (reduce the number of recv syscall) */
    char*                           endpoint_cache_pos;    /**< current position in the cache */
    size_t                          endpoint_cache_length; /**< length of the data in the cache */
#endif  /* MCA_BTL_SCTP_ENDPOINT_CACHE */
    struct mca_btl_sctp_frag_t*      endpoint_send_frag;    /**< current send frag being processed */
    struct mca_btl_sctp_frag_t*      endpoint_recv_frag;    /**< current recv frag being processed */
    mca_btl_sctp_state_t             endpoint_state;        /**< current state of the connection */
    size_t                          endpoint_retries;      /**< number of connection retries attempted */
    opal_list_t                     endpoint_frags;        /**< list of pending frags to send */
    opal_mutex_t                    endpoint_send_lock;    /**< lock for concurrent access to endpoint state */
    opal_mutex_t                    endpoint_recv_lock;    /**< lock for concurrent access to endpoint state */
    opal_event_t                    endpoint_send_event;   /**< event for async processing of send frags */
    opal_event_t                    endpoint_recv_event;   /**< event for async processing of recv frags */
    bool                            endpoint_nbo;          /**< convert headers to network byte order? */
    int                             endpoint_has_initialized;
    int                             endpoint_in_list;
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_sctp_endpoint_t;
OBJ_CLASS_DECLARATION(mca_btl_sctp_endpoint_t);

struct our_sctp_endpoint {
    opal_list_item_t super;
    mca_btl_sctp_endpoint_t *endpoint;
};
typedef struct our_sctp_endpoint our_sctp_endpoint;
OBJ_CLASS_DECLARATION(our_sctp_endpoint);

    
int mca_btl_sctp_set_socket_options(int sd);
void mca_btl_sctp_endpoint_close(mca_btl_base_endpoint_t*);
int  mca_btl_sctp_endpoint_send(mca_btl_base_endpoint_t*, struct mca_btl_sctp_frag_t*);
bool mca_btl_sctp_endpoint_accept(mca_btl_base_endpoint_t*, struct sockaddr_in*, int);
void mca_btl_sctp_endpoint_shutdown(mca_btl_base_endpoint_t*);

END_C_DECLS
#endif
