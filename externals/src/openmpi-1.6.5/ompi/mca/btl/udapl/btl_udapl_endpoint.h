/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2006-2009 Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_UDAPL_ENDPOINT_H
#define MCA_BTL_UDAPL_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_udapl_frag.h"
#include "btl_udapl.h"
#include "btl_udapl_eager_rdma.h"

BEGIN_C_DECLS


#define BTL_UDAPL_TOKEN_AVAIL(E, C, T) \
do {					\
    (T) = 0;				\
    if ( (E)->endpoint_lwqe_tokens[(C)] > 0 && 	\
        ((E)->endpoint_sr_tokens[(C)] +		\
        (((C) == BTL_UDAPL_EAGER_CONNECTION)?(E)->endpoint_eager_rdma_remote.tokens:0)) 				\
        ) { 				\
              (T) = 1;			\
    }					\
} while (0)

/**
 * Structure used to publish uDAPL id information to peers.
 */
struct mca_btl_udapl_addr_t {
    DAT_CONN_QUAL port;
    DAT_SOCK_ADDR addr;
    bool inuse;
};
typedef struct mca_btl_udapl_addr_t mca_btl_udapl_addr_t;

    
/**
 * State of uDAPL endpoint connection.
 */

typedef enum {
    MCA_BTL_UDAPL_CONN_EAGER,
    MCA_BTL_UDAPL_CONN_MAX,
    MCA_BTL_UDAPL_CONNECTED,
    MCA_BTL_UDAPL_CLOSED,
    MCA_BTL_UDAPL_FAILED
} mca_btl_udapl_endpoint_state_t;

/*
 * Establish a name for the 2 connections opened per peer
 */
typedef enum {
    BTL_UDAPL_EAGER_CONNECTION,
    BTL_UDAPL_MAX_CONNECTION,
    BTL_UDAPL_NUM_CONNECTION
} mca_btl_udapl_endpoint_conn_t;
    
/*
 * Encapsulate data that describes sendrecv credit information.
 */
struct mca_btl_udapl_sr_credit_t {
        mca_btl_udapl_control_header_t control;
        uint32_t credits;
        int connection; /* 0 == BTL_UDAPL_EAGER_CONNECTION;
                           1 == BTL_UDAPL_MAX_CONNECTION */
};
typedef struct mca_btl_udapl_sr_credit_t mca_btl_udapl_sr_credit_t;

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
*/

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

    struct mca_btl_udapl_module_t* endpoint_btl;
    /**< BTL instance that created this connection */

    struct mca_btl_udapl_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */

    mca_btl_udapl_endpoint_state_t endpoint_state;
    /**< current state of the endpoint connection */

    opal_list_t endpoint_eager_frags;
    opal_list_t endpoint_max_frags;
    /**< pending send frags on this endpoint */

    int32_t endpoint_eager_sends;
    int32_t endpoint_max_sends;
    /**< number of sends that may be posted */

    int32_t endpoint_sr_tokens[BTL_UDAPL_NUM_CONNECTION];
    /**< number of sends that may be posted */

    int32_t endpoint_sr_credits[BTL_UDAPL_NUM_CONNECTION];
    /**< number of recvs that are now available */

    int32_t endpoint_lwqe_tokens[BTL_UDAPL_NUM_CONNECTION];
    /**< number of local work queue credits available (combination of
       posted sends and rdma writes allowed per endpoint */

    int32_t endpoint_connection_seq;
    /**< sequence number of sendrecv message for the connection est */

    int32_t endpoint_connections_completed;
    /**< count of completed connections for priv data connection est. */

    opal_mutex_t endpoint_lock;
    /**< lock for concurrent access to endpoint state */

    mca_btl_udapl_addr_t endpoint_addr;
    /**< remote address on the other side of this endpoint */

    DAT_EP_HANDLE endpoint_eager;
    DAT_EP_HANDLE endpoint_max;
    /**< uDAPL endpoint handle */

    int32_t endpoint_eager_rdma_index;
    /**< index into array of endpoints with RDMA buffers */
    mca_btl_udapl_eager_rdma_local_t endpoint_eager_rdma_local;
    /**< info about local RDMA buffer */
    mca_btl_udapl_eager_rdma_remote_t endpoint_eager_rdma_remote; 
    /**< info about remote RDMA buffer */
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_udapl_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_udapl_endpoint_t);


/*
 * Start sending data on an endpoint.
 */

int mca_btl_udapl_endpoint_send(mca_btl_base_endpoint_t* endpoint,
                                mca_btl_udapl_frag_t* frag);

/*
 * Set up OOB recv callback.
 */

void mca_btl_udapl_endpoint_post_oob_recv(void);

/*
 * Finish establishing a connection
 */

int mca_btl_udapl_endpoint_finish_connect(struct mca_btl_udapl_module_t* btl,
                                          mca_btl_udapl_addr_t* addr,
                                          int32_t* seq,
                                          DAT_EP_HANDLE endpoint);

/*
 * Send number of eager rdma credits
 */
int mca_btl_udapl_endpoint_send_eager_rdma_credits(mca_btl_base_endpoint_t* endpoint);

/*
 * Establish uDAPL endpoint parameters
 */
int mca_btl_udapl_endpoint_get_params(struct mca_btl_udapl_module_t* btl,
                                      DAT_EP_PARAM* ep_param);

/*
 * Create uDAPL endpoint
 */
int mca_btl_udapl_endpoint_create(struct mca_btl_udapl_module_t* btl,
                                  DAT_EP_HANDLE* udapl_endpoint);

 /*
 * Send number of send recv credits
 */
int mca_btl_udapl_endpoint_send_sr_credits(mca_btl_base_endpoint_t* endpoint,
                                           const int connection);

/*
 * Handle the established DAT endpoint when private data is in use
 */
int mca_btl_udapl_endpoint_pd_established_conn(
    struct mca_btl_udapl_module_t* btl,
    DAT_EP_HANDLE established_ep);

/*
 * Utility routine. Search list of endpoints to find one that matches
 * the given address.
 */
mca_btl_udapl_endpoint_t* mca_btl_udapl_find_endpoint_address_match(
    struct mca_btl_udapl_module_t* btl,
    mca_btl_udapl_addr_t addr);

END_C_DECLS
#endif
