/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
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

#ifndef MCA_BTL_MX_ENDPOINT_H
#define MCA_BTL_MX_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "btl_mx_frag.h"
#include "btl_mx.h"

#include <myriexpress.h>

BEGIN_C_DECLS

#define MCA_BTL_MX_NOT_CONNECTED       0x0000
#define MCA_BTL_MX_NOT_REACHEABLE      0x0001
#define MCA_BTL_MX_CONNECTION_PENDING  0x0002
#define MCA_BTL_MX_CONNECTED           0x0004

/**
 * Structure used to publish MX information to peers
 */
struct mca_btl_mx_addr_t {
    uint64_t nic_id;
    uint32_t endpoint_id;
    uint32_t unique_network_id;  /* Unique identifier for each MX network */
};
typedef struct mca_btl_mx_addr_t mca_btl_mx_addr_t;

#define BTL_MX_ADDR_HTON(h)                                     \
    do {                                                        \
        h.nic_id = hton64(h.nic_id);                            \
        h.endpoint_id = htonl(h.endpoint_id);                   \
        h.unique_network_id = htonl(h.unique_network_id);       \
    } while (0)

#define BTL_MX_ADDR_NTOH(h)                                     \
    do {                                                        \
        h.nic_id = ntoh64(h.nic_id);                            \
        h.endpoint_id = ntohl(h.endpoint_id);                   \
        h.unique_network_id = ntohl(h.unique_network_id);       \
    } while (0)


/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t            super;

    struct mca_btl_mx_module_t* endpoint_btl;
    /**< BTL instance that created this connection */

    struct mca_btl_mx_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */

    struct mca_btl_mx_addr_t*   mx_peer;
    /** the address as reported by the peer */

    mx_endpoint_addr_t          mx_peer_addr;
    /** the remote MX endpoint address */

    int status;  /**< status of the endpoint */
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_mx_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_mx_endpoint_t);

END_C_DECLS
#endif
