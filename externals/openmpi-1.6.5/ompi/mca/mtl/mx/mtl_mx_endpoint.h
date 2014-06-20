/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_MTL_MX_ENDPOINT_H
#define MCA_MTL_MX_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/mtl/mtl.h"
#include "mtl_mx.h"

#include "myriexpress.h"

BEGIN_C_DECLS

OBJ_CLASS_DECLARATION(mca_mtl_mx_endpoint_t);

/**
 * Structure used to publish MX information to peers
 */
struct mca_mtl_mx_addr_t {
    uint64_t nic_id;
    uint32_t endpoint_id;
};
typedef struct mca_mtl_mx_addr_t mca_mtl_mx_addr_t;

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_mtl_base_endpoint_t is associated w/ each process
 * and MTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_mtl_base_endpoint_t {
    opal_list_item_t super;
   
    struct mca_mtl_mx_module_t* mtl_mx_module;
    /**< MTL instance that created this connection */
    
    struct mca_mtl_mx_addr_t*    mx_peer;
    /** the address as reported by the peer */

    mx_endpoint_addr_t         mx_peer_addr;
    /** the remote MX endpoint address */
    
    
};
typedef struct mca_mtl_base_endpoint_t mca_mtl_base_endpoint_t;
typedef mca_mtl_base_endpoint_t  mca_mtl_mx_endpoint_t;
OBJ_CLASS_DECLARATION(mca_mtl_mx_endpoint);

mca_mtl_mx_endpoint_t* mca_mtl_mx_endpoint_create(ompi_proc_t*);


END_C_DECLS
#endif
