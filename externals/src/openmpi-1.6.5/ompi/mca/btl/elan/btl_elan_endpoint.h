/*
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_ELAN_ENDPOINT_H
#define MCA_BTL_ELAN_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "btl_elan_frag.h"
#include "btl_elan.h"

BEGIN_C_DECLS

/**
 * State of ELAN endpoint connection.
 */

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_btl_base_endpoint_t is associated w/ each process
 * and BTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_btl_base_endpoint_t {
    opal_list_item_t              super;

    struct mca_btl_elan_module_t* endpoint_btl;
    /**< BTL instance that created this connection */

    struct mca_btl_elan_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */

    unsigned int 		  elan_vp;
};

typedef struct mca_btl_base_endpoint_t mca_btl_base_endpoint_t;
typedef mca_btl_base_endpoint_t  mca_btl_elan_endpoint_t;
OBJ_CLASS_DECLARATION(mca_btl_elan_endpoint_t);

END_C_DECLS

#endif  /* MCA_BTL_ELAN_ENDPOINT_H */
