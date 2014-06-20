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
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_MTL_PSM_ENDPOINT_H
#define MCA_MTL_PSM_ENDPOINT_H

#include "opal/class/opal_list.h"
#include "opal/event/event.h"
#include "ompi/mca/mtl/mtl.h"
#include "mtl_psm.h"

#include "psm.h"

BEGIN_C_DECLS

OBJ_CLASS_DECLARATION(mca_mtl_psm_endpoint_t);

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_mtl_base_endpoint_t is associated w/ each process
 * and MTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_mtl_base_endpoint_t {
    opal_list_item_t super;
   
    struct mca_mtl_psm_module_t* mtl_psm_module;
    /**< MTL instance that created this connection */
    
    psm_epid_t	    peer_epid;
    /**< The unique epid for the opened port */

    psm_epaddr_t    peer_addr;
    /**< The connected endpoint handle*/
};

typedef struct mca_mtl_base_endpoint_t mca_mtl_base_endpoint_t;
typedef mca_mtl_base_endpoint_t  mca_mtl_psm_endpoint_t;
OBJ_CLASS_DECLARATION(mca_mtl_psm_endpoint);

END_C_DECLS
#endif
