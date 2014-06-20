/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_MTL_MXM_ENDPOINT_H
#define MCA_MTL_MXM_ENDPOINT_H
#include "opal/class/opal_list.h"
#include "ompi/mca/mtl/mtl.h"
#include "mtl_mxm.h"

BEGIN_C_DECLS

OBJ_CLASS_DECLARATION(mca_mtl_mxm_endpoint_t);

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_mtl_base_endpoint_t is associated w/ each process
 * and MTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_mtl_base_endpoint_t {
    opal_list_item_t super;

    struct mca_mtl_mxm_module_t* mtl_mxm_module;
    /**< MTL instance that created this connection */

    mxm_conn_h mxm_conn;
    /**< MXM Connection handle*/
};

typedef struct mca_mtl_base_endpoint_t mca_mtl_base_endpoint_t;
typedef mca_mtl_base_endpoint_t mca_mtl_mxm_endpoint_t;
OBJ_CLASS_DECLARATION(mca_mtl_mxm_endpoint);

END_C_DECLS
#endif
