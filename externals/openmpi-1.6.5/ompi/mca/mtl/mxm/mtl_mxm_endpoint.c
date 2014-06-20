/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <sys/time.h>
#include <time.h>
#include "ompi/types.h"

#include "mtl_mxm.h"
#include "mtl_mxm_types.h"
#include "mtl_mxm_endpoint.h" 

/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_mtl_mxm_endpoint_construct(mca_mtl_mxm_endpoint_t* endpoint)
{
    endpoint->mtl_mxm_module = NULL;
}

/*
 * Destroy a endpoint
 *
 */

static void mca_mtl_mxm_endpoint_destruct(mca_mtl_mxm_endpoint_t* endpoint)
{
}

OBJ_CLASS_INSTANCE(
        mca_mtl_mxm_endpoint_t,
        opal_list_item_t,
        mca_mtl_mxm_endpoint_construct,
        mca_mtl_mxm_endpoint_destruct);
