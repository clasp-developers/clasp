/*
 * Copyright (c) 2008 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_openib.h"
#include "btl_openib_endpoint.h"
#include "connect/connect.h"

static void empty_component_register(void);
static int empty_component_init(void);
static int empty_component_query(mca_btl_openib_module_t *btl, 
                                 ompi_btl_openib_connect_base_module_t **cpc);

ompi_btl_openib_connect_base_component_t ompi_btl_openib_connect_empty = {
    "empty",
    empty_component_register,
    empty_component_init,
    empty_component_query,
    NULL
};

static void empty_component_register(void)
{
    /* Nothing to do */
}

static int empty_component_init(void)
{
    /* Never let this CPC run */
    return OMPI_ERR_NOT_SUPPORTED;
}

static int empty_component_query(mca_btl_openib_module_t *btl, 
                                 ompi_btl_openib_connect_base_module_t **cpc)
{
    /* Never let this CPC run */
    return OMPI_ERR_NOT_SUPPORTED;
}
