/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"


#include "orte/mca/routed/base/base.h"
#include "routed_radix.h"

static int orte_routed_radix_component_query(mca_base_module_t **module, int *priority);

/**
 * component definition
 */
orte_routed_radix_component_t mca_routed_radix_component = {
    {
        /* First, the mca_base_component_t struct containing meta
        information about the component itself */

        {
        ORTE_ROUTED_BASE_VERSION_2_0_0,

        "radix", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL,
        orte_routed_radix_component_query
        },
        {
        /* This component can be checkpointed */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

static int orte_routed_radix_component_query(mca_base_module_t **module, int *priority)
{
    int tmp;
    mca_base_component_t *c = &mca_routed_radix_component.super.base_version;

    mca_base_param_reg_int(c, NULL,
                           "Radix to be used for routed radix tree",
                           false, false, -1, &tmp);
    if (0 < tmp) {
        mca_routed_radix_component.radix = tmp;
        *priority = 150;
        *module = (mca_base_module_t *) &orte_routed_radix_module;
        return ORTE_SUCCESS;
    }
    
    /* if radix not provided, then we can't run */
    *priority = 0;
    *module = NULL;
    return ORTE_ERROR;
}
