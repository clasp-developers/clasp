/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
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

#include "orte/util/proc_info.h"

#include "orte/mca/routed/base/base.h"
#include "routed_cm.h"

static int orte_routed_cm_component_query(mca_base_module_t **module, int *priority);

/**
 * component definition
 */
orte_routed_component_t mca_routed_cm_component = {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        ORTE_ROUTED_BASE_VERSION_2_0_0,

        "cm", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        NULL,
        NULL,
        orte_routed_cm_component_query
      },
      {
          /* This component can be checkpointed */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      }
};

static int orte_routed_cm_component_query(mca_base_module_t **module, int *priority)
{
    bool is_required = false;

    mca_base_is_component_required(&orte_routed_base_components,
                                   &mca_routed_cm_component.base_version,
                                   true,
                                   &is_required);

    if( !is_required ) {
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }
    
    *priority = 1000;
    *module = (mca_base_module_t *)&orte_routed_cm_module;
    return ORTE_SUCCESS;
}
