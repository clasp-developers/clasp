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

#include "ras_cm.h"


/*
 * Local functions
 */
static int ras_cm_open(void);
static int orte_ras_cm_component_query(mca_base_module_t **module, int *priority);


orte_ras_base_component_t mca_ras_cm_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
        ORTE_RAS_BASE_VERSION_2_0_0,
        
        /* Component name and version */
        "cm",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        ras_cm_open,
        NULL,
        orte_ras_cm_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int ras_cm_open(void)
{
    return ORTE_SUCCESS;
}


static int orte_ras_cm_component_query(mca_base_module_t **module, int *priority)
{
    char *directive;
    
    /* determine if we were specified */
    directive = getenv("OMPI_MCA_ras");
    
    if (NULL == directive || 0 != strcmp("cm", directive)) {
        *priority = 0;
        *module = NULL;
        return ORTE_ERROR;
    }
    
    /* we were specified */
    *priority = 100;
    *module = (mca_base_module_t *) &orte_ras_cm_module;
    return ORTE_SUCCESS;
}
