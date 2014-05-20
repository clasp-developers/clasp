/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include "orte_config.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_loadleveler.h"


/*
 * Local variables
 */
static int param_priority;


/*
 * Local functions
 */
static int orte_ras_loadleveler_open(void);
static int orte_ras_loadleveler_component_query(mca_base_module_t **module, int *priority);


orte_ras_base_component_t mca_ras_loadleveler_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */
    {
        ORTE_RAS_BASE_VERSION_2_0_0,
        
        /* Component name and version */
        "loadleveler",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        orte_ras_loadleveler_open,
        NULL,
        orte_ras_loadleveler_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int orte_ras_loadleveler_open(void)
{
    /* for now we set the priority lower then the priority of the POE RAS
     * so that it is used whenever the LOADL_PROCESSOR_LIST is actually set */
    param_priority = 
        mca_base_param_reg_int(&mca_ras_loadleveler_component.base_version,
                               "priority",
                               "Priority of the loadleveler ras component",
                               false, false, 90, NULL);

    return ORTE_SUCCESS;
}

static int orte_ras_loadleveler_component_query(mca_base_module_t **module, int *priority)
{
    /* Are we running under a LOADLEVELER job? */
    if (NULL != getenv("LOADL_STEP_ID")) {
        mca_base_param_lookup_int(param_priority, priority);
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                             "%s ras:loadleveler: available for selection",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        *module = (mca_base_module_t *) &orte_ras_loadleveler_module;
        return ORTE_SUCCESS;
    }

    /* Sadly, no */
    OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                         "%s ras:loadleveler: NOT available for selection",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    *module = NULL;
    return ORTE_ERROR;
}

