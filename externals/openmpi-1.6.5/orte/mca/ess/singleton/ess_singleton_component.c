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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/singleton/ess_singleton.h"

extern orte_ess_base_module_t orte_ess_singleton_module;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_ess_base_component_t mca_ess_singleton_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        ORTE_ESS_BASE_VERSION_2_0_0,

        /* Component name and version */
        "singleton",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_ess_singleton_component_open,
        orte_ess_singleton_component_close,
        orte_ess_singleton_component_query
    },
    {
        /* The component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    }
};


int
orte_ess_singleton_component_open(void)
{
    return ORTE_SUCCESS;
}

int orte_ess_singleton_component_query(mca_base_module_t **module, int *priority)
{
    /* if we are an HNP, daemon, or tool, then we
     * are definitely not a singleton!
     */
    if (ORTE_PROC_IS_HNP ||
        ORTE_PROC_IS_DAEMON ||
        ORTE_PROC_IS_TOOL) {
        *module = NULL;
        return ORTE_ERROR;
    }
    
    /* okay, we still could be a singleton or
     * an application process. If we have been
     * given an HNP URI, then we are definitely
     * not a singleton
     */
    if (NULL != orte_process_info.my_hnp_uri) {
        *module = NULL;
        return ORTE_ERROR;
    }
    
    /* okay, we could still be an application process,
     * but launched in "standalone" mode - i.e., directly
     * launched by an environment instead of via mpirun.
     * We need to set our priority low so that any enviro
     * component will override us. If they don't, then we
     * want to be selected as we must be a singleton
     */
    *priority = 25;
    *module = (mca_base_module_t *)&orte_ess_singleton_module;
    return ORTE_SUCCESS;
}


int
orte_ess_singleton_component_close(void)
{
    return ORTE_SUCCESS;
}

