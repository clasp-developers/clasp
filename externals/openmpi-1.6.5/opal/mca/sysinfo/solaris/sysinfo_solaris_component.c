/*
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved. 
 *
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

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/sysinfo/sysinfo.h"
#include "sysinfo_solaris.h"

/*
 * Public string showing the sysinfo ompi_linux component version number
 */
const char *opal_sysinfo_solaris_component_version_string =
    "OPAL solaris sysinfo MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int sysinfo_solaris_component_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_sysinfo_base_component_t mca_sysinfo_solaris_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        OPAL_SYSINFO_BASE_VERSION_2_0_0,

        /* Component name and version */
        "solaris",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        NULL,
        NULL,
        sysinfo_solaris_component_query,
        NULL,
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int sysinfo_solaris_component_query(mca_base_module_t **module, int *priority)
{
    *priority = 20;
    *module = (mca_base_module_t *)&opal_sysinfo_solaris_module;
    
    return OPAL_SUCCESS;
}

