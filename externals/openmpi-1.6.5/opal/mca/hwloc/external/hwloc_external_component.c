/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/hwloc/hwloc.h"

/*
 * Public string showing the sysinfo ompi_linux component version number
 */
const char *opal_hwloc_external_component_version_string =
    "OPAL hwloc_external hwloc MCA component version " OPAL_VERSION;


/*
 * Local function
 */
static int hwloc_external_open(void);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_hwloc_component_t mca_hwloc_external_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        OPAL_HWLOC_BASE_VERSION_2_0_0,

        /* Component name and version */
        "external",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        hwloc_external_open,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int hwloc_external_open(void)
{
    /* Must have some code in this file, or the OS X linker may
       eliminate the whole file */
    return OPAL_SUCCESS;
}
