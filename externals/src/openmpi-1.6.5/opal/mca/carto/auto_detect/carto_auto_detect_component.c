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
 * Copyright (c) 2007      Cisco Systems, Inc. All rights reserved.
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
#include "opal/mca/carto/carto.h"
#include "carto_auto_detect.h"

/*
 * Public string showing the carto ompi_file component version number
 */
const char *opal_carto_auto_detect_component_version_string =
    "OPAL auto detect carto MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int auto_detect_open(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_carto_base_component_2_0_0_t mca_carto_auto_detect_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        OPAL_CARTO_BASE_VERSION_2_0_0,

        /* Component name and version */
        "auto_detect",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        auto_detect_open,
        NULL,
        opal_carto_auto_detect_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int auto_detect_open(void)
{
    mca_base_param_reg_int(&mca_carto_auto_detect_component.base_version,
                           "priority",
                           "Priority of the auto_detect carto component",
                           false, false, 11, NULL);

    return OPAL_SUCCESS;
}

