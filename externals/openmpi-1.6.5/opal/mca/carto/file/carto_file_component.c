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
#include "carto_file.h"

/*
 * Public string showing the carto ompi_file component version number
 */
const char *opal_carto_file_component_version_string =
    "OPAL file carto MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int file_open(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_carto_base_component_2_0_0_t mca_carto_file_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        OPAL_CARTO_BASE_VERSION_2_0_0,

        /* Component name and version */
        "file",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        file_open,
        NULL,
        opal_carto_file_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int file_open(void)
{


    mca_base_param_reg_string(&mca_carto_file_component.base_version,
                              "path",
                              "The path to the cartography file",
                              false, false, NULL, &carto_file_path);

    /**
     * If the user specified the carto file path (not NULL), use the
     * carto file component. The auto detect component is with
     * higher priority, so by default it will be chosen.
     */
    if (NULL == carto_file_path) {
        mca_base_param_reg_int(&mca_carto_file_component.base_version,
                               "priority",
                               "Priority of the file carto component",
                               false, false, 10, NULL);
    }else{
        mca_base_param_reg_int(&mca_carto_file_component.base_version,
                               "priority",
                               "Priority of the file carto component",
                               false, false, 12, NULL);
    }
    return OPAL_SUCCESS;
}
