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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
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
#include "opal/mca/pstat/pstat.h"
#include "pstat_darwin.h"

/*
 * Public string showing the pstat ompi_darwin component version number
 */
const char *opal_pstat_darwin_component_version_string =
    "OPAL darwin pstat MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int pstat_darwin_component_query(mca_base_module_t **module, int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const opal_pstat_base_component_t mca_pstat_darwin_component = {

    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a pstat v1.1.0 component (which also
           implies a specific MCA version) */
        
        OPAL_PSTAT_BASE_VERSION_2_0_0,

        /* Component name and version */

        "darwin",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */

        NULL,
        NULL,
        pstat_darwin_component_query,
        NULL
    },
    /* Next the MCA v1.0.0 component meta data */
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int pstat_darwin_component_query(mca_base_module_t **module, int *priority)
{
    *priority = 20;
    *module = (mca_base_module_t *)&opal_pstat_darwin_module;
    
    return OPAL_SUCCESS;
}
