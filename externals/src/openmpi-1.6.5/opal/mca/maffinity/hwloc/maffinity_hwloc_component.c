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
 * Copyright (c) 2007-2011 Cisco Systems, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>

#include "opal/constants.h"
#include "opal/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/maffinity/maffinity.h"
#include "maffinity_hwloc.h"

/*
 * Public string showing the maffinity ompi_hwloc component version number
 */
const char *opal_maffinity_hwloc_component_version_string =
    "OPAL hwloc maffinity MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int hwloc_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_maffinity_hwloc_component_2_0_0_t mca_maffinity_hwloc_component = {
    {
        /* First, the mca_component_t struct containing meta information
           about the component itself */
        {
            OPAL_MAFFINITY_BASE_VERSION_2_0_0,
            
            /* Component name and version */
            "hwloc",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,
            
            /* Component open and close functions */
            NULL,
            NULL,
            opal_maffinity_hwloc_component_query,
            hwloc_register,
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    },

    /* Priority */
    40,

    /* NULL fill the rest of the component data */
};


static int hwloc_register(void)
{
    mca_base_param_reg_int(&mca_maffinity_hwloc_component.base.base_version,
                           "priority",
                           "Priority of the hwloc maffinity component",
                           false, false, 40, 
                           &mca_maffinity_hwloc_component.priority);

    return OPAL_SUCCESS;
}
