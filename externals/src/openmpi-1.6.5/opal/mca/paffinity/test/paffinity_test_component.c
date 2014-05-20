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
#include "opal/mca/paffinity/paffinity.h"
#include "paffinity_test.h"

/*
 * Public string showing the paffinity ompi_test component version number
 */
const char *opal_paffinity_test_component_version_string =
    "OPAL test paffinity MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int test_open(void);
static int test_query(mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_paffinity_test_component_t mca_paffinity_test_component = {
    {
        /* First, the mca_component_t struct containing meta information
         * about the component itself
         */

        {
            /* Indicate that we are a paffinity v1.1.0 component (which also
             implies a specific MCA version) */

            OPAL_PAFFINITY_BASE_VERSION_2_0_0,

            /* Component name and version */

            "test",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,

            /* Component open and close functions */

            test_open,
            NULL,
            test_query
        },
        /* Next the MCA v1.0.0 component meta data */
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


static int test_open(void)
{
    int tmp;
    
    mca_base_param_reg_int(&mca_paffinity_test_component.super.base_version, "bound",
                           "Whether or not to test as if externally bound (default=0: no)",
                           false, false, (int)false, &tmp);
    mca_paffinity_test_component.bound = OPAL_INT_TO_BOOL(tmp);
    
    mca_base_param_reg_int(&mca_paffinity_test_component.super.base_version, "num_sockets",
                           "Number of sockets on each node (default=4)",
                           false, false, 4, &mca_paffinity_test_component.num_sockets);

    mca_base_param_reg_int(&mca_paffinity_test_component.super.base_version, "num_cores",
                           "Number of cores in each socket (default=4)",
                           false, false, 4, &mca_paffinity_test_component.num_cores);
    return OPAL_SUCCESS;
}

static int test_query(mca_base_module_t **module, int *priority)
{
    /* set this priority so I can only be selected if directed */
    *priority = 00;
    *module = (mca_base_module_t *)&opal_paffinity_test_module;
    
    return OPAL_SUCCESS;
}
