/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/mca/base/mca_base_param.h"


#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/odls/process/odls_process.h"

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_odls_base_component_t mca_odls_process_component = {
    /* First, the mca_component_t struct containing meta information
    about the component itself */
    {
        ORTE_ODLS_BASE_VERSION_2_0_0,
        /* Component name and version */
        "process",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_odls_process_component_open,
        orte_odls_process_component_close,
        orte_odls_process_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

int orte_odls_process_component_open(void)
{
    return ORTE_SUCCESS;
}

int orte_odls_process_component_query(mca_base_module_t **module, int *priority)
{
    /* the base open/select logic protects us against operation when
     * we are NOT in a daemon, so we don't have to check that here
     */
    
    *priority = 1;
    *module = (mca_base_module_t *) &orte_odls_process_module;
    return ORTE_SUCCESS;
}

int orte_odls_process_component_close(void)
{
    return ORTE_SUCCESS;
}

int orte_odls_process_component_finalize(void)
{
    return ORTE_SUCCESS;
}
