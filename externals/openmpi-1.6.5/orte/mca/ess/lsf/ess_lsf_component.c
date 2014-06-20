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
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <lsf/lsbatch.h>

#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/lsf/ess_lsf.h"

extern orte_ess_base_module_t orte_ess_lsf_module;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_ess_base_component_t mca_ess_lsf_component = {
    {
        ORTE_ESS_BASE_VERSION_2_0_0,

        /* Component name and version */
        "lsf",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_ess_lsf_component_open,
        orte_ess_lsf_component_close,
        orte_ess_lsf_component_query
    },
    {
        /* The component is not checkpoint ready */
        MCA_BASE_METADATA_PARAM_NONE
    }
};


int orte_ess_lsf_component_open(void)
{
    return ORTE_SUCCESS;
}


int orte_ess_lsf_component_query(mca_base_module_t **module, int *priority)
{
    /* Are we running under an LSF job? Were
     * we given a path back to the HNP? If the
     * answer to both is "yes", then we were launched
     * by mpirun in an LSF world
     */
    
    if (NULL != getenv("LSB_JOBID") &&
        NULL != orte_process_info.my_hnp_uri) {
        *priority = 40;
        *module = (mca_base_module_t *)&orte_ess_lsf_module;
        return ORTE_SUCCESS;
    }
   
    /* nope, not here */
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}


int orte_ess_lsf_component_close(void)
{
    return ORTE_SUCCESS;
}

