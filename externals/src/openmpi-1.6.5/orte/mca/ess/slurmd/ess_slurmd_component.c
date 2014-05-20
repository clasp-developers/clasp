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

#include "orte/util/proc_info.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/slurmd/ess_slurmd.h"

extern orte_ess_base_module_t orte_ess_slurmd_module;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_ess_base_component_t mca_ess_slurmd_component = {
    {
        ORTE_ESS_BASE_VERSION_2_0_0,

        /* Component name and version */
        "slurmd",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        orte_ess_slurmd_component_open,
        orte_ess_slurmd_component_close,
        orte_ess_slurmd_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


int
orte_ess_slurmd_component_open(void)
{
    return ORTE_SUCCESS;
}


int orte_ess_slurmd_component_query(mca_base_module_t **module, int *priority)
{
    /* Are we an MPI proc running under a SLURM job? Were
     * we given a path back to the HNP? If the
     * answer to the first is "yes" and the second
     * is "no", then we were not launched
     * by mpirun but are in a slurm world
     */
    
    if (ORTE_PROC_IS_MPI &&
        NULL != getenv("SLURM_JOBID") &&
        NULL != getenv("SLURM_STEPID") &&
        NULL == orte_process_info.my_hnp_uri) {
        *priority = 30;
        *module = (mca_base_module_t *)&orte_ess_slurmd_module;
        return ORTE_SUCCESS;
    }
    
    /* Sadly, no */
    *priority = -1;
    *module = NULL;
    return ORTE_ERROR;
}


int
orte_ess_slurmd_component_close(void)
{
    return ORTE_SUCCESS;
}

