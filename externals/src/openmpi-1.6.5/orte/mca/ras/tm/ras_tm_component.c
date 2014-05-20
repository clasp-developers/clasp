/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
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

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/basename.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_tm.h"


/*
 * Local variables
 */
static int param_priority;


/*
 * Local functions
 */
static int ras_tm_open(void);
static int orte_ras_tm_component_query(mca_base_module_t **module, int *priority);


orte_ras_tm_component_t mca_ras_tm_component = {
    {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */

        {
            ORTE_RAS_BASE_VERSION_2_0_0,
            
            /* Component name and version */
            "tm",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            ras_tm_open,
            NULL,
            orte_ras_tm_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


static int ras_tm_open(void)
{
    mca_base_component_t *c        = &mca_ras_tm_component.super.base_version;
    char *pbs_nodefile_env         = NULL;
    char *default_nodefile_dir     = NULL;
    bool free_default_nodefile_dir = false;
    
    param_priority = 
        mca_base_param_reg_int(c,
                               "priority",
                               "Priority of the tm ras component",
                               false, false, 100, NULL);

    /* try to detect the default directory */
    pbs_nodefile_env = getenv("PBS_NODEFILE");
    if ( NULL != pbs_nodefile_env ) {
        default_nodefile_dir = opal_dirname(pbs_nodefile_env);
        if ( NULL != default_nodefile_dir ) {
            free_default_nodefile_dir = true;
        } else {
            default_nodefile_dir = "/var/torque/aux";
        }
    } else {
        default_nodefile_dir = "/var/torque/aux";
    }
    
    mca_base_param_reg_string(c, "nodefile_dir",
                              "The directory where the PBS nodefile can be found",
                              false, false, default_nodefile_dir,
                              &mca_ras_tm_component.nodefile_dir);
    
    if ( free_default_nodefile_dir ) {
        free(default_nodefile_dir);
    }
            
    return ORTE_SUCCESS;
}


static int orte_ras_tm_component_query(mca_base_module_t **module, int *priority)
{
    /* Are we running under a TM job? */
    if (NULL != getenv("PBS_ENVIRONMENT") &&
        NULL != getenv("PBS_JOBID")) {
        mca_base_param_lookup_int(param_priority, priority);
        *module = (mca_base_module_t *) &orte_ras_tm_module;
        return ORTE_SUCCESS;
    }

    /* Sadly, no */
    *module = NULL;
    return ORTE_ERROR;
}
