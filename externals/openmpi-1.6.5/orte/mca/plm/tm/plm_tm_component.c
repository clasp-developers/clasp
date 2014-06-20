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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
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

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"


#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "plm_tm.h"


/*
 * Public string showing the plm ompi_tm component version number
 */
const char *mca_plm_tm_component_version_string =
  "Open MPI tm plm MCA component version " ORTE_VERSION;



/*
 * Local function
 */
static int plm_tm_open(void);
static int plm_tm_close(void);
static int orte_plm_tm_component_query(mca_base_module_t **module, int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_plm_tm_component_t mca_plm_tm_component = {
    {
        /* First, the mca_component_t struct containing meta information
           about the component itself */

        {
            ORTE_PLM_BASE_VERSION_2_0_0,

            /* Component name and version */
            "tm",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,

            /* Component open and close functions */
            plm_tm_open,
            plm_tm_close,
            orte_plm_tm_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


static int plm_tm_open(void)
{
    int tmp;
    mca_base_component_t *comp = &mca_plm_tm_component.super.base_version;

    mca_base_param_reg_int(comp, "want_path_check",
                           "Whether the launching process should check for the plm_tm_orted executable in the PATH before launching (the TM API does not give an indication of failure; this is a somewhat-lame workaround; non-zero values enable this check)",
                           false, false, (int) true, &tmp);
    mca_plm_tm_component.want_path_check = OPAL_INT_TO_BOOL(tmp);
    
    mca_plm_tm_component.checked_paths = NULL;
   
    return ORTE_SUCCESS;
}


static int plm_tm_close(void)
{
    if (NULL != mca_plm_tm_component.checked_paths) {
        opal_argv_free(mca_plm_tm_component.checked_paths);
    }

    return ORTE_SUCCESS;
}


static int orte_plm_tm_component_query(mca_base_module_t **module, int *priority)
{
    /* Are we running under a TM job? */

    if (NULL != getenv("PBS_ENVIRONMENT") &&
        NULL != getenv("PBS_JOBID")) {

        *priority = 75;
        *module = (mca_base_module_t *) &orte_plm_tm_module;
        return ORTE_SUCCESS;
    }

    /* Sadly, no */
    *module = NULL;
    return ORTE_ERROR;
}
