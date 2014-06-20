/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"

#include "opal/mca/base/mca_base_param.h"
#include "orte/constants.h"

#include "orte/util/proc_info.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "plm_ccp.h"

/* Import the Windows CCP API. */
#import "ccpapi.tlb" named_guids no_namespace raw_interfaces_only   \
    rename("SetEnvironmentVariable","SetEnvVar")                    \
    rename("GetJob", "GetSingleJob")                                \
    rename("AddJob", "AddSingleJob")


/*
 * Public string showing the plm ompi_ccp component version number
 */
const char *mca_plm_ccp_component_version_string =
  "Open MPI ccp plm MCA component version " ORTE_VERSION;



/*
 * Local function
 */
static int plm_ccp_open(void);
static int plm_ccp_close(void);
static int orte_plm_ccp_component_query(mca_base_module_t **module, int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_plm_ccp_component_t mca_plm_ccp_component = {
    {
        /* First, the mca_component_t struct containing meta information
           about the component itself */

        {
            ORTE_PLM_BASE_VERSION_2_0_0,

            /* Component name and version */
            "ccp",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,

            /* Component open and close functions */
            plm_ccp_open,
            plm_ccp_close,
            orte_plm_ccp_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


static int plm_ccp_open(void)
{
    int tmp, value;
    mca_base_component_t *comp = &mca_plm_ccp_component.super.base_version;

    mca_base_param_reg_int(comp, "debug", "Enable debugging of the CCP plm",
                           false, false, 0, &mca_plm_ccp_component.debug);
    mca_base_param_reg_int(comp, "verbose", "Enable verbose output of the ccp plm",
                           false, false, 0, &mca_plm_ccp_component.verbose);

    mca_base_param_reg_int(comp, "priority", "Default selection priority",
                           false, false, 75, &mca_plm_ccp_component.priority);

    mca_base_param_reg_int(comp, "want_path_check",
                           "Whether the launching process should check for "
                           "the plm_ccp_orted executable in the PATH before "
                           "launching (the CCP API does not give an indication "
                           "of failure; this is a somewhat-lame workaround; "
                           "non-zero values enable this check)",
                           false, false, (int) true, &tmp);
    mca_plm_ccp_component.want_path_check = OPAL_INT_TO_BOOL(tmp);

    mca_base_param_reg_string(comp, "stdout_file",
                              "Path and file name for stdout on cluster nodes. "
                              "By default, stdout will be sent to Job Scheduler. "
                              "If no path specified, the user home path will be used. "
                              "UNC path will not work for this param. ",
                              false, false, NULL,
                              &mca_plm_ccp_component.stdout_file);
    
    mca_base_param_reg_string(comp, "stderr_file",
                              "Path and file name for stderr on cluster nodes. "
                              "By default, stderr will be sent to Job Scheduler. "
                              "If no path specified, the user home path will be used. "
                              "UNC path will not work for this param. ",
                              false, false, NULL,
                              &mca_plm_ccp_component.stderr_file);

    mca_base_param_reg_string(comp, "job_name",
                              "The job name for displaying in the scheduler. "
                              "It is set to the application name by default.",
                              false, false, NULL,
                              &mca_plm_ccp_component.job_name);

    tmp = mca_base_param_reg_int_name("orte", "timing",
                                      "Request that critical timing loops be measured",
                                      false, false, 0, &value);
    if (value != 0) {
        mca_plm_ccp_component.timing = true;
    } else {
        mca_plm_ccp_component.timing = false;
    }
    
    mca_plm_ccp_component.checked_paths = NULL;

    return ORTE_SUCCESS;
}


static int plm_ccp_close(void)
{
    return ORTE_SUCCESS;
}


static int orte_plm_ccp_component_query(mca_base_module_t **module, int *priority)
{
    ICluster* pCluster = NULL;
    HRESULT hr = S_OK;

    /* CCP is not thread safe. Use the apartment model. */
    CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);

    /* Try to create the Cluster object. */
    hr = CoCreateInstance( __uuidof(Cluster),
                           NULL,
                           CLSCTX_INPROC_SERVER,
                           __uuidof(ICluster),
                           reinterpret_cast<void **> (&pCluster) );
    if (FAILED(hr)) {
        /* We are not Windows clusters, don't select us.*/
        *module = NULL;
        return ORTE_ERROR;
    }

    /* if we are NOT an HNP, then don't select us */
    if (!ORTE_PROC_IS_HNP) {
        pCluster->Release();
        *module = NULL;
        return ORTE_ERROR;
    }

    /* We are Windows clusters and this is HNP. */
    pCluster->Release();
    *priority = mca_plm_ccp_component.priority;
    *module = (mca_base_module_t *) &orte_plm_ccp_module;
    return ORTE_SUCCESS;
}
