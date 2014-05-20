/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "opal/util/output.h"
#include "orte/constants.h"



#include "orte/mca/filem/filem.h"
#include "orte/mca/filem/base/base.h"
#include "filem_rsh.h"

/*
 * Public string for version number
 */
const char *orte_filem_rsh_component_version_string = 
"ORTE FILEM rsh MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int filem_rsh_open(void);
static int filem_rsh_close(void);

int orte_filem_rsh_max_incomming = 10;
int orte_filem_rsh_max_outgoing = 10;

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_filem_rsh_component_t mca_filem_rsh_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itrsh
         */
        {
            ORTE_FILEM_BASE_VERSION_2_0_0,
            /* Component name and version */
            "rsh",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            filem_rsh_open,
            filem_rsh_close,
            orte_filem_rsh_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },

        /* Verbosity level */
        0,
        /* opal_output handler */
        -1,
        /* Default priority */
        20
    },

    /* cp_command */
    NULL,

    /* cp_local_command */
    NULL,

    /* remote_sh_command */
    NULL
};

static int filem_rsh_open(void) 
{
    mca_base_param_reg_int(&mca_filem_rsh_component.super.base_version,
                           "priority",
                           "Priority of the FILEM rsh component",
                           false, false,
                           mca_filem_rsh_component.super.priority,
                           &mca_filem_rsh_component.super.priority);
    
    mca_base_param_reg_int(&mca_filem_rsh_component.super.base_version,
                           "verbose",
                           "Verbose level for the FILEM rsh component",
                           false, false,
                           mca_filem_rsh_component.super.verbose, 
                           &mca_filem_rsh_component.super.verbose);
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_filem_rsh_component.super.verbose) {
        mca_filem_rsh_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_filem_rsh_component.super.output_handle,
                                  mca_filem_rsh_component.super.verbose);
    } else {
        mca_filem_rsh_component.super.output_handle = orte_filem_base_output;
    }
    
    mca_base_param_reg_string(&mca_filem_rsh_component.super.base_version,
                              "rcp",
                              "The rsh cp command for the FILEM rsh component",
                              false, false,
                              "scp",
                              &mca_filem_rsh_component.cp_command);
    mca_base_param_reg_string(&mca_filem_rsh_component.super.base_version,
                              "cp",
                              "The Unix cp command for the FILEM rsh component",
                              false, false,
                              "cp",
                              &mca_filem_rsh_component.cp_local_command);
    mca_base_param_reg_string(&mca_filem_rsh_component.super.base_version,
                              "rsh",
                              "The remote shell command for the FILEM rsh component",
                              false, false,
                              "ssh",
                              &mca_filem_rsh_component.remote_sh_command);

    mca_base_param_reg_int(&mca_filem_rsh_component.super.base_version,
                           "max_incomming",
                           "Maximum number of incomming connections (0 = any)",
                           false, false,
                           orte_filem_rsh_max_incomming,
                           &orte_filem_rsh_max_incomming);

    if( orte_filem_rsh_max_incomming < 0 ) {
        orte_filem_rsh_max_incomming = 1;
    }

    mca_base_param_reg_int(&mca_filem_rsh_component.super.base_version,
                           "max_outgoing",
                           "Maximum number of out going connections (0 = any)",
                           false, false,
                           orte_filem_rsh_max_outgoing,
                           &orte_filem_rsh_max_outgoing);

    if( orte_filem_rsh_max_outgoing < 0 ) {
        orte_filem_rsh_max_outgoing = 1;
    }

    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_filem_rsh_component.super.output_handle,
                        "filem:rsh: open()");
    opal_output_verbose(20, mca_filem_rsh_component.super.output_handle,
                        "filem:rsh: open: priority   = %d", 
                        mca_filem_rsh_component.super.priority);
    opal_output_verbose(20, mca_filem_rsh_component.super.output_handle,
                        "filem:rsh: open: verbosity  = %d", 
                        mca_filem_rsh_component.super.verbose);
    opal_output_verbose(20, mca_filem_rsh_component.super.output_handle,
                        "filem:rsh: open: cp command  = %s", 
                        mca_filem_rsh_component.cp_command);
    opal_output_verbose(20, mca_filem_rsh_component.super.output_handle,
                        "filem:rsh: open: cp local command  = %s", 
                        mca_filem_rsh_component.cp_local_command);
    opal_output_verbose(20, mca_filem_rsh_component.super.output_handle,
                        "filem:rsh: open: rsh command  = %s", 
                        mca_filem_rsh_component.remote_sh_command);

    return ORTE_SUCCESS;
}

static int filem_rsh_close(void)
{
    opal_output_verbose(10, mca_filem_rsh_component.super.output_handle,
                        "filem:rsh: close()");

    return ORTE_SUCCESS;
}
