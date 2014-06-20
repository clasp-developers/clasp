/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
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

#include "ompi_config.h"

#include "opal/util/output.h"

#include "ompi/mca/crcp/crcp.h"
#include "ompi/mca/crcp/base/base.h"
#include "crcp_bkmrk.h"

/*
 * Public string for version number
 */
const char *ompi_crcp_bkmrk_component_version_string = 
"OMPI CRCP bkmrk MCA component version " OMPI_VERSION;

int timing_enabled = 0;

/*
 * Local functionality
 */
static int crcp_bkmrk_open(void);
static int crcp_bkmrk_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
ompi_crcp_bkmrk_component_t mca_crcp_bkmrk_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component
         */
        {
            OMPI_CRCP_BASE_VERSION_2_0_0,
            /* Component name and version */
            "bkmrk",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,
            
            /* Component open and close functions */
            crcp_bkmrk_open,
            crcp_bkmrk_close,
            ompi_crcp_bkmrk_component_query
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
    }
};

static int crcp_bkmrk_open(void) 
{
    int val;

    /*
     * This should be the last componet to ever get used since
     * it doesn't do anything.
     */
    mca_base_param_reg_int(&mca_crcp_bkmrk_component.super.base_version,
                           "priority",
                           "Priority of the CRCP bkmrk component",
                           false, false,
                           mca_crcp_bkmrk_component.super.priority,
                           &mca_crcp_bkmrk_component.super.priority);
    
    mca_base_param_reg_int(&mca_crcp_bkmrk_component.super.base_version,
                           "verbose",
                           "Verbose level for the CRCP bkmrk component",
                           false, false,
                           mca_crcp_bkmrk_component.super.verbose, 
                           &mca_crcp_bkmrk_component.super.verbose);
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_crcp_bkmrk_component.super.verbose) {
        mca_crcp_bkmrk_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_crcp_bkmrk_component.super.output_handle,
                                  mca_crcp_bkmrk_component.super.verbose);
    } else {
        mca_crcp_bkmrk_component.super.output_handle = ompi_crcp_base_output;
    }

    mca_base_param_reg_int(&mca_crcp_bkmrk_component.super.base_version,
                           "timing",
                           "Enable Performance timing",
                           false, false,
                           0,
                           &val);
    timing_enabled = val;

    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: open()");
    opal_output_verbose(20, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: open: priority   = %d", 
                        mca_crcp_bkmrk_component.super.priority);
    opal_output_verbose(20, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: open: verbosity  = %d", 
                        mca_crcp_bkmrk_component.super.verbose);

    return OMPI_SUCCESS;
}

static int crcp_bkmrk_close(void)
{
    opal_output_verbose(10, mca_crcp_bkmrk_component.super.output_handle,
                        "crcp:bkmrk: close()");

    return OMPI_SUCCESS;
}
