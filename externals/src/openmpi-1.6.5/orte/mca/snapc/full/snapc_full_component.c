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

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"
#include "snapc_full.h"

/*
 * Public string for version number
 */
const char *orte_snapc_full_component_version_string = 
"ORTE SNAPC full MCA component version " ORTE_VERSION;

/*
 * Local functionality
 */
static int snapc_full_open(void);
static int snapc_full_close(void);

bool orte_snapc_full_skip_filem = false;
bool orte_snapc_full_skip_app   = false;
bool orte_snapc_full_timing_enabled = false;
int orte_snapc_full_max_wait_time = 20;

/*
 * Instantiate the public struct with all of our public information
 * and pointer to our public functions in it
 */
orte_snapc_full_component_t mca_snapc_full_component = {
    /* First do the base component stuff */
    {
        /* Handle the general mca_component_t struct containing 
         *  meta information about the component itfull
         */
        {
            ORTE_SNAPC_BASE_VERSION_2_0_0,
            /* Component name and version */
            "full",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            snapc_full_open,
            snapc_full_close,
            orte_snapc_full_component_query
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

static int snapc_full_open(void) 
{
    int value;

    /*
     * This should be the last componet to ever get used since
     * it doesn't do anything.
     */
    mca_base_param_reg_int(&mca_snapc_full_component.super.base_version,
                           "priority",
                           "Priority of the SNAPC full component",
                           false, false,
                           mca_snapc_full_component.super.priority,
                           &mca_snapc_full_component.super.priority);
    
    mca_base_param_reg_int(&mca_snapc_full_component.super.base_version,
                           "verbose",
                           "Verbose level for the SNAPC full component",
                           false, false,
                           mca_snapc_full_component.super.verbose, 
                           &mca_snapc_full_component.super.verbose);
    /* If there is a custom verbose level for this component than use it
     * otherwise take our parents level and output channel
     */
    if ( 0 != mca_snapc_full_component.super.verbose) {
        mca_snapc_full_component.super.output_handle = opal_output_open(NULL);
        opal_output_set_verbosity(mca_snapc_full_component.super.output_handle,
                                  mca_snapc_full_component.super.verbose);
    } else {
        mca_snapc_full_component.super.output_handle = orte_snapc_base_output;
    }

    mca_base_param_reg_int(&mca_snapc_full_component.super.base_version,
                           "skip_filem",
                           "Not for general use! For debugging only! Pretend to move files. [Default = disabled]",
                           false, false,
                           0,
                           &value);
    orte_snapc_full_skip_filem = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int(&mca_snapc_full_component.super.base_version,
                           "skip_app",
                           "Not for general use! For debugging only! Shortcut app level coord. [Default = disabled]",
                           false, false,
                           0,
                           &value);
    orte_snapc_full_skip_app = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int(&mca_snapc_full_component.super.base_version,
                           "enable_timing",
                           "Enable timing information. [Default = disabled]",
                           false, false,
                           0,
                           &value);
    orte_snapc_full_timing_enabled = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int(&mca_snapc_full_component.super.base_version,
                           "max_wait_time",
                           "Wait time before orted gives up on checkpoint (seconds)",
                           false, false,
                           20,
                           &orte_snapc_full_max_wait_time);

    /*
     * Debug Output
     */
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "snapc:full: open()");
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "snapc:full: open: priority    = %d", 
                        mca_snapc_full_component.super.priority);
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "snapc:full: open: verbosity   = %d", 
                        mca_snapc_full_component.super.verbose);
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "snapc:full: open: max_wait_time = %d", 
                        orte_snapc_full_max_wait_time);
    opal_output_verbose(20, mca_snapc_full_component.super.output_handle,
                        "snapc:full: open: skip_filem  = %s", 
                        (orte_snapc_full_skip_filem == true ? "True" : "False"));

    return ORTE_SUCCESS;
}

static int snapc_full_close(void)
{
    opal_output_verbose(10, mca_snapc_full_component.super.output_handle,
                        "snapc:full: close()");

    return ORTE_SUCCESS;
}
