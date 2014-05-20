/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/snapc/snapc.h"
#include "orte/mca/snapc/base/base.h"


static orte_snapc_base_component_t none_component = {
    /* Handle the general mca_component_t struct containing 
     *  meta information about the component itself
     */
    {
        ORTE_SNAPC_BASE_VERSION_2_0_0,
        /* Component name and version */
        "none",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        orte_snapc_base_none_open,
        orte_snapc_base_none_close,
        orte_snapc_base_none_query
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
    1
};

static orte_snapc_base_module_t none_module = {
    /** Initialization Function */
    orte_snapc_base_module_init,
    /** Finalization Function */
    orte_snapc_base_module_finalize,
    orte_snapc_base_none_setup_job,
    orte_snapc_base_none_release_job,
    orte_snapc_base_none_ft_event,
    orte_snapc_base_none_start_ckpt,
    orte_snapc_base_none_end_ckpt
};

int orte_snapc_base_select(bool seed, bool app)
{
    int exit_status = OPAL_SUCCESS;
    orte_snapc_base_component_t *best_component = NULL;
    orte_snapc_base_module_t *best_module = NULL;
    char *include_list = NULL;

    /*
     * Register the framework MCA param and look up include list
     */
    mca_base_param_reg_string_name("snapc", NULL,
                                   "Which SNAPC component to use (empty = auto-select)",
                                   false, false,
                                   strdup("none"), &include_list);
    if(NULL != include_list && 0 == strncmp(include_list, "none", strlen("none")) ){ 
        opal_output_verbose(10, orte_snapc_base_output,
                            "snapc:select: Using %s component",
                            include_list);
        best_component = &none_component;
        best_module    = &none_module;
        /* Close all components since none will be used */
        mca_base_components_close(orte_snapc_base_output,
                                  &orte_snapc_base_components_available,
                                  NULL, false);
        /* JJH: Todo: Check if none is in the list */
        goto skip_select;
    }

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("snapc", orte_snapc_base_output,
                                        &orte_snapc_base_components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected */
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

 skip_select:
    /* Save the winner */
    orte_snapc_base_selected_component = *best_component;
    orte_snapc = *best_module;

    /* Initialize the winner */
    if (NULL != best_module) {
        if (OPAL_SUCCESS != orte_snapc.snapc_init(seed, app)) {
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }

 cleanup:
    if( NULL != include_list ) {
        free(include_list);
        include_list = NULL;
    }

    return exit_status;
}
