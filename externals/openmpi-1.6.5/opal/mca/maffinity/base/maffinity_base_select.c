/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"

/*
 * Globals
 */
bool opal_maffinity_base_selected = false;
const opal_maffinity_base_component_2_0_0_t *opal_maffinity_base_component = NULL;
const opal_maffinity_base_module_1_0_0_t *opal_maffinity_base_module = NULL;


int opal_maffinity_base_select(void)
{
    int ret, exit_status = OPAL_SUCCESS;
    opal_maffinity_base_component_2_0_0_t *best_component = NULL;
    opal_maffinity_base_module_1_0_0_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("maffinity", opal_maffinity_base_output,
                                        &opal_maffinity_base_components_opened,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected */
        exit_status = OPAL_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Save the winner */
    opal_maffinity_base_component = best_component;
    opal_maffinity_base_module    = best_module;
    opal_maffinity_base_selected  = true;

    /* Initialize the winner */
    if (NULL != opal_maffinity_base_module) {
        if (OPAL_SUCCESS != (ret = opal_maffinity_base_module->maff_module_init()) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    return exit_status;
}

