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
 * Copyright (c) 2007      Cisco Systems, Inc. All rights reserved.
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
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"

/*
 * Globals
 */
bool opal_carto_base_selected = false;
const opal_carto_base_component_2_0_0_t *opal_carto_base_component = NULL;
const opal_carto_base_module_1_0_0_t *opal_carto_base_module = NULL;


int opal_carto_base_select(void)
{
    int exit_status = OPAL_SUCCESS;
    opal_carto_base_component_2_0_0_t *best_component = NULL;
    opal_carto_base_module_1_0_0_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("carto", opal_carto_base_output,
                                        &opal_carto_base_components_opened,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected, so
         * use the default module instead
         */
        opal_carto_base_module    = &opal_carto_default_module;
        opal_carto_base_selected  = true;
        goto cleanup;
    }

    /* Save the winner */
    opal_carto_base_component = best_component;
    opal_carto_base_module    = best_module;
    opal_carto_base_selected  = true;

cleanup:
    /* Initialize the winner */
    if (NULL != opal_carto_base_module) {
        exit_status = opal_carto_base_module->carto_module_init();
    }

    return exit_status;
}

