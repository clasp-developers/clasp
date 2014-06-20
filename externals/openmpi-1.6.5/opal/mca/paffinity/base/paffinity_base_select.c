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
 * Copyright (c) 2007-2010 Cisco Systems, Inc. All rights reserved.
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
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"

/*
 * Globals
 */
bool opal_paffinity_base_selected = false;
const opal_paffinity_base_component_2_0_0_t *opal_paffinity_base_component = NULL;
const opal_paffinity_base_module_1_1_0_t *opal_paffinity_base_module = NULL;


int opal_paffinity_base_select(void)
{
    int ret = OPAL_SUCCESS;
    opal_paffinity_base_component_2_0_0_t *best_component = NULL;
    opal_paffinity_base_module_1_1_0_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("paffinity", opal_paffinity_base_output,
                                        &opal_paffinity_base_components_opened,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* It is okay if we don't find a module - we will report an
         * error if/when someone tries to actually use affinity
         */
        return OPAL_SUCCESS;
    }

    /* Save the winner */
    opal_paffinity_base_component = best_component;
    opal_paffinity_base_module    = best_module;
    opal_paffinity_base_selected  = true;

    /* Initialize the winner */
    if (NULL != opal_paffinity_base_module) {
        ret = opal_paffinity_base_module->paff_module_init();
    }

    return ret;
}
