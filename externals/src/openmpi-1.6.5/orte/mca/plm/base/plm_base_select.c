/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
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

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "orte/util/proc_info.h"

#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/base/base.h"


/**
* Function for selecting one component from all those that are
 * available.
 */

int orte_plm_base_select(void)
{
    int exit_status = ORTE_SUCCESS;
    orte_plm_base_component_t *best_component = NULL;
    orte_plm_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("plm", orte_plm_globals.output,
                                        &orte_plm_base.available_components,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected
         *
         * If we didn't find one, and we are a daemon, then default to retaining the proxy.
         * Otherwise, if we didn't find one to select, that is unacceptable. 
         */
        if (ORTE_PROC_IS_DAEMON) {
            /* don't record a selected component or flag selected
             * so we finalize correctly - just leave the plm alone
             * as it defaults to pointing at the proxy
             */
            goto cleanup;
        } else {
            exit_status = ORTE_ERR_NOT_FOUND;
            goto cleanup;
        }
    }

    /* Save the winner */
    orte_plm = *best_module;
    orte_plm_base.selected_component = *best_component;
    orte_plm_base.selected = true;
    
 cleanup:
    return exit_status;
}
