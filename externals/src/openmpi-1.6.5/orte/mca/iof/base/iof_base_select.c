/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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


#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/base.h"

/**
 * Call the init function on all available components to find out if
 * they want to run.  Select the single component with the highest 
 * priority.
 */
int orte_iof_base_select(void)
{
    int exit_status = ORTE_SUCCESS;
    orte_iof_base_component_t *best_component = NULL;
    orte_iof_base_module_t *best_module = NULL;
    
    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("iof", orte_iof_base.iof_output,
                                        &orte_iof_base.iof_components_opened,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected, which
         * is an error.
         *
         * NOTE: processes do not open/select the IOF - only daemons,
         * the HNP, and tools do. 
         */
        exit_status = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    
    /* Save the winner */
    orte_iof = *best_module;

cleanup:
    return exit_status;
    
}

