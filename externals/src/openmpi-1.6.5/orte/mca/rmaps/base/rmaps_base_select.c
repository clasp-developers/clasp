/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include <string.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/rmaps/base/base.h"

/*
 * Function for selecting one component from all those that are
 * available.
 */
int orte_rmaps_base_select(void)
{
    orte_rmaps_base_component_t *best_component = NULL;
    orte_rmaps_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("rmaps", orte_rmaps_base.rmaps_output,
                                        &orte_rmaps_base.available_components,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected */
        return ORTE_ERR_NOT_FOUND;
    }

    /* Save the winner */
    /* No global component structure */
    orte_rmaps_base.active_module = best_module;

    return ORTE_SUCCESS;;
}
