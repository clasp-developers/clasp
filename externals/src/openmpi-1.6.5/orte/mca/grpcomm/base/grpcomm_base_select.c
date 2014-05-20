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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/grpcomm/base/base.h"


/**
 * Function for selecting one component from all those that are
 * available.
 */
int orte_grpcomm_base_select(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_grpcomm_base_component_t *best_component = NULL;
    orte_grpcomm_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("grpcomm", orte_grpcomm_base_output,
                                        &mca_grpcomm_base_components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* This will only happen if no component was selected */
        exit_status = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* Save the winner */
    /* No global component structure */
    orte_grpcomm = *best_module;

    if (ORTE_SUCCESS != (ret = orte_grpcomm.init()) ) {
        exit_status = ret;
        goto cleanup;
    }

    mca_grpcomm_base_selected = true;

 cleanup:
    return exit_status;
}
