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
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/notifier/base/base.h"


/**
 * Function for selecting one component from all those that are
 * available.
 */
int orte_notifier_base_select(void)
{
    int ret, exit_status = ORTE_SUCCESS;
    orte_notifier_base_component_t *best_component = NULL;
    orte_notifier_base_module_t *best_module = NULL;
    char *include_list = NULL;

    /*
     * Register the framework MCA param and look up include list
     */
    mca_base_param_reg_string_name("notifier", NULL,
                                   "Which notifier component to use (empty = none)",
                                   false, false,
                                   NULL, &include_list);
    
    /* If we do not have any components to select this is ok. Just use the default
     * "no-op" component and move on.
     */
    if( 0 >= opal_list_get_size(&mca_notifier_base_components_available) || NULL == include_list) { 
        /* Close all components since none will be used */
        mca_base_components_close(orte_notifier_base_output,
                                  &mca_notifier_base_components_available,
                                  NULL, false);
        goto cleanup;
    }
    
    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != mca_base_select("notifier", orte_notifier_base_output,
                                        &mca_notifier_base_components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component) ) {
        /* It is okay if no component was selected - we just leave
         * the orte_notifier module as the default
         */
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

    if (NULL != orte_notifier.init) {
        /* if an init function is provided, use it */
        if (ORTE_SUCCESS != (ret = orte_notifier.init()) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

    /* Save the winner */
    orte_notifier = *best_module;

 cleanup:
    return exit_status;
}
