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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "orte/mca/ess/base/base.h"

extern opal_list_t orte_ess_base_components_available;

int
orte_ess_base_close(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;

    /* unload all remaining components */
    while (NULL != (item = opal_list_remove_first(&orte_ess_base_components_available))) {
        orte_ess_base_component_t* component;
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_ess_base_component_t *) cli->cli_component;
        opal_output_verbose(10, 0,
                            "orte_ess_base_close: module %s unloaded",
                            component->base_version.mca_component_name);
        mca_base_component_repository_release((mca_base_component_t *) component);
        OBJ_RELEASE(item);
    }
    
    OBJ_DESTRUCT(&orte_ess_base_components_available);
    opal_output_close(orte_ess_base_output);
    return ORTE_SUCCESS;
}

