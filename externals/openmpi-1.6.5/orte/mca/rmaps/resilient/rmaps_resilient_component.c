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

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "rmaps_resilient.h"

/*
 * Local functions
 */

static int orte_rmaps_resilient_open(void);
static int orte_rmaps_resilient_close(void);
static int orte_rmaps_resilient_query(mca_base_module_t **module, int *priority);

orte_rmaps_res_component_t mca_rmaps_resilient_component = {
    {
        {
            ORTE_RMAPS_BASE_VERSION_2_0_0,
            
            "resilient", /* MCA component name */
            ORTE_MAJOR_VERSION,  /* MCA component major version */
            ORTE_MINOR_VERSION,  /* MCA component minor version */
            ORTE_RELEASE_VERSION,  /* MCA component release version */
            orte_rmaps_resilient_open,  /* component open  */
            orte_rmaps_resilient_close, /* component close */
            orte_rmaps_resilient_query  /* component query */
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


/**
  * component open/close/init function
  */
static int orte_rmaps_resilient_open(void)
{
    mca_base_component_t *c = &mca_rmaps_resilient_component.super.base_version;

    /* initialize globals */
    OBJ_CONSTRUCT(&mca_rmaps_resilient_component.fault_grps, opal_list_t);
    
    /* lookup parameters */
    mca_base_param_reg_string(c, "fault_grp_file",
                              "Filename that contains a description of fault groups for this system",
                              false, false, NULL,  &mca_rmaps_resilient_component.fault_group_file);
    
    return ORTE_SUCCESS;
}


static int orte_rmaps_resilient_query(mca_base_module_t **module, int *priority)
{    
    *priority = 0;  /* select only if specified */
    *module = (mca_base_module_t *)&orte_rmaps_resilient_module;
    
    /* if a fault group file was provided, we definitely want to be selected */
    if (NULL != mca_rmaps_resilient_component.fault_group_file) {
        *priority = 1000;
    }
    
    return ORTE_SUCCESS;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_resilient_close(void)
{
    opal_list_item_t *item;
    
    while (NULL != (item = opal_list_remove_first(&mca_rmaps_resilient_component.fault_grps))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&mca_rmaps_resilient_component.fault_grps);
    
    if (NULL != mca_rmaps_resilient_component.fault_group_file) {
        free(mca_rmaps_resilient_component.fault_group_file);
    }
    
    return ORTE_SUCCESS;
}

static void ftgrp_res_construct(orte_rmaps_res_ftgrp_t *ptr)
{
    ptr->ftgrp = -1;
    ptr->used = false;
    ptr->included = false;
    OBJ_CONSTRUCT(&ptr->nodes, opal_pointer_array_t);
    opal_pointer_array_init(&ptr->nodes,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE);    
}
static void ftgrp_res_destruct(orte_rmaps_res_ftgrp_t *ptr)
{
    int n;
    orte_node_t *node;
    
    for (n=0; n < ptr->nodes.size; n++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(&ptr->nodes, n))) {
            continue;
        }
        OBJ_RELEASE(node);
    }
    OBJ_DESTRUCT(&ptr->nodes);
}
OBJ_CLASS_INSTANCE(orte_rmaps_res_ftgrp_t,
                   opal_list_item_t,
                   ftgrp_res_construct,
                   ftgrp_res_destruct);

