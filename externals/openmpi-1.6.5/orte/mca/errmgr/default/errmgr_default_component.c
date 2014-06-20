/* -*- C -*-
 *
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
/** @file:
 *
 * The Open MPI General Purpose Registry - Proxy component
 *
 */

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"


#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"
#include "orte/util/proc_info.h"

#include "errmgr_default.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_errmgr_base_component_t mca_errmgr_default_component = {
    {
    ORTE_ERRMGR_BASE_VERSION_2_0_0,

    "default", /* MCA component name */
    ORTE_MAJOR_VERSION,  /* MCA component major version */
    ORTE_MINOR_VERSION,  /* MCA component minor version */
    ORTE_RELEASE_VERSION,  /* MCA component release version */
    orte_errmgr_default_component_open,  /* component open */
    orte_errmgr_default_component_close, /* component close */
    orte_errmgr_default_component_query  /* component query */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

/*
 * setup the function pointers for the module
 */
orte_errmgr_base_module_t orte_errmgr_default = {
    orte_errmgr_default_proc_aborted,
    orte_errmgr_default_incomplete_start,
    orte_errmgr_default_register_callback,
    orte_errmgr_base_error_abort
};


/*
 * Open the component
 */
int orte_errmgr_default_component_open(void)
{
    return ORTE_SUCCESS;
}

/*
 * Close the component
 */
int orte_errmgr_default_component_close(void)
{
    return ORTE_SUCCESS;
}

int orte_errmgr_default_component_query(mca_base_module_t **module, int *priority)
{
    /* If we are an HNP or a CM, then pick us! */
    if (ORTE_PROC_IS_HNP || ORTE_PROC_IS_CM) {
        /* Return a module (choose an arbitrary, positive priority --
         it's only relevant compared to other components). */
        
        *priority = 100;
        *module = (mca_base_module_t *)&orte_errmgr_default;
        return ORTE_SUCCESS;
    }
    
    /* otherwise, don't take me! */
    *module = NULL;
    return ORTE_ERROR;
    
}
