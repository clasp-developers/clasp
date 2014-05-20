/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#if !ORTE_DISABLE_FULL_SUPPORT

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/ras/base/ras_private.h"

#endif

#include "orte/mca/ras/base/base.h"


/* NOTE: the RAS does not require a proxy as only the
 * HNP can open the framework in orte_init - non-HNP
 * procs are not allowed to allocate resources
 */
 
/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/ras/base/static-components.h"

#if ORTE_DISABLE_FULL_SUPPORT
/* have to include a bogus function here so that
 * the build system sees at least one function
 * in the library
 */
int orte_ras_base_open(void)
{
    return ORTE_SUCCESS;
}

#else

/*
 * Global variables
 */
orte_ras_t orte_ras = {
    orte_ras_base_allocate
};

orte_ras_base_t orte_ras_base;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_ras_base_open(void)
{
    int value;
    bool btmp;
    
    /* set default flags */
    orte_ras_base.active_module = NULL;
    orte_ras_base.allocation_read = false;
    
    /* should we display the allocation after determining it? */
    mca_base_param_reg_int_name("ras", "base_display_alloc",
                                "Whether to display the allocation after it is determined",
                                false, false, (int)false, &value);
    orte_ras_base.display_alloc = OPAL_INT_TO_BOOL(value);

    /* should we display a detailed (developer-quality) version of the allocation after determining it? */
    mca_base_param_reg_int_name("ras", "base_display_devel_alloc",
                                "Whether to display a developer-detail allocation after it is determined",
                                false, false, (int)false, &value);
    btmp = OPAL_INT_TO_BOOL(value);
    if (btmp) {
        orte_ras_base.display_alloc = true;
        orte_devel_level_output = true;
    }

    /* Debugging / verbose output.  Always have stream open, with
        verbose set by the mca open system... */
    orte_ras_base.ras_output = opal_output_open(NULL);
    
    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("ras", orte_ras_base.ras_output,
                                 mca_ras_base_static_components, 
                                 &orte_ras_base.ras_opened, true)) {
        return ORTE_ERROR;
    }

    /* All done */
    return ORTE_SUCCESS;
}

#endif /* ORTE_DISABLE_FULL_SUPPORT */
