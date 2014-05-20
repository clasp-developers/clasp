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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/trace.h"
#include "opal/util/output.h"


#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/errmgr/base/static-components.h"

/*
 * globals
 */

/*
 * Global variables
 */
int orte_errmgr_base_output = -1;
/*
 * define a default module that all application procs
 * can use without having to open the framework. The
 * decision on whether or not to open the framework is
 * made in orte_init
 */
orte_errmgr_base_module_t orte_errmgr = {
    orte_errmgr_base_proc_aborted_not_avail,
    orte_errmgr_base_incomplete_start_not_avail,
    orte_errmgr_base_register_cb_not_avail,
    orte_errmgr_base_error_abort
};

bool orte_errmgr_base_selected = false;
opal_list_t orte_errmgr_base_components_available;
mca_errmgr_base_component_t orte_errmgr_base_selected_component;
bool orte_errmgr_initialized = false;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_errmgr_base_open(void)
{
    OPAL_TRACE(5);
    
    if (!orte_errmgr_initialized) { /* ensure we only do this once */
      
        orte_errmgr_base_output = opal_output_open(NULL);

        /* Open up all available components */
    
        if (ORTE_SUCCESS != 
            mca_base_components_open("errmgr", orte_errmgr_base_output,
                                     mca_errmgr_base_static_components, 
                                     &orte_errmgr_base_components_available, true)) {
            return ORTE_ERROR;
        }
    
        orte_errmgr_initialized = true;
    }
    
    /* All done */
    
    return ORTE_SUCCESS;
}
