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
/** @file:
 */

#ifndef ORTE_MCA_ERRMGR_BASE_H
#define ORTE_MCA_ERRMGR_BASE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"

#include "opal/class/opal_list.h"

#include "opal/mca/mca.h"
#include "orte/mca/errmgr/errmgr.h"


/*
 * Global functions for MCA overall collective open and close
 */
BEGIN_C_DECLS

/*
 * Internal definitions
 */
/*
 * function definitions
 */
ORTE_DECLSPEC    int orte_errmgr_base_open(void);
ORTE_DECLSPEC    int orte_errmgr_base_select(void);
ORTE_DECLSPEC    int orte_errmgr_base_close(void);

/*
 * globals that might be needed
 */

extern bool orte_errmgr_base_selected;
extern bool orte_errmgr_initialized;
ORTE_DECLSPEC extern opal_list_t orte_errmgr_base_components_available;
ORTE_DECLSPEC extern mca_errmgr_base_component_t orte_errmgr_base_selected_component;

/*
 * external API functions will be documented in the mca/errmgr/errmgr.h file
 */

END_C_DECLS

#endif
