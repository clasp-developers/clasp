/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef MCA_ESS_BASE_H
#define MCA_ESS_BASE_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"


#include "orte/mca/ess/ess.h"

BEGIN_C_DECLS

/*
 * Global functions for MCA overall collective open and close
 */

/**
 * Open the ess framework
 */
ORTE_DECLSPEC int orte_ess_base_open(void);

/**
 * Select a ess module
 */
ORTE_DECLSPEC int orte_ess_base_select(void);

/**
 * Close the ess framework
 */
ORTE_DECLSPEC int orte_ess_base_close(void);

/*
 * The verbose channel for debug output
 */
ORTE_DECLSPEC extern int orte_ess_base_output;

ORTE_DECLSPEC extern opal_list_t orte_ess_base_components_available;


#if !ORTE_DISABLE_FULL_SUPPORT

/*
 * Internal helper functions used by components
 */
ORTE_DECLSPEC int orte_ess_env_get(void);

ORTE_DECLSPEC int orte_ess_base_std_prolog(void);

ORTE_DECLSPEC int orte_ess_base_app_setup(void);
ORTE_DECLSPEC int orte_ess_base_app_finalize(void);
ORTE_DECLSPEC void orte_ess_base_app_abort(int status, bool report) __opal_attribute_noreturn__;

ORTE_DECLSPEC int orte_ess_base_tool_setup(void);
ORTE_DECLSPEC int orte_ess_base_tool_finalize(void);

ORTE_DECLSPEC int orte_ess_base_orted_setup(char **hosts);
ORTE_DECLSPEC int orte_ess_base_orted_finalize(void);

/*
 * Put functions
 */
ORTE_DECLSPEC int orte_ess_env_put(orte_std_cntr_t num_procs,
                                   orte_std_cntr_t num_local_procs,
                                   char ***env);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif
