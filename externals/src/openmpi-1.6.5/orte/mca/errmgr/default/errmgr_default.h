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
 *
 */
#ifndef ORTE_ERRMGR_HNP_H
#define ORTE_ERRMGR_HNP_H


#include "orte_config.h"
#include "orte/types.h"

#include "orte/mca/plm/plm_types.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int orte_errmgr_default_component_open(void);
int orte_errmgr_default_component_close(void);
int orte_errmgr_default_component_query(mca_base_module_t **module, int *priority);


/*
 * Component API functions
 */
void orte_errmgr_default_proc_aborted(orte_process_name_t *name, int exit_code);

void orte_errmgr_default_incomplete_start(orte_jobid_t job, int exit_code);

int orte_errmgr_default_register_callback(orte_jobid_t job,
                                      orte_job_state_t state,
                                      orte_err_cb_fn_t cbfunc,
                                      void *cbdata);

ORTE_MODULE_DECLSPEC extern mca_errmgr_base_component_t mca_errmgr_default_component;

END_C_DECLS

#endif
