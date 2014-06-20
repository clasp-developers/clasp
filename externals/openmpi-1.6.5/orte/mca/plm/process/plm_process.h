/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2009 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file:
 * Part of the process launcher. See plm_process.h for an overview of how it works.
 */

#ifndef ORTE_PLM_PROCESS_EXPORT_H
#define ORTE_PLM_PROCESS_EXPORT_H

#include "orte_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/threads/condition.h"
#include "opal/mca/mca.h"
#include "orte/mca/plm/plm.h"

BEGIN_C_DECLS
/*
 * Module open / close
 */
int orte_plm_process_component_open(void);
int orte_plm_process_component_close(void);
int orte_plm_process_component_query(mca_base_module_t **module, int *priority);

/*
 * Startup / Shutdown
 */
int orte_plm_process_finalize(void);

/**
 * PLM Component
 */
struct orte_plm_process_component_t {
    orte_plm_base_component_t super;
    bool debug;
    bool debug_malloc;
    bool debug_daemons;
    bool timing;
    bool reap;
    bool assume_same_shell;
    bool force_process;
    bool use_gui_prompt;
    bool remote_reg_prefix;
    bool remote_env_prefix;
    int delay;
    int priority;
    char* orted;
    orte_std_cntr_t num_children;
    orte_std_cntr_t num_concurrent;
    opal_mutex_t lock;
    opal_condition_t cond;
};
typedef struct orte_plm_process_component_t orte_plm_process_component_t;

ORTE_MODULE_DECLSPEC extern orte_plm_process_component_t mca_plm_process_component;
extern orte_plm_base_module_t orte_plm_process_module;

END_C_DECLS

#endif /* ORTE_PLM_PROCESS_EXPORT_H */
