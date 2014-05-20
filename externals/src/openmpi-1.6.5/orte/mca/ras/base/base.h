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

#ifndef ORTE_MCA_RAS_BASE_H
#define ORTE_MCA_RAS_BASE_H

/*
 * includes
 */
#include "orte_config.h"

#include "orte/mca/ras/ras.h"
/*
 * Global functions for MCA overall collective open and close
 */

BEGIN_C_DECLS

ORTE_DECLSPEC int orte_ras_base_open(void);

#if !ORTE_DISABLE_FULL_SUPPORT
/*
 * globals that might be needed
 */
typedef struct orte_ras_base_t {
    int ras_output;
    opal_list_t ras_opened;
    bool allocation_read;
    bool display_alloc;
    orte_ras_base_module_t *active_module;
} orte_ras_base_t;

ORTE_DECLSPEC extern orte_ras_base_t orte_ras_base;


/*
 * function definitions
 */
ORTE_DECLSPEC int orte_ras_base_select(void);
ORTE_DECLSPEC int orte_ras_base_finalize(void);
ORTE_DECLSPEC int orte_ras_base_close(void);

ORTE_DECLSPEC int orte_ras_base_add_hosts(orte_job_t *jdata);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif
