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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_PLM_TM_EXPORT_H
#define ORTE_PLM_TM_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/plm/plm.h"

BEGIN_C_DECLS

struct orte_plm_tm_component_t {
    orte_plm_base_component_t super;
    bool want_path_check;
    char **checked_paths;
};
typedef struct orte_plm_tm_component_t orte_plm_tm_component_t;

/* Globally exported variables */
ORTE_DECLSPEC extern orte_plm_tm_component_t mca_plm_tm_component;
extern orte_plm_base_module_t orte_plm_tm_module;

END_C_DECLS

#endif /* ORTE_PLM_TM_EXPORT_H */
