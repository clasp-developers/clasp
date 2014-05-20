/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_plm_CCP_EXPORT_H
#define ORTE_plm_CCP_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/plm/plm.h"

BEGIN_C_DECLS

struct orte_plm_ccp_component_t {
    orte_plm_base_component_t super;
    int priority;
    int debug;
    int verbose;
    bool want_path_check;
    char **checked_paths;
    char *stdout_file;
    char *stderr_file;
    char *job_name;
    bool timing;
};
typedef struct orte_plm_ccp_component_t orte_plm_ccp_component_t;
/* Globally exported variables */
ORTE_DECLSPEC extern orte_plm_ccp_component_t mca_plm_ccp_component;
extern orte_plm_base_module_t orte_plm_ccp_module;

END_C_DECLS

#endif /* ORTE_plm_CCP_EXPORT_H */
