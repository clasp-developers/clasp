/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTE_ODLS_PROCESS_EXPORT_H
#define ORTE_ODLS_PROCESS_EXPORT_H

#include "orte_config.h"

#include "opal/mca/mca.h"

#include "orte/mca/odls/odls.h"

BEGIN_C_DECLS
/*
 * Module open / close
 */
int orte_odls_process_component_open(void);
int orte_odls_process_component_close(void);
int orte_odls_process_component_query(mca_base_module_t **module, int *priority);

/*
 * ODLS Process module
 */
extern orte_odls_base_module_t orte_odls_process_module;
ORTE_MODULE_DECLSPEC extern orte_odls_base_component_t mca_odls_process_component;

END_C_DECLS

#endif /* ORTE_ODLS_PROCESS_EXPORT_H */
