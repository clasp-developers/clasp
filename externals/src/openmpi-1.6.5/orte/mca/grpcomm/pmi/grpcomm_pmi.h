/* -*- C -*-
 * 
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#ifndef GRPCOMM_PMI_H
#define GRPCOMM_PMI_H

#include "orte_config.h"


#include "orte/mca/grpcomm/grpcomm.h"

BEGIN_C_DECLS

/*
 * Component open / close
 */
int orte_grpcomm_pmi_open(void);
int orte_grpcomm_pmi_close(void);
int orte_grpcomm_pmi_component_query(mca_base_module_t **module, int *priority);


/*
 * Grpcomm interfaces
 */

ORTE_MODULE_DECLSPEC extern orte_grpcomm_base_component_t mca_grpcomm_pmi_component;
extern orte_grpcomm_base_module_t orte_grpcomm_pmi_module;

END_C_DECLS

#endif
