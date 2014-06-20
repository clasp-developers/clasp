/* -*- C -*-
 * 
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
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
 *
 */
#ifndef GRPCOMM_CNOS_H
#define GRPCOMM_CNOS_H

#include "orte_config.h"
#include "orte/constants.h"


#include "orte/mca/grpcomm/grpcomm.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int orte_grpcomm_cnos_open(void);
int orte_grpcomm_cnos_close(void);
int orte_grpcomm_cnos_query(mca_base_module_t **module, int *priority);


/*
 * Grpcomm interfaces
 */

ORTE_MODULE_DECLSPEC extern orte_grpcomm_base_component_t mca_grpcomm_cnos_component;
extern orte_grpcomm_base_module_t orte_grpcomm_cnos_module;

END_C_DECLS

#endif
