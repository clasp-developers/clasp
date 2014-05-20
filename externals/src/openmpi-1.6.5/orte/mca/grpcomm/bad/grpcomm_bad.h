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
#ifndef GRPCOMM_BAD_H
#define GRPCOMM_BAD_H

#include "orte_config.h"


#include "orte/mca/grpcomm/grpcomm.h"

BEGIN_C_DECLS

/*
 * Component open / close
 */
int orte_grpcomm_bad_open(void);
int orte_grpcomm_bad_close(void);
int orte_grpcomm_bad_component_query(mca_base_module_t **module, int *priority);


/*
 * Grpcomm interfaces
 */

ORTE_MODULE_DECLSPEC extern orte_grpcomm_base_component_t mca_grpcomm_bad_component;
extern orte_grpcomm_base_module_t orte_grpcomm_bad_module;

END_C_DECLS

#endif
