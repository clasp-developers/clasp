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
/**
 * @file
 *
 * Resource Allocation (LSF)
 */
#ifndef ORTE_RAS_LSF_H
#define ORTE_RAS_LSF_H

#include "orte_config.h"
#include "orte/mca/ras/ras.h"

BEGIN_C_DECLS

/**
 * RAS Component 
 */
ORTE_DECLSPEC extern orte_ras_base_component_t mca_ras_lsf_component;
ORTE_DECLSPEC extern orte_ras_base_module_t orte_ras_lsf_module;

END_C_DECLS

#endif
