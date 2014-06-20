/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_DPM_ORTE_H
#define OMPI_DPM_ORTE_H

#include "ompi_config.h"


#include "ompi/mca/dpm/dpm.h"

BEGIN_C_DECLS

/* access to module */
extern ompi_dpm_base_module_t ompi_dpm_orte_module;

OMPI_MODULE_DECLSPEC extern ompi_dpm_base_component_t mca_dpm_orte_component;

END_C_DECLS

#endif /* OMPI_DPM_ORTE_H */
