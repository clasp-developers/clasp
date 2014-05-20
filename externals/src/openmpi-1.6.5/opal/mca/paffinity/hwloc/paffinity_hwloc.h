/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_PAFFINITY_HWLOC_EXPORT_H
#define MCA_PAFFINITY_HWLOC_EXPORT_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/paffinity/paffinity.h"
#include "hwloc.h"


BEGIN_C_DECLS

/**
 * Globally exported variable
 */
OPAL_DECLSPEC extern opal_paffinity_base_component_t
     mca_paffinity_hwloc_component;

/**
 * paffinity query API function
 *
 * Query function for paffinity components.  Simply returns a priority
 * to rank it against other available paffinity components (assumedly,
 * only one component will be available per platform, but it's
 * possible that there could be more than one available).
 */
int opal_paffinity_hwloc_component_query(mca_base_module_t **module, 
                                         int *priority);

END_C_DECLS

#endif /* MCA_PAFFINITY_HWLOC_EXPORT_H */
