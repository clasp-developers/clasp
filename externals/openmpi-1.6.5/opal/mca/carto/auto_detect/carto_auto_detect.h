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
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * The auto detect component discover automaticly the structure
 * of the host.
 */

#ifndef MCA_CARTO_AUTO_DETECT_H
#define MCA_CARTO_AUTO_DETECT_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/carto/carto.h"

BEGIN_C_DECLS

/**
 * Globally exported variable
 */
OPAL_DECLSPEC extern const opal_carto_base_component_2_0_0_t
mca_carto_auto_detect_component;


/**
 * carto query API function
 *
 * Query function for carto components.  Simply returns a priority
 * to rank it against other available carto components (assumedly,
 * only one component will be available per platform, but it's
 * possible that there could be more than one available).
 */
int opal_carto_auto_detect_component_query(mca_base_module_t **module, int *priority);

END_C_DECLS

#endif /* MCA_CARTO_FILE_EXPORT_H */

