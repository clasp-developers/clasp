/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
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
 * Resource Allocation (CCP)
 */
#ifndef ORTE_RAS_ccp_H
#define ORTE_RAS_ccp_H

#include "orte_config.h"
#include "orte/mca/ras/ras.h"
#include "orte/mca/ras/base/base.h"

BEGIN_C_DECLS

    struct orte_ras_ccp_component_t {
        /** Base RAS component */
        orte_ras_base_component_t super;
        /** What's the priority of this component */
        int priority;
    };
    typedef struct orte_ras_ccp_component_t orte_ras_ccp_component_t;
    
    ORTE_DECLSPEC extern orte_ras_ccp_component_t mca_ras_ccp_component;
    ORTE_DECLSPEC extern orte_ras_base_module_t orte_ras_ccp_module;

END_C_DECLS

#endif
