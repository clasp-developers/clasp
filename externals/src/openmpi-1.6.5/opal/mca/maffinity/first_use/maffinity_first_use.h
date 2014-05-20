/*
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
 */

/**
 * @file
 *
 * Processor affinity for First_Use.
 */


#ifndef MCA_MAFFINITY_FIRST_USE_EXPORT_H
#define MCA_MAFFINITY_FIRST_USE_EXPORT_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/maffinity/maffinity.h"


BEGIN_C_DECLS

    /**
     * Globally exported variable
     */
    OPAL_MODULE_DECLSPEC extern const opal_maffinity_base_component_2_0_0_t
        mca_maffinity_first_use_component;


    /**
     * maffinity query API function
     *
     * Query function for maffinity components.  Simply returns a priority
     * to rank it against other available maffinity components (assumedly,
     * only one component will be available per platform, but it's
     * possible that there could be more than one available).
     */
    int opal_maffinity_first_use_component_query(mca_base_module_t **module, int *priority);

END_C_DECLS
#endif /* MCA_MAFFINITY_FIRST_USE_EXPORT_H */
