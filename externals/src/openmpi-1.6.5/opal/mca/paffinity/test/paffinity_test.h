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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_PAFFINITY_TEST_EXPORT_H
#define MCA_PAFFINITY_TEST_EXPORT_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/paffinity/paffinity.h"

BEGIN_C_DECLS

/*
 * Globally exported variable
 */
typedef struct opal_paffinity_test_component_t {
    opal_paffinity_base_component_t super;
    bool bound;
    int num_sockets;
    int num_cores;
} opal_paffinity_test_component_t;

OPAL_MODULE_DECLSPEC extern opal_paffinity_test_component_t mca_paffinity_test_component;

extern opal_paffinity_base_module_t opal_paffinity_test_module;

END_C_DECLS

#endif /* MCA_PAFFINITY_TEST_EXPORT_H */
