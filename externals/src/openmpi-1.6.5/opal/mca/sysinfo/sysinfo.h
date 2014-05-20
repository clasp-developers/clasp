/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
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
 * system resource info framework component interface.
 *
 * Intent
 *
 * self-discovery of available local resources.
 *
 */

#ifndef OPAL_MCA_SYSINFO_H
#define OPAL_MCA_SYSINFO_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/class/opal_list.h"

#include "opal/mca/sysinfo/sysinfo_types.h"

BEGIN_C_DECLS

/**
 * Module initialization function.  Should return OPAL_SUCCESS.
 */
typedef int (*opal_sysinfo_base_module_init_fn_t)(void);

typedef int (*opal_sysinfo_base_module_query_fn_t)(char **keys, opal_list_t *values);

typedef int (*opal_sysinfo_base_module_fini_fn_t)(void);

/* Public API module */
struct opal_sysinfo_API_module_1_0_0_t {
    opal_sysinfo_base_module_query_fn_t   query;
};
typedef struct opal_sysinfo_API_module_1_0_0_t opal_sysinfo_API_module_t;


/**
 * Structure for sysinfo components.
 */
struct opal_sysinfo_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;
};

/**
 * Convenience typedef
 */
typedef struct opal_sysinfo_base_component_2_0_0_t opal_sysinfo_base_component_2_0_0_t;
typedef struct opal_sysinfo_base_component_2_0_0_t opal_sysinfo_base_component_t;

/**
 * Structure for sysinfo modules
 */
struct opal_sysinfo_base_module_1_0_0_t {
    opal_sysinfo_base_module_init_fn_t    init;
    opal_sysinfo_base_module_query_fn_t   query;
    opal_sysinfo_base_module_fini_fn_t    finalize;
};

/**
 * Convenience typedef
 */
typedef struct opal_sysinfo_base_module_1_0_0_t opal_sysinfo_base_module_1_0_0_t;
typedef struct opal_sysinfo_base_module_1_0_0_t opal_sysinfo_base_module_t;

typedef struct {
    opal_list_item_t super;
    opal_sysinfo_base_module_t *module;
} opal_sysinfo_module_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_sysinfo_module_t);


/**
 * Macro for use in components that are of type sysinfo
 */
#define OPAL_SYSINFO_BASE_VERSION_2_0_0 \
    MCA_BASE_VERSION_2_0_0, \
    "sysinfo", 2, 0, 0

/* Global structure for accessing sysinfo functions */
OPAL_DECLSPEC extern opal_sysinfo_API_module_t opal_sysinfo;

END_C_DECLS

#endif /* OPAL_MCA_SYSINFO_H */
