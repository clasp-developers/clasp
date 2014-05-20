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
 * Resource flags
 *
 * Intent
 *
 * self-discovery of available local resources.
 *
 */

#ifndef OPAL_MCA_SYSINFO_TYPE_H
#define OPAL_MCA_SYSINFO_TYPE_H

#include "opal_config.h"

#include "opal/class/opal_list.h"
#include "opal/dss/dss_types.h"

BEGIN_C_DECLS

#define OPAL_SYSINFO_CPU_TYPE   "CPU_TYPE"
#define OPAL_SYSINFO_CPU_MODEL  "CPU_MODEL"
#define OPAL_SYSINFO_NUM_CPUS   "NUM_CPUS"
#define OPAL_SYSINFO_MEM_SIZE   "MEMORY"

typedef struct {
    opal_list_item_t super;
    char *key;
    opal_data_type_t type;
    union {
        int64_t i64;
        char *str;
    } data;
} opal_sysinfo_value_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_sysinfo_value_t);

END_C_DECLS

#endif /* OPAL_MCA_SYSINFO_TYPE_H */
