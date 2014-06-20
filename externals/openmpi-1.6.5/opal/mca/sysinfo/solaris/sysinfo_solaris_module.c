/*
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

/* This component will only be compiled on Linux, where we are
   guaranteed to have <unistd.h> and friends */
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#include <sys/param.h>  /* for HZ to convert jiffies to actual time */

#include "opal/mca/base/mca_base_param.h"
#include "opal/dss/dss_types.h"
#include "opal/util/printf.h"

#include "sysinfo_solaris.h"
#include "chiptype.h"

/*
 * Module functions
 */
static int solaris_module_init(void);
static int query(char **keys, opal_list_t *values);
static int solaris_module_fini(void);

/*
 * Solaris sysinfo module
 */
const opal_sysinfo_base_module_t opal_sysinfo_solaris_module = {
    solaris_module_init,
    query,
    solaris_module_fini
};


/* Local data */
static int int_cpu_type=0;
static int int_cpu_model=0;
static char *cpu_type=NULL;
static char *cpu_model=NULL;
static int num_cpus=0;
static int64_t mem_size=0;
static char input[256];

static int solaris_module_init(void)
{
    FILE *fp;
    char *data, *value, *ptr;

    /* Get CPU Type */
    cpu_type = get_sparc_chip_manufacturer();
    /* Get CPU Model */
    cpu_model = get_sparc_chip_mode();

    /* Get number of cores */

    /* get memory size */
    
    return OPAL_SUCCESS;
}

static int solaris_module_fini(void)
{
    return OPAL_SUCCESS;
}

static int query(char **keys, opal_list_t *values)
{
    int i;
    opal_sysinfo_value_t *data;

    /* cycle through the requested keys */
    for (i=0; NULL != keys[i]; i++) {
        if (0 == strcmp(keys[i], OPAL_SYSINFO_CPU_TYPE) &&
            NULL != cpu_type) {
            data = OBJ_NEW(opal_sysinfo_value_t);
            data->key = strdup(OPAL_SYSINFO_CPU_TYPE);
            data->type = OPAL_STRING;
            data->data.str = strdup(cpu_type);
            opal_list_append(values, &data->super);
            continue;
        }
        if (0 == strcmp(keys[i], OPAL_SYSINFO_CPU_MODEL) &&
            NULL != cpu_model) {
            data = OBJ_NEW(opal_sysinfo_value_t);
            data->key = strdup(OPAL_SYSINFO_CPU_MODEL);
            data->type = OPAL_STRING;
            data->data.str = strdup(cpu_model);
            opal_list_append(values, &data->super);
            continue;
        }
        if (0 == strcmp(keys[i], OPAL_SYSINFO_NUM_CPUS) &&
            num_cpus > 0) {
            data = OBJ_NEW(opal_sysinfo_value_t);
            data->key = strdup(OPAL_SYSINFO_NUM_CPUS);
            data->type = OPAL_INT64;
            data->data.i64 = (int64_t)num_cpus;
            opal_list_append(values, &data->super);
            continue;
        }
        if (0 == strcmp(keys[i], OPAL_SYSINFO_MEM_SIZE) &&
            mem_size > 0) {
            data = OBJ_NEW(opal_sysinfo_value_t);
            data->key = strdup(OPAL_SYSINFO_MEM_SIZE);
            data->type = OPAL_INT64;
            data->data.i64 = mem_size;
            opal_list_append(values, &data->super);
            continue;
        }
    }    

    return OPAL_SUCCESS;
}
