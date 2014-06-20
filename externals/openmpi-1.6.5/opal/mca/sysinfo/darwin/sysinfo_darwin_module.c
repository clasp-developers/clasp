/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

/* This component will only be compiled on Mac OSX, where we are
   guaranteed to have these headers */
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/sysctl.h>
#include <assert.h>
#include <time.h>
#include <string.h>

#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/sysinfo/sysinfo.h"
#include "opal/mca/sysinfo/base/base.h"

#include "sysinfo_darwin.h"

static int init(void);
static int query(char **keys, opal_list_t *values);
static int fini(void);

/*
 * Darwin sysinfo module
 */
const opal_sysinfo_base_module_t opal_sysinfo_darwin_module = {
    init,
    query,
    fini
};

static int init(void)
{
    return OPAL_SUCCESS;
}

static int fini(void)
{
    return OPAL_SUCCESS;
}

/* Mac OSX does things a little differently than Linux
 * by providing process stats via an API. This means we
 * don't have to parse files that could change!
 */
static int query(char **keys, opal_list_t *values)
{
    int mib[2], i;
    size_t len;
    int64_t i64;
    int iint;
    opal_sysinfo_value_t *data;
    char strval[128];
    
    mib[0] = CTL_HW;
    
    /* cycle through the requested keys */
    for (i=0; NULL != keys[i]; i++) {
        if (0 == strcmp(keys[i], OPAL_SYSINFO_CPU_TYPE)) {
            mib[1] = HW_MACHINE;
            len = 128;
            sysctl(mib, 2, &strval, &len, NULL, 0);
            data = OBJ_NEW(opal_sysinfo_value_t);
            data->key = strdup(OPAL_SYSINFO_CPU_TYPE);
            data->type = OPAL_STRING;
            data->data.str = strdup(strval);
            opal_list_append(values, &data->super);
            continue;
        }
        if (0 == strcmp(keys[i], OPAL_SYSINFO_CPU_MODEL)) {
            mib[1] = HW_MODEL;
            len = 128;
            sysctl(mib, 2, &strval, &len, NULL, 0);
            data = OBJ_NEW(opal_sysinfo_value_t);
            data->key = strdup(OPAL_SYSINFO_CPU_MODEL);
            data->type = OPAL_STRING;
            data->data.str = strdup(strval);
            opal_list_append(values, &data->super);
            continue;
        }
        if (0 == strcmp(keys[i], OPAL_SYSINFO_NUM_CPUS)) {
            mib[1] = HW_NCPU;
            len = sizeof(int);
            sysctl(mib, 2, &iint, &len, NULL, 0);
            data = OBJ_NEW(opal_sysinfo_value_t);
            data->key = strdup(OPAL_SYSINFO_NUM_CPUS);
            data->type = OPAL_INT64;
            data->data.i64 = (int64_t)iint;
            opal_list_append(values, &data->super);
            continue;
        }
        if (0 == strcmp(keys[i], OPAL_SYSINFO_MEM_SIZE)) {
            mib[1] = HW_MEMSIZE;
            len = sizeof(int64_t);
            sysctl(mib, 2, &i64, &len, NULL, 0);
            data = OBJ_NEW(opal_sysinfo_value_t);
            data->key = strdup(OPAL_SYSINFO_MEM_SIZE);
            data->type = OPAL_INT64;
            data->data.i64 = i64 / (1 << 20);
            opal_list_append(values, &data->super);
            continue;
        }
    }

    return OPAL_SUCCESS;
}
