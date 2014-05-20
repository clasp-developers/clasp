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

#include "sysinfo_linux.h"

/*
 * Module functions
 */
static int linux_module_init(void);
static int query(char **keys, opal_list_t *values);
static int linux_module_fini(void);

/*
 * Linux sysinfo module
 */
const opal_sysinfo_base_module_t opal_sysinfo_linux_module = {
    linux_module_init,
    query,
    linux_module_fini
};


/* Local functions */
static char *local_getline(FILE *fp);
static char *local_stripper(char *data);

/* Local data */
static char *cpu_type=NULL;
static char *cpu_model=NULL;
static int num_cpus=0;
static int64_t mem_size=0;
static char input[256];

static int linux_module_init(void)
{
    FILE *fp;
    char *data, *value, *ptr;

    /* see if we can open the cpuinfo file */
    if (NULL == (fp = fopen("/proc/cpuinfo", "r"))) {
        /* can't access this file - most likely, this means we
         * aren't really on a supported system, or the proc no
         * longer exists. Just return an error
         */
        return OPAL_ERR_NOT_SUPPORTED;
    }
    
    /* read the file one line at a time */
    while (NULL != (data = local_getline(fp))) {
        if (NULL == (value = local_stripper(data))) {
            /* cannot process */
            continue;
        }
        if (NULL == cpu_type && 0 == strcmp(data, "vendor_id")) {
            cpu_type = strdup(value);
            continue;
        }
        if (NULL == cpu_model && 0 == strcmp(data, "model name")) {
            cpu_model = strdup(value);
        }
        if (0 == strcmp(data, "processor")) {
            /* increment the num_cpus */
            ++num_cpus;
        }
    }
    fclose(fp);

    /* see if we can open the meminfo file */
    if (NULL == (fp = fopen("/proc/meminfo", "r"))) {
        /* ignore this */
        return OPAL_SUCCESS;
    }
    
    /* read the file one line at a time */
    while (NULL != (data = local_getline(fp))) {
        if (NULL == (value = local_stripper(data))) {
            /* cannot process */
            continue;
        }
        if (0 == strcmp(data, "MemTotal")) {
            /* find units */
            ptr = &value[strlen(value)-2];
            value[strlen(value)-3] = '\0';
            /* compute base value */
            mem_size = strtol(value, NULL, 10);
            /* get the unit multiplier */
            if (0 == strcmp(ptr, "kB")) {
                mem_size = mem_size / 1024;
            }
            continue;
        }
    }
    fclose(fp);
    
    return OPAL_SUCCESS;
}

static int linux_module_fini(void)
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

static char *local_getline(FILE *fp)
{
    char *ret;
    
    ret = fgets(input, 256, fp);
    if (NULL != ret) {
        input[strlen(input)-1] = '\0';  /* remove newline */
        return input;
    }
    
    return NULL;
}

static char *local_stripper(char *data)
{
    char *ptr, *end, *enddata;
    int len = strlen(data);
    
    /* find the colon */
    if (NULL == (end = strchr(data, ':'))) {
        return NULL;
    }
    ptr = end;
    --end;
    /* working backwards, look for first non-whitespace */
    while (end != data && !isalnum(*end)) {
        --end;
    }
    ++end;
    *end = '\0';
    /* now look for value */
    ptr++;
    enddata = &(data[len-1]);
    while (ptr != enddata && !isalnum(*ptr)) {
        ++ptr;
    }
    return ptr;
}
