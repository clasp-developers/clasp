/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

#include "opal_config.h"

#include "opal_stdint.h"
#include <stdio.h>

#include "opal/dss/dss_internal.h"

int opal_dss_print(char **output, char *prefix, void *src, opal_data_type_t type)
{
    opal_dss_type_info_t *info;

    /* check for error */
    if (NULL == output) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* Lookup the print function for this type and call it */

    if(NULL == (info = (opal_dss_type_info_t*)opal_pointer_array_get_item(&opal_dss_types, type))) {
        return OPAL_ERR_UNKNOWN_DATA_TYPE;
    }

    return info->odti_print_fn(output, prefix, src, type);
}

/*
 * STANDARD PRINT FUNCTIONS FOR SYSTEM TYPES
 */
int opal_dss_print_byte(char **output, char *prefix, uint8_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_BYTE\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_BYTE\tValue: %x", prefix, *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_string(char **output, char *prefix, char *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_STRING\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_STRING\tValue: %s", prefx, src);

    return OPAL_SUCCESS;
}

int opal_dss_print_size(char **output, char *prefix, size_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_SIZE\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_SIZE\tValue: %lu", prefx, (unsigned long) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_pid(char **output, char *prefix, pid_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_PID\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_PID\tValue: %lu", prefx, (unsigned long) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_bool(char **output, char *prefix, bool *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_BOOL\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_BOOL\tValue: %s", prefx, *src ? "TRUE" : "FALSE");

    return OPAL_SUCCESS;
}

int opal_dss_print_int(char **output, char *prefix, int *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_INT\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_INT\tValue: %ld", prefx, (long) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_uint(char **output, char *prefix, int *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_UINT\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_UINT\tValue: %lu", prefx, (unsigned long) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_uint8(char **output, char *prefix, uint8_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_UINT8\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_UINT8\tValue: %u", prefx, (unsigned int) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_uint16(char **output, char *prefix, uint16_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_UINT16\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_UINT16\tValue: %u", prefx, (unsigned int) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_uint32(char **output, char *prefix, uint32_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_UINT32\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_UINT32\tValue: %u", prefx, (unsigned int) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_int8(char **output, char *prefix, int8_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_INT8\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_INT8\tValue: %d", prefx, (int) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_int16(char **output, char *prefix, int16_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_INT16\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_INT16\tValue: %d", prefx, (int) *src);

    return OPAL_SUCCESS;
}

int opal_dss_print_int32(char **output, char *prefix, int32_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_INT32\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_INT32\tValue: %d", prefx, (int) *src);

    return OPAL_SUCCESS;
}
int opal_dss_print_uint64(char **output, char *prefix,
#ifdef HAVE_INT64_T
                          uint64_t *src,
#else
                          void *src,
#endif  /* HAVE_INT64_T */
                          opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_UINT64\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

#ifdef HAVE_INT64_T
    asprintf(output, "%sData type: OPAL_UINT64\tValue: %lu", prefx, (unsigned long) *src);
#else
    asprintf(output, "%sData type: OPAL_UINT64\tValue: unsupported", prefx);
#endif  /* HAVE_INT64_T */

    return OPAL_SUCCESS;
}

int opal_dss_print_int64(char **output, char *prefix,
#ifdef HAVE_INT64_T
                          int64_t *src,
#else
                          void *src,
#endif  /* HAVE_INT64_T */
                         opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_INT64\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

#ifdef HAVE_INT64_T
    asprintf(output, "%sData type: OPAL_INT64\tValue: %ld", prefx, (long) *src);
#else
    asprintf(output, "%sData type: OPAL_INT64\tValue: unsupported", prefx);
#endif  /* HAVE_INT64_T */

    return OPAL_SUCCESS;
}

int opal_dss_print_null(char **output, char *prefix, void *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_NULL\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_NULL", prefx);

    return OPAL_SUCCESS;
}


/* PRINT FUNCTIONS FOR GENERIC OPAL TYPES */

/*
 * OPAL_DATA_TYPE
 */
int opal_dss_print_data_type(char **output, char *prefix, opal_data_type_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_DATA_TYPE\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_DATA_TYPE\tValue: %lu", prefx, (unsigned long) *src);
    return OPAL_SUCCESS;
}

/*
 * OPAL_DATA_VALUE
 */
int opal_dss_print_data_value(char **output, char *prefix, opal_dss_value_t *src, opal_data_type_t type)
{
    char *pfx, *tmp1, *tmp2;
    int rc;

   /* if src is NULL, just print data type and return */
    if (NULL == src) {
        if (NULL != prefix) {
            asprintf(output, "%sData type: OPAL_DATA_VALUE\tValue: NULL pointer", prefix);
        } else {
            asprintf(output, "Data type: OPAL_DATA_VALUE\tValue: NULL pointer");
        }
        return OPAL_SUCCESS;
    }

    if (NULL != prefix) {
        asprintf(&pfx, "%s\t", prefix);
        asprintf(&tmp1, "%sData type: OPAL_DATA_VALUE:\n", prefix);
    } else {
        asprintf(&tmp1, "Data type: OPAL_DATA_VALUE:\n");
        asprintf(&pfx, "\t");
    }

    /* if data is included, print it */
    if (OPAL_UNDEF == src->type) { /* undefined data type - just report it */
        asprintf(&tmp2, "%sData type: OPAL_UNDEF\tValue: N/A", pfx);
    } else if (NULL != src->data) {
        if (OPAL_SUCCESS != (rc = opal_dss.print(&tmp2, pfx, src->data, src->type))) {
            if (NULL != tmp1) free(tmp1);
            if (NULL != pfx) free(pfx);
            *output = NULL;
            return rc;
        }
    } else { /* indicate the data field was NULL */
        asprintf(&tmp2, "%sData field is NULL", pfx);
    }

    asprintf(output, "%s%s", tmp1, tmp2);
    free(tmp1);
    free(tmp2);
    if (NULL != pfx) free(pfx);

    return OPAL_SUCCESS;
}

/*
 * OPAL_BYTE_OBJECT
 */
int opal_dss_print_byte_object(char **output, char *prefix, opal_byte_object_t *src, opal_data_type_t type)
{
    char *prefx;

    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;

    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_BYTE_OBJECT\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }

    asprintf(output, "%sData type: OPAL_BYTE_OBJECT\tSize: %lu", prefx, (unsigned long) src->size);

    return OPAL_SUCCESS;
}

/*
 * OPAL_PSTAT
 */
int opal_dss_print_pstat(char **output, char *prefix, opal_pstats_t *src, opal_data_type_t type)
{
    char *prefx;
    
    /* deal with NULL prefix */
    if (NULL == prefix) asprintf(&prefx, " ");
    else prefx = prefix;
    
    /* if src is NULL, just print data type and return */
    if (NULL == src) {
        asprintf(output, "%sData type: OPAL_PSTATS\tValue: NULL pointer", prefx);
        return OPAL_SUCCESS;
    }
    
    asprintf(output, "%snode: %s rank: %d pid: %d cmd: %s state: %c pri: %d #threads: %d Processor: %d\n"
                     "%s\ttime: %" PRIu64 " VMsize: %" PRIu64 " PeakVMSize: %" PRIu64 " RSS: %" PRIu64 " Share: %" PRIu64 "\n",
             prefx, src->node, src->rank, src->pid, src->cmd, src->state, src->priority, src->num_threads, src->processor,
             prefx, src->time, src->vsize, src->peak_vsize, src->rss, src->shared_size);
    
    return OPAL_SUCCESS;
}
