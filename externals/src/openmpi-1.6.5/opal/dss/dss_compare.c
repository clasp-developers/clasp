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
#include <stdlib.h>

#include "opal/dss/dss_internal.h"

int opal_dss_compare(const void *value1, const void *value2, opal_data_type_t type)
{
    opal_dss_type_info_t *info;

    /* check for error */
    if (NULL == value1 || NULL == value2) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* Lookup the compare function for this type and call it */

    if (NULL == (info = (opal_dss_type_info_t*)opal_pointer_array_get_item(&opal_dss_types, type))) {
        return OPAL_ERR_UNKNOWN_DATA_TYPE;
    }

    return info->odti_compare_fn(value1, value2, type);
}

/*
 * NUMERIC COMPARE FUNCTIONS
 */
int opal_dss_compare_int(int *value1, int *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_uint(unsigned int *value1, unsigned int *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_size(size_t *value1, size_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_pid(pid_t *value1, pid_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_byte(char *value1, char *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_char(char *value1, char *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_int8(int8_t *value1, int8_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_uint8(uint8_t *value1, uint8_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_int16(int16_t *value1, int16_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_uint16(uint16_t *value1, uint16_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_int32(int32_t *value1, int32_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_uint32(uint32_t *value1, uint32_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_int64(int64_t *value1, int64_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

int opal_dss_compare_uint64(uint64_t *value1, uint64_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

/*
 * NON-NUMERIC SYSTEM TYPES
 */

/* NULL */
int opal_dss_compare_null(char *value1, char *value2, opal_data_type_t type)
{
    return OPAL_EQUAL;
}

/* BOOL */
int opal_dss_compare_bool(bool *value1, bool *value2, opal_data_type_t type)
{
    if (*value1 && !(*value2)) return OPAL_VALUE1_GREATER;

    if (*value2 && !(*value1)) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;

}

/* STRING */
int opal_dss_compare_string(char *value1, char *value2, opal_data_type_t type)
{
    if (0 < strcmp(value1, value2)) return OPAL_VALUE2_GREATER;

    if (0 > strcmp(value1, value2)) return OPAL_VALUE1_GREATER;

    return OPAL_EQUAL;
}

/* COMPARE FUNCTIONS FOR GENERIC OPAL TYPES */
/* OPAL_DATA_TYPE */
int opal_dss_compare_dt(opal_data_type_t *value1, opal_data_type_t *value2, opal_data_type_t type)
{
    if (*value1 > *value2) return OPAL_VALUE1_GREATER;

    if (*value2 > *value1) return OPAL_VALUE2_GREATER;

    return OPAL_EQUAL;
}

/* OPAL_DATA_VALUE */
int opal_dss_compare_data_value(opal_dss_value_t *value1, opal_dss_value_t *value2, opal_data_type_t type)
{
    /* can't compare if the two types don't match */
    if (value1->type != value2->type) {
        return OPAL_ERR_TYPE_MISMATCH;
    }

    /* okay, go ahead and compare the values themselves */
    return opal_dss.compare(value1->data, value2->data, value1->type);
}

/* OPAL_BYTE_OBJECT */
int opal_dss_compare_byte_object(opal_byte_object_t *value1, opal_byte_object_t *value2, opal_data_type_t type)
{
    int checksum, diff;
    int32_t i;

    /* compare the sizes first - bigger size object is "greater than" */
    if (value1->size > value2->size) return OPAL_VALUE1_GREATER;

    if (value2->size > value1->size) return OPAL_VALUE2_GREATER;

    /* get here if the two sizes are identical - now do a simple checksum-style
     * calculation to determine "biggest"
     */
    checksum = 0;

    for (i=0; i < value1->size; i++) {
        /* protect against overflows */
        diff = value1->bytes[i] - value2->bytes[i];
        if (INT_MAX-abs(checksum)-abs(diff) < 0) { /* got an overflow condition */
            checksum = 0;
        }
        checksum += diff;
    }

    if (0 > checksum) return OPAL_VALUE2_GREATER;  /* sum of value2 bytes was greater */

    if (0 < checksum) return OPAL_VALUE1_GREATER;  /* of value1 bytes was greater */

    return OPAL_EQUAL;  /* sum of both value's bytes was identical */
}

/* OPAL_PSTAT */
int opal_dss_compare_pstat(opal_pstats_t *value1, opal_pstats_t *value2, opal_data_type_t type)
{
    return OPAL_EQUAL;  /* eventually compare field to field */
}
