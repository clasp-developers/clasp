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

#include "opal/dss/dss_internal.h"

int opal_dss_size(size_t *size, void *src, opal_data_type_t type)
{
    opal_dss_type_info_t *info;

    /* check for error */
    if (NULL == size) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* Lookup the size function for this type and call it */

    if (NULL == (info = (opal_dss_type_info_t*)opal_pointer_array_get_item(&opal_dss_types, type))) {
        return OPAL_ERR_UNKNOWN_DATA_TYPE;
    }

    return info->odti_size_fn(size, src, type);
}

/*
 * STANDARD SIZE FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int opal_dss_std_size(size_t *size, void *src, opal_data_type_t type)
{
    switch(type) {
        case OPAL_BOOL:
            *size = sizeof(bool);
            break;

        case OPAL_INT:
        case OPAL_UINT:
            *size = sizeof(int);
            break;

        case OPAL_SIZE:
            *size = sizeof(size_t);
            break;

        case OPAL_PID:
            *size = sizeof(pid_t);
            break;

        case OPAL_BYTE:
        case OPAL_INT8:
        case OPAL_UINT8:
        case OPAL_NULL:
            *size = 1;
            break;

        case OPAL_INT16:
        case OPAL_UINT16:
            *size = sizeof(uint16_t);
            break;

        case OPAL_INT32:
        case OPAL_UINT32:
            *size = sizeof(uint32_t);
            break;

        case OPAL_INT64:
        case OPAL_UINT64:
            *size = sizeof(uint64_t);
            break;

        case OPAL_DATA_TYPE:
            *size = sizeof(opal_data_type_t);
            break;

        default:
            *size = 0;
            return OPAL_ERR_UNKNOWN_DATA_TYPE;
    }

    return OPAL_SUCCESS;
}

/* SIZE FUNCTIONS FOR NON-STANDARD SYSTEM TYPES */

/*
 * STRING
 */
int opal_dss_size_string(size_t *size, char *src, opal_data_type_t type)
{
    if (NULL != src) {
        *size = strlen(src) + 1;
    } else {
        *size = sizeof(char*);  /* account for NULL */
    }

    return OPAL_SUCCESS;

}

/* SIZE FUNCTIONS FOR GENERIC OPAL TYPES */

/*
 * OPAL_DATA_VALUE
 */
int opal_dss_size_data_value(size_t *size, opal_dss_value_t *src, opal_data_type_t type)
{
    size_t data_size;
    int rc;

    /* account for size of object itself... */
    *size = sizeof(opal_dss_value_t);

    if (NULL != src) {
        /* ...and the number of bytes in the payload, IF an actual object was provided */
        if (OPAL_SUCCESS != (rc = opal_dss.size(&data_size, src->data, src->type))) {
            return rc;
        }
        *size += data_size;
    }

    return OPAL_SUCCESS;
}


/*
 * OPAL_BYTE_OBJECT
 */
int opal_dss_size_byte_object(size_t *size, opal_byte_object_t *src, opal_data_type_t type)
{
    /* account for size of object itself... */
    *size = sizeof(opal_byte_object_t);

    if (NULL != src) {
        /* ...and the number of bytes in the payload, IF an actual object was provided */
        *size += src->size;
    }

    return OPAL_SUCCESS;
}

/*
 * OPAL_PSTAT
 */
int opal_dss_size_pstat(size_t *size, opal_pstats_t *src, opal_data_type_t type)
{
    *size = sizeof(opal_pstats_t);
    return OPAL_SUCCESS;
}

