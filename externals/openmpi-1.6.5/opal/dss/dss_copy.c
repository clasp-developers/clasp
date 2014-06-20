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

int opal_dss_copy(void **dest, void *src, opal_data_type_t type)
{
    opal_dss_type_info_t *info;

    /* check for error */
    if (NULL == dest) {
        return OPAL_ERR_BAD_PARAM;
    }
    if (NULL == src && (OPAL_NULL != type && OPAL_STRING != type)) {
        return OPAL_ERR_BAD_PARAM;
    }

   /* Lookup the copy function for this type and call it */

    if (NULL == (info = (opal_dss_type_info_t*)opal_pointer_array_get_item(&opal_dss_types, type))) {
        return OPAL_ERR_UNKNOWN_DATA_TYPE;
    }

    return info->odti_copy_fn(dest, src, type);
}

/*
 * STANDARD COPY FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int opal_dss_std_copy(void **dest, void *src, opal_data_type_t type)
{
    size_t datasize;
    uint8_t *val = NULL;

    switch(type) {
        case OPAL_BOOL:
            datasize = sizeof(bool);
            break;

        case OPAL_INT:
        case OPAL_UINT:
            datasize = sizeof(int);
            break;

        case OPAL_SIZE:
            datasize = sizeof(size_t);
            break;

        case OPAL_PID:
            datasize = sizeof(pid_t);
            break;

        case OPAL_BYTE:
        case OPAL_INT8:
        case OPAL_UINT8:
            datasize = 1;
            break;

        case OPAL_INT16:
        case OPAL_UINT16:
            datasize = 2;
            break;

        case OPAL_INT32:
        case OPAL_UINT32:
            datasize = 4;
            break;

        case OPAL_INT64:
        case OPAL_UINT64:
            datasize = 8;
            break;

        case OPAL_DATA_TYPE:
            datasize = sizeof(opal_data_type_t);
            break;

        default:
            return OPAL_ERR_UNKNOWN_DATA_TYPE;
    }

    val = (uint8_t*)malloc(datasize);
    if (NULL == val) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    memcpy(val, src, datasize);
    *dest = val;

    return OPAL_SUCCESS;
}

/* COPY FUNCTIONS FOR NON-STANDARD SYSTEM TYPES */

/*
 * NULL
 */
int opal_dss_copy_null(char **dest, char *src, opal_data_type_t type)
{
    char *val;

    *dest = (char*)malloc(sizeof(char*));
    if (NULL == *dest) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    val = *dest;  /* save the address of the value */

    /* set the dest to null */
    *val = 0x00;

    return OPAL_SUCCESS;
}

/*
 * STRING
 */
int opal_dss_copy_string(char **dest, char *src, opal_data_type_t type)
{
    if (NULL == src) {  /* got zero-length string/NULL pointer - store NULL */
        *dest = NULL;
    } else {
        *dest = strdup(src);
    }

    return OPAL_SUCCESS;
}

/* COPY FUNCTIONS FOR GENERIC OPAL TYPES */

/*
 * OPAL_DATA_VALUE
 */
int opal_dss_copy_data_value(opal_dss_value_t **dest, opal_dss_value_t *src,
                             opal_data_type_t type)
{
    int rc;

    /* create the new object */
    *dest = OBJ_NEW(opal_dss_value_t);
    if (NULL == *dest) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    (*dest)->type = src->type;

    /* copy the payload with its associated copy function */
    if (OPAL_SUCCESS != (rc = opal_dss.copy(&((*dest)->data), src->data, src->type))) {
        OBJ_RELEASE(*dest);
        return rc;
    }

    return OPAL_SUCCESS;
}


/*
 * OPAL_BYTE_OBJECT
 */
int opal_dss_copy_byte_object(opal_byte_object_t **dest, opal_byte_object_t *src,
                              opal_data_type_t type)
{
    /* allocate space for the new object */
    *dest = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
    if (NULL == *dest) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    (*dest)->size = src->size;

    /* allocate the required space for the bytes */
    (*dest)->bytes = (uint8_t*)malloc(src->size);
    if (NULL == (*dest)->bytes) {
        OBJ_RELEASE(*dest);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* copy the data across */
    memcpy((*dest)->bytes, src->bytes, src->size);

    return OPAL_SUCCESS;
}

/* OPAL_PSTAT */
int opal_dss_copy_pstat(opal_pstats_t **dest, opal_pstats_t *src,
                        opal_data_type_t type)
{
    opal_pstats_t *p;
    
    /* create the new object */
    *dest = OBJ_NEW(opal_pstats_t);
    if (NULL == *dest) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    p = *dest;
    
    /* copy the individual fields */
    memcpy(p->node, src->node, sizeof(src->node));
    p->rank = src->rank;
    p->pid = src->pid;
    memcpy(p->cmd, src->cmd, sizeof(src->node));
    p->state = src->state;
    p->time = src->time;
    p->priority = src->priority;
    p->num_threads = src->num_threads;
    p->vsize = src->vsize;
    p->rss = src->rss;
    p->peak_vsize = src->peak_vsize;
    p->shared_size = src->shared_size;
    p->processor = src->processor;
    
    return OPAL_SUCCESS;
}
