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
#include "opal/types.h"
#include "opal/util/output.h"
#include "opal/dss/dss_internal.h"

int opal_dss_unpack(opal_buffer_t *buffer, void *dst, int32_t *num_vals,
                    opal_data_type_t type)
{
    int rc, ret;
    int32_t local_num, n=1;
    opal_data_type_t local_type;

    /* check for error */
    if (NULL == buffer || NULL == dst || NULL == num_vals) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* if user provides a zero for num_vals, then there is no storage allocated
     * so return an appropriate error
     */
    if (0 == *num_vals) {
        return OPAL_ERR_UNPACK_INADEQUATE_SPACE;
    }

    /** Unpack the declared number of values
     * REMINDER: it is possible that the buffer is corrupted and that
     * the DSS will *think* there is a proper int32_t variable at the
     * beginning of the unpack region - but that the value is bogus (e.g., just
     * a byte field in a string array that so happens to have a value that
     * matches the int32_t data type flag). Therefore, this error check is
     * NOT completely safe. This is true for ALL unpack functions, not just
     * int32_t as used here.
     */
    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        if (OPAL_SUCCESS != (
            rc = opal_dss_get_data_type(buffer, &local_type))) {
            *num_vals = 0;
            return rc;
        }
        if (OPAL_INT32 != local_type) { /* if the length wasn't first, then error */
            *num_vals = 0;
            return OPAL_ERR_UNPACK_FAILURE;
        }
    }

    n=1;
    if (OPAL_SUCCESS != (rc = opal_dss_unpack_int32(buffer, &local_num, &n, OPAL_INT32))) {
        *num_vals = 0;
        return rc;
    }

    /** if the storage provided is inadequate, set things up
     * to unpack as much as we can and to return an error code
     * indicating that everything was not unpacked - the buffer
     * is left in a state where it can not be further unpacked.
     */
    if (local_num > *num_vals) {
        local_num = *num_vals;
        ret = OPAL_ERR_UNPACK_INADEQUATE_SPACE;
    } else {  /** enough or more than enough storage */
        *num_vals = local_num;  /** let the user know how many we actually unpacked */
        ret = OPAL_SUCCESS;
    }

    /** Unpack the value(s) */
    if (OPAL_SUCCESS != (rc = opal_dss_unpack_buffer(buffer, dst, &local_num, type))) {
        *num_vals = 0;
        ret = rc;
    }

    return ret;
}

int opal_dss_unpack_buffer(opal_buffer_t *buffer, void *dst, int32_t *num_vals,
                    opal_data_type_t type)
{
    int rc;
    opal_data_type_t local_type;
    opal_dss_type_info_t *info;

    OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_buffer( %p, %p, %lu, %d )\n",
                   (void*)buffer, dst, (long unsigned int)*num_vals, (int)type ) );

    /** Unpack the declared data type */
    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        if (OPAL_SUCCESS != (rc = opal_dss_get_data_type(buffer, &local_type))) {
            return rc;
        }
        /* if the data types don't match, then return an error */
        if (type != local_type) {
            opal_output(0, "OPAL dss:unpack: got type %d when expecting type %d", local_type, type);
            return OPAL_ERR_PACK_MISMATCH;
        }
    }

    /* Lookup the unpack function for this type and call it */

    if (NULL == (info = (opal_dss_type_info_t*)opal_pointer_array_get_item(&opal_dss_types, type))) {
        return OPAL_ERR_UNPACK_FAILURE;
    }

    return info->odti_unpack_fn(buffer, dst, num_vals, type);
}


/* UNPACK GENERIC SYSTEM TYPES */

/*
 * BOOL
 */
int opal_dss_unpack_bool(opal_buffer_t *buffer, void *dest,
                         int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    opal_data_type_t remote_type;

    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (OPAL_SUCCESS != (ret = opal_dss_peek_type(buffer, &remote_type))) {
            return ret;
        }
    } else {
        if (OPAL_SUCCESS != (ret = opal_dss_get_data_type(buffer, &remote_type))) {
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_BOOL) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_BOOL))) {
        }
    } else {
        /* slow path - types are different sizes */
        UNPACK_SIZE_MISMATCH(bool, remote_type, ret);
    }
    return ret;
}

/*
 * INT
 */
int opal_dss_unpack_int(opal_buffer_t *buffer, void *dest,
                        int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    opal_data_type_t remote_type;

    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (OPAL_SUCCESS != (ret = opal_dss_peek_type(buffer, &remote_type))) {
            return ret;
        }
    } else {
        if (OPAL_SUCCESS != (ret = opal_dss_get_data_type(buffer, &remote_type))) {
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_INT) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_INT))) {
        }
    } else {
        /* slow path - types are different sizes */
        UNPACK_SIZE_MISMATCH(int, remote_type, ret);
    }
    
    return ret;
}

/*
 * SIZE_T
 */
int opal_dss_unpack_sizet(opal_buffer_t *buffer, void *dest,
                          int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    opal_data_type_t remote_type;

    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (OPAL_SUCCESS != (ret = opal_dss_peek_type(buffer, &remote_type))) {
            return ret;
        }
    } else {
        if (OPAL_SUCCESS != (ret = opal_dss_get_data_type(buffer, &remote_type))) {
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_SIZE_T) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_SIZE_T))) {
        }
    } else {
        /* slow path - types are different sizes */
        UNPACK_SIZE_MISMATCH(size_t, remote_type, ret);
    }
    
    return ret;
}

/*
 * PID_T
 */
int opal_dss_unpack_pid(opal_buffer_t *buffer, void *dest,
                        int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    opal_data_type_t remote_type;

    if (OPAL_DSS_BUFFER_FULLY_DESC == buffer->type) {
        /* see what type was actually packed */
        if (OPAL_SUCCESS != (ret = opal_dss_peek_type(buffer, &remote_type))) {
            return ret;
        }
    } else {
        if (OPAL_SUCCESS != (ret = opal_dss_get_data_type(buffer, &remote_type))) {
            return ret;
        }
    }

    if (remote_type == DSS_TYPE_PID_T) {
        /* fast path it if the sizes are the same */
        /* Turn around and unpack the real type */
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, dest, num_vals, DSS_TYPE_PID_T))) {
        }
    } else {
        /* slow path - types are different sizes */
        UNPACK_SIZE_MISMATCH(pid_t, remote_type, ret);
    }
    
    return ret;
}


/* UNPACK FUNCTIONS FOR NON-GENERIC SYSTEM TYPES */

/*
 * NULL
 */
int opal_dss_unpack_null(opal_buffer_t *buffer, void *dest,
                         int32_t *num_vals, opal_data_type_t type)
{
    OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_null * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (opal_dss_too_small(buffer, *num_vals)) {
        return OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    memcpy(dest, buffer->unpack_ptr, *num_vals);

    /* update buffer pointer */
    buffer->unpack_ptr += *num_vals;

    return OPAL_SUCCESS;
}

/*
 * BYTE, CHAR, INT8
 */
int opal_dss_unpack_byte(opal_buffer_t *buffer, void *dest,
                         int32_t *num_vals, opal_data_type_t type)
{
    OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_byte * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (opal_dss_too_small(buffer, *num_vals)) {
        return OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    memcpy(dest, buffer->unpack_ptr, *num_vals);

    /* update buffer pointer */
    buffer->unpack_ptr += *num_vals;

    return OPAL_SUCCESS;
}

int opal_dss_unpack_int16(opal_buffer_t *buffer, void *dest,
                          int32_t *num_vals, opal_data_type_t type)
{
    int32_t i;
    uint16_t tmp, *desttmp = (uint16_t*) dest;

   OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_int16 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (opal_dss_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        desttmp[i] = ntohs(tmp);
        buffer->unpack_ptr += sizeof(tmp);
    }

    return OPAL_SUCCESS;
}

int opal_dss_unpack_int32(opal_buffer_t *buffer, void *dest,
                          int32_t *num_vals, opal_data_type_t type)
{
    int32_t i;
    uint32_t tmp, *desttmp = (uint32_t*) dest;

   OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_int32 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (opal_dss_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        desttmp[i] = ntohl(tmp);
        buffer->unpack_ptr += sizeof(tmp);
    }

    return OPAL_SUCCESS;
}

int opal_dss_unpack_int64(opal_buffer_t *buffer, void *dest,
                          int32_t *num_vals, opal_data_type_t type)
{
    int32_t i;
    uint64_t tmp, *desttmp = (uint64_t*) dest;

   OPAL_OUTPUT( ( opal_dss_verbose, "opal_dss_unpack_int64 * %d\n", (int)*num_vals ) );
    /* check to see if there's enough data in buffer */
    if (opal_dss_too_small(buffer, (*num_vals)*sizeof(tmp))) {
        return OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER;
    }

    /* unpack the data */
    for (i = 0; i < (*num_vals); ++i) {
        memcpy( &(tmp), buffer->unpack_ptr, sizeof(tmp) );
        desttmp[i] = ntoh64(tmp);
        buffer->unpack_ptr += sizeof(tmp);
    }

    return OPAL_SUCCESS;
}

int opal_dss_unpack_string(opal_buffer_t *buffer, void *dest,
                           int32_t *num_vals, opal_data_type_t type)
{
    int ret;
    int32_t i, len, n=1;
    char **sdest = (char**) dest;

    for (i = 0; i < (*num_vals); ++i) {
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_int32(buffer, &len, &n, OPAL_INT32))) {
            return ret;
        }
        if (0 ==  len) {   /* zero-length string - unpack the NULL */
            sdest[i] = NULL;
        } else {
        sdest[i] = (char*)malloc(len);
            if (NULL == sdest[i]) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_byte(buffer, sdest[i], &len, OPAL_BYTE))) {
                return ret;
            }
        }
    }

    return OPAL_SUCCESS;
}


/* UNPACK FUNCTIONS FOR GENERIC OPAL TYPES */

/*
 * OPAL_DATA_TYPE
 */
int opal_dss_unpack_data_type(opal_buffer_t *buffer, void *dest, int32_t *num_vals,
                             opal_data_type_t type)
{
     /* turn around and unpack the real type */
    return opal_dss_unpack_buffer(buffer, dest, num_vals, OPAL_DATA_TYPE_T);
}

/*
 * OPAL_DATA_VALUE
 */
int opal_dss_unpack_data_value(opal_buffer_t *buffer, void *dest, int32_t *num,
                             opal_data_type_t type)
{
    opal_dss_type_info_t *info;
    opal_dss_value_t **ddv;
    int32_t i, n;
    opal_data_type_t dt;
    size_t nsize;
    int ret;

    ddv = (opal_dss_value_t **) dest;

    for (i = 0; i < *num; ++i) {
        /* see what the data type is */
        n = 1;
        if (OPAL_SUCCESS != (ret = opal_dss_get_data_type(buffer, &dt))) {
            return ret;
        }
        
        /* if it is OPAL_NULL, then do nothing */
        if (OPAL_NULL == dt) continue;
        
        /* otherwise, allocate the new object and set the type */
        
        ddv[i] = OBJ_NEW(opal_dss_value_t);
        if (NULL == ddv[i]) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        ddv[i]->type = dt;

        /* if it is UNDEF, then nothing more to do */
        if (OPAL_UNDEF == ddv[i]->type) continue;

        /* get enough memory to hold it */
        if (OPAL_SUCCESS != (ret = opal_dss.size(&nsize, NULL, ddv[i]->type))) {
            return ret;
        }
        ddv[i]->data = (void*)malloc(nsize);
        if (NULL == ddv[i]->data) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        /* Lookup the unpack function for this type and call it */

        if (NULL == (info = (opal_dss_type_info_t*)opal_pointer_array_get_item(&opal_dss_types, ddv[i]->type))) {
            return OPAL_ERR_PACK_FAILURE;
        }

        if (info->odti_structured) {
            n=1;
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &(ddv[i]->data), &n, ddv[i]->type))) {
                return ret;
            }
        } else {
            n=1;
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, ddv[i]->data, &n, ddv[i]->type))) {
                return ret;
            }
        }
    }

    return OPAL_SUCCESS;
}


/*
 * OPAL_BYTE_OBJECT
 */
int opal_dss_unpack_byte_object(opal_buffer_t *buffer, void *dest, int32_t *num,
                             opal_data_type_t type)
{
    int ret;
    int32_t i, n, m=1;
    opal_byte_object_t **dbyteptr;

    dbyteptr = (opal_byte_object_t**)dest;
    n = *num;
    for(i=0; i<n; i++) {
        /* allocate memory for the byte object itself */
        dbyteptr[i] = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
        if (NULL == dbyteptr[i]) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        /* unpack object size in bytes */
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_int32(buffer, &(dbyteptr[i]->size), &m, OPAL_INT32))) {
            return ret;
        }
        if (0 < dbyteptr[i]->size) {
            dbyteptr[i]->bytes = (uint8_t*)malloc(dbyteptr[i]->size);
            if (NULL == dbyteptr[i]->bytes) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            if (OPAL_SUCCESS != (ret = opal_dss_unpack_byte(buffer, (dbyteptr[i]->bytes),
                                            &(dbyteptr[i]->size), OPAL_BYTE))) {
                return ret;
            }
        }
    }

    return OPAL_SUCCESS;
}

/*
 * OPAL_PSTAT
 */
int opal_dss_unpack_pstat(opal_buffer_t *buffer, void *dest,
                          int32_t *num_vals, opal_data_type_t type)
{
    opal_pstats_t **ptr;
    int32_t i, n, m;
    int ret;
    char *cptr;
    
    ptr = (opal_pstats_t **) dest;
    n = *num_vals;
    
    for (i = 0; i < n; ++i) {
        /* allocate the new object */
        ptr[i] = OBJ_NEW(opal_pstats_t);
        if (NULL == ptr[i]) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &cptr, &m, OPAL_STRING))) {
            return ret;
        }
        memmove(ptr[i]->node, cptr, strlen(cptr));
        free(cptr);
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->rank, &m, OPAL_INT32))) {
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->pid, &m, OPAL_PID))) {
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &cptr, &m, OPAL_STRING))) {
            return ret;
        }
        memmove(ptr[i]->cmd, cptr, strlen(cptr));
        free(cptr);
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->state, &m, OPAL_BYTE))) {
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->time, &m, OPAL_UINT64))) {
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->priority, &m, OPAL_INT32))) {
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->num_threads, &m, OPAL_INT16))) {
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->vsize, &m, OPAL_UINT64))) {
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->rss, &m, OPAL_UINT64))) {
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->peak_vsize, &m, OPAL_UINT64))) {
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->shared_size, &m, OPAL_UINT64))) {
            return ret;
        }
        m=1;
        if (OPAL_SUCCESS != (ret = opal_dss_unpack_buffer(buffer, &ptr[i]->processor, &m, OPAL_INT16))) {
            return ret;
        }
    }
    
    return OPAL_SUCCESS;
}
