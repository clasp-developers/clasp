/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
 
/*
 * DSS Buffer Operations
 */
#include "opal_config.h"

#include "opal/dss/dss_internal.h"


int opal_dss_unload(opal_buffer_t *buffer, void **payload,
                    int32_t *bytes_used)
{
    char *hdr_dst = NULL;
    opal_dss_buffer_type_t type;
    
    /* check that buffer is not null */
    if (!buffer) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* were we given someplace to point to the payload */
    if (NULL == payload) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* anything in the buffer - if not, nothing to do */
    if (NULL == buffer->base_ptr || 0 == buffer->bytes_used) {
        *payload = NULL;
        *bytes_used = 0;
        return OPAL_SUCCESS;
    }

    /* add room for our description of the buffer -- currently just the type */
    if (NULL == (hdr_dst = opal_dss_buffer_extend(buffer, 
                                                  sizeof(opal_dss_buffer_type_t)))) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* add the header (at the end, so perhaps it's a footer? */
    type = buffer->type;
    OPAL_DSS_BUFFER_TYPE_HTON(type);
    memcpy(hdr_dst, &type, sizeof(opal_dss_buffer_type_t));
    buffer->bytes_used += sizeof(opal_dss_buffer_type_t);
    
    /* okay, we have something to provide - pass it back */
    *payload = buffer->base_ptr;
    *bytes_used = buffer->bytes_used;
    
    /* dereference everything in buffer */
    buffer->base_ptr = NULL;
    buffer->pack_ptr = buffer->unpack_ptr = NULL;
    buffer->bytes_allocated = buffer->bytes_used = 0;

    /* All done */

    return OPAL_SUCCESS;
}


int opal_dss_load(opal_buffer_t *buffer, void *payload,
                  int32_t bytes_used)
{
    char *hdr_ptr;
    opal_dss_buffer_type_t type;

    /* check to see if the buffer has been initialized */
    if (NULL == buffer) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* check that the payload is there */
    if (NULL == payload) {
        return OPAL_SUCCESS;
    }
    
    /* check if buffer already has payload - free it if so */
    if (NULL != buffer->base_ptr) {
        free(buffer->base_ptr);
    }

    /* get our header */
    hdr_ptr = (char*) payload + bytes_used - sizeof(opal_dss_buffer_type_t);
    memcpy(&type, hdr_ptr, sizeof(opal_dss_buffer_type_t));
    OPAL_DSS_BUFFER_TYPE_NTOH(type);
    buffer->type = type;
    bytes_used -= sizeof(opal_dss_buffer_type_t);
    
    /* populate the buffer */
    buffer->base_ptr = (char*)payload; 

    /* set pack/unpack pointers */
    buffer->pack_ptr = ((char*)buffer->base_ptr) + bytes_used; 
    buffer->unpack_ptr = buffer->base_ptr;

    /* set counts for size and space */
    buffer->bytes_allocated = buffer->bytes_used = bytes_used;

    /* All done */

    return OPAL_SUCCESS;    
}


/* Copy the UNPACKED portion of a source buffer into a destination buffer
 * The complete contents of the src buffer are NOT copied - only that
 * portion that has not been previously unpacked is copied.
 */
int opal_dss_copy_payload(opal_buffer_t *dest, opal_buffer_t *src)
{
    char *dst_ptr;
    int32_t bytes_left;

    /* ensure we have valid source and destination */
    if (NULL == dest || NULL == src) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* if the dest is already populated, check to ensure that both
     * source and dest are of the same buffer type
     */
    if (0 != dest->bytes_used) {
        if (dest->type != src->type) {
            return OPAL_ERR_BUFFER;
        }
    }
    
    /* either the dest was empty or the two types already match -
     * either way, just ensure the two types DO match
     */
    dest->type = src->type;
    
    /* compute how much of the src buffer remains unpacked
     * buffer->bytes_used is the total number of bytes in the buffer that
     * have been packed. However, we may have already unpacked some of
     * that data. We only want to unload what remains unpacked. This
     * means we have to look at how much of the buffer remains "used"
     * beyond the unpack_ptr
     */
    bytes_left = src->bytes_used - (src->unpack_ptr - src->base_ptr);
    
    /* if nothing is left, then nothing to do */
    if (0 == bytes_left) {
        return OPAL_SUCCESS;
    }
    
    /* add room to the dest for the src buffer's payload */
    if (NULL == (dst_ptr = opal_dss_buffer_extend(dest, bytes_left))) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    
    /* copy the src payload to the specified location in dest */
    memcpy(dst_ptr, src->unpack_ptr, bytes_left);
    
    /* adjust the dest buffer's bookkeeping */
    dest->bytes_used += bytes_left;
    dest->pack_ptr = ((char*)dest->pack_ptr) + bytes_left;
    
    return OPAL_SUCCESS;
}

