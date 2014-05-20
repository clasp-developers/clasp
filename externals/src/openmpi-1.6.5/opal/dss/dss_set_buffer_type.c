/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

int opal_dss_set_buffer_type(opal_buffer_t *buffer, opal_dss_buffer_type_t type)
{
    /** check for error */
    if (NULL == buffer) {
        return OPAL_ERR_BAD_PARAM;
    }

    /** see if the buffer is empty - if not, generate error */
    if (buffer->base_ptr != buffer->pack_ptr) {
        return OPAL_ERR_BUFFER;
    }

    /** set the type */
    buffer->type = type;

    return OPAL_SUCCESS;
}

