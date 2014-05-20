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

int opal_dss_get(void **data, opal_dss_value_t *value, opal_data_type_t type)
{
    /* check for error */
    if (NULL == value || NULL == data) {
        return OPAL_ERR_BAD_PARAM;
    }

    /* okay, we assume that the user has provided memory for the destination.
     * check to ensure the data types match.
     * this is absolutely critical as "get" will NOT allocate space
     * but will assume that *data contains a valid address for the
     * type of data being requested
     */
    if (type != value->type) {
        return OPAL_ERR_TYPE_MISMATCH;
    }

    /* point the destination at the value */
    *data = value->data;
    
    return OPAL_SUCCESS;
}

