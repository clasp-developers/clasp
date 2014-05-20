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

#include "opal_config.h"

#include "opal/dss/dss_internal.h"

void opal_dss_release(opal_dss_value_t *value)
{
    opal_dss_type_info_t *info = NULL;

    /* check for error */
    if (NULL == value) {
        return;
    }

    /* Lookup the release function for this type and call it */

    if (NULL == (info = (opal_dss_type_info_t*)opal_pointer_array_get_item(&opal_dss_types, value->type))) {
        return;
    }

    info->odti_release_fn(value);
}

/*
 * STANDARD RELEASE FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
void opal_dss_std_release(opal_dss_value_t *value)
{
    free(value->data);
    value->data = NULL;
}

/*
 * STANDARD OBJECT RELEASE FUNCTION - WORKS FOR EVERYTHING
 */
void opal_dss_std_obj_release(opal_dss_value_t *value)
{
   OBJ_RELEASE(value->data);
}


/*
 * OPAL_BYTE_OBJECT
 */
void opal_dss_release_byte_object(opal_dss_value_t *value)
{
   opal_byte_object_t *bo;

   bo = (opal_byte_object_t*)value->data;
   free(bo->bytes);

   free(value->data);
   value->data = NULL;
}
