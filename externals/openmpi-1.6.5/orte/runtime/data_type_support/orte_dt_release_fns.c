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

#include "orte_config.h"
#include "opal/dss/dss.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

/*
 * STANDARD OBJECT RELEASE
 */
void orte_dt_std_obj_release(opal_dss_value_t *value)
{
    OBJ_RELEASE(value->data);
}

/*
 * STANDARD RELEASE FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
void orte_dt_std_release(opal_dss_value_t *value)
{
    if (NULL == value) return;
    
    free(value->data);
    value->data = NULL;
}
