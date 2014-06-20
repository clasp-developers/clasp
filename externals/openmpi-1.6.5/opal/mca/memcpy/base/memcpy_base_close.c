/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/memcpy/memcpy.h"
#include "opal/mca/memcpy/base/base.h"


int opal_memcpy_base_close(void)
{
    /* Close all components that are still open (this should only
       happen during laminfo). */
    mca_base_components_close(0, &opal_memcpy_base_components_opened, 
                              NULL, true);
    OBJ_DESTRUCT(&opal_memcpy_base_components_opened);

    /* All done */
    return OPAL_SUCCESS;
}
