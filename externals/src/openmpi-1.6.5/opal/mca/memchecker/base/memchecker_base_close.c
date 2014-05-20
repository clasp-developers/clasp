/*
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "opal/mca/memchecker/base/base.h"
#include "opal/mca/memchecker/memchecker.h"

int opal_memchecker_base_close(void)
{
    /* Close all components that are still open (this should only
       happen during laminfo). */
    mca_base_components_close(0, &opal_memchecker_base_components_opened,
                              NULL, true);
    OBJ_DESTRUCT(&opal_memchecker_base_components_opened);

    /* All done */
    return OPAL_SUCCESS;
}
