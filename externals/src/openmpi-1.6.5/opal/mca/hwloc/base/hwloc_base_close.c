/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
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
#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/hwloc/base/base.h"

int opal_hwloc_base_close(void)
{
    if (!opal_hwloc_base_inited) {
        return OPAL_SUCCESS;
    }

#if OPAL_HAVE_HWLOC
    {
        opal_list_item_t *item;

        /* no need to close the component as it was statically opened */

        /* for support of tools such as ompi_info */
        for (item = opal_list_remove_first(&opal_hwloc_base_components);
             NULL != item; 
             item = opal_list_remove_first(&opal_hwloc_base_components)) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&opal_hwloc_base_components);
    }
#endif

    /* All done */
    opal_hwloc_base_inited = false;
    return OPAL_SUCCESS;
}
