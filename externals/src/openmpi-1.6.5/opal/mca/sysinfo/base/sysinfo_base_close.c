/*
 * Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
 *
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
#include "opal/mca/sysinfo/sysinfo.h"
#include "opal/mca/sysinfo/base/base.h"

int opal_sysinfo_base_close(void)
{
    opal_list_item_t *item;
    opal_sysinfo_module_t *mod;
    
    /* call the finalize of all available modules */
    while (NULL != (item = opal_list_remove_first(&opal_sysinfo_avail_modules))) {
        mod = (opal_sysinfo_module_t*)item;
        if (NULL != mod->module->finalize) {
            mod->module->finalize();
        }
    }
    OBJ_DESTRUCT(&opal_sysinfo_avail_modules);
    
    /* Close all components that are still open (this should only
     happen during ompi_info). */
    
    mca_base_components_close(opal_sysinfo_base_output,
                              &opal_sysinfo_base_components_opened,
                              NULL, true);
    OBJ_DESTRUCT(&opal_sysinfo_base_components_opened);
    
    /* All done */
    
    return OPAL_SUCCESS;
}
