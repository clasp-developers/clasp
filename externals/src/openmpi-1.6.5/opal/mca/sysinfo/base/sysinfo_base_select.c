/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
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
#include "opal/mca/base/mca_base_param.h"

#include "opal/mca/sysinfo/sysinfo.h"
#include "opal/mca/sysinfo/base/base.h"

/*
 * Globals
 */

int opal_sysinfo_base_select(void)
{
    int pri;
    opal_sysinfo_module_t *module;
    opal_sysinfo_base_module_t *sysmod;
    mca_base_module_t *mod;
    mca_base_component_list_item_t *cli;
    mca_base_component_t *component;
    opal_list_item_t *item;

    if (opal_sysinfo_selected) {
        return OPAL_SUCCESS;
    }
    opal_sysinfo_selected = true;
    
    /*
     * Select all available components
     */
    for (item = opal_list_get_first(&opal_sysinfo_base_components_opened);
         item != opal_list_get_end(&opal_sysinfo_base_components_opened);
         item = opal_list_get_next(item)) {
        
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_base_component_t *) cli->cli_component;

        if (NULL == component->mca_query_component) {
            /* no way to get the module */
            continue;
        }
        if (OPAL_SUCCESS != component->mca_query_component(&mod, &pri)) {
            continue;
        }
        /* init the module */
        sysmod = (opal_sysinfo_base_module_t*)mod;
        if (NULL != sysmod->init) {
            if (OPAL_SUCCESS != sysmod->init()) {
                /* can't run */
                continue;
            }
        }
        module = OBJ_NEW(opal_sysinfo_module_t);
        module->module = sysmod;
        opal_list_append(&opal_sysinfo_avail_modules, &module->super);
    }
    
    return OPAL_SUCCESS;
}
