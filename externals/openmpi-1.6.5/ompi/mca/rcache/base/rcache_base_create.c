/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/rcache/rcache.h"
#include "ompi/mca/rcache/base/base.h"



mca_rcache_base_module_t* mca_rcache_base_module_create(const char* name) 
{
    
    mca_rcache_base_component_t* component = NULL; 
    mca_rcache_base_module_t* module = NULL; 
    opal_list_item_t* item;
    mca_rcache_base_selected_module_t *sm;
    bool found = false;

    for (item = opal_list_get_first(&mca_rcache_base_components);
         item != opal_list_get_end(&mca_rcache_base_components);
         item = opal_list_get_next(item)) {
        mca_base_component_list_item_t *cli = 
             (mca_base_component_list_item_t *) item;
         component = 
             (mca_rcache_base_component_t *) cli->cli_component;
         if(0 == strcmp(component->rcache_version.mca_component_name, name)) {
             found = true;
             break;
         }
    }
    
    if (!found) {
        return NULL;
    }
    module = component->rcache_init();
    sm = OBJ_NEW(mca_rcache_base_selected_module_t); 
    sm->rcache_component = component; 
    sm->rcache_module = module; 
    opal_list_append(&mca_rcache_base_modules, (opal_list_item_t*) sm); 
    return module; 
}

