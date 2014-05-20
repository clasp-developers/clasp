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
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/sysinfo/sysinfo.h"
#include "opal/mca/sysinfo/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/sysinfo/base/static-components.h"

/* unsupported functions */
static int opal_sysinfo_base_query(char **keys, opal_list_t *values);

/*
 * Globals
 */
int opal_sysinfo_base_output = -1;
opal_list_t opal_sysinfo_base_components_opened;
opal_list_t opal_sysinfo_avail_modules;
bool opal_sysinfo_initialized=false;
bool opal_sysinfo_selected=false;

opal_sysinfo_API_module_t opal_sysinfo = {
    opal_sysinfo_base_query
};


/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int opal_sysinfo_base_open(void)
{
    if (opal_sysinfo_initialized) {
        return OPAL_SUCCESS;
    }
    opal_sysinfo_initialized = true;
    
    opal_sysinfo_base_output = opal_output_open(NULL);

    /* init the list of available modules */
    OBJ_CONSTRUCT(&opal_sysinfo_avail_modules, opal_list_t);
    
    /* Open up all available components */
    OBJ_CONSTRUCT( &opal_sysinfo_base_components_opened, opal_list_t );

    if (OPAL_SUCCESS !=
        mca_base_components_open("sysinfo", opal_sysinfo_base_output,
                                 mca_sysinfo_base_static_components,
                                 &opal_sysinfo_base_components_opened, 
                                 true)) {
        return OPAL_ERROR;
    }

    /* All done */

    return OPAL_SUCCESS;
}

static int opal_sysinfo_base_query(char **keys, opal_list_t *values)
{
    opal_list_item_t *item;
    opal_sysinfo_module_t *mod;
 
    /* query all the available modules */
    for (item = opal_list_get_first(&opal_sysinfo_avail_modules);
         item != opal_list_get_end(&opal_sysinfo_avail_modules);
         item = opal_list_get_next(item)) {
        mod = (opal_sysinfo_module_t*)item;
        if (NULL != mod->module->query) {
            mod->module->query(keys, values);
        }
    }
    return OPAL_SUCCESS;
}

/****   SETUP SYSINFO MODULE OBJECTS   ****/
static void mod_constructor(opal_sysinfo_module_t *ptr)
{
    ptr->module = NULL;
}
OBJ_CLASS_INSTANCE(opal_sysinfo_module_t,
                   opal_list_item_t,
                   mod_constructor, NULL);

static void val_constructor(opal_sysinfo_value_t *ptr)
{
    ptr->key = NULL;
}
static void val_destructor(opal_sysinfo_value_t *ptr)
{
    if (NULL != ptr->key) {
        free(ptr->key);
    }
}
OBJ_CLASS_INSTANCE(opal_sysinfo_value_t,
                   opal_list_item_t,
                   val_constructor, val_destructor);
