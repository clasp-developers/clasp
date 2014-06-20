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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdio.h>
#include <string.h>

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"

#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"


OBJ_CLASS_INSTANCE(
    mca_oob_t,
    opal_list_item_t,
    NULL,
    NULL
);

OBJ_CLASS_INSTANCE(
    mca_oob_base_info_t,
    opal_list_item_t,
    NULL,
    NULL
);


/**
 * Function for selecting one module from all those that are
 * available.
 *
 * Call the init function on all available modules.
 */
int mca_oob_base_init(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    mca_oob_base_component_t *component;
    mca_oob_t *module;
    mca_oob_t *s_module = NULL;
    int  s_priority = -1;

    /* Traverse the list of available modules; call their init functions. */
    for (item = opal_list_get_first(&mca_oob_base_components);
        item != opal_list_get_end(&mca_oob_base_components);
        item = opal_list_get_next(item)) {
        mca_oob_base_info_t *inited;

        cli = (mca_base_component_list_item_t *) item;
        component = (mca_oob_base_component_t *) cli->cli_component;

        if (NULL == component->oob_init) {
            opal_output_verbose(10, mca_oob_base_output, "mca_oob_base_init: no init function; ignoring component");
        } else {
            int priority = -1;
            module = component->oob_init(&priority);
            if (NULL != module) {
                inited = OBJ_NEW(mca_oob_base_info_t);
                inited->oob_component = component;
                inited->oob_module = module;
                opal_list_append(&mca_oob_base_modules, &inited->super);

                /* setup highest priority oob channel */
                if(priority > s_priority) {
                    s_priority = priority;
                    s_module = module;
                }
            }
        }
    }
    /* set the global variable to point to the first initialize module */
    if(s_module == NULL) {
        opal_output_verbose(10, mca_oob_base_output, "mca_oob_base_init: no OOB modules available\n");
      return ORTE_ERROR;
   }

   mca_oob = *s_module;
   return ORTE_SUCCESS;
}


/**
 * Called to request the selected oob components to
 * initialize their connections to the HNP (if not an HNP), or
 * to setup a listener for incoming connections (if an HNP)
 */
int mca_oob_base_module_init(void)
{
  opal_list_item_t* item;

  for (item =  opal_list_get_first(&mca_oob_base_modules);
       item != opal_list_get_end(&mca_oob_base_modules);
       item =  opal_list_get_next(item)) {
    mca_oob_base_info_t* base = (mca_oob_base_info_t *) item;
    if (NULL != base->oob_module->oob_init)
        base->oob_module->oob_init();
  }
  return ORTE_SUCCESS;
}
