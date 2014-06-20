/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/bml/base/base.h" 
#include "opal/mca/base/base.h"

#include "opal/mca/mca.h"
#include "opal/runtime/opal.h"

int mca_bml_base_output = -1;

mca_bml_base_module_t mca_bml = {
    NULL,                    /* bml_component */
    NULL,                    /* bml_add_procs */ 
    NULL,                    /* bml_del_procs */
    NULL,                    /* bml_add_btl */
    NULL,                    /* bml_del_btl */
    NULL,                    /* bml_del_proc_btl */
    NULL,                    /* bml_register */ 
    NULL,                    /* bml_register_error */ 
    NULL,                    /* bml_finalize*/
    NULL                     /* FT event */
};
mca_bml_base_component_t mca_bml_component;

static bool init_called = false;

bool
mca_bml_base_inited(void)
{
    return init_called;
}

int mca_bml_base_init( bool enable_progress_threads, 
                       bool enable_mpi_threads) {
    opal_list_item_t *item = NULL;
    mca_bml_base_component_t *component = NULL, *best_component = NULL; 
    mca_bml_base_module_t *module = NULL, *best_module = NULL; 
    int priority = 0, best_priority = -1; 
    mca_base_component_list_item_t *cli = NULL; 

    init_called = true;

    for (item = opal_list_get_first(&mca_bml_base_components_available); 
         opal_list_get_end(&mca_bml_base_components_available) != item; 
         item = opal_list_get_next(item)) { 
        cli = (mca_base_component_list_item_t*) item; 
        component = (mca_bml_base_component_t*) cli->cli_component; 
        if(NULL == component->bml_init) {
            opal_output_verbose( 10, mca_bml_base_output, 
                                 "select: no init function; ignoring component %s", 
                                 component->bml_version.mca_component_name ); 
            continue; 
        }
        module = component->bml_init(&priority, 
                                     enable_progress_threads, 
                                     enable_mpi_threads); 

        if(NULL == module) { 
            continue; 
        } 
        if(priority > best_priority) { 
            best_priority = priority;
            best_component = component;
            best_module = module;
        }
        
    }
    if(NULL == best_module) { 
        return OMPI_SUCCESS; 
    }
    else { 
        mca_bml_component = *best_component; 
        mca_bml = *best_module; 
        if (opal_profile) {
            opal_output(0, "bml:%s", mca_bml_component.bml_version.mca_component_name);
        }
        return mca_base_components_close(mca_bml_base_output, 
                                         &mca_bml_base_components_available, 
                                         (mca_base_component_t*) best_component,
                                         false); 
    }
}
