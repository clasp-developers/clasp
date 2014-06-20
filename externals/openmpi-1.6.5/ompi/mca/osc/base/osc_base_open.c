/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"


#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "ompi/mca/osc/base/static-components.h"


opal_list_t ompi_osc_base_open_components;
opal_list_t ompi_osc_base_avail_components;
int ompi_osc_base_output = 0;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int
ompi_osc_base_open(void)
{
    int ret;

    /* setup the output stream */
    ompi_osc_base_output = opal_output_open(NULL);

    /* initialize the base code */
    OBJ_CONSTRUCT(&ompi_osc_base_open_components, opal_list_t);
    OBJ_CONSTRUCT(&ompi_osc_base_avail_components, opal_list_t);

    /* Open up all available components */
    if (OMPI_SUCCESS != 
        (ret = mca_base_components_open("osc", ompi_osc_base_output,
                                        mca_osc_base_static_components, 
                                        &ompi_osc_base_open_components, true))) {
        return ret;
    }

    /* All done */
    return OMPI_SUCCESS;
}


int
ompi_osc_base_find_available(bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    opal_list_item_t *component_item, *tmp;
    
    for (component_item = opal_list_get_first(&ompi_osc_base_open_components) ; 
         component_item != opal_list_get_end(&ompi_osc_base_open_components) ;
         component_item = opal_list_get_next(component_item)) {
        int ret;
        ompi_osc_base_component_t *component = (ompi_osc_base_component_t*)
            ((mca_base_component_list_item_t*) component_item)->cli_component;

        /* see if this component is ready to run... */
        ret = component->osc_init(enable_progress_threads, enable_mpi_threads);
        if (OMPI_SUCCESS != ret) {
            /* leave the component in the list and move on */
            continue;
        } else {
            /* the component is useable on this node.  put it in the
               available list */
            tmp = component_item;
            component_item = opal_list_remove_item(&ompi_osc_base_open_components,
                                                   component_item);
            opal_list_append(&ompi_osc_base_avail_components, tmp);
        }
    }

    mca_base_components_close(ompi_osc_base_output,
                              &ompi_osc_base_open_components, NULL, false);

    return OMPI_SUCCESS;
}
