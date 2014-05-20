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

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "opal/class/opal_list.h"


int
ompi_osc_base_finalize(void)
{
    opal_list_item_t* item;

    /* Finalize all available modules */
    while (NULL != 
           (item = opal_list_remove_first(&ompi_osc_base_avail_components))) {
        ompi_osc_base_component_t *component = (ompi_osc_base_component_t*)
            ((mca_base_component_list_item_t*) item)->cli_component;
        component->osc_finalize();
    }
    return OMPI_SUCCESS;
}

int 
ompi_osc_base_close(void)
{
    /* close all components not already closed*/

    /* join the two lists of components */
    opal_list_join(&ompi_osc_base_open_components,
                   opal_list_get_end(&ompi_osc_base_open_components),
                   &ompi_osc_base_avail_components);

    mca_base_components_close(ompi_osc_base_output,
                              &ompi_osc_base_open_components, NULL, true);

    OBJ_DESTRUCT(&ompi_osc_base_open_components);
    OBJ_DESTRUCT(&ompi_osc_base_avail_components);

    return OMPI_SUCCESS;
}
