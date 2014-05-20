/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "ompi/mca/pubsub/pubsub.h"
#include "ompi/mca/pubsub/base/base.h"


int ompi_pubsub_base_select(void)
{
    int ret;
    ompi_pubsub_base_component_t *best_component = NULL;
    ompi_pubsub_base_module_t *best_module = NULL;

    /*
     * Select the best component
     */
    if( OPAL_SUCCESS != (ret = mca_base_select("pubsub", ompi_pubsub_base_output,
                                        &ompi_pubsub_base_components_available,
                                        (mca_base_module_t **) &best_module,
                                        (mca_base_component_t **) &best_component))) {
        /* it is okay not to find any executable components */
        if (OMPI_ERR_NOT_FOUND == ret) {
            ret = OPAL_SUCCESS;
        }
        goto cleanup;
    }

    /* Save the winner */
    ompi_pubsub = *best_module;
    ompi_pubsub_base_selected_component = *best_component;
    
    /* init the selected module */
    if (NULL != ompi_pubsub.init) {
        ret = ompi_pubsub.init();
    }

 cleanup:
    return ret;
}
