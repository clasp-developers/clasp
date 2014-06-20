/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include "pubsub_orte.h"

static int pubsub_orte_component_open(void);
static int pubsub_orte_component_close(void);
static int pubsub_orte_component_query(mca_base_module_t **module, int *priority);

static int my_priority = 50;

ompi_pubsub_orte_component_t mca_pubsub_orte_component = {
    {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */

        {
          OMPI_PUBSUB_BASE_VERSION_2_0_0,
        
          "orte", /* MCA component name */
          OMPI_MAJOR_VERSION,  /* MCA component major version */
          OMPI_MINOR_VERSION,  /* MCA component minor version */
          OMPI_RELEASE_VERSION,  /* MCA component release version */
          pubsub_orte_component_open,  /* component open */
          pubsub_orte_component_close, /* component close */
          pubsub_orte_component_query  /* component query */
        },
        {
            /* This component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


static int pubsub_orte_component_open(void)
{
    mca_base_component_t *c = &mca_pubsub_orte_component.super.base_version;

    mca_base_param_reg_int(c, "priority",
                           "Priority of the pubsub pmi component",
                           false, false, my_priority,
                           &my_priority);
    return OMPI_SUCCESS;
}

static int pubsub_orte_component_close(void)
{
    if (NULL != mca_pubsub_orte_component.server_uri) {
        free(mca_pubsub_orte_component.server_uri);
        mca_pubsub_orte_component.server_uri = NULL;
    }
    return OMPI_SUCCESS;
}

static int pubsub_orte_component_query(mca_base_module_t **module, int *priority)
{
    mca_base_component_t *comp = &mca_pubsub_orte_component.super.base_version;
    
    mca_base_param_reg_string(comp, "server",
                              "Contact info for ompi_server for publish/subscribe operations",
                              false, false, NULL,
                              &mca_pubsub_orte_component.server_uri);
    
    mca_pubsub_orte_component.server_found = false;
    
    *priority = my_priority;
    *module = (mca_base_module_t *) &ompi_pubsub_orte_module;
    return OMPI_SUCCESS;
}
