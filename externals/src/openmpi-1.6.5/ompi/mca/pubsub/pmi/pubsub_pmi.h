/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_PUBSUB_PMI_H
#define OMPI_PUBSUB_PMI_H

#include "ompi_config.h"

#include "orte/types.h"

#include "ompi/mca/pubsub/pubsub.h"

BEGIN_C_DECLS

/* access to module */
extern ompi_pubsub_base_module_t ompi_pubsub_pmi_module;

/* access to component */
OMPI_MODULE_DECLSPEC extern ompi_pubsub_base_component_t mca_pubsub_pmi_component;

END_C_DECLS

#endif /* OMPI_PUBSUB_PMI_H */
