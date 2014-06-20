/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_PUBSUB_ORTE_H
#define OMPI_PUBSUB_ORTE_H

#include "ompi_config.h"

#include "orte/types.h"

#include "ompi/mca/pubsub/pubsub.h"

BEGIN_C_DECLS

/*
 * Extend the pubsub component to hold some useful
 * values for this component
 */
typedef struct {
    ompi_pubsub_base_component_t super;
    orte_process_name_t server;
    char *server_uri;
    bool server_found;
} ompi_pubsub_orte_component_t;

/* access to module */
extern ompi_pubsub_base_module_t ompi_pubsub_orte_module;

/* access to component so we can get to the locally
 * global values
 */
OMPI_MODULE_DECLSPEC extern ompi_pubsub_orte_component_t mca_pubsub_orte_component;

END_C_DECLS

#endif /* OMPI_PUBSUB_ORTE_H */
