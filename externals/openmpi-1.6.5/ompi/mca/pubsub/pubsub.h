/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
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
/**
 * @file
 *
 * Dynamic Process Management Interface
 *
 */

#ifndef OMPI_MCA_PUBSUB_H
#define OMPI_MCA_PUBSUB_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"


#include "ompi/info/info.h"

BEGIN_C_DECLS

/*
 * Initialize a module
 */
typedef int (*ompi_pubsub_base_module_init_fn_t)(void);

/*
 * Publish a data item
 */
typedef int (*ompi_pubsub_base_module_publish_fn_t)(char *service, ompi_info_t *info, char *port);

/*
 * Unpublish a data item
 */
typedef int (*ompi_pubsub_base_module_unpublish_fn_t)(char *service, ompi_info_t *info);

/*
 * Lookup a data item
 */
typedef char* (*ompi_pubsub_base_module_lookup_fn_t)(char *service, ompi_info_t *info);

/*
 * Finalize a module
 */
typedef int (*ompi_pubsub_base_module_finalize_fn_t)(void);

/**
* Structure for PUBSUB  modules
 */
struct ompi_pubsub_base_module_1_0_0_t {
    /** Initialization Function */
    ompi_pubsub_base_module_init_fn_t       init;
    /* Publish */
    ompi_pubsub_base_module_publish_fn_t    publish;
    /* Unpublish */
    ompi_pubsub_base_module_unpublish_fn_t  unpublish;
    /* Lookup */
    ompi_pubsub_base_module_lookup_fn_t     lookup;
    /* finalize */
    ompi_pubsub_base_module_finalize_fn_t   finalize;
};
typedef struct ompi_pubsub_base_module_1_0_0_t ompi_pubsub_base_module_1_0_0_t;
typedef struct ompi_pubsub_base_module_1_0_0_t ompi_pubsub_base_module_t;

OMPI_DECLSPEC extern ompi_pubsub_base_module_t ompi_pubsub;


/**
 * Structure for PUBSUB components.
 */
struct ompi_pubsub_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;
};
typedef struct ompi_pubsub_base_component_2_0_0_t ompi_pubsub_base_component_2_0_0_t;
typedef struct ompi_pubsub_base_component_2_0_0_t ompi_pubsub_base_component_t;

/**
 * Macro for use in components that are of type PUBSUB
 */
#define OMPI_PUBSUB_BASE_VERSION_2_0_0 \
    MCA_BASE_VERSION_2_0_0, \
    "pubsub", 2, 0, 0


END_C_DECLS

#endif /* OMPI_MCA_PUBSUB_H */
