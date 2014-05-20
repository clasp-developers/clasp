/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#ifndef OMPI_MCA_PUBSUB_BASE_H
#define OMPI_MCA_PUBSUB_BASE_H

#include "ompi_config.h"
#include "ompi/constants.h"

#include "ompi/mca/pubsub/pubsub.h"

/*
 * Global functions for MCA overall PUBSUB
 */

BEGIN_C_DECLS

/**
 * Initialize the PUBSUB MCA framework
 *
 * @retval OMPI_SUCCESS Upon success
 * @retval OMPI_ERROR   Upon failures
 * 
 * This function is invoked during ompi_init();
 */
OMPI_DECLSPEC int ompi_pubsub_base_open(void);

/**
 * Select an available component.
 *
 * @retval OMPI_SUCCESS Upon Success
 * @retval OMPI_NOT_FOUND If no component can be selected
 * @retval OMPI_ERROR Upon other failure
 *
 */
OMPI_DECLSPEC int ompi_pubsub_base_select(void);

/**
 * Finalize the PUBSUB MCA framework
 *
 * @retval OMPI_SUCCESS Upon success
 * @retval OMPI_ERROR   Upon failures
 * 
 * This function is invoked during ompi_finalize();
 */
OMPI_DECLSPEC int ompi_pubsub_base_close(void);


/* NULL functions */
OMPI_DECLSPEC int ompi_pubsub_base_null_publish(char *service, ompi_info_t *info, char *port);
OMPI_DECLSPEC int ompi_pubsub_base_null_unpublish(char *service, ompi_info_t *info);
OMPI_DECLSPEC char* ompi_pubsub_base_null_lookup(char *service, ompi_info_t *info);

/* useful globals */
OMPI_DECLSPEC extern int  ompi_pubsub_base_output;
OMPI_DECLSPEC extern opal_list_t ompi_pubsub_base_components_available;
OMPI_DECLSPEC extern ompi_pubsub_base_component_t ompi_pubsub_base_selected_component;
OMPI_DECLSPEC extern ompi_pubsub_base_module_t ompi_pubsub;

END_C_DECLS

#endif /* OMPI_MCA_PUBSUB_BASE_H */
