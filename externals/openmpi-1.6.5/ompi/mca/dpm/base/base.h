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
#ifndef OMPI_MCA_DPM_BASE_H
#define OMPI_MCA_DPM_BASE_H

#include "ompi_config.h"
#include "ompi/constants.h"

#include "ompi/mca/dpm/dpm.h"

/*
 * Global functions for MCA overall DPM
 */

BEGIN_C_DECLS

struct ompi_dpm_base_disconnect_obj {
    ompi_communicator_t       *comm;
    int                       size;
    struct ompi_request_t     **reqs;
    int                       buf;
};
typedef struct ompi_dpm_base_disconnect_obj ompi_dpm_base_disconnect_obj;

/**
 * Initialize the DPM MCA framework
 *
 * @retval OMPI_SUCCESS Upon success
 * @retval OMPI_ERROR   Upon failures
 * 
 * This function is invoked during ompi_init();
 */
OMPI_DECLSPEC int ompi_dpm_base_open(void);

/**
 * Select an available component.
 *
 * @retval OMPI_SUCCESS Upon Success
 * @retval OMPI_NOT_FOUND If no component can be selected
 * @retval OMPI_ERROR Upon other failure
 *
 */
OMPI_DECLSPEC int ompi_dpm_base_select(void);

/**
 * Finalize the DPM MCA framework
 *
 * @retval OMPI_SUCCESS Upon success
 * @retval OMPI_ERROR   Upon failures
 * 
 * This function is invoked during ompi_finalize();
 */
OMPI_DECLSPEC int ompi_dpm_base_close(void);

#if !ORTE_DISABLE_FULL_SUPPORT

/* Internal support functions */
OMPI_DECLSPEC char* ompi_dpm_base_dyn_init (void);
OMPI_DECLSPEC int ompi_dpm_base_dyn_finalize (void);
OMPI_DECLSPEC void ompi_dpm_base_mark_dyncomm (ompi_communicator_t *comm);
OMPI_DECLSPEC ompi_dpm_base_disconnect_obj *ompi_dpm_base_disconnect_init ( ompi_communicator_t *comm);
OMPI_DECLSPEC void ompi_dpm_base_disconnect_waitall (int count, ompi_dpm_base_disconnect_obj **objs);

#endif

/* NULL component functions */
int ompi_dpm_base_null_connect_accept (ompi_communicator_t *comm, int root,
                                       char *port_string, bool send_first,
                                       ompi_communicator_t **newcomm);
void ompi_dpm_base_null_disconnect(ompi_communicator_t *comm);
int ompi_dpm_base_null_spawn(int count, char **array_of_commands,
                             char ***array_of_argv,
                             int *array_of_maxprocs,
                             MPI_Info *array_of_info,
                             char *port_name);
int ompi_dpm_base_null_dyn_init(void);
int ompi_dpm_base_null_dyn_finalize (void);
void ompi_dpm_base_null_mark_dyncomm (ompi_communicator_t *comm);
int ompi_dpm_base_null_open_port(char *port_name, orte_rml_tag_t given_tag);
int ompi_dpm_base_null_parse_port(char *port_name, 
                                  char **hnp_uri, char **rml_uri, orte_rml_tag_t *tag);
int ompi_dpm_base_null_route_to_port(char *rml_uri, orte_process_name_t *rproc);
int ompi_dpm_base_null_close_port(char *port_name);

/* useful globals */
OMPI_DECLSPEC extern int  ompi_dpm_base_output;
OMPI_DECLSPEC extern opal_list_t ompi_dpm_base_components_available;
OMPI_DECLSPEC extern ompi_dpm_base_component_t ompi_dpm_base_selected_component;
OMPI_DECLSPEC extern ompi_dpm_base_module_t ompi_dpm;

END_C_DECLS

#endif /* OMPI_MCA_DPM_BASE_H */
