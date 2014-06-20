/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
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
#ifndef ORTE_FILEM_BASE_H
#define ORTE_FILEM_BASE_H

#include "orte_config.h"

#if !ORTE_DISABLE_FULL_SUPPORT
#include "orte/mca/rml/rml.h"
#endif

#include "orte/mca/filem/filem.h"

/*
 * Global functions for MCA overall FILEM
 */

BEGIN_C_DECLS

/**
 * Initialize the FILEM MCA framework
 *
 * @retval ORTE_SUCCESS Upon success
 * @retval ORTE_ERROR   Upon failures
 * 
 * This function is invoked during orte_init();
 */
ORTE_DECLSPEC int orte_filem_base_open(void);

#if !ORTE_DISABLE_FULL_SUPPORT
/*
 * cmds for base receive
 */
typedef uint8_t orte_filem_cmd_flag_t;
#define ORTE_FILEM_CMD  OPAL_UINT8
#define ORTE_FILEM_GET_PROC_NODE_NAME_CMD  1
#define ORTE_FILEM_GET_REMOTE_PATH_CMD     2

    /**
     * FileM request object maintenance functions
     */
    ORTE_DECLSPEC void orte_filem_base_process_set_construct(orte_filem_base_process_set_t *obj);
    ORTE_DECLSPEC void orte_filem_base_process_set_destruct( orte_filem_base_process_set_t *obj);

    ORTE_DECLSPEC void orte_filem_base_file_set_construct(orte_filem_base_file_set_t *obj);
    ORTE_DECLSPEC void orte_filem_base_file_set_destruct( orte_filem_base_file_set_t *obj);

    ORTE_DECLSPEC void orte_filem_base_construct(orte_filem_base_request_t *obj);
    ORTE_DECLSPEC void orte_filem_base_destruct( orte_filem_base_request_t *obj);

    
    /**
     * Select an available component.
     *
     * @retval ORTE_SUCCESS Upon Success
     * @retval ORTE_NOT_FOUND If no component can be selected
     * @retval ORTE_ERROR Upon other failure
     *
     */
    ORTE_DECLSPEC int orte_filem_base_select(void);
    
    /**
     * Finalize the FILEM MCA framework
     *
     * @retval ORTE_SUCCESS Upon success
     * @retval ORTE_ERROR   Upon failures
     * 
     * This function is invoked during orte_finalize();
     */
    ORTE_DECLSPEC int orte_filem_base_close(void);

    /**
     * Globals
     */
    ORTE_DECLSPEC extern int  orte_filem_base_output;
    ORTE_DECLSPEC extern opal_list_t orte_filem_base_components_available;
    ORTE_DECLSPEC extern orte_filem_base_component_t orte_filem_base_selected_component;
    ORTE_DECLSPEC extern orte_filem_base_module_t orte_filem;
    ORTE_DECLSPEC extern bool orte_filem_base_is_active;

    /**
     * 'None' component functions
     * These are to be used when no component is selected.
     * They just return success, and empty strings as necessary.
     */
    ORTE_DECLSPEC int orte_filem_base_none_open(void);
    ORTE_DECLSPEC int orte_filem_base_none_close(void);
    ORTE_DECLSPEC int orte_filem_base_none_query(mca_base_module_t **module, int *priority);

    int orte_filem_base_module_init(void);
    int orte_filem_base_module_finalize(void);

    int orte_filem_base_none_put(orte_filem_base_request_t *request);
    int orte_filem_base_none_put_nb(orte_filem_base_request_t *request);
    int orte_filem_base_none_get(orte_filem_base_request_t *request);
    int orte_filem_base_none_get_nb(orte_filem_base_request_t *request);
    int orte_filem_base_none_rm( orte_filem_base_request_t *request);
    int orte_filem_base_none_rm_nb( orte_filem_base_request_t *request);
    int orte_filem_base_none_wait( orte_filem_base_request_t *request);
    int orte_filem_base_none_wait_all( opal_list_t *request_list);

    /**
     * Some utility functions
     */
    /* base comm functions */
    ORTE_DECLSPEC int orte_filem_base_comm_start(void);
    ORTE_DECLSPEC int orte_filem_base_comm_stop(void);
    ORTE_DECLSPEC void orte_filem_base_recv(int status, orte_process_name_t* sender,
                                            opal_buffer_t* buffer, orte_rml_tag_t tag,
                                            void* cbdata);


    /**
     * Get Node Name for an ORTE process
     */
    ORTE_DECLSPEC int orte_filem_base_get_proc_node_name(orte_process_name_t *proc, char **machine_name);
    ORTE_DECLSPEC int orte_filem_base_get_remote_path(char **remote_ref, orte_process_name_t *peer, int *flag);

    /**
     * Setup request structure
     */
    ORTE_DECLSPEC int orte_filem_base_prepare_request(orte_filem_base_request_t *request, int move_type);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif /* ORTE_FILEM_BASE_H */
