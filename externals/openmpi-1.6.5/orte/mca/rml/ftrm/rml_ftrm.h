/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 * ORTE RML Fault Tolerance Wrapper - Ready Message Protocol (FTRM)
 *
 * @file
 */
#ifndef MCA_RML_FTRM_H
#define MCA_RML_FTRM_H

#include "orte_config.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"

BEGIN_C_DECLS

    extern int rml_ftrm_output_handle;

    /*
     * Component Information
     */
    ORTE_MODULE_DECLSPEC extern orte_rml_component_t mca_rml_ftrm_component;
    ORTE_MODULE_DECLSPEC extern orte_rml_module_t    orte_rml_ftrm_module;

    ORTE_MODULE_DECLSPEC extern orte_rml_component_t mca_rml_ftrm_wrapped_component;
    ORTE_MODULE_DECLSPEC extern orte_rml_module_t    orte_rml_ftrm_wrapped_module;

    /*
     * Init (Component)
     */
    orte_rml_module_t* orte_rml_ftrm_component_init(int  *priority);

    /*
     * Init (Module)
     */
    int orte_rml_ftrm_module_enable_comm(void);
    
    /*
     * Finalize (Module)
     */
    int orte_rml_ftrm_module_finalize(void);

    /*
     * Get URI
     */
    char * orte_rml_ftrm_get_contact_info(void);

    /*
     * Set URI
     */
    int orte_rml_ftrm_set_contact_info(const char* uri);

    int orte_rml_ftrm_get_new_name(orte_process_name_t *name);

    /*
     * Ping
     */
    int orte_rml_ftrm_ping(const char* uri, const struct timeval* tv);

    /*
     * Send
     */
    int orte_rml_ftrm_send(orte_process_name_t* peer,
                           struct iovec *msg,
                           int count,
                           int tag,
                           int flags);

    /*
     * Send Non-blocking
     */
    int orte_rml_ftrm_send_nb(orte_process_name_t* peer,
                              struct iovec* msg,
                              int count,
                              orte_rml_tag_t tag,
                              int flags,
                              orte_rml_callback_fn_t cbfunc,
                              void* cbdata);

    /*
     * Send Buffer
     */
    int orte_rml_ftrm_send_buffer(orte_process_name_t* peer,
                                  opal_buffer_t* buffer,
                                  orte_rml_tag_t tag,
                                  int flags);

    /*
     * Send Buffer Non-blocking
     */
    int orte_rml_ftrm_send_buffer_nb(orte_process_name_t* peer,
                                     opal_buffer_t* buffer,
                                     orte_rml_tag_t tag,
                                     int flags,
                                     orte_rml_buffer_callback_fn_t cbfunc,
                                     void* cbdata);

    /*
     * Recv
     */
    int orte_rml_ftrm_recv(orte_process_name_t* peer,
                           struct iovec *msg,
                           int count,
                           orte_rml_tag_t tag,
                           int flags);

    /*
     * Recv Non-blocking
     */
    int orte_rml_ftrm_recv_nb(orte_process_name_t* peer,
                              struct iovec* msg,
                              int count,
                              orte_rml_tag_t tag,
                              int flags,
                              orte_rml_callback_fn_t cbfunc,
                              void* cbdata);

    /*
     * Recv Buffer
     */
    int orte_rml_ftrm_recv_buffer(orte_process_name_t* peer,
                                  opal_buffer_t *buf,
                                  orte_rml_tag_t tag,
                                  int flags);

    /*
     * Recv Buffer Non-blocking
     */
    int orte_rml_ftrm_recv_buffer_nb(orte_process_name_t* peer,
                                     orte_rml_tag_t tag,
                                     int flags,
                                     orte_rml_buffer_callback_fn_t cbfunc,
                                     void* cbdata);
    
    /*
     * Recv Cancel
     */
    int orte_rml_ftrm_recv_cancel(orte_process_name_t* peer, orte_rml_tag_t tag);

    /*
     * Register a callback on loss of connection
     */
    int orte_rml_ftrm_add_exception_handler(orte_rml_exception_callback_t cbfunc);
    int orte_rml_ftrm_del_exception_handler(orte_rml_exception_callback_t cbfunc);

    /*
     * FT Event
     */
    int orte_rml_ftrm_ft_event(int state);

END_C_DECLS

#endif
