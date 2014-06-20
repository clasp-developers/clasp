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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * RML Framework maintenence interface
 *
 * Interface for starting / stopping / controlling the RML framework,
 * as well as support for modifying RML datatypes. 
 *
 * @note The only RML datatype exposed to the user is the RML tag.
 * This will always be an integral value, so the only datatype support
 * really required is the internal DSS functions for packing /
 * unpacking / comparing tags.  The user should never need to deal
 * with these.
 */

#ifndef MCA_RML_BASE_H
#define MCA_RML_BASE_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/rml/rml.h"
#include "opal/dss/dss_types.h"

BEGIN_C_DECLS


/* ******************************************************************** */


/**
 * Open the RML framework
 *
 * Open the RML framework.  Loads all available RML components and
 * runs their open functions.
 *
 * @retval ORTE_SUCCESS Components successfully loaded
 * @retval ORTE_ERROR   An unknown error occurred
 */
ORTE_DECLSPEC int orte_rml_base_open(void);

/**
 * Select an active RML component
 *
 * Select an RML component from the list of frameworks that were
 * opened during orte_rml_base_open().  The orte_rml_base_select()
 * function will fill in the orte_rml structure so that all functions
 * in the interface exist.  Note that there are still calling
 * restrictions at that point (see the documentation for
 * orte_rml_component_init_fn_t).
 *
 * All components that are not selected will be closed during this
 * call.
 *
 * @retval ORTE_SUCCESS Components successfully selected
 * @retval ORTE_ERROR   An unknown error occurred
 */
ORTE_DECLSPEC int orte_rml_base_select(void);


/**
 * Close the RML framework
 *
 * Close the RML framework, releasing all resources associated with
 * the framework interface.  Also closes the active component used
 * during the application.
 *
 * @retval ORTE_SUCCESS Framework successfully shut down
 */
ORTE_DECLSPEC int orte_rml_base_close(void);

/**
 * Post receive to get updates regarding contact information
 *
 * Post a non-blocking receive (likely during orte_init()) to receive
 * updated contact information from the HNP when it becomes available.
 * This should be called in any process that needs such updates, and
 * the receive will continue to get update callbacks until
 * orte_rml_base_comm_stop() is called.
 *
 * @retval ORTE_SUCCESS Receive successfully started
 * @retval ORTE_ERROR   An unknown error occurred
 */
ORTE_DECLSPEC int orte_rml_base_comm_start(void);


/**
 * Stop receiving contact information updates
 *
 * Shut down the receive posted during orte_rml_base_comm_start(),
 * likely during orte_finalize().
 *
 * @retval ORTE_SUCCESS Receive succesffully cancelled.
 */
ORTE_DECLSPEC int orte_rml_base_comm_stop(void);


/**
 * Output stream for RML debugging
 *
 * Output stream for the opal_output() code intended for RML output.
 * It will be have its verbosity set according to the MCA parameter
 * rml_base_verbose.  Useable between call to orte_rml_base_open() and
 * orte_rml_base_close().
 */
ORTE_DECLSPEC extern int orte_rml_base_output;


/**
 * List of components that are available to the RML
 *
 * List of components that are currently available to the RML
 * framework.  Useable between calls to orte_rml_base_open() and
 * orte_rml_base_close().
 *
 * @note This list should not be used by code outside the RML base.
 */
ORTE_DECLSPEC extern opal_list_t orte_rml_base_components;


/**
 * Component structure for the selected RML component
 *
 * Component structure pointer for the currently selected RML
 * component.  Useable between calls to orte_rml_base_select() and
 * orte_rml_base_close().
 * 
 * @note This pointer should not be used outside the RML base.  It is
 * available outside the RML base only for the F/T component.
 */
ORTE_DECLSPEC extern orte_rml_component_t *orte_rml_component;


/*
 * This is the base priority for a RML wrapper component
 * If there exists more than one wrapper, then the one with 
 * the lowest priority wins.
 */
#define RML_SELECT_WRAPPER_PRIORITY -128

/* null functions */
int orte_rml_base_null_send(struct orte_process_name_t* peer,
                                          struct iovec *msg,
                                          int count,
                                          int tag,
                                          int flags);
int orte_rml_base_null_send_nb(struct orte_process_name_t* peer,
                               struct iovec* msg,
                               int count,
                               orte_rml_tag_t tag,
                               int flags,
                               orte_rml_callback_fn_t cbfunc,
                               void* cbdata);
int orte_rml_base_null_send_buffer(struct orte_process_name_t* peer,
                                   struct opal_buffer_t* buffer,
                                   orte_rml_tag_t tag,
                                   int flags);
int orte_rml_base_null_send_buffer_nb(struct orte_process_name_t* peer,
                                      struct opal_buffer_t* buffer,
                                      orte_rml_tag_t tag,
                                      int flags,
                                      orte_rml_buffer_callback_fn_t cbfunc,
                                      void* cbdata);
int orte_rml_base_null_recv(struct orte_process_name_t* peer,
                            struct iovec *msg,
                            int count,
                            orte_rml_tag_t tag,
                            int flags);
int orte_rml_base_null_recv_nb(struct orte_process_name_t* peer,
                               struct iovec* msg,
                               int count,
                               orte_rml_tag_t tag,
                               int flags,
                               orte_rml_callback_fn_t cbfunc,
                               void* cbdata);
int orte_rml_base_null_recv_buffer(struct orte_process_name_t* peer,
                                   struct opal_buffer_t *buf,
                                   orte_rml_tag_t tag,
                                   int flags);
int orte_rml_base_null_recv_buffer_nb(struct orte_process_name_t* peer,
                                      orte_rml_tag_t tag,
                                      int flags,
                                      orte_rml_buffer_callback_fn_t cbfunc,
                                      void* cbdata);
int orte_rml_base_null_recv_cancel(orte_process_name_t* peer,
                                   orte_rml_tag_t tag);

END_C_DECLS

#endif /* MCA_RML_BASE_H */
