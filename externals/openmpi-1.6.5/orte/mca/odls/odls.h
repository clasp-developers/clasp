/* -*- C -*-
 *
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
 * The OpenRTE Daemon's Local Launch Subsystem
 *
 */

#ifndef ORTE_MCA_ODLS_H
#define ORTE_MCA_ODLS_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/mca/mca.h"
#include "opal/class/opal_pointer_array.h"

#include "opal/dss/dss_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/odls/odls_types.h"

BEGIN_C_DECLS

/*
 * odls module functions
 */
    
/*
 * Construct a buffer for use in adding local processes
 * In order to reuse daemons, we need a way for the HNP to construct a buffer that
 * contains the data needed by the active ODLS component to launch a local process. Since the
 * only one that knows what a particular ODLS component needs is that component, we require an
 * entry point that the HNP can call to get the required buffer. This is constructed
 * for *all* nodes - the individual orteds then parse that data to find the specific launch info
 * for procs on their node
 */
typedef int (*orte_odls_base_module_get_add_procs_data_fn_t)(opal_buffer_t *data,
                                                             orte_jobid_t job);

/**
 * Locally launch the provided processes
 */
typedef int (*orte_odls_base_module_launch_local_processes_fn_t)(opal_buffer_t *data);

/**
 * Kill the local processes on this node
 */
typedef int (*orte_odls_base_module_kill_local_processes_fn_t)(opal_pointer_array_t *procs, bool set_state);

/**
 * Signal local processes
 */
typedef int (*orte_odls_base_module_signal_local_process_fn_t)(const orte_process_name_t *proc,
                                                              int32_t signal);

/**
 * Deliver a message to local processes
 */
typedef int (*orte_odls_base_module_deliver_message_fn_t)(orte_jobid_t job, opal_buffer_t *buffer,
                                                          orte_rml_tag_t tag);

/**
 * Register to require sync before termination
 */
typedef int (*orte_odls_base_module_require_sync_fn_t)(orte_process_name_t *proc,
                                                       opal_buffer_t *buffer,
                                                       bool drop_nidmap);

/**
 * pls module version
 */
struct orte_odls_base_module_1_3_0_t {
    orte_odls_base_module_get_add_procs_data_fn_t           get_add_procs_data;
    orte_odls_base_module_launch_local_processes_fn_t       launch_local_procs;
    orte_odls_base_module_kill_local_processes_fn_t         kill_local_procs;
    orte_odls_base_module_signal_local_process_fn_t   		signal_local_procs;
    orte_odls_base_module_deliver_message_fn_t              deliver_message;
    orte_odls_base_module_require_sync_fn_t                 require_sync;
};

/** shorten orte_odls_base_module_1_3_0_t declaration */
typedef struct orte_odls_base_module_1_3_0_t orte_odls_base_module_1_3_0_t;
/** shorten orte_odls_base_module_t declaration */
typedef struct orte_odls_base_module_1_3_0_t orte_odls_base_module_t;

/**
 * odls component
 */
struct orte_odls_base_component_2_0_0_t {
    /** component version */
    mca_base_component_t version;
    /** component data */
    mca_base_component_data_t base_data;
};
/** Convenience typedef */
typedef struct orte_odls_base_component_2_0_0_t orte_odls_base_component_2_0_0_t;
/** Convenience typedef */
typedef orte_odls_base_component_2_0_0_t orte_odls_base_component_t;


/**
 * Macro for use in modules that are of type odls
 */
#define ORTE_ODLS_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "odls", 2, 0, 0

/* Global structure for accessing ODLS functions
*/
ORTE_DECLSPEC extern orte_odls_base_module_t orte_odls;  /* holds selected module's function pointers */

END_C_DECLS

#endif /* MCA_ODLS_H */
