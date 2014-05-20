/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef ORTE_RAS_PRIVATE_H
#define ORTE_RAS_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/class/opal_list.h"

#include "orte/mca/ras/ras_types.h"

#include "orte/mca/ras/ras.h"
#include "orte/mca/ras/base/base.h"


BEGIN_C_DECLS
    
/*
 * API function definitions
 */
ORTE_DECLSPEC int orte_ras_base_allocate(orte_job_t *jdata);

/**
* Add the specified node definitions to the registry
 */
ORTE_DECLSPEC int orte_ras_base_node_insert(opal_list_t*, orte_job_t*);

#if 0
/*
 * Internal support functions
 */
ORTE_DECLSPEC int orte_ras_base_allocate_nodes(orte_jobid_t jobid, 
                                               opal_list_t* nodes);

ORTE_DECLSPEC int orte_ras_base_reallocate(orte_jobid_t parent_jobid,
                                           orte_jobid_t child_jobid);

ORTE_DECLSPEC int orte_ras_base_set_oversubscribe_override(orte_jobid_t job);

ORTE_DECLSPEC int orte_ras_base_hostfile_query(char *hostfile);

ORTE_DECLSPEC int orte_ras_base_get_oversubscribe_override(orte_jobid_t job, bool *flag);

ORTE_DECLSPEC int orte_ras_base_read_nodename_file(opal_list_t *nodes, char *filename);

/*
 * Query the registry for all available nodes 
 */
ORTE_DECLSPEC int orte_ras_base_node_query(opal_list_t*);

/*
 * Query the registry for a specific node 
 */
ORTE_DECLSPEC orte_ras_node_t* orte_ras_base_node_lookup(const char* nodename);

/**
    * Query the registry for all nodes allocated to a specific job
 */
ORTE_DECLSPEC int orte_ras_base_node_query_alloc(opal_list_t*, orte_jobid_t);

ORTE_DECLSPEC int orte_ras_base_proc_query_alloc(opal_list_t* procs);

/**
    * Add the specified node definitions to the registry
 */
ORTE_DECLSPEC int orte_ras_base_node_insert(opal_list_t*);

ORTE_DECLSPEC int orte_ras_base_proc_insert(opal_list_t* procs, orte_jobid_t jobid);

/**
    * Delete the specified nodes from the registry
 */
ORTE_DECLSPEC int orte_ras_base_node_delete(opal_list_t*);

/**
    * Assign the allocated slots on the specified nodes to the  
 * indicated jobid.
 */
ORTE_DECLSPEC int orte_ras_base_node_assign(opal_list_t*, orte_jobid_t);

/**
    * Check to see if the node segment is empty
 */
ORTE_DECLSPEC int orte_ras_base_node_segment_empty(bool *empty);

#endif

END_C_DECLS

#endif
