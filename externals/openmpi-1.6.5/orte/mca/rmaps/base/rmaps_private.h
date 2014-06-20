/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
/** @file:
 */

#ifndef ORTE_MCA_RMAPS_PRIVATE_H
#define ORTE_MCA_RMAPS_PRIVATE_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/types.h"

#include "orte/runtime/orte_globals.h"

#include "orte/mca/rmaps/rmaps.h"

BEGIN_C_DECLS

/*
 * Base API functions
 */

/*
 * Map a job
 * All calls to rmaps.map_job are routed through this function. This allows callers to
 * the RMAPS framework to specify the particular mapper they wish to use.
 */
ORTE_DECLSPEC int orte_rmaps_base_map_job(orte_job_t *jdata);
ORTE_DECLSPEC orte_job_map_t* orte_rmaps_base_get_job_map(orte_jobid_t job);


/* LOCAL FUNCTIONS for use by RMAPS components */

/*
 * Function to add a mapped_proc entry to a map
 * Scans list of nodes on map to see if the specified one already
 * exists - if so, just add this entry to that node's list of
 * procs. If not, then add new node entry and put this proc
 * on its list.
 */
int orte_rmaps_base_add_proc_to_map(orte_job_map_t *map, orte_node_t *node,
                                    bool oversubscribed, orte_proc_t *proc);

ORTE_DECLSPEC int orte_rmaps_base_get_target_nodes(opal_list_t* node_list,
                                                   orte_std_cntr_t *total_num_slots,
                                                   orte_app_context_t *app,
                                                   orte_mapping_policy_t policy);
ORTE_DECLSPEC int orte_rmaps_base_get_target_procs(opal_list_t *procs);

ORTE_DECLSPEC int orte_rmaps_base_update_node_usage(opal_list_t *nodes);
ORTE_DECLSPEC int orte_rmaps_base_get_mapped_targets(opal_list_t *mapped_node_list,
                                                     orte_app_context_t *app,
                                                     opal_list_t *master_node_list,
                                                     orte_std_cntr_t *total_num_slots);

ORTE_DECLSPEC int orte_rmaps_base_claim_slot(orte_job_t *jdata,
                                             orte_node_t *current_node,
                                             int32_t stride,
                                             int32_t app_idx,
                                             opal_list_t *nodes,
                                             bool oversubscribe,
                                             bool remove_from_list,
                                             orte_proc_t **returnproc);

ORTE_DECLSPEC int orte_rmaps_base_compute_vpids(orte_job_t *jdata);

ORTE_DECLSPEC int orte_rmaps_base_compute_local_ranks(orte_job_t *jdata);

ORTE_DECLSPEC void orte_rmaps_base_update_local_ranks(orte_job_t *jdata, orte_node_t *oldnode,
                                                      orte_node_t *newnode, orte_proc_t *newproc);

ORTE_DECLSPEC int orte_rmaps_base_rearrange_map(orte_app_context_t *app, orte_job_map_t *map, opal_list_t *procs);

ORTE_DECLSPEC int orte_rmaps_base_define_daemons(orte_job_map_t *map);


ORTE_DECLSPEC opal_list_item_t* orte_rmaps_base_get_starting_point(opal_list_t *node_list, orte_job_t *jdata);

ORTE_DECLSPEC int orte_rmaps_base_map_byslot(orte_job_t *jdata, orte_app_context_t *app,
                                             opal_list_t *node_list, orte_vpid_t num_procs,
                                             opal_list_item_t *cur_node_item);

ORTE_DECLSPEC int orte_rmaps_base_map_bynode(orte_job_t *jdata, orte_app_context_t *app,
                                             opal_list_t *node_list, orte_vpid_t num_procs,
                                             opal_list_item_t *cur_node_item);


END_C_DECLS

#endif
