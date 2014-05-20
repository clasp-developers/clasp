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
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>

#include "opal/util/if.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/hostfile/hostfile.h"
#include "orte/util/dash_host/dash_host.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"

/*
 * Query the registry for all nodes allocated to a specified app_context
 */
int orte_rmaps_base_get_target_nodes(opal_list_t *allocated_nodes, orte_std_cntr_t *total_num_slots,
                                     orte_app_context_t *app, orte_mapping_policy_t policy)
{
    opal_list_item_t *item, *next;
    orte_node_t *node, *nptr;
    orte_std_cntr_t num_slots;
    orte_std_cntr_t i;
    int rc;
    opal_list_t nodes;
    bool ignore;

    /** set default answer */
    *total_num_slots = 0;
    
    /* if this is NOT a managed allocation, then we use the nodes
     * that were specified for this app - there is no need to collect
     * all available nodes and "filter" them
     */
    if (!orte_managed_allocation) {
        OBJ_CONSTRUCT(&nodes, opal_list_t);
        /* if the app provided a dash-host, then use those nodes */
        if (NULL != app->dash_host) {
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s using dash_host",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            if (ORTE_SUCCESS != (rc = orte_util_add_dash_host_nodes(&nodes, &ignore,
                                                                    app->dash_host))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else if (NULL != app->hostfile) {
            /* otherwise, if the app provided a hostfile, then use that */
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s using hostfile %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 app->hostfile));
            if (ORTE_SUCCESS != (rc = orte_util_add_hostfile_nodes(&nodes, &ignore,
                                                                   app->hostfile))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else if (NULL != orte_rankfile) {
            /* use the rankfile, if provided */
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s using rankfile %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 orte_rankfile));
            if (ORTE_SUCCESS != (rc = orte_util_add_hostfile_nodes(&nodes, &ignore,
                                                                   orte_rankfile))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (0 == opal_list_get_size(&nodes)) {
                OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                     "%s nothing found in given rankfile",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                OBJ_DESTRUCT(&nodes);
                return ORTE_ERR_BAD_PARAM;
            }
        } else if (NULL != orte_default_hostfile) {
            /* fall back to the default hostfile, if provided */
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s using default hostfile %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 orte_default_hostfile));
            if (ORTE_SUCCESS != (rc = orte_util_add_hostfile_nodes(&nodes, &ignore,
                                                                   orte_default_hostfile))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* this is a special case - we always install a default
             * hostfile, but it is empty. If the user didn't remove it
             * or put something into it, then we will have pursued that
             * option and found nothing. This isn't an error, we just need
             * to use the local host
             */
            if (0 == opal_list_get_size(&nodes)) {
                OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                     "%s nothing in default hostfile - using local host",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
                OBJ_RETAIN(node);
                opal_list_append(allocated_nodes, &node->super);
                OBJ_DESTRUCT(&nodes);
                goto complete;
            }
        } else {
            /* if nothing else was available, then use the local host */
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s using local host",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
            OBJ_RETAIN(node);
            opal_list_append(allocated_nodes, &node->super);
            OBJ_DESTRUCT(&nodes);
            goto complete;
        }
        /** if we still don't have anything */
        if (0 == opal_list_get_size(&nodes)) {
            orte_show_help("help-orte-rmaps-base.txt",
                           "orte-rmaps-base:no-available-resources",
                           true);
            OBJ_DESTRUCT(&nodes);
            return ORTE_ERR_SILENT;
        }
        /* find the nodes in our node array */
        while (NULL != (item = opal_list_remove_first(&nodes))) {
            nptr = (orte_node_t*)item;
            for (i=0; i < orte_node_pool->size; i++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
                    continue;
                }
                if (0 != strcmp(node->name, nptr->name)) {
                    continue;
                }
                /* retain a copy for our use in case the item gets
                 * destructed along the way
                 */
                OBJ_RETAIN(node);
                opal_list_append(allocated_nodes, &node->super);
                OBJ_RELEASE(nptr);
                break;
            }
        }
        OBJ_DESTRUCT(&nodes);
        /* now prune for usage and compute total slots */
        goto complete;
    }

    /* if the hnp was allocated, include it */
    if (orte_hnp_is_allocated) {
        node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, 0);
        OBJ_RETAIN(node);
        opal_list_append(allocated_nodes, &node->super);
    }
    
    /* add everything in the node pool */
    for (i=1; i < orte_node_pool->size; i++) {
        if (NULL != (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, i))) {
            /* retain a copy for our use in case the item gets
             * destructed along the way
             */
            OBJ_RETAIN(node);
            opal_list_append(allocated_nodes, &node->super);
        } 
    }

    /** check that anything is here */
    if (0 == opal_list_get_size(allocated_nodes)) {
        orte_show_help("help-orte-rmaps-base.txt",
                       "orte-rmaps-base:no-available-resources",
                       true);
        return ORTE_ERR_SILENT;
    }
    
    /* did the app_context contain a hostfile? */
    if (NULL != app->hostfile) {
        /* yes - filter the node list through the file, removing
         * any nodes not found in the file
         */
        if (ORTE_SUCCESS != (rc = orte_util_filter_hostfile_nodes(allocated_nodes,
                                                                  app->hostfile))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is here */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, app->hostfile);
            return ORTE_ERR_SILENT;
        }
    }
    
    
    /* did the app_context contain an add-hostfile? */
    if (NULL != app->add_hostfile) {
        /* yes - filter the node list through the file, removing
         * any nodes not found in the file
         */
        if (ORTE_SUCCESS != (rc = orte_util_filter_hostfile_nodes(allocated_nodes,
                                                                  app->add_hostfile))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is here */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, app->hostfile);
            return ORTE_ERR_SILENT;
        }
    }
    
    /* now filter the list through any -host specification */
    if (NULL != app->dash_host) {
        if (ORTE_SUCCESS != (rc = orte_util_filter_dash_host_nodes(allocated_nodes,
                                                                   app->dash_host))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is left! */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, "");
            return ORTE_ERR_SILENT;
        }
    }
    
    /* now filter the list through any add-host specification */
    if (NULL != app->add_host) {
        if (ORTE_SUCCESS != (rc = orte_util_filter_dash_host_nodes(allocated_nodes,
                                                                   app->add_host))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /** check that anything is left! */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:no-mapped-node",
                           true, app->app, "");
            return ORTE_ERR_SILENT;
        }
    }
    
    /* If the "no local" option was set, then remove the local node
     * from the list
     */
    if (policy & ORTE_MAPPING_NO_USE_LOCAL) {
        /* we don't need to check through the entire list as
         * the head node - if it is on the list at all - will
         * always be in the first position
         */
        item = opal_list_get_first(allocated_nodes);
        node = (orte_node_t*)item;
        /* need to check ifislocal because the name in the
         * hostfile may not have been FQDN, while name returned
         * by gethostname may have been (or vice versa)
         */
        if (opal_ifislocal(node->name)) {
            opal_list_remove_item(allocated_nodes, item);
            OBJ_RELEASE(item);  /* "un-retain" it */
        }
        /** check that anything is left! */
        if (0 == opal_list_get_size(allocated_nodes)) {
            orte_show_help("help-orte-rmaps-base.txt",
                           "orte-rmaps-base:nolocal-no-available-resources", true);
            return ORTE_ERR_SILENT;
        }
    }

 complete:
    /* remove all nodes that are already at max usage, and
     * compute the total number of allocated slots while
     * we do so
     */
    num_slots = 0;
    item  = opal_list_get_first(allocated_nodes);
    while (item != opal_list_get_end(allocated_nodes)) {

        /** save the next pointer in case we remove this node */
        next  = opal_list_get_next(item);

        /** check to see if this node is fully used - remove if so */
        node = (orte_node_t*)item;
        if (0 != node->slots_max && node->slots_inuse > node->slots_max) {
            opal_list_remove_item(allocated_nodes, item);
            OBJ_RELEASE(item);  /* "un-retain" it */
        } else { /** otherwise, add the slots for our job to the total */
            num_slots += node->slots_alloc;
        }

        /** go on to next item */
        item = next;
    }

    /* Sanity check to make sure we have resources available */
    if (0 == num_slots) {
        orte_show_help("help-orte-rmaps-base.txt", 
                       "orte-rmaps-base:all-available-resources-used", true);
        return ORTE_ERR_SILENT;
    }
    
    *total_num_slots = num_slots;
    
    return ORTE_SUCCESS;
}


int orte_rmaps_base_add_proc_to_map(orte_job_map_t *map, orte_node_t *node,
                                    bool oversubscribed, orte_proc_t *proc)
{
    orte_std_cntr_t i;
    orte_node_t *node_from_map;
    int rc;
    
    /* see if this node has already been assigned to the map - if
     * not, then add the pointer to the pointer array
     */
    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (node_from_map = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        if (node_from_map->index == node->index) {
            /* we have this node in the array */
            goto PROCESS;
        }
    }
    /* if we get here, then this node isn't already in the map - add it */
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base: adding node %s to map",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == node->name) ? "NULL" : node->name));
    
    if (ORTE_SUCCESS > (rc = opal_pointer_array_add(map->nodes, (void*)node))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    OBJ_RETAIN(node);  /* maintain accounting on object */
    ++map->num_nodes;
    
PROCESS:
    /* add the proc to this node's local processes - it is assumed
     * that the proc isn't already there as this would be an error
     * in the mapper
     */
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base: mapping proc for job %s to node %s whose daemon is %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(proc->name.jobid),
                         (NULL == node->name) ? "NULL" : node->name,
                         (NULL == node->daemon) ? "NULL" : ORTE_NAME_PRINT(&(node->daemon->name))));
    
    if (0 > (rc = opal_pointer_array_add(node->procs, (void*)proc))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* retain the proc struct so that we correctly track its release */
    OBJ_RETAIN(proc);
    ++node->num_procs;

    /* update the oversubscribed state of the node */
    node->oversubscribed = oversubscribed;
    
    return ORTE_SUCCESS;
}


/*
 * Claim a slot for a specified job on a node
 */
int orte_rmaps_base_claim_slot(orte_job_t *jdata,
                               orte_node_t *current_node,
                               int32_t cpus_per_rank,
                               int32_t app_idx,
                               opal_list_t *nodes,
                               bool oversubscribe,
                               bool remove_from_list,
                               orte_proc_t **returnproc)
{
    orte_proc_t *proc;
    bool oversub;
    int rc;

    /* if we were given a proc, just use it */
    if (NULL != returnproc && NULL != *returnproc) {
        proc = *returnproc;
    } else {
        /* create mapped_proc object */
        proc = OBJ_NEW(orte_proc_t);
        if (NULL == proc) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        /* set the jobid */
        proc->name.jobid = jdata->jobid;
        /* we do not set the vpid here - this will be done
         * during a second phase
         */
        proc->app_idx = app_idx;
        OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                             "%s rmaps:base:claim_slot: created new proc %s for app_idx %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&proc->name), app_idx));
        
        /* provide returned proc, if requested */
        if (NULL != returnproc) {
            *returnproc = proc;
        }
    }

    OBJ_RETAIN(current_node);  /* maintain accounting on object */
    
    proc->node = current_node;
    proc->nodename = current_node->name;
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base:claim_slot mapping proc in job %s app %d to node %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid), proc->app_idx, current_node->name));
    
    /* Be sure to demarcate the slots for this proc as claimed from the node */
    current_node->slots_inuse += 1;
    
    /* see if this node is oversubscribed now */
    if (current_node->slots_inuse > current_node->slots) {
        oversub = true;
    } else {
        oversub = false;
    }
    
    /* assign the proc to the node and ensure the node is on the map */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_add_proc_to_map(jdata->map, current_node,
                                                              oversub, proc))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(proc);
        return rc;
    }
    
    /* If this node has reached its max number of allocatable slots OR it has
     * reached the soft limit AND we are in a "no oversubscribe" state, then
     * we need to return a flag telling the mapper this is the case so it
     * can move on to the next node
     */
    if ((0 != current_node->slots_max  &&
        current_node->slots_inuse >= current_node->slots_max) ||
        (!oversubscribe && current_node->slots_inuse >= current_node->slots)) {
        /* see if we are supposed to remove the node from the list - some
         * mappers want us to do so to avoid any chance of continuing to
         * add procs to it
         */
        if (NULL != nodes && remove_from_list) {
            opal_list_remove_item(nodes, (opal_list_item_t*)current_node);
            /* release it - it was retained when we started, so this
             * just ensures the instance counter is correctly updated
             */
            OBJ_RELEASE(current_node);
        }
        /* now return the proper code so the caller knows this node
         * is fully used
         */
        return ORTE_ERR_NODE_FULLY_USED;
    }

    return ORTE_SUCCESS;
}

int orte_rmaps_base_compute_vpids(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_vpid_t vpid, cnt;
    int i, j;
    orte_node_t *node;
    orte_proc_t *proc, *ptr;
    int rc;
    
    map = jdata->map;
    
    if (ORTE_MAPPING_BYSLOT & map->policy ||
        ORTE_MAPPING_BYSOCKET & map->policy ||
        ORTE_MAPPING_BYBOARD & map->policy) {
        /* assign the ranks sequentially */
        vpid = 0;
        for (i=0; i < map->nodes->size; i++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
                continue;
            }
            for (j=0; j < node->procs->size; j++) {
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                    continue;
                }
                /* ignore procs from other jobs */
                if (proc->name.jobid != jdata->jobid) {
                    continue;
                }
                if (ORTE_VPID_INVALID == proc->name.vpid) {
                    /* find the next available vpid */
                    while (NULL != (ptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, vpid)) &&
                           ORTE_VPID_INVALID != ptr->name.vpid) {
                        vpid++;
                    }
                    proc->name.vpid = vpid++;
                }
                /* some mappers require that we insert the proc into the jdata->procs
                 * array, while others will have already done it - so check and
                 * do the operation if required
                 */
                if (NULL == opal_pointer_array_get_item(jdata->procs, proc->name.vpid)) {
                    if (ORTE_SUCCESS != (rc = opal_pointer_array_set_item(jdata->procs, proc->name.vpid, proc))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }                    
                }
            }
        }
        return ORTE_SUCCESS;
    }
    
    if (ORTE_MAPPING_BYNODE & map->policy) {
        /* assign the ranks round-robin across nodes */
        cnt=0;
        vpid=0;
        while (cnt < jdata->num_procs) {
            for (i=0; i < map->nodes->size; i++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
                    continue;
                }
                for (j=0; j < node->procs->size; j++) {
                    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                        continue;
                    }
                    /* ignore procs from other jobs */
                    if (proc->name.jobid != jdata->jobid) {
                        continue;
                    }
                    if (ORTE_VPID_INVALID != proc->name.vpid) {
                        /* vpid was already assigned, probably by the
                         * round-robin mapper. Some mappers require that
                         * we insert the proc into the jdata->procs
                         * array, while others will have already done it - so check and
                         * do the operation if required
                         */
                        if (NULL == opal_pointer_array_get_item(jdata->procs, proc->name.vpid)) {
                            if (ORTE_SUCCESS != (rc = opal_pointer_array_set_item(jdata->procs, proc->name.vpid, proc))) {
                                ORTE_ERROR_LOG(rc);
                                return rc;
                            }
                            /* if we added it to the array, then account for
                             * it in our loop - otherwise don't as we would be
                             * double counting
                             */
                            cnt++;
                        }
                        continue;
                    }
                    /* find next available vpid */
                    while (NULL != (ptr = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, vpid)) &&
                           ORTE_VPID_INVALID != ptr->name.vpid) {
                        vpid++;
                    }
                    proc->name.vpid = vpid++;
                    /* insert the proc into the jdata->procs array - can't already
                     * be there as the only way to this point in the code is for the
                     * vpid to have been INVALID
                     */
                    if (ORTE_SUCCESS != (rc = opal_pointer_array_set_item(jdata->procs, proc->name.vpid, proc))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }
                    cnt++;
                    break;  /* move on to next node */
                }
            }
        }
        return ORTE_SUCCESS;
    }

    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_rmaps_base_compute_local_ranks(orte_job_t *jdata)
{
    orte_std_cntr_t i;
    int j, k;
    orte_node_t *node;
    orte_proc_t *proc, *psave, *psave2;
    orte_vpid_t minv, minv2;
    orte_local_rank_t local_rank;
    orte_job_map_t *map;
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base:compute_usage",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* point to map */
    map = jdata->map;
    
    /* for each node in the map... */
    for (i=0; i < map->nodes->size; i++) {
        /* cycle through the array of procs on this node, setting
         * local and node ranks, until we
         * have done so for all procs on nodes in this map
         */
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        
        /* init search values */
        local_rank = 0;
        
        /* the proc map may have holes in it, so cycle
         * all the way through and avoid the holes
         */
        for (k=0; k < node->procs->size; k++) {
            /* if this proc is NULL, skip it */
            if (NULL == opal_pointer_array_get_item(node->procs, k)) {
                continue;
            }
            minv = ORTE_VPID_MAX;
            minv2 = ORTE_VPID_MAX;
            psave = NULL;
            psave2 = NULL;
            /* find the minimum vpid proc */
            for (j=0; j < node->procs->size; j++) {
                /* if this proc is NULL, skip it */
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                    continue;
                }
                /* only look at procs for this job when
                 * determining local rank
                 */
                if (proc->name.jobid == jdata->jobid &&
                    ORTE_LOCAL_RANK_INVALID == proc->local_rank &&
                    proc->name.vpid < minv) {
                    minv = proc->name.vpid;
                    psave = proc;
                }
                /* no matter what job...still have to handle node_rank */
                if (ORTE_NODE_RANK_INVALID == proc->node_rank &&
                    proc->name.vpid < minv2) {
                    minv2 = proc->name.vpid;
                    psave2 = proc;
                }
            }
            if (NULL == psave && NULL == psave2) {
                /* we must have processed them all for this node! */
                break;
            }
            if (NULL != psave) {
                psave->local_rank = local_rank;
                ++local_rank;
            }
            if (NULL != psave2) {
                psave2->node_rank = node->next_node_rank;
                node->next_node_rank++;
            }
        }
    }

    return ORTE_SUCCESS;
}

/* when we restart a process on a different node, we have to
 * ensure that the node and local ranks assigned to the proc
 * don't overlap with any pre-existing proc on that node. If
 * we don't, then it would be possible for procs to conflict
 * when opening static ports, should that be enabled.
 */
void orte_rmaps_base_update_local_ranks(orte_job_t *jdata, orte_node_t *oldnode,
                                        orte_node_t *newnode, orte_proc_t *newproc)
{
    int k;
    orte_node_rank_t node_rank;
    orte_local_rank_t local_rank;
    orte_proc_t *proc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base:update_usage",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* if the node hasn't changed, then we can just use the
     * pre-defined values
     */
    if (oldnode == newnode) {
        return;
    }
    
    /* if the node has changed, then search the new node for the
     * lowest unused local and node rank
     */
    node_rank = 0;
retry_nr:
    for (k=0; k < newnode->procs->size; k++) {
        /* if this proc is NULL, skip it */
        if (NULL == (proc = (orte_proc_t *) opal_pointer_array_get_item(newnode->procs, k))) {
            continue;
        }
        if (node_rank == proc->node_rank) {
            node_rank++;
            goto retry_nr;
        }
    }
    newproc->node_rank = node_rank;
    
    local_rank = 0;
retry_lr:
    for (k=0; k < newnode->procs->size; k++) {
        /* if this proc is NULL, skip it */
        if (NULL == (proc = (orte_proc_t *) opal_pointer_array_get_item(newnode->procs, k))) {
            continue;
        }
        /* ignore procs from other jobs */
        if (proc->name.jobid != jdata->jobid) {
            continue;
        }
        if (local_rank == proc->local_rank) {
            local_rank++;
            goto retry_lr;
        }
    }
    newproc->local_rank = local_rank;
}


int orte_rmaps_base_define_daemons(orte_job_map_t *map)
{
    orte_node_t *node;
    orte_proc_t *proc;
    orte_job_t *daemons;
    int i;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                         "%s rmaps:base:define_daemons",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* get the daemon job data struct */
    if (NULL == (daemons = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid))) {
        /* bad news */
        ORTE_ERROR_LOG(ORTE_ERR_FATAL);
        return ORTE_ERR_FATAL;
    }
    
    /* initialize the #new daemons */
    map->num_new_daemons = 0;
    
    /* go through the nodes in the map, checking each one's daemon name
     */
    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        if (NULL == node->daemon) {
            /* we haven't defined one for it
             * yet, so do so now and indicate it is to be launched
             */
            proc = OBJ_NEW(orte_proc_t);
            if (NULL == proc) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            proc->name.jobid = ORTE_PROC_MY_NAME->jobid;
            if (ORTE_VPID_MAX-1 <= daemons->num_procs) {
                /* no more daemons available */
                orte_show_help("help-orte-rmaps-base.txt", "out-of-vpids", true);
                OBJ_RELEASE(proc);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            proc->name.vpid = daemons->num_procs;  /* take the next available vpid */
            proc->node = node;
            proc->nodename = node->name;
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s rmaps:base:define_daemons add new daemon %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            /* add the daemon to the daemon job object */
            if (0 > (rc = opal_pointer_array_add(daemons->procs, (void*)proc))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            ++daemons->num_procs;
            /* point the node to the daemon */
            node->daemon = proc;
            OBJ_RETAIN(proc);  /* maintain accounting */
            /* track number of daemons to be launched */
            ++map->num_new_daemons;
            /* and their starting vpid */
            if (ORTE_VPID_INVALID == map->daemon_vpid_start) {
                map->daemon_vpid_start = proc->name.vpid;
            }
        } else {
            /* this daemon was previously defined - flag it */
            node->daemon_launched = true;
            OPAL_OUTPUT_VERBOSE((5, orte_rmaps_base.rmaps_output,
                                 "%s rmaps:base:define_daemons existing daemon %s already launched",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&node->daemon->name)));
        }
    }

    return ORTE_SUCCESS;
}
