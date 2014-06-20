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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/trace.h"
#include "opal/mca/carto/base/base.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_topo.h"

static int topo_map(orte_job_t *jdata);

orte_rmaps_base_module_t orte_rmaps_topo_module = {
    topo_map
};

/*
 * Local variable
 */
static opal_list_item_t *cur_node_item = NULL;
static int ppn = 0;

/*
 * Create a default mapping for the application, scheduling round
 * robin by node.
 */
static int map_app_by_node(
    orte_app_context_t* app,
    orte_job_t* jdata,
    orte_vpid_t vpid_start,
    opal_list_t* nodes)
{
    int rc = ORTE_SUCCESS;
    opal_list_item_t *next;
    orte_node_t *node;
    orte_std_cntr_t num_alloc=0;

    OPAL_TRACE(2);
    
    /* This loop continues until all procs have been mapped or we run
       out of resources. We determine that we have "run out of
       resources" when all nodes have slots_max processes mapped to them,
       thus there are no free slots for a process to be mapped, or we have
       hit the soft limit on all nodes and are in a "no oversubscribe" state.
       If we still have processes that haven't been mapped yet, then it's an 
       "out of resources" error.

       In this scenario, we rely on the claim_slot function to handle the
       oversubscribed case. The claim_slot function will leave a node on the
       list until it either reaches slots_max OR reaches the
       soft limit and the "no_oversubscribe" flag has been set - at which point,
       the node will be removed to prevent any more processes from being mapped to
       it. Since we are taking one slot from each node as we cycle through, the
       list, oversubscription is automatically taken care of via this logic.
        */
    
    while (num_alloc < app->num_procs) {
        
        /** see if any nodes remain unused and available. We need to do this check
         * each time since we may remove nodes from the list (as they become fully
         * used) as we cycle through the loop */
        if(0 >= opal_list_get_size(nodes) ) {
            /* No more nodes to allocate :( */
            orte_show_help("help-orte-rmaps-topo.txt", "orte-rmaps-topo:alloc-error",
                           true, app->num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        
        /* Save the next node we can use before claiming slots, since
         * we may need to prune the nodes list removing overused nodes.
         * Wrap around to beginning if we are at the end of the list */
        if (opal_list_get_end(nodes) == opal_list_get_next(cur_node_item)) {
            next = opal_list_get_first(nodes);
        }
        else {
            next = opal_list_get_next(cur_node_item);
        }
        
        /* Allocate a slot on this node */
        node = (orte_node_t*) cur_node_item;
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,  1, app->idx,
                                             nodes, jdata->map->oversubscribe, true, NULL))) {
            /** if the code is ORTE_ERR_NODE_FULLY_USED, then we know this
             * really isn't an error - we just need to break from the loop
             * since the node is fully used up. For now, just don't report
             * an error
             */
            if (ORTE_ERR_NODE_FULLY_USED != rc) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        ++num_alloc;

        cur_node_item = next;
    }

    return ORTE_SUCCESS;
}
   

/*
 * Create a default mapping for the application, scheduling one round
 * robin by slot.
 */
static int map_app_by_slot(
    orte_app_context_t* app,
    orte_job_t* jdata,
    orte_vpid_t vpid_start,
    opal_list_t* nodes)
{
    int rc = ORTE_SUCCESS;
    orte_std_cntr_t i, num_slots_to_take;
    orte_node_t *node;
    opal_list_item_t *next;
    orte_std_cntr_t num_alloc=0;

    OPAL_TRACE(2);
    
    /* This loop continues until all procs have been mapped or we run
       out of resources. We determine that we have "run out of
       resources" when either all nodes have slots_max processes mapped to them,
       (thus there are no free slots for a process to be mapped), OR all nodes
       have reached their soft limit and the user directed us to "no oversubscribe".
       If we still have processes that haven't been mapped yet, then it's an
       "out of resources" error. */
    
    while ( num_alloc < app->num_procs) {

        /** see if any nodes remain unused and available. We need to do this check
        * each time since we may remove nodes from the list (as they become fully
        * used) as we cycle through the loop */
        if(0 >= opal_list_get_size(nodes) ) {
            /* Everything is at max usage! :( */
            orte_show_help("help-orte-rmaps-topo.txt", "orte-rmaps-topo:alloc-error",
                           true, app->num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        
        /* Save the next node we can use before claiming slots, since
        * we may need to prune the nodes list removing overused nodes.
        * Wrap around to beginning if we are at the end of the list */
        if (opal_list_get_end(nodes) == opal_list_get_next(cur_node_item)) {
            next = opal_list_get_first(nodes);
        }
        else {
            next = opal_list_get_next(cur_node_item);
        }
        
        /** declare a shorter name for convenience in the code below */
        node = (orte_node_t*) cur_node_item;
        
        /* If we have available slots on this node, claim all of them 
         * If node_slots == 0, assume 1 slot for that node. 
         * JJH - is this assumption fully justified?
         *
         * If we are now oversubscribing the nodes, then we still take:
         * (a) if the node has not been used yet, we take a full node_slots
         * (b) if some of the slots are in-use, then we take the number of
         *     remaining slots before hitting the soft limit (node_slots)
         * (c) if we are at or above the soft limit, we take a full node_slots
         *
         * Note: if node_slots is zero, then we always just take 1 slot
         *
         * We continue this process until either everything is done,
         * or all nodes have hit their hard limit. This algorithm ensures we
         * fully utilize each node before oversubscribing, and preserves the ratio
         * of processes between the nodes thereafter (e.g., if one node has twice as
         * many processes as another before oversubscribing, it will continue
         * to do so after oversubscribing).
         */
        if (0 == node->slots_inuse ||
            node->slots_inuse >= node->slots) {
            num_slots_to_take = (node->slots == 0) ? 1 : node->slots;
        } else {
            num_slots_to_take = node->slots - node->slots_inuse;
        }
        
        /* check if we are in npernode mode - if so, then set the num_slots_to_take
         * to the num_per_node
         */
        if (0 < jdata->map->npernode) {
            num_slots_to_take = jdata->map->npernode;
        }
        
        for( i = 0; i < num_slots_to_take; ++i) {
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node, 1, app->idx,
                                                 nodes, jdata->map->oversubscribe, true, NULL))) {
                /** if the code is ORTE_ERR_NODE_FULLY_USED, then we know this
                 * really isn't an error - we just need to break from the loop
                 * since the node is fully used up. For now, just don't report
                 * an error
                 */
                if (ORTE_ERR_NODE_FULLY_USED != rc) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
            
            /* Update the number of procs allocated */
            ++num_alloc;

            /** if all the procs have been mapped, we return */
            if (num_alloc == app->num_procs) {
                return ORTE_SUCCESS;
            }

            /* if we have fully used up this node
             * OR we are at our ppn and loadbalancing, then break from the loop 
             */
            if (ORTE_ERR_NODE_FULLY_USED == rc ||
                (orte_rmaps_base.loadbalance && (int)node->num_procs >= ppn)) {
                break;
            }
        }

        /* we move on to the next node in all cases EXCEPT if we came
         * out of the loop without having taken a full bite AND the
         * node is NOT max'd out
         *
         */
        if (i < (num_slots_to_take-1) &&
            ORTE_ERR_NODE_FULLY_USED != rc &&
            (orte_rmaps_base.loadbalance && (int)node->num_procs < ppn)) {
            continue;
        }
        cur_node_item = next;
    }

    return ORTE_SUCCESS;
}
   

/*
 * Create a topo-aware mapping for the job.
 */
static int topo_map(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_app_context_t *app, **apps;
    orte_std_cntr_t i;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_node_t *node, *nd1;
    orte_vpid_t vpid_start;
    orte_std_cntr_t num_nodes, num_slots;
    int rc;
    orte_std_cntr_t slots_per_node;
    opal_carto_graph_t *graph;
    opal_carto_base_node_t *crnode;
    opal_value_array_t distance;
    
    OPAL_TRACE(1);
    
    /* conveniece def */
    map = jdata->map;
    apps = (orte_app_context_t**)jdata->apps->addr;
    
    /* start at the beginning... */
    vpid_start = 0;
    jdata->num_procs = 0;
    
    /* get the graph of nodes */
    if (ORTE_SUCCESS != (rc = opal_carto_base_get_host_graph(&graph, "SLOT"))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* cycle through the app_contexts, mapping them sequentially */
    for(i=0; i < jdata->num_apps; i++) {
        app = apps[i];

        /* if the number of processes wasn't specified, then we know there can be only
         * one app_context allowed in the launch, and that we are to launch it across
         * all available slots. We'll double-check the single app_context rule first
         */
        if (0 == app->num_procs && 1 < jdata->num_apps) {
            orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:multi-apps-and-zero-np",
                           true, jdata->num_apps, NULL);
            rc = ORTE_ERR_SILENT;
            goto error;
        }

        /* for each app_context, we have to get the list of nodes that it can
         * use since that can now be modified with a hostfile and/or -host
         * option
         */
        OBJ_CONSTRUCT(&node_list, opal_list_t);
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        num_nodes = (orte_std_cntr_t)opal_list_get_size(&node_list);

        /* if a bookmark exists from some prior mapping, set us to start there */
        if (NULL != jdata->bookmark) {
            cur_node_item = NULL;
            /* find this node on the list */
            for (item = opal_list_get_first(&node_list);
                 item != opal_list_get_end(&node_list);
                 item = opal_list_get_next(item)) {
                node = (orte_node_t*)item;
                
                if (node->index == jdata->bookmark->index) {
                    cur_node_item = item;
                    break;
                }
            }
            /* see if we found it - if not, just start at the beginning */
            if (NULL == cur_node_item) {
                cur_node_item = opal_list_get_first(&node_list); 
            }
        } else {
            /* if no bookmark, then just start at the beginning of the list */
            cur_node_item = opal_list_get_first(&node_list);
        }

        /* order this list by network nearness - i.e., the next item in the
         * list should be the node that is closest [in a network sense] to
         * the prior item in the list
         *
         * RHC: start the list with the bookmark nodeas this is where
         * we would start mapping
         */
        node = (orte_node_t*)cur_node_item;
        if (NULL == (crnode = opal_carto_base_find_node(graph, node->name))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            rc = ORTE_ERR_NOT_FOUND;
            goto error;
        }
        OBJ_CONSTRUCT(&distance, opal_value_array_t);
        if (ORTE_SUCCESS != (rc = opal_carto_base_get_nodes_distance(graph, crnode,
                                                                "SLOT", &distance))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        /* cycle through the nodes in the distance array - these
         * should be in order based on distance
         */
#if 0
        /* RHC: need to create a working list of nodes that is ordered
         * according to distance. The get_nodes_distance function returns
         * this, but it covers -all- nodes, so we have to filter that
         * against the allocated node list to create the new
         * working_node_list
         */
        for (i=0; i < distance.size; i++) {
            if 
        }
        for (item = opal_list_get_first(&node_list);
             item != opal_list_get_end(&node_list);
             item = opal_list_get_next(item)) {
            node = (orte_node_t*)item;

            if (NULL == (crnode = opal_carto.find_node(graph, node->name))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                rc = ORTE_ERR_NOT_FOUND;
                goto error;
            }
            
            /* look this node up in the distance array */
#endif
        
        /* is this node oversubscribed? */
        node = (orte_node_t*)cur_node_item;
        if (node->slots_inuse > node->slots) {
            /* work down the list - is there another node that
             * would not be oversubscribed?
             */
            if (cur_node_item != opal_list_get_end(&node_list)) {
                item = opal_list_get_next(cur_node_item);
            } else {
                item = opal_list_get_first(&node_list);
            }
            while (item != cur_node_item) {
                nd1 = (orte_node_t*)item;
                if (nd1->slots_inuse < nd1->slots) {
                    /* this node is not oversubscribed! use it! */
                    cur_node_item = item;
                    goto proceed;
                }
                if (item == opal_list_get_end(&node_list)) {
                    item = opal_list_get_first(&node_list);
                } else {
                    item= opal_list_get_next(item);
                }
            }
            /* if we get here, then we cycled all the way around the
             * list without finding a better answer - just use what
             * we have
             */
        }
        
    proceed:
        if (map->npernode == 1) {
            /* there are three use-cases that we need to deal with:
            * (a) if -np was not provided, then we just use the number of nodes
            * (b) if -np was provided AND #procs > #nodes, then error out
            * (c) if -np was provided AND #procs <= #nodes, then launch
            *     the specified #procs one/node. In this case, we just
            *     leave app->num_procs alone
            */
            if (0 == app->num_procs) {
                app->num_procs = num_nodes;
            } else if (app->num_procs > num_nodes) {
                orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:per-node-and-too-many-procs",
                               true, app->num_procs, num_nodes, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
        } else if (map->npernode > 1) {
            /* first, let's check to see if there are enough slots/node to
             * meet the request - error out if not
             */
            slots_per_node = num_slots / num_nodes;
            if (map->npernode > slots_per_node) {
                orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:n-per-node-and-not-enough-slots",
                               true, map->npernode, slots_per_node, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
            /* there are three use-cases that we need to deal with:
            * (a) if -np was not provided, then we just use the n/node * #nodes
            * (b) if -np was provided AND #procs > (n/node * #nodes), then error out
            * (c) if -np was provided AND #procs <= (n/node * #nodes), then launch
            *     the specified #procs n/node. In this case, we just
            *     leave app->num_procs alone
            */
            if (0 == app->num_procs) {
                /* set the num_procs to equal the specified num/node * the number of nodes */
                app->num_procs = map->npernode * num_nodes;
            } else if (app->num_procs > (map->npernode * num_nodes)) {
                orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:n-per-node-and-too-many-procs",
                               true, app->num_procs, map->npernode, num_nodes, num_slots, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
        } else if (0 == app->num_procs) {
            /** set the num_procs to equal the number of slots on these mapped nodes - if
            user has specified "-bynode", then set it to the number of nodes
            */
            if (map->policy & ORTE_MAPPING_BYNODE) {
                app->num_procs = num_nodes;
            } else if (map->policy & ORTE_MAPPING_BYSLOT) {
                app->num_procs = num_slots;
            } else {
                /* we can't handle this - it should have been set when we got
                 * the map info. If it wasn't, then we can only error out
                 */
                orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:no-np-and-user-map",
                               true, app->num_procs, map->npernode, num_nodes, num_slots, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
        }
        
        /** track the total number of processes we mapped */
        jdata->num_procs += app->num_procs;

        /* Make assignments */
        if (map->policy == ORTE_MAPPING_BYNODE) {
            rc = map_app_by_node(app, jdata, vpid_start, &node_list);
        } else {
            rc = map_app_by_slot(app, jdata, vpid_start, &node_list);
        }

        /* update the starting vpid for the next app_context */
        vpid_start += app->num_procs;
        
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }

        /* save the bookmark */
        jdata->bookmark = (orte_node_t*)cur_node_item;
        
        /* cleanup the node list - it can differ from one app_context
         * to another, so we have to get it every time
         */
        while(NULL != (item = opal_list_remove_first(&node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&node_list);
    }

    /* compute and save convenience values */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_local_ranks(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* define the daemons that we will use for this job */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_define_daemons(map))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    return ORTE_SUCCESS;

error:
    while(NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);

    return rc;
}


