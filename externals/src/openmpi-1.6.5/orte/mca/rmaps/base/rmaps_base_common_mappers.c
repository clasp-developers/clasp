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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
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
 * determine the proper starting point for the next mapping operation
 */
opal_list_item_t* orte_rmaps_base_get_starting_point(opal_list_t *node_list, orte_job_t *jdata)
{
    opal_list_item_t *item, *cur_node_item;
    orte_node_t *node, *nd1, *ndmin;
    int overload;
    
    /* if a bookmark exists from some prior mapping, set us to start there */
    if (NULL != jdata->bookmark) {
        cur_node_item = NULL;
        /* find this node on the list */
        for (item = opal_list_get_first(node_list);
             item != opal_list_get_end(node_list);
             item = opal_list_get_next(item)) {
            node = (orte_node_t*)item;
            
            if (node->index == jdata->bookmark->index) {
                cur_node_item = item;
                break;
            }
        }
        /* see if we found it - if not, just start at the beginning */
        if (NULL == cur_node_item) {
            cur_node_item = opal_list_get_first(node_list); 
        }
    } else {
        /* if no bookmark, then just start at the beginning of the list */
        cur_node_item = opal_list_get_first(node_list);
    }
    
    /* is this node fully subscribed? If so, then the first
     * proc we assign will oversubscribe it, so let's look
     * for another candidate
     */
    node = (orte_node_t*)cur_node_item;
    ndmin = node;
    overload = ndmin->slots_inuse - ndmin->slots_alloc;
    if (node->slots_inuse >= node->slots_alloc) {
        /* work down the list - is there another node that
         * would not be oversubscribed?
         */
        if (cur_node_item != opal_list_get_last(node_list)) {
            item = opal_list_get_next(cur_node_item);
        } else {
            item = opal_list_get_first(node_list);
        }
        while (item != cur_node_item) {
            nd1 = (orte_node_t*)item;
            if (nd1->slots_inuse < nd1->slots_alloc) {
                /* this node is not oversubscribed! use it! */
                return item;
            }
            /* this one was also oversubscribed, keep track of the
             * node that has the least usage - if we can't
             * find anyone who isn't fully utilized, we will
             * start with the least used node
             */
            if (overload >= (nd1->slots_inuse - nd1->slots_alloc)) {
                ndmin = nd1;
                overload = ndmin->slots_inuse - ndmin->slots_alloc;
            }
            if (item == opal_list_get_last(node_list)) {
                item = opal_list_get_first(node_list);
            } else {
                item= opal_list_get_next(item);
            }
        }
        /* if we get here, then we cycled all the way around the
         * list without finding a better answer - just use the node
         * that is minimally overloaded
         */
        cur_node_item = (opal_list_item_t*)ndmin;
    }

    return cur_node_item;
}

/*
 * Query the registry for all nodes allocated to a specified app_context
 */
int orte_rmaps_base_map_byslot(orte_job_t *jdata, orte_app_context_t *app,
                               opal_list_t *node_list, orte_vpid_t num_procs,
                               opal_list_item_t *cur_node_item)
{
    int rc=ORTE_SUCCESS;
    int i;
    orte_node_t *node;
    orte_proc_t *proc;
    opal_list_item_t *next;
    orte_vpid_t num_alloc = 0;
    orte_vpid_t start;
    int num_procs_to_assign, num_possible_procs;
    
    /* This loop continues until all procs have been mapped or we run
     out of resources. We determine that we have "run out of
     resources" when either all nodes have slots_max processes mapped to them,
     (thus there are no free slots for a process to be mapped), OR all nodes
     have reached their soft limit and the user directed us to "no oversubscribe".
     If we still have processes that haven't been mapped yet, then it's an
     "out of resources" error. */
    
    start = jdata->num_procs;
    
    while ( num_alloc < num_procs) {
        /** see if any nodes remain unused and available. We need to do this check
         * each time since we may remove nodes from the list (as they become fully
         * used) as we cycle through the loop */
        if(0 >= opal_list_get_size(node_list) ) {
            /* Everything is at max usage! :( */
            orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:alloc-error",
                           true, num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        
        /* Save the next node we can use before claiming slots, since
         * we may need to prune the nodes list removing overused nodes.
         * Wrap around to beginning if we are at the end of the list */
        if (opal_list_get_end(node_list) == opal_list_get_next(cur_node_item)) {
            next = opal_list_get_first(node_list);
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
         *     unless we are loadbalancing, in which case we only take one
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
        if (node->slots_inuse >= node->slots_alloc || 0 == node->slots_inuse) {
            if (0 == node->slots_alloc) {
                num_procs_to_assign = 1;
            } else {
                /* 'num_possible_procs' defines the number of ranks */
                num_possible_procs = node->slots_alloc;
                if (0 == num_possible_procs) {
                    num_procs_to_assign = 1;
                } else {
                    num_procs_to_assign = num_possible_procs;
                }
            }
        } else {
            /* 'num_possible_procs' define number of ranks on the node. Each
             * rank occupies one slot. Each slot may represent more than one
             * cpu, depending on the cpus-per-task setting
             */
            num_possible_procs = (node->slots_alloc - node->slots_inuse);
            if (0 == num_possible_procs) {
                num_procs_to_assign = 1;
            } else {
                num_procs_to_assign = num_possible_procs;
            }
        }
        
        /* check if we are in npernode mode - if so, then set the num_slots_to_take
         * to the num_per_node
         */
        if (0 < jdata->map->npernode) {
            num_procs_to_assign = jdata->map->npernode;
        }
        
        for( i = 0; i < num_procs_to_assign; ++i) {
            proc = NULL;
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                 jdata->map->cpus_per_rank, app->idx,
                                                                 node_list, jdata->map->oversubscribe,
                                                                 true, &proc))) {
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
            
            /* assign the vpid */
            proc->name.vpid = start++;
            
            /* Update the number of procs allocated */
            ++num_alloc;
            
            /** if all the procs have been mapped, we return */
            if (num_alloc == num_procs) {
                goto complete;
            }
            
            /* if we have fully used up this node, then break from the loop */
            if (ORTE_ERR_NODE_FULLY_USED == rc) {
                break;
            }
        }
        
        /* we move on to the next node in all cases EXCEPT if we came
         * out of the loop without having taken a full bite AND the
         * node is NOT max'd out
         *
         */
        if (i < (num_procs_to_assign-1) && ORTE_ERR_NODE_FULLY_USED != rc) {
            continue;
        }
        cur_node_item = next;
    }
    
complete:    
    /* save the bookmark */
    jdata->bookmark = (orte_node_t*)cur_node_item;
    
    return ORTE_SUCCESS;
}

int orte_rmaps_base_map_bynode(orte_job_t *jdata, orte_app_context_t *app,
                               opal_list_t *node_list, orte_vpid_t num_procs,
                               opal_list_item_t *cur_node_item)
{
    int rc = ORTE_SUCCESS;
    opal_list_item_t *next;
    orte_node_t *node;
    orte_proc_t *proc;
    orte_vpid_t num_alloc=0;
    orte_vpid_t start;
    
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
    
    start = jdata->num_procs;
    
    while (num_alloc < num_procs) {
        /** see if any nodes remain unused and available. We need to do this check
         * each time since we may remove nodes from the list (as they become fully
         * used) as we cycle through the loop */
        if(0 >= opal_list_get_size(node_list) ) {
            /* No more nodes to allocate :( */
            orte_show_help("help-orte-rmaps-rr.txt", "orte-rmaps-rr:alloc-error",
                           true, num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        
        /* Save the next node we can use before claiming slots, since
         * we may need to prune the nodes list removing overused nodes.
         * Wrap around to beginning if we are at the end of the list */
        if (opal_list_get_end(node_list) == opal_list_get_next(cur_node_item)) {
            next = opal_list_get_first(node_list);
        }
        else {
            next = opal_list_get_next(cur_node_item);
        }
        
        /* Allocate a slot on this node */
        node = (orte_node_t*) cur_node_item;
        proc = NULL;
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node, jdata->map->cpus_per_rank, app->idx,
                                                             node_list, jdata->map->oversubscribe, true, &proc))) {
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
        
        /* assign the vpid */
        proc->name.vpid = start++;
        
        /* Update the number of procs allocated */
        ++num_alloc;
        
        cur_node_item = next;
    }
    
    /* save the bookmark */
    jdata->bookmark = (orte_node_t*)cur_node_item;

    return ORTE_SUCCESS;
}
