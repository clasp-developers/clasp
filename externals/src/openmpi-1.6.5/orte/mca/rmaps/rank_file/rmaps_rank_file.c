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
 *
 * Copyright (c) 2008      Voltaire. All rights reserved
 *  
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
#include "opal/util/argv.h"
#include "opal/util/if.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/show_help.h"
#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmaps/rank_file/rmaps_rank_file.h"
#include "orte/mca/rmaps/rank_file/rmaps_rank_file_lex.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/ras/ras_types.h"

static int orte_rmaps_rank_file_parse(const char *);
static char *orte_rmaps_rank_file_parse_string_or_int(void);
static const char *orte_rmaps_rank_file_name_cur = NULL;
static opal_mutex_t orte_rmaps_rank_file_mutex;
char *orte_rmaps_rank_file_slot_list;

/*
 * Local variable
 */
static opal_list_item_t *cur_node_item = NULL;
static opal_pointer_array_t rankmap;

/*
 * Create a default mapping for the application, mapping rank by rank_file and
 * by node.
 */
static int map_app_by_node(orte_app_context_t* app,
                           orte_job_t* jdata,
                           orte_vpid_t vpid_start,
                           opal_list_t* nodes )
{
    int rc = ORTE_SUCCESS;
    opal_list_item_t *next;
    orte_node_t *node;
    orte_std_cntr_t num_alloc = 0;
    orte_proc_t *proc;

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
        if (NULL != opal_pointer_array_get_item(jdata->procs, vpid_start+num_alloc)) {
            /* this rank was already mapped */
            ++num_alloc;
            continue;
        }
        /** see if any nodes remain unused and available. We need to do this check
         * each time since we may remove nodes from the list (as they become fully
         * used) as we cycle through the loop */
        if(0 >= opal_list_get_size(nodes) ) {
            /* No more nodes to allocate :( */
            orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:alloc-error",
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
        /* grab the slot - have a new proc object created */
        proc = NULL;
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node, 1, app->idx,
                                                             nodes, jdata->map->oversubscribe, true, &proc))) {
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
        if (NULL != orte_rmaps_base.slot_list) {
            proc->slot_list = strdup(orte_rmaps_base.slot_list);
        }
        ++num_alloc;
        cur_node_item = next;
    }
    
    return ORTE_SUCCESS;
}

/*
 * Create a default mapping for the application, scheduling ranks byr rank_file
 * and by slot.
 */
static int map_app_by_slot(orte_app_context_t* app,
                           orte_job_t* jdata,
                           orte_vpid_t vpid_start,
                           opal_list_t* nodes )
{
    int rc = ORTE_SUCCESS;
    orte_std_cntr_t i, num_slots_to_take, num_alloc = 0;
    orte_node_t *node;
    opal_list_item_t *next;
    orte_proc_t *proc;

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
            orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:alloc-error",
                           true, app->num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        
        /* Save the next node we can use before claiming slots, since
         * we may need to prune the nodes list removing overused nodes.
         * Wrap around to beginning if we are at the end of the list */
        if (opal_list_get_end(nodes) == opal_list_get_next(cur_node_item)) {
            next = opal_list_get_first(nodes);
        } else {
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
            node->slots_inuse >= node->slots_alloc) {
            num_slots_to_take = (node->slots_alloc == 0) ? 1 : node->slots_alloc;
        } else {
            num_slots_to_take = node->slots_alloc - node->slots_inuse;
        }
        
        /* check if we are in npernode mode - if so, then set the num_slots_to_take
         * to the num_per_node
         */
        if (0 < jdata->map->npernode) {
            num_slots_to_take = jdata->map->npernode;
        }
        
        i = 0;
        while (num_alloc < app->num_procs && i < num_slots_to_take) {
            if (NULL != opal_pointer_array_get_item(jdata->procs, vpid_start+num_alloc)) {
                /* this rank was already mapped */
                ++num_alloc;
                continue;
            }
            /* grab the slot - have a new proc object created */
            proc = NULL;
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node, 1, app->idx,
                                                                 nodes, jdata->map->oversubscribe, true, &proc))) {
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
            if (NULL != orte_rmaps_base.slot_list) {
                proc->slot_list = strdup(orte_rmaps_base.slot_list);
            }
            /* Update the rank */
            ++num_alloc;
            /* track #slots taken */
            i++;
            
            /** if all the procs have been mapped OR we have fully used up this node, then
             * break from the loop
             */
            if(num_alloc == app->num_procs || ORTE_ERR_NODE_FULLY_USED == rc) {
                break;
            }
        }
        
        /* we move on to the next node in all cases EXCEPT if we came
         * out of the loop without having taken a full bite AND the
         * node is NOT max'd out
         *
         */
        if (i < (num_slots_to_take-1) && ORTE_ERR_NODE_FULLY_USED != rc) {
            continue;
        }
        cur_node_item = next;
    }
    
    return ORTE_SUCCESS;
}

/*
 * Create a rank_file  mapping for the job.
 */
static int orte_rmaps_rf_map(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_app_context_t *app=NULL;
    orte_std_cntr_t i, k;
    orte_vpid_t total_procs;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_node_t *node, *nd, *root_node;
    orte_vpid_t rank, vpid_start;
    orte_std_cntr_t num_nodes, num_slots;
    orte_rmaps_rank_file_map_t *rfmap;
    orte_std_cntr_t slots_per_node, relative_index, tmp_cnt;
    int rc;
    orte_proc_t *proc;
    
    /* convenience def */
    map = jdata->map;
    
    /* pickup the first app - there must be at least one */
    if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, 0))) {
        rc = ORTE_ERR_SILENT;
        goto error;
    }
    
    /* SANITY CHECKS */
    
    /* if the number of processes wasn't specified, then we know there can be only
     * one app_context allowed in the launch, and that we are to launch it across
     * all available slots. We'll double-check the single app_context rule first
     */
    if (0 == app->num_procs && 1 < jdata->num_apps) {
        orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:multi-apps-and-zero-np",
                       true, jdata->num_apps, NULL);
        rc = ORTE_ERR_SILENT;
        goto error;
    }
    
    /* likewise, we only support pernode options for a single app_context */
    if (0 < map->npernode && 1 < jdata->num_apps) {
        orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:multi-apps-and-zero-np",
                       true, jdata->num_apps, NULL);
        rc = ORTE_ERR_SILENT;
        goto error;
        
    }
    
    /* END SANITY CHECKS */
    
    /* flag the map as containing cpu_lists */
    map->cpu_lists = true;
    
    /* start at the beginning... */
    vpid_start = 0;
    jdata->num_procs = 0;
    total_procs = 0;
    OBJ_CONSTRUCT(&node_list, opal_list_t);
    OBJ_CONSTRUCT(&rankmap, opal_pointer_array_t);
    
    /* parse the rankfile, storing its results in the rankmap */
    if ( NULL != orte_rankfile ) {
        if ( ORTE_SUCCESS != (rc = orte_rmaps_rank_file_parse(orte_rankfile))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
    }
    
    /* cycle through the app_contexts, mapping them sequentially */
    for(i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        
        /* for each app_context, we have to get the list of nodes that it can
         * use since that can now be modified with a hostfile and/or -host
         * option
         */
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        num_nodes = (orte_std_cntr_t)opal_list_get_size(&node_list);
        
        /* we already checked for sanity, so these are okay to just do here */
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
                orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:per-node-and-too-many-procs",
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
                orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:n-per-node-and-not-enough-slots",
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
                orte_show_help("help-rmaps_rank_file.txt", "orte-rmaps-rf:n-per-node-and-too-many-procs",
                               true, app->num_procs, map->npernode, num_nodes, num_slots, NULL);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
        } else if (0 == app->num_procs) {
            /** set the num_procs to equal the number of slots on these mapped nodes */
            app->num_procs = num_slots;
        }

        /* keep track of the total #procs in this job */
        total_procs += app->num_procs;
        
        for (k=0; k < app->num_procs; k++) {
            rank = vpid_start + k;
            /* get the rankfile entry for this rank */
            if (NULL == (rfmap = (orte_rmaps_rank_file_map_t*)opal_pointer_array_get_item(&rankmap, rank))) {
                /* no entry for this rank */
                continue;
            }
            
            /* find the node where this proc was assigned */
            node = NULL;
            for (item = opal_list_get_first(&node_list);
                 item != opal_list_get_end(&node_list);
                 item = opal_list_get_next(item)) {
                nd = (orte_node_t*)item;
                if (NULL != rfmap->node_name &&
                    0 == strcmp(nd->name, rfmap->node_name)) {
                    node = nd;
                    break;
                } else if (NULL != rfmap->node_name &&
                           (('+' == rfmap->node_name[0]) && 
                            (('n' == rfmap->node_name[1]) ||
                             ('N' == rfmap->node_name[1])))) {

                    relative_index=atoi(strtok(rfmap->node_name,"+n"));
                    if ( relative_index >= (int)opal_list_get_size (&node_list) || ( 0 > relative_index)){
                        orte_show_help("help-rmaps_rank_file.txt","bad-index", true,rfmap->node_name);
                        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                        return ORTE_ERR_BAD_PARAM;
                    }
                    root_node = (orte_node_t*) opal_list_get_first(&node_list);
                    for(tmp_cnt=0; tmp_cnt<relative_index; tmp_cnt++) {
                        root_node = (orte_node_t*) opal_list_get_next(root_node);
                    }
                    node = root_node;
                    break;
                }  
					                
            }
            if (NULL == node) {
                orte_show_help("help-rmaps_rank_file.txt","bad-host", true, rfmap->node_name);
                return ORTE_ERR_SILENT;
            }
            if (0 == strlen(rfmap->slot_list)) {
                /* rank was specified but no slot list given - that's an error */
                orte_show_help("help-rmaps_rank_file.txt","no-slot-list", true, rank, rfmap->node_name);
                return ORTE_ERR_SILENT;
            }
            proc = NULL;
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node, 1, app->idx,
                                                                 &node_list, jdata->map->oversubscribe, true, &proc))) {
                if (ORTE_ERR_NODE_FULLY_USED != rc) {
                    /* if this is a true error and not the node just being
                     * full, then report the error and abort
                     */
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
            proc->name.vpid = rank;
            proc->slot_list = strdup(rfmap->slot_list);
            jdata->num_procs++;
        }
        /* update the starting point */
        vpid_start += app->num_procs;
        /* cleanup the node list - it can differ from one app_context
         * to another, so we have to get it every time
         */
        while(NULL != (item = opal_list_remove_first(&node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&node_list);
        OBJ_CONSTRUCT(&node_list, opal_list_t);
    }
    OBJ_DESTRUCT(&node_list);
    
    /* did we map all the procs, or did the user's rankfile not contain
     * a specification for every rank?
     */
    if (jdata->num_procs < total_procs) {
        /* we need to map the remainder of the procs according to the
         * mapping policy
         */
        vpid_start = 0;
        for(i=0; i < jdata->apps->size; i++) {
            if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
                continue;
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
            if (map->policy & ORTE_MAPPING_BYNODE) {
                rc = map_app_by_node(app, jdata, vpid_start, &node_list);
            } else {
                rc = map_app_by_slot(app, jdata, vpid_start, &node_list);
            }
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                goto error;
            }
            vpid_start += app->num_procs;
            /* cleanup the node list - it can differ from one app_context
             * to another, so we have to get it every time
             */
            while(NULL != (item = opal_list_remove_first(&node_list))) {
                OBJ_RELEASE(item);
            }
            OBJ_DESTRUCT(&node_list);
        }
        /* save the bookmark */
        jdata->bookmark = (orte_node_t*)cur_node_item;
    }
    
    /* update the job's number of procs */
    jdata->num_procs = total_procs;
    
    /* compute vpids and add proc objects to the job */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
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
    
    /* cleanup the rankmap */
    for (i=0; i < rankmap.size; i++) {
        if (NULL != (rfmap = opal_pointer_array_get_item(&rankmap, i))) {
            OBJ_RELEASE(rfmap);
        }
    }
    OBJ_DESTRUCT(&rankmap);
    return ORTE_SUCCESS;

 error:
    while(NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);
    
    return rc;
}

orte_rmaps_base_module_t orte_rmaps_rank_file_module = {
orte_rmaps_rf_map
};


static int orte_rmaps_rank_file_parse(const char *rankfile)
{
    int token;
    int rc = ORTE_SUCCESS;
    int cnt;
    char* node_name = NULL;
    char* username = NULL; 
    char** argv;
    char buff[64];
    char* value;
    int rank=-1;
    int i;
    orte_node_t *hnp_node;
    orte_rmaps_rank_file_map_t *rfmap=NULL;

    OPAL_THREAD_LOCK(&orte_rmaps_rank_file_mutex);
    
    /* get the hnp node's info */
    hnp_node = (orte_node_t*)(orte_node_pool->addr[0]);
    
    orte_rmaps_rank_file_name_cur = rankfile;
    orte_rmaps_rank_file_done = false;
    orte_rmaps_rank_file_in = fopen(rankfile, "r");
    
    if (NULL == orte_rmaps_rank_file_in) {
        orte_show_help("help-rmaps_rank_file.txt", "no-rankfile", true, rankfile);
        rc = OPAL_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(rc);
        goto unlock;
    }
    
    while (!orte_rmaps_rank_file_done) {
        token = orte_rmaps_rank_file_lex();
        
        switch (token) {
            case ORTE_RANKFILE_ERROR:
                opal_output(0, "Got an error!");
                break;
            case ORTE_RANKFILE_QUOTED_STRING:
                orte_show_help("help-rmaps_rank_file.txt", "not-supported-rankfile", true, "QUOTED_STRING", rankfile);
                rc = ORTE_ERR_BAD_PARAM;
                ORTE_ERROR_LOG(rc);
                goto unlock;
            case ORTE_RANKFILE_NEWLINE:
                rank = -1;
                if (NULL != node_name) {
                    free(node_name);
                }
                node_name = NULL;
                rfmap = NULL;
                break;
            case ORTE_RANKFILE_RANK:
                token = orte_rmaps_rank_file_lex();
                if (ORTE_RANKFILE_INT == token) {
                    rank = orte_rmaps_rank_file_value.ival;
                    rfmap = OBJ_NEW(orte_rmaps_rank_file_map_t);
                    opal_pointer_array_set_item(&rankmap, rank, rfmap);
                } else {
                    orte_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                    rc = ORTE_ERR_BAD_PARAM;
                    ORTE_ERROR_LOG(rc);
                    goto unlock;
                }
                break;
            case ORTE_RANKFILE_USERNAME:
                orte_show_help("help-rmaps_rank_file.txt", "not-supported-rankfile", true, "USERNAME", rankfile);
                rc = ORTE_ERR_BAD_PARAM;
                ORTE_ERROR_LOG(rc);
                goto unlock;
                break;
            case ORTE_RANKFILE_EQUAL:
                if (rank < 0) {
                    orte_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                    rc = ORTE_ERR_BAD_PARAM;
                    ORTE_ERROR_LOG(rc);
                    goto unlock;
                }
                token = orte_rmaps_rank_file_lex();
                switch (token) {
                    case ORTE_RANKFILE_HOSTNAME:
                    case ORTE_RANKFILE_IPV4:
                    case ORTE_RANKFILE_IPV6:
                    case ORTE_RANKFILE_STRING:
                    case ORTE_RANKFILE_INT:
                    case ORTE_RANKFILE_RELATIVE:
                        if(ORTE_RANKFILE_INT == token) {
                            sprintf(buff,"%d", orte_rmaps_rank_file_value.ival);
                            value = buff;
                        } else {
                            value = orte_rmaps_rank_file_value.sval;
                        }
                        argv = opal_argv_split (value, '@');
                        cnt = opal_argv_count (argv);
                        if (1 == cnt) {
                            node_name = strdup(argv[0]);
                        } else if (2 == cnt) {
                            username = strdup(argv[0]);
                            node_name = strdup(argv[1]);
                        }
                        else {
                            orte_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                            rc = ORTE_ERR_BAD_PARAM;
                            ORTE_ERROR_LOG(rc);
                            goto unlock;
                        }
                        opal_argv_free (argv);
                        /* check the rank item */
                        if (NULL == rfmap) {
                            orte_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                            rc = ORTE_ERR_BAD_PARAM;
                            ORTE_ERROR_LOG(rc);
                            goto unlock;
                        }
                        /* check if this is the local node */
                        if (0 == strcmp(node_name, hnp_node->name) ||
                            opal_ifislocal(node_name)) {
                            rfmap->node_name = strdup(hnp_node->name);
                        } else {
                            rfmap->node_name = strdup(node_name);
                        }
                }
                break;
            case ORTE_RANKFILE_SLOT:
                if (NULL == node_name || rank < 0 ||
                    NULL == (value = orte_rmaps_rank_file_parse_string_or_int())) {
                    orte_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                    rc = ORTE_ERR_BAD_PARAM;
                    ORTE_ERROR_LOG(rc);
                    goto unlock;
                }
                /* check the rank item */
                if (NULL == rfmap) {
                    orte_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                    rc = ORTE_ERR_BAD_PARAM;
                    ORTE_ERROR_LOG(rc);
                    goto unlock;
                }
                /* we no longer support physical mappings */
                if ('P' == value[0] || 'p' == value[0]) {
                    orte_show_help("help-rmaps_rank_file.txt", "not-supported", true, rankfile);
                    rc = ORTE_ERR_SILENT;
                    ORTE_ERROR_LOG(rc);
                    goto unlock;
                }
                for (i=0; i < 64 && '\0' != value[i]; i++) {
                    rfmap->slot_list[i] = value[i];
                }
                break;
        }
    }
    fclose(orte_rmaps_rank_file_in);
    orte_rmaps_rank_file_in = NULL;
    
unlock:
    if (NULL != node_name) {
        free(node_name);
    }
    orte_rmaps_rank_file_name_cur = NULL;
    OPAL_THREAD_UNLOCK(&orte_rmaps_rank_file_mutex);
    return rc;
}


static char *orte_rmaps_rank_file_parse_string_or_int(void)
{
    int rc;
    char tmp_str[64];
    
    if (ORTE_RANKFILE_EQUAL != orte_rmaps_rank_file_lex()){
        return NULL;
    }
    
    rc = orte_rmaps_rank_file_lex();
    switch (rc) {
        case ORTE_RANKFILE_STRING:
            return strdup(orte_rmaps_rank_file_value.sval);
        case ORTE_RANKFILE_INT:
            sprintf(tmp_str,"%d",orte_rmaps_rank_file_value.ival);
            return strdup(tmp_str);
        default:
            return NULL;
            
    }
    
}
