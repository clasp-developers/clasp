/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
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
#include <stdio.h>

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_resilient.h"


/*
 * Local variable
 */
static opal_list_item_t *cur_node_item = NULL;

static char *orte_getline(FILE *fp);

/* default round-robin mapper */
static int rr_map_default(orte_job_t *jdata, orte_app_context_t *app,
                         opal_list_t *node_list, orte_vpid_t num_procs)
{
    int rc;
    
    /* if a bookmark exists from some prior mapping, set us to start there */
    cur_node_item = orte_rmaps_base_get_starting_point(node_list, jdata);
    
    /* now perform the mapping */
    if (ORTE_MAPPING_BYNODE & jdata->map->policy) {
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_map_bynode(jdata, app, node_list,
                                                             num_procs, cur_node_item))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    } else {
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_map_byslot(jdata, app, node_list,
                                                             num_procs, cur_node_item))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;
}

static void flag_nodes(opal_list_t *node_list)
{
    opal_list_item_t *item, *nitem;
    orte_node_t *node, *nd;
    orte_rmaps_res_ftgrp_t *ftgrp;
    int k;
    
    for (item = opal_list_get_first(&mca_rmaps_resilient_component.fault_grps);
         item != opal_list_get_end(&mca_rmaps_resilient_component.fault_grps);
         item = opal_list_get_next(item)) {
        ftgrp = (orte_rmaps_res_ftgrp_t*)item;
        /* reset the flags */
        ftgrp->used = false;
        ftgrp->included = false;
        /* if at least one node in our list is included in this
         * ftgrp, then flag it as included
         */
        for (nitem = opal_list_get_first(node_list);
             !ftgrp->included && nitem != opal_list_get_end(node_list);
             nitem = opal_list_get_next(nitem)) {
            node = (orte_node_t*)nitem;
            for (k=0; k < ftgrp->nodes.size; k++) {
                if (NULL == (nd = (orte_node_t*)opal_pointer_array_get_item(&ftgrp->nodes, k))) {
                    continue;
                }
                if (0 == strcmp(nd->name, node->name)) {
                    ftgrp->included = true;
                    break;
                }
            }
        }
    }
}


/*
 * Loadbalance the cluster
 */
static int orte_rmaps_resilient_map(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_app_context_t *app;
    int i, j, k, totnodes;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_std_cntr_t num_slots;
    int rc;
    float avgload, minload;
    orte_node_t *node, *nd=NULL, *oldnode;
    orte_rmaps_res_ftgrp_t *ftgrp, *target;
    orte_vpid_t totprocs, lowprocs, num_assigned;
    FILE *fp;
    char *ftinput;
    int grp;
    char **nodes;
    bool found;
    orte_proc_t *proc;
    
    /* have we already constructed the fault group list? */
    if (0 == opal_list_get_size(&mca_rmaps_resilient_component.fault_grps) &&
        NULL != mca_rmaps_resilient_component.fault_group_file) {
        /* construct it */
        OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                             "%s rmaps:resilient: constructing fault groups",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        fp = fopen(mca_rmaps_resilient_component.fault_group_file, "r");
        if (NULL == fp) { /* not found */
            orte_show_help("help-orte-rmaps-resilient.txt", "orte-rmaps-resilient:file-not-found",
                           true, mca_rmaps_resilient_component.fault_group_file);
            return ORTE_ERR_SILENT;
        }
        /* build list of fault groups */
        grp = 0;
        while (NULL != (ftinput = orte_getline(fp))) {
            ftgrp = OBJ_NEW(orte_rmaps_res_ftgrp_t);
            ftgrp->ftgrp = grp++;
            nodes = opal_argv_split(ftinput, ',');
            /* find the referenced nodes */
            for (k=0; k < opal_argv_count(nodes); k++) {
                found = false;
                for (i=0; i < orte_node_pool->size && !found; i++) {
                    if (NULL == (node = opal_pointer_array_get_item(orte_node_pool, i))) {
                        continue;
                    }
                    if (0 == strcmp(node->name, nodes[k])) {
                        OBJ_RETAIN(node);
                        OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                                             "%s rmaps:resilient: adding node %s to fault group %d",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             node->name, ftgrp->ftgrp));
                        opal_pointer_array_add(&ftgrp->nodes, node);
                        found = true;
                        break;
                    }
                }
            }
            opal_list_append(&mca_rmaps_resilient_component.fault_grps, &ftgrp->super);
            opal_argv_free(nodes);
            free(ftinput);
        }
        fclose(fp);
    }
    
    /* the map will never be NULL as we initialize it before getting here,
     * so check to see if any nodes are in the map - this will be our
     * indicator that this is the prior map for a failed job that
     * needs to be re-mapped
     *
     * NOTE: if a proc is being ADDED to an existing job, then its
     * node field will be NULL.
     */
    if (0 < jdata->map->num_nodes) {
        OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                             "%s rmaps:resilient: remapping job %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid)));
        /* cycle through all the procs in this job to find the one(s) that failed */
        for (i=0; i < jdata->procs->size; i++) {
            /* get the proc object */
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
                continue;
            }
            /* is this proc to be restarted? */
            if (proc->state != ORTE_PROC_STATE_RESTART) {
                continue;
            }
            /* save the current node */
            oldnode = proc->node;
            /* point to the app */
            app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, proc->app_idx);
            OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                                 "%s rmaps:resilient: proc %s from node %s is to be restarted",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name),
                                 (NULL == proc->node) ? "NULL" : proc->node->name));
             /* if we have fault groups, flag all the fault groups that
             * include this node so we don't reuse them
             */
            target = NULL;
            minload = 1000000.0;
            for (item = opal_list_get_first(&mca_rmaps_resilient_component.fault_grps);
                 item != opal_list_get_end(&mca_rmaps_resilient_component.fault_grps);
                 item = opal_list_get_next(item)) {
                ftgrp = (orte_rmaps_res_ftgrp_t*)item;
                /* see if the node is in this fault group */
                ftgrp->included = true;
                ftgrp->used = false;
                for (k=0; k < ftgrp->nodes.size; k++) {
                    if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(&ftgrp->nodes, k))) {
                        continue;
                    }
                    if (NULL != proc->node && 0 == strcmp(node->name, proc->node->name)) {
                        /* yes - mark it to not be included */
                        OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                                             "%s rmaps:resilient: node %s is in fault group %d, which will be excluded",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             proc->node->name, ftgrp->ftgrp));
                        ftgrp->included = false;
                        break;
                    }
                }
                /* if this ftgrp is not included, then skip it */
                if (!ftgrp->included) {
                    continue;
                }
                /* compute the load average on this fault group */
                totprocs = 0;
                totnodes = 0;
                for (k=0; k < ftgrp->nodes.size; k++) {
                    if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(&ftgrp->nodes, k))) {
                        continue;
                    }
                    totnodes++;
                    totprocs += node->num_procs;
                }
                avgload = (float)totprocs / (float)totnodes;
                /* now find the lightest loaded of the included fault groups */
                if (avgload < minload) {
                    minload = avgload;
                    target = ftgrp;
                    OPAL_OUTPUT_VERBOSE((2, orte_rmaps_base.rmaps_output,
                                         "%s rmaps:resilient: found new min load ftgrp %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ftgrp->ftgrp));
                }
            }
            /* if no ftgrps are available, then just map it on the lightest loaded
             * node known to the system, avoiding the current node if possible and
             * taking into account any limitations specified by user in hostfile
             * and -host options
             */
            if (NULL == target) {
                nd = oldnode;  /* put it back where it was if nothing else is found */
                totprocs = 1000000;
                OBJ_CONSTRUCT(&node_list, opal_list_t);
                map = jdata->map;
                if (ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app, map->policy))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* find the lightest loaded node while deconstructing the list */
                while (NULL != (item = opal_list_remove_first(&node_list))) {
                    node = (orte_node_t*)item;
                    if (node->num_procs < totprocs) {
                        nd = node;
                        totprocs = node->num_procs;
                    }
                    OBJ_RELEASE(item);
                }
                OBJ_DESTRUCT(&node_list);

                OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                                     "%s rmaps:resilient: no avail fault groups found - placing proc on node %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     nd->name));
                /* put proc on the found node */
                if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, nd, jdata->map->cpus_per_rank, proc->app_idx,
                                                                     NULL, jdata->map->oversubscribe, false, &proc))) {
                    /** if the code is ORTE_ERR_NODE_FULLY_USED, then we know this
                     * really isn't an error
                     */
                    if (ORTE_ERR_NODE_FULLY_USED != rc) {
                        ORTE_ERROR_LOG(rc);
                        goto error;
                    }
                }
                /* flag the proc state as non-launched so we'll know to launch it */
                proc->state = ORTE_PROC_STATE_INIT;
                /* update the node and local ranks so static ports can
                 * be properly selected if active
                 */
                orte_rmaps_base_update_local_ranks(jdata, oldnode, nd, proc);
                continue;
            }
            /* if we did find a target, re-map the proc to the lightest loaded
             * node in that group
             */
            lowprocs = 1000000;
            nd = NULL;
            for (k=0; k < target->nodes.size; k++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(&target->nodes, k))) {
                    continue;
                }
                if (node->num_procs < lowprocs) {
                    lowprocs = node->num_procs;
                    nd = node;
                }
            }
            OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                                 "%s rmaps:resilient: placing proc %s into fault group %d node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name), target->ftgrp, nd->name));
            if (NULL != proc->node) {
                OBJ_RELEASE(proc->node);  /* required to maintain bookkeeping */
            }
            /* put proc on the found node */
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, nd, jdata->map->cpus_per_rank, proc->app_idx,
                                                                 NULL, jdata->map->oversubscribe, false, &proc))) {
                /** if the code is ORTE_ERR_NODE_FULLY_USED, then we know this
                 * really isn't an error
                 */
                if (ORTE_ERR_NODE_FULLY_USED != rc) {
                    ORTE_ERROR_LOG(rc);
                    goto error;
                }
            }
            /* flag the proc state as non-launched so we'll know to launch it */
            proc->state = ORTE_PROC_STATE_INIT;
            /* update the node and local ranks so static ports can
             * be properly selected if active
             */
            orte_rmaps_base_update_local_ranks(jdata, oldnode, nd, proc);
        }
        /* define the daemons that we will use for this job */
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_define_daemons(jdata->map))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        return ORTE_SUCCESS;
    }
    
    
    /* CREATE INITIAL MAP FOR A JOB */
    /* we map each app_context separately when creating an initial job map. For
     * each app_context, we get the list of available nodes as this can be
     * app_context specific based on hostfile and -host options. We then organize
     * that list into fault groups based on the fault group definitions, if
     * provided, and then divide the specified number of copies across them in
     * a load-balanced way
     */
    
    OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                         "%s rmaps:resilient: creating initial map for job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));

    /* start at the beginning... */
    jdata->num_procs = 0;
    map = jdata->map;
    
    for (i=0; i < jdata->apps->size; i++) {
        /* get the app_context */
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        /* you cannot use this mapper unless you specify the number of procs to
         * launch for each app
         */
        if (0 == app->num_procs) {
            orte_show_help("help-orte-rmaps-resilient.txt",
                           "orte-rmaps-resilient:num-procs",
                           true);
            return ORTE_ERR_SILENT;
        }
        num_assigned = 0;
        /* for each app_context, we have to get the list of nodes that it can
         * use since that can now be modified with a hostfile and/or -host
         * option
         */
        OBJ_CONSTRUCT(&node_list, opal_list_t);
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        /* were we given a fault group definition? */
        if (0 < opal_list_get_size(&mca_rmaps_resilient_component.fault_grps)) {
            OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                                 "%s rmaps:resilient: using fault groups",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            /* flag the fault groups included by these nodes */
            flag_nodes(&node_list);
            /* map each copy to a different fault group - if more copies are
             * specified than fault groups, then overlap in a round-robin fashion
             */
            for (j=0; j < app->num_procs; j++) {
                /* find unused included fault group with lowest average load - if none
                 * found, then break
                 */
                target = NULL;
                minload = 1000000000.0;
                for (item = opal_list_get_first(&mca_rmaps_resilient_component.fault_grps);
                     item != opal_list_get_end(&mca_rmaps_resilient_component.fault_grps);
                     item = opal_list_get_next(item)) {
                    ftgrp = (orte_rmaps_res_ftgrp_t*)item;
                    OPAL_OUTPUT_VERBOSE((2, orte_rmaps_base.rmaps_output,
                                         "%s rmaps:resilient: fault group %d used: %s included %s",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ftgrp->ftgrp,
                                         ftgrp->used ? "YES" : "NO",
                                         ftgrp->included ? "YES" : "NO" ));
                    /* if this ftgrp has already been used or is not included, then
                     * skip it
                     */
                    if (ftgrp->used || !ftgrp->included) {
                        continue;
                    }
                    /* compute the load average on this fault group */
                    totprocs = 0;
                    totnodes = 0;
                    for (k=0; k < ftgrp->nodes.size; k++) {
                        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(&ftgrp->nodes, k))) {
                            continue;
                        }
                        totnodes++;
                        totprocs += node->num_procs;
                    }
                    avgload = (float)totprocs / (float)totnodes;
                    if (avgload < minload) {
                        minload = avgload;
                        target = ftgrp;
                        OPAL_OUTPUT_VERBOSE((2, orte_rmaps_base.rmaps_output,
                                             "%s rmaps:resilient: found new min load ftgrp %d",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             ftgrp->ftgrp));
                    }
                }
                /* if we have more procs than fault groups, then we simply
                 * map the remaining procs on available nodes in a round-robin
                 * fashion - it doesn't matter where they go as they will not
                 * be contributing to fault tolerance by definition
                 */
                if (NULL == target) {
                    OPAL_OUTPUT_VERBOSE((2, orte_rmaps_base.rmaps_output,
                                         "%s rmaps:resilient: no available fault group - mapping rr",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    if (ORTE_SUCCESS != (rc = rr_map_default(jdata, app, &node_list, app->num_procs-num_assigned))) {
                        goto error;
                    }
                    goto cleanup;
                }
                /* pick node with lowest load from within that group */
                totprocs = 1000000;
                for (k=0; k < target->nodes.size; k++) {
                    if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(&target->nodes, k))) {
                        continue;
                    }
                    if (node->num_procs < totprocs) {
                        totprocs = node->num_procs;
                        nd = node;
                    }
                }
                OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                                     "%s rmaps:resilient: placing proc into fault group %d node %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     target->ftgrp, nd->name));
                /* put proc on that node */
                if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, nd, jdata->map->cpus_per_rank, app->idx,
                                                                     &node_list, jdata->map->oversubscribe, false, NULL))) {
                    /** if the code is ORTE_ERR_NODE_FULLY_USED, then we know this
                     * really isn't an error
                     */
                    if (ORTE_ERR_NODE_FULLY_USED != rc) {
                        ORTE_ERROR_LOG(rc);
                        goto error;
                    }
                }
                /* track number of procs mapped */
                num_assigned++;
                
                /* flag this fault group as used */
                target->used = true;
            }
        } else {
            /* if we don't have a fault group definition, then just map the
             * procs in a round-robin manner
             */
            OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                                 "%s rmaps:resilient: no fault groups provided - mapping rr",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            if (ORTE_SUCCESS != (rc = rr_map_default(jdata, app, &node_list, app->num_procs))) {
                goto error;
            }
        }
        
    cleanup:
        /* compute vpids and add proc objects to the job - this has to be
         * done after each app_context is mapped in order to keep the
         * vpids contiguous within an app_context
         */
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* track number of procs */
        jdata->num_procs += app->num_procs;

        /* cleanup the node list - it can differ from one app_context
         * to another, so we have to get it every time
         */
        while (NULL != (item = opal_list_remove_first(&node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&node_list);
    }
    
    /* compute and save local ranks */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_local_ranks(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* define the daemons that we will use for this job */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_define_daemons(jdata->map))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;

error:
    while (NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);

    return rc;
}

orte_rmaps_base_module_t orte_rmaps_resilient_module = {
    orte_rmaps_resilient_map
};

static char *orte_getline(FILE *fp)
{
    char *ret, *buff;
    char input[1024];
    
    ret = fgets(input, 1024, fp);
    if (NULL != ret) {
        input[strlen(input)-1] = '\0';  /* remove newline */
        buff = strdup(input);
        return buff;
    }
    
    return NULL;
}

