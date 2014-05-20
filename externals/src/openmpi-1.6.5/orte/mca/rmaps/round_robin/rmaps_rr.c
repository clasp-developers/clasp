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

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_rr.h"


/*
 * Create a round-robin mapping for the job.
 */
static int orte_rmaps_rr_map(orte_job_t *jdata)
{
    orte_app_context_t *app;
    int i;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_std_cntr_t num_nodes, num_slots;
    int rc;
    opal_list_item_t *cur_node_item;
    
    /* start at the beginning... */
    jdata->num_procs = 0;
    
    /* cycle through the app_contexts, mapping them sequentially */
    for(i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        
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
                                                                  jdata->map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        num_nodes = (orte_std_cntr_t)opal_list_get_size(&node_list);

        /* if a bookmark exists from some prior mapping, set us to start there */
        cur_node_item = orte_rmaps_base_get_starting_point(&node_list, jdata);
        
       if (0 == app->num_procs) {
            /* set the num_procs to equal the number of slots on these mapped nodes */
            app->num_procs = num_slots;
        }
        
        /* Make assignments */
        if (jdata->map->policy & ORTE_MAPPING_BYNODE) {
            rc = orte_rmaps_base_map_bynode(jdata, app, &node_list,
                                            app->num_procs, cur_node_item);
        } else {
            rc = orte_rmaps_base_map_byslot(jdata, app, &node_list,
                                            app->num_procs, cur_node_item);
        }
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }

        /* track the total number of processes we mapped */
        jdata->num_procs += app->num_procs;
        
        /* cleanup the node list - it can differ from one app_context
         * to another, so we have to get it every time
         */
        while(NULL != (item = opal_list_remove_first(&node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&node_list);
    }

    /* compute vpids and add proc objects to the job */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
        ORTE_ERROR_LOG(rc);
        return rc;
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
    while(NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);

    return rc;
}

orte_rmaps_base_module_t orte_rmaps_round_robin_module = {
    orte_rmaps_rr_map
};

