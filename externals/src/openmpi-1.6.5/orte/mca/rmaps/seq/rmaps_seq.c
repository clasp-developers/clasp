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
#include "opal/util/if.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/hostfile/hostfile.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_seq.h"

static int orte_rmaps_seq_map(orte_job_t *jdata);

/* define the module */
orte_rmaps_base_module_t orte_rmaps_seq_module = {
    orte_rmaps_seq_map
};


/*
 * Sequentially map the ranks according to the placement in the
 * specified hostfile
 */
static int orte_rmaps_seq_map(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_app_context_t *app;
    orte_std_cntr_t i, j;
    opal_list_item_t *item;
    orte_node_t *node, *nd, *save=NULL;
    orte_vpid_t vpid;
    orte_std_cntr_t num_nodes;
    int rc;
    opal_list_t *default_node_list=NULL;
    opal_list_t *node_list=NULL;
    orte_proc_t *proc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_rmaps_base.rmaps_output,
                         "%s rmaps:seq mapping job %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));

    /* conveniece def */
    map = jdata->map;
      
    /* if there is a default hostfile, go and get its ordered list of nodes */
    if (NULL != orte_default_hostfile) {
        default_node_list = OBJ_NEW(opal_list_t);
        if (ORTE_SUCCESS != (rc = orte_util_get_ordered_host_list(default_node_list, orte_default_hostfile))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
    }
    
    /* start at the beginning... */
    vpid = 0;
    jdata->num_procs = 0;
    if (NULL != default_node_list) {
        save = (orte_node_t*)opal_list_get_first(default_node_list);
    }
    
    /* cycle through the app_contexts, mapping them sequentially */
    for(i=0; i < jdata->num_apps; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        
       /* for each app_context, if a hostfile was specified, then we let it
         * override what we may have obtained from the default hostfile
         */
        if (NULL != app->hostfile) {
            node_list = OBJ_NEW(opal_list_t);
            if (ORTE_SUCCESS != (rc = orte_util_get_ordered_host_list(node_list, app->hostfile))) {
                ORTE_ERROR_LOG(rc);
                goto error;
            }
            nd = (orte_node_t*)opal_list_get_first(node_list);
        } else {
            node_list = default_node_list;
            nd = save;
        }
        
        /* check for nolocal and remove the head node, if required */
        if (map->policy & ORTE_MAPPING_NO_USE_LOCAL) {
            for (item  = opal_list_get_first(node_list);
                 item != opal_list_get_end(node_list);
                 item  = opal_list_get_next(item) ) {
                node = (orte_node_t*)item;
                /* need to check ifislocal because the name in the
                 * hostfile may not have been FQDN, while name returned
                 * by gethostname may have been (or vice versa)
                 */
                if (opal_ifislocal(node->name)) {
                    opal_list_remove_item(node_list, item);
                    OBJ_RELEASE(item);  /* "un-retain" it */
                    break;
                }
            }
        }
            
        if (NULL == node_list || 0 == (num_nodes = (orte_std_cntr_t)opal_list_get_size(node_list))) {
            orte_show_help("help-orte-rmaps-base.txt",
                           "orte-rmaps-base:no-available-resources",
                           true);
            return ORTE_ERR_SILENT;
        }

        /* if num_procs wasn't specified, set it now */
        if (0 == app->num_procs) {
            app->num_procs = num_nodes;
        }
        
        for (i=0; i < app->num_procs; i++) {
            /* find this node on the global array - this is necessary so
             * that our mapping gets saved on that array as the objects
             * returned by the hostfile function are -not- on the array
             */
            node = NULL;
            for (j=0; j < orte_node_pool->size; j++) {
                if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(orte_node_pool, j))) {
                    continue;
                } 
                if (0 == strcmp(nd->name, node->name)) {
                    break;
                }
            }
            if (NULL == node) {
                /* wasn't found - that is an error */
                orte_show_help("help-orte-rmaps-seq.txt",
                               "orte-rmaps-seq:resource-not-found",
                               true, nd->name);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
            
            /* assign proc to this node - do NOT allow claim_slot to remove
             * an oversubscribed node from the list!
             */
            proc = NULL;
            if (ORTE_SUCCESS != (rc = orte_rmaps_base_claim_slot(jdata, node,
                                                                 jdata->map->cpus_per_rank, app->idx,
                                                                 node_list,
                                                                 jdata->map->oversubscribe,
                                                                 false, &proc))) {
                if (ORTE_ERR_NODE_FULLY_USED != rc) {
                    ORTE_ERROR_LOG(rc);
                    goto error;
                }
            }
            /* assign the vpid */
            proc->name.vpid = vpid++;
            /* add to the jdata proc array */
            if (ORTE_SUCCESS != (rc = opal_pointer_array_set_item(jdata->procs, proc->name.vpid, proc))) {
                ORTE_ERROR_LOG(rc);
                goto error;
            }
            /* move to next node */
            nd = (orte_node_t*)opal_list_get_next((opal_list_item_t*)nd);
        }

        /** track the total number of processes we mapped */
        jdata->num_procs += app->num_procs;
        
        /* cleanup the node list if it came from this app_context */
        if (node_list != default_node_list) {
            while (NULL != (item = opal_list_remove_first(node_list))) {
                OBJ_RELEASE(item);
            }
            OBJ_RELEASE(node_list);
        } else {
            save = nd;
        }
    }

    /* compute and save local ranks */
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
    if (NULL != default_node_list) {
        while (NULL != (item = opal_list_remove_first(default_node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(default_node_list);
    }
    if (NULL != node_list) {
        while (NULL != (item = opal_list_remove_first(node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(node_list);
    }
    
    return rc;
}

