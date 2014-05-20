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
 * Copyright (c) 2006-2009 Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file:
 * Resource Allocation for Grid Engine
 */
#include "orte_config.h"
#include "orte/constants.h"

#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/ras/gridengine/ras_gridengine.h"

/*
 * Local functions
 */
static int orte_ras_gridengine_allocate(opal_list_t *nodes);
static int orte_ras_gridengine_finalize(void);
#if 0
static int get_slot_count(char* node_name, int* slot_cnt);
#endif

/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_gridengine_module = {
    orte_ras_gridengine_allocate,
    orte_ras_gridengine_finalize
};

/**
 *  Discover available (pre-allocated) nodes. Allocate the
 *  requested number of nodes/process slots to the job.
 *  
 */
static int orte_ras_gridengine_allocate(opal_list_t *nodelist)
{
    char *pe_hostfile = getenv("PE_HOSTFILE");
    char *job_id = getenv("JOB_ID");
    char buf[1024], *tok, *num, *queue, *arch, *ptr;
    int rc;
    FILE *fp;
    orte_node_t *node;
    opal_list_item_t *item;
    bool found;

    /* show the Grid Engine's JOB_ID */
    if (mca_ras_gridengine_component.show_jobid ||
        mca_ras_gridengine_component.verbose != -1) {
        opal_output(0, "ras:gridengine: JOB_ID: %s", job_id);
    }
   
    /* check the PE_HOSTFILE before continuing on */
    if (!(fp = fopen(pe_hostfile, "r"))) {
        orte_show_help("help-ras-gridengine.txt", "cannot-read-pe-hostfile",
            true, pe_hostfile, strerror(errno));
        rc = ORTE_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* parse the pe_hostfile for hostname, slots, etc, then compare the
     * current node with a list of hosts in the nodelist, if the current
     * node is not found in nodelist, add it in */
    opal_output(mca_ras_gridengine_component.verbose, 
		"ras:gridengine: PE_HOSTFILE: %s", pe_hostfile);

    while (fgets(buf, sizeof(buf), fp)) {
        ptr = strtok_r(buf, " \n", &tok);
        num = strtok_r(NULL, " \n", &tok);
        queue = strtok_r(NULL, " \n", &tok);
        arch = strtok_r(NULL, " \n", &tok);

        /* see if we already have this node */
        found = false;
        for (item = opal_list_get_first(nodelist);
             item != opal_list_get_end(nodelist);
             item = opal_list_get_next(item)) {
            node = (orte_node_t*)item;
            if (0 == strcmp(ptr, node->name)) {
                /* just add the slots */
                node->slots += (int)strtol(num, (char **)NULL, 10);
                found = true;
                opal_output(mca_ras_gridengine_component.verbose,
                            "ras:gridengine: %s: PE_HOSTFILE increased to slots=%d",
                            node->name, node->slots);
                break;
            }
        }
        if (!found) {
            /* create a new node entry */
            node = OBJ_NEW(orte_node_t);
            if (NULL == node) {
                fclose(fp);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            node->name = strdup(ptr);
            node->state = ORTE_NODE_STATE_UP;
            node->slots_inuse = 0;
            node->slots_max = 0;
            node->slots = (int)strtol(num, (char **)NULL, 10);
            opal_output(mca_ras_gridengine_component.verbose,
                        "ras:gridengine: %s: PE_HOSTFILE shows slots=%d",
                        node->name, node->slots);
            opal_list_append(nodelist, &node->super);
        }
    } /* finished reading the $PE_HOSTFILE */

cleanup:
    fclose(fp);

    /* in gridengine, if we didn't find anything, then something
     * is wrong. The user may not have indicated this was a parallel
     * job, or may not have an allocation at all. In any case, this
     * is considered an unrecoverable error and we need to report it
     */
    if (opal_list_is_empty(nodelist)) {
        orte_show_help("help-ras-gridengine.txt", "no-nodes-found", true);
        return ORTE_ERR_NOT_FOUND;
    }

    return ORTE_SUCCESS;
    
}

#if 0
/**
 * This function is not used currently, but may be used eventually.
 * Parse the PE_HOSTFILE to determine the number of process
 * slots/processors available on the node.
 */
static int get_slot_count(char* node_name, int* slot_cnt)
{   
    char buf[1024], *tok, *name, *num, *queue, *arch;
    char *pe_hostfile = getenv("PE_HOSTFILE");
    FILE *fp;
    
    /* check the PE_HOSTFILE before continuing on */
    if (!(fp = fopen(pe_hostfile, "r"))) {
        orte_show_help("help-ras-gridengine.txt", "cannot-read-pe-hostfile",
            true, pe_hostfile, strerror(errno));
        ORTE_ERROR_LOG(ORTE_ERROR);
        return(ORTE_ERROR);
    }
        
    while (fgets(buf, sizeof(buf), fp)) {
        name = strtok_r(buf, " \n", &tok);
        num = strtok_r(NULL, " \n", &tok);
        queue = strtok_r(NULL, " \n", &tok);
        arch = strtok_r(NULL, " \n", &tok);
        
        if(strcmp(node_name,name) == 0) {
            *slot_cnt = (int) strtol(num, (char **)NULL, 10);
            opal_output(mca_ras_gridengine_component.verbose,
                "ras:gridengine: %s: PE_HOSTFILE shows slots=%d",
                node_name, *slot_cnt);
            fclose(fp);
            return ORTE_SUCCESS;
        }
    }

    /* when there is no match */
    fclose(fp);
    return ORTE_ERROR;
}
#endif

/**
 * finalize
 */
static int orte_ras_gridengine_finalize(void)
{
    /* Nothing to do */
    opal_output(mca_ras_gridengine_component.verbose,
        "ras:gridengine:finalize: success (nothing to do)");
    return ORTE_SUCCESS;
}
