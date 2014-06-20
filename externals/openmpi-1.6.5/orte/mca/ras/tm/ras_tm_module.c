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
#include <unistd.h>
#include <string.h>

#include "orte/util/show_help.h"
#include "opal/util/os_path.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_tm.h"


/*
 * Local functions
 */
static int allocate(opal_list_t *nodes);
static int finalize(void);

static int discover(opal_list_t* nodelist, char *pbs_jobid);
static char *tm_getline(FILE *fp);

#define TM_FILE_MAX_LINE_LENGTH 512

static char *filename;

/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_tm_module = {
    allocate,
    finalize
};


/**
 * Discover available (pre-allocated) nodes and report
 * them back to the caller.
 *  
 */
static int allocate(opal_list_t *nodes)
{
    int ret;
    char *pbs_jobid;

    /* get our PBS jobid from the environment */
    if (NULL == (pbs_jobid = getenv("PBS_JOBID"))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* save that value in the global job ident string for
     * later use in any error reporting
     */
    orte_job_ident = strdup(pbs_jobid);
    
    if (ORTE_SUCCESS != (ret = discover(nodes, pbs_jobid))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* in the TM world, if we didn't find anything, then this
     * is an unrecoverable error - report it
     */
    if (opal_list_is_empty(nodes)) {
        orte_show_help("help-ras-tm.txt", "no-nodes-found", true, filename);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* All done */
    return ORTE_SUCCESS;
}

/*
 * There's really nothing to do here
 */
static int finalize(void)
{
    OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                         "%s ras:tm:finalize: success (nothing to do)",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    return ORTE_SUCCESS;
}


/**
 * Discover the available resources.  Obtain directly from TM (and
 * therefore have no need to validate) -- ignore hostfile or any other
 * user-specified parameters.
 *
 *  - validate any nodes specified via hostfile/commandline
 *  - check for additional nodes that have already been allocated
 */

static int discover(opal_list_t* nodelist, char *pbs_jobid)
{
    int32_t nodeid;
    orte_node_t *node;
    opal_list_item_t* item;
    FILE *fp;
    char *hostname;

    /* Ignore anything that the user already specified -- we're
       getting nodes only from TM. */

    /* TM "nodes" may actually correspond to PBS "VCPUs", which means
       there may be multiple "TM nodes" that correspond to the same
       physical node.  This doesn't really affect what we're doing
       here (we actually ignore the fact that they're duplicates --
       slightly inefficient, but no big deal); just mentioned for
       completeness... */

    /* setup the full path to the PBS file */
    filename = opal_os_path(false, mca_ras_tm_component.nodefile_dir,
                            pbs_jobid, NULL);
    fp = fopen(filename, "r");
    if (NULL == fp) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        free(filename);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }

    /* Iterate through all the nodes and make an entry for each.  TM
       node ID's will never be duplicated, but they may end up
       resolving to the same hostname (i.e., vcpu's on a single
       host). */

    nodeid=0;
    while (NULL != (hostname = tm_getline(fp))) {

        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                             "%s ras:tm:allocate:discover: got hostname %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), hostname));

        /* Remember that TM may list the same node more than once.  So
           we have to check for duplicates. */

        for (item = opal_list_get_first(nodelist);
             opal_list_get_end(nodelist) != item;
             item = opal_list_get_next(item)) {
            node = (orte_node_t*) item;
            if (0 == strcmp(node->name, hostname)) {
                ++node->slots;

                OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                                     "%s ras:tm:allocate:discover: found -- bumped slots to %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), node->slots));
                
                break;
            }
        }

        /* Did we find it? */

        if (opal_list_get_end(nodelist) == item) {

            /* Nope -- didn't find it, so add a new item to the list */
            
            OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                                 "%s ras:tm:allocate:discover: not found -- added to list",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            node = OBJ_NEW(orte_node_t);
            node->name = hostname;
            node->launch_id = nodeid;
            node->slots_inuse = 0;
            node->slots_max = 0;
            node->slots = 1;
            opal_list_append(nodelist, &node->super);
        } else {

            /* Yes, so we need to free the hostname that came back */
            free(hostname);
        }

        /* up the nodeid */
        nodeid++;
    }

    return ORTE_SUCCESS;
}

static char *tm_getline(FILE *fp)
{
    char *ret, *buff;
    char input[TM_FILE_MAX_LINE_LENGTH];
    
    ret = fgets(input, TM_FILE_MAX_LINE_LENGTH, fp);
    if (NULL != ret) {
        input[strlen(input)-1] = '\0';  /* remove newline */
        buff = strdup(input);
        return buff;
    }
    
    return NULL;
}

