/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
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
 * Copyright (c) 2010-2011 IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
 
#include "orte_config.h"

#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "opal/util/argv.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/constants.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_loadleveler.h"


/*
 * Local functions
 */
static int orte_ras_loadleveler_allocate(opal_list_t *nodes);
static int orte_ras_loadleveler_finalize(void);

static int orte_ras_loadleveler_discover(opal_list_t *nodelist);
static int ll_getline(FILE *fp, char *input);

#define LL_FILE_MAX_LINE_LENGTH 512

/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_loadleveler_module = {
    orte_ras_loadleveler_allocate,
    orte_ras_loadleveler_finalize
};


/*
 * Discover available (pre-allocated) nodes.  Allocate the
 * requested number of nodes/process slots to the job.
 */
static int orte_ras_loadleveler_allocate(opal_list_t *nodes)
{
    int ret = ORTE_SUCCESS;

    if (ORTE_SUCCESS != (ret = orte_ras_loadleveler_discover(nodes))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

     /* If we didn't find anything, then this
      * is an unrecoverable error - report it
      */
    if (opal_list_is_empty(nodes)) {
        opal_output(orte_ras_base.ras_output,
                "ras:loadleveler:allocate: No nodes were found in the LOADL_HOSTFILE - %s",
		getenv("LOADL_HOSTFILE"));
        return ORTE_ERR_NOT_FOUND;
    }
    
    return ret;
}

/*
 * There's really nothing to do here
 */
static int orte_ras_loadleveler_finalize(void)
{
    OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output, 
                "ras:loadleveler:finalize: success (nothing to do)"));
    return ORTE_SUCCESS;
}

/**
 *  Discover the available resources.  Obtain directly from LoadLeveler (and
 *  therefore have no need to validate) -- ignore hostfile or any other
 *  user-specified parameters.
 */
static int orte_ras_loadleveler_discover(opal_list_t* nodelist)
{
    orte_node_t *node;
    opal_list_item_t* item;
    FILE *fp;
    char *hostname;
    char *filename;
    char input[LL_FILE_MAX_LINE_LENGTH];

    /* Ignore anything that the user already specified -- we're
       getting nodes only from LoadLeveler. */
    filename = getenv("LOADL_HOSTFILE");
    if(NULL == filename) {
        opal_output(orte_ras_base.ras_output,
                "ras:loadleveler:allocate:discover: LOADL_HOSTFILE not set. "
                "Unable to discover allocated nodes.");
        return ORTE_ERROR;
    }
    fp = fopen(filename, "r");
    if (NULL == fp) {
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        return ORTE_ERR_FILE_OPEN_FAILURE;
    }

    /* Iterate through all the nodes and make an entry for each */
    while (0 != ll_getline(fp, input)) {
        hostname = strdup(input);
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                             "%s ras:loadleveler:allocate:discover: got hostname %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), hostname));

        /* Remember that LoadLeveler may list the same node more than once.
           So we have to check for duplicates. */
        for (item = opal_list_get_first(nodelist);
             opal_list_get_end(nodelist) != item;
             item = opal_list_get_next(item)) {
            node = (orte_node_t*) item;
            if (0 == strcmp(node->name, hostname)) {
                ++node->slots;

                OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                                     "%s ras:loadleveler:allocate:discover: found -- bumped slots to %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), node->slots));
                break;
            }
        }

        /* Did we find it? */
        if (opal_list_get_end(nodelist) == item) {
            /* Nope -- didn't find it, so add a new item to the list */
            OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                                 "%s ras:loadleveler:allocate:discover: not found -- added to list",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

            node = OBJ_NEW(orte_node_t);
            node->name = hostname;
            node->state = ORTE_NODE_STATE_UP;
            node->slots_inuse = 0;
            node->slots_max = 0;
            node->slots = 1;
            opal_list_append(nodelist, &node->super);
        } else {
            /* Yes, so we need to free the hostname that came back */
            free(hostname);
        }
    }
    fclose(fp);

    return ORTE_SUCCESS;
}

static int ll_getline(FILE *fp, char *input)
{
    char *ret;

    ret = fgets(input, LL_FILE_MAX_LINE_LENGTH, fp);
    if (NULL != ret) {
        input[strlen(input)-1] = '\0';  /* remove newline */
        return 1;
    }

    return 0;
}
