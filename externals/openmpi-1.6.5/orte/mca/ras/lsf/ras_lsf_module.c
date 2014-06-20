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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/constants.h"

#include <errno.h>
#include <unistd.h>
#include <string.h>

#define SR1_PJOBS
#include <lsf/lsbatch.h>

#include "opal/util/argv.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_lsf.h"


/*
 * Local functions
 */
static int allocate(opal_list_t *nodes);
static int finalize(void);


/*
 * Global variable
 */
orte_ras_base_module_t orte_ras_lsf_module = {
    allocate,
    finalize
};


static int allocate(opal_list_t *nodes)
{
    char **nodelist;
    orte_node_t *node;
    int i, num_nodes;

    /* get the list of allocated nodes */
    if ((num_nodes = lsb_getalloc(&nodelist)) < 0) {
        orte_show_help("help-ras-lsf.txt", "nodelist-failed", true);
        return ORTE_ERR_NOT_AVAILABLE;
    }
    
    node = NULL;
    
    /* step through the list */
    for (i = 0; i < num_nodes; i++) {
        /* is this a repeat of the current node? */
        if (NULL != node && 0 == strcmp(nodelist[i], node->name)) {
            /* it is a repeat - just bump the slot count */
            ++node->slots;
            continue;
        }
        
        /* not a repeat - create a node entry for it */
        node = OBJ_NEW(orte_node_t);
        node->name = strdup(nodelist[i]);
        node->slots_inuse = 0;
        node->slots_max = 0;
        node->slots = 1;
        opal_list_append(nodes, &node->super);
    }
        
    /* release the nodelist from lsf */
    opal_argv_free(nodelist);

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    return ORTE_SUCCESS;
}
