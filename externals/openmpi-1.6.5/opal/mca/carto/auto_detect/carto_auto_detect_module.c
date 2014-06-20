/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

/* This component will only be compiled on File, where we are
   guaranteed to have <unistd.h> and friends */
#include <stdio.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif  /* HAVE_ERRNO_H */

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"
#include "opal/mca/carto/base/carto_base_graph.h"
#include "carto_auto_detect.h"


static int opal_carto_auto_detect_init(void);
static int opal_carto_auto_detect_finalize(void);


/*
 * Auto detect carto module
 */
static const opal_carto_base_module_1_0_0_t loc_module = {
    opal_carto_auto_detect_init,
    opal_carto_base_graph_get_host_graph_fn,
    opal_carto_base_free_graph_fn,
    opal_carto_base_get_nodes_distance_fn,
    opal_carto_base_graph_spf_fn,
    opal_carto_base_graph_find_node_fn,
    opal_carto_auto_detect_finalize,
};

int opal_carto_auto_detect_component_query(mca_base_module_t **module, int *priority)
{
    int param;

    param = mca_base_param_find("carto", "auto_detect", "priority");
    mca_base_param_lookup_int(param, priority);

    *module = (mca_base_module_t *)&loc_module;

    return OPAL_SUCCESS;
}


/**
 * Init the auto detect module. this function build an empty
 * carto grap and does nothing more. to comleate this module,
 * this function should read the system files and fill the
 * carto graph
 * 
 * @return int success or error
 */
static int opal_carto_auto_detect_init(void)
{
    /* create an empty graph */
    if (NULL == opal_carto_base_common_host_graph) {
        opal_carto_base_graph_create_fn(&opal_carto_base_common_host_graph);
    }
    return OPAL_SUCCESS;
}

/**
 * Cleans the auto detect module.
 * 
 * @return int success or error
 */
static int opal_carto_auto_detect_finalize(void)
{
    /* free the host cartography graph. */
    if (NULL != opal_carto_base_common_host_graph) {
        opal_carto_base_free_graph_fn(opal_carto_base_common_host_graph);
    }
    return OPAL_SUCCESS;
}



