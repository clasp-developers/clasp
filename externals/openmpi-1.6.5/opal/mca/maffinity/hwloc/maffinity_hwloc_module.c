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
 * Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

/* This component will only be compiled on Hwloc, where we are
   guaranteed to have <unistd.h> and friends */
#include <stdio.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"
#include "opal/mca/hwloc/base/base.h"
#include "maffinity_hwloc.h"
#include "opal/mca/hwloc/hwloc.h"

/*
 * Local functions
 */
static int hwloc_module_init(void);
static int hwloc_module_set(opal_maffinity_base_segment_t *segments,
                              size_t num_segments);
static int hwloc_module_node_name_to_id(char *, int *);
static int hwloc_module_bind(opal_maffinity_base_segment_t *, size_t, int);

/*
 * Hwloc maffinity module
 */
static const opal_maffinity_base_module_1_0_0_t local_module = {
    /* Initialization function */
    hwloc_module_init,

    /* Module function pointers */
    hwloc_module_set,
    hwloc_module_node_name_to_id,
    hwloc_module_bind
};

int opal_maffinity_hwloc_component_query(mca_base_module_t **module, 
                                         int *priority)
{
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERROR;
    }

    *priority = mca_maffinity_hwloc_component.priority;
    *module = (mca_base_module_t *) &local_module;

    return OPAL_SUCCESS;
}


static int hwloc_module_init(void)
{
    /* Nothing to do! */

    return OPAL_SUCCESS;
}


static int hwloc_module_set(opal_maffinity_base_segment_t *segments,
                              size_t num_segments)
{
    int rc = OPAL_SUCCESS;
    char *msg = NULL;
    size_t i;
    hwloc_cpuset_t cpuset = NULL;

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        msg = "hwloc_set_area_membind() failure - topology not available";
        return opal_hwloc_base_report_bind_failure(__FILE__, __LINE__,
                                                   msg, rc);
    }

    /* This module won't be used unless the process is already
       processor-bound.  So find out where we're processor bound, and
       bind our memory there, too. */
    cpuset = hwloc_bitmap_alloc();
    if (NULL == cpuset) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        msg = "hwloc_bitmap_alloc() failure";
        goto out;
    }
    hwloc_get_cpubind(opal_hwloc_topology, cpuset, 0);
    for (i = 0; i < num_segments; ++i) {
        if (0 != hwloc_set_area_membind(opal_hwloc_topology, 
                                        segments[i].mbs_start_addr,
                                        segments[i].mbs_len, cpuset,
                                        HWLOC_MEMBIND_BIND, 
                                        HWLOC_MEMBIND_STRICT)) {
            rc = OPAL_ERROR;
            msg = "hwloc_set_area_membind() failure";
            goto out;
        }
    }

 out:
    if (NULL != cpuset) {
        hwloc_bitmap_free(cpuset);
    }
    if (OPAL_SUCCESS != rc) {
        return opal_hwloc_base_report_bind_failure(__FILE__, __LINE__,
                                                   msg, rc);
    }
    return OPAL_SUCCESS;
}

static int hwloc_module_node_name_to_id(char *node_name, int *id)
{
    /* GLB: fix me */
    *id = atoi(node_name + 3);

    return OPAL_SUCCESS;
}

static int hwloc_module_bind(opal_maffinity_base_segment_t *segs,
                             size_t count, int node_id)
{
    size_t i;
    int rc = OPAL_SUCCESS;
    char *msg = NULL;
    hwloc_cpuset_t cpuset = NULL;

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        msg = "hwloc_set_area_membind() failure - topology not available";
        return opal_hwloc_base_report_bind_failure(__FILE__, __LINE__,
                                                   msg, rc);
    }

    cpuset = hwloc_bitmap_alloc();
    if (NULL == cpuset) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
        msg = "hwloc_bitmap_alloc() failure";
        goto out;
    }
    hwloc_bitmap_set(cpuset, node_id);
    for(i = 0; i < count; i++) {
        if (0 != hwloc_set_area_membind(opal_hwloc_topology, 
                                        segs[i].mbs_start_addr,
                                        segs[i].mbs_len, cpuset,
                                        HWLOC_MEMBIND_BIND, 
                                        HWLOC_MEMBIND_STRICT)) {
            rc = OPAL_ERROR;
            msg = "hwloc_set_area_membind() failure";
            goto out;
        }
    }

 out:
    if (NULL != cpuset) {
        hwloc_bitmap_free(cpuset);
    }
    if (OPAL_SUCCESS != rc) {
        return opal_hwloc_base_report_bind_failure(__FILE__, __LINE__,
                                                   msg, rc);
    }
    return OPAL_SUCCESS;
}
