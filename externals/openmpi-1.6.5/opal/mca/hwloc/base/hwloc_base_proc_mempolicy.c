/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"

#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/hwloc/base/base.h"


/*
 * Don't use show_help() here (or print any error message at all).
 * Let the upper layer output a relevant message, because doing so may
 * be complicated (e.g., this might be called from the ORTE ODLS,
 * which has to do some extra steps to get error messages to be
 * displayed).
 */
int opal_hwloc_base_set_process_membind_policy(void)
{
    int rc = 0, flags;
    hwloc_membind_policy_t policy;
    hwloc_cpuset_t cpuset;

    /* Make sure opal_hwloc_topology has been set by the time we've
       been called */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_BAD_PARAM;
    }
    
    /* Set the default memory allocation policy according to MCA
       param */
    switch (opal_hwloc_base_map) {
    case OPAL_HWLOC_BASE_MAP_LOCAL_ONLY:
        policy = HWLOC_MEMBIND_BIND;
        flags = HWLOC_MEMBIND_STRICT;
        break;
        
    case OPAL_HWLOC_BASE_MAP_NONE:
    default:
        policy = HWLOC_MEMBIND_DEFAULT;
        flags = 0;
        break;
    }
    
    cpuset = hwloc_bitmap_alloc();
    if (NULL == cpuset) {
        rc = OPAL_ERR_OUT_OF_RESOURCE;
    } else {
        hwloc_get_cpubind(opal_hwloc_topology, cpuset, 0);
        rc = hwloc_set_membind(opal_hwloc_topology, 
                               cpuset, HWLOC_MEMBIND_BIND, flags);
        hwloc_bitmap_free(cpuset);
    }
    
    return (0 == rc) ? OPAL_SUCCESS : OPAL_ERROR;
}
