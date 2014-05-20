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
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * 24 May 2012
 *
 * Paffinity term          Corresponding value in this module
 * ----------------------- ----------------------------------
 * Physical CPU            hwloc logical core ID
 * Physical processor ID   hwloc logical core ID
 * Physical socket ID      hwloc logical socket ID
 * Physical core ID        hwloc logical core ID
 *
 * Logical CPU             hwloc logical core ID
 * Logical processor ID    hwloc logical core ID
 * Logical socket ID       hwloc logical socket ID
 * Logical core ID         [0, num_cores on socket)
 *
 * This mapping is done because underlying physical/OS IDs may not be
 * unique.  So we always use hwloc logical IDs, except for the case of
 * "paffinity logical core ID", in which the range is [0, num_cores on
 * socket), because that value is relative to the socket, as opposed
 * to hwloc's logical core IDs, which are in the range [0,
 * total_num_cores) and are unique across all cores.
 *
 * Also, note that the paffinity framework has no concept of PUs.  So
 * when it asks for physical processor IDs, it only makes sense to
 * return a unique core ID.  Specifically: in this module, we define
 * that physical processor IDs are hwloc logical core IDs.
 *
 * This really only has relevance for the v1.5/v1.6 branch, as the
 * trunk/v1.7 has been revamped w.r.t. paffinity, and we use hwloc
 * objects for everything.  Meaning: this whole paffinity mess goes
 * away in v1.7.  There is hope.
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
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "paffinity_hwloc.h"
#include "opal/mca/hwloc/hwloc.h"

/*
 * Local functions
 */
static int module_init(void);
static int module_set(opal_paffinity_base_cpu_set_t cpumask);
static int module_get(opal_paffinity_base_cpu_set_t *cpumask);
static int module_map_to_processor_id(int socket, int core, int *processor_id);
static int module_map_to_socket_core(int processor_id, int *socket, int *core);
static int module_get_processor_info(int *num_processors);
static int module_get_socket_info(int *num_sockets);
static int module_get_core_info(int socket, int *num_cores);
static int module_get_physical_processor_id(int logical_processor_id);
static int module_get_physical_socket_id(int logical_socket_id);
static int module_get_physical_core_id(int physical_socket_id, 
                                       int logical_core_id);

/*
 * Local values
 */
static int core_type = HWLOC_OBJ_CORE;


/*
 * Hwloc paffinity module
 */
static const opal_paffinity_base_module_1_1_0_t loc_module = {
    /* Initialization function */
    module_init,

    /* Module function pointers */
    module_set,
    module_get,
    module_map_to_processor_id,
    module_map_to_socket_core,
    module_get_processor_info,
    module_get_socket_info,
    module_get_core_info,
    module_get_physical_processor_id,
    module_get_physical_socket_id,
    module_get_physical_core_id,
    NULL
};

/*
 * Trivial DFS traversal recursion function
 */
static hwloc_obj_t dfs_find_nth_item(hwloc_obj_t root, 
                                     hwloc_obj_type_t type, 
                                     unsigned *current,
                                     unsigned n)
{
    unsigned i;
    hwloc_obj_t ret;

    if (root->type == type) {
        if (*current == n) {
            return root;
        }
        ++(*current);
    }
    for (i = 0; i < root->arity; ++i) {
        ret = dfs_find_nth_item(root->children[i], type, current, n);
        if (NULL != ret) {
            return ret;
        }
    }

    return NULL;
}

/*
 * Trivial DFS traversal recursion function
 */
static int dfs_count_type(hwloc_obj_t root, hwloc_obj_type_t type)
{
    unsigned i;
    int count = 0;
    if (root->type == type) {
        ++count;
    }
    for (i = 0; i < root->arity; ++i) {
        count += dfs_count_type(root->children[i], type);
    }

    return count;
}


int opal_paffinity_hwloc_component_query(mca_base_module_t **module, 
                                         int *priority)
{
    int param;

    param = mca_base_param_find("paffinity", "hwloc", "priority");
    mca_base_param_lookup_int(param, priority);

    *module = (mca_base_module_t *)&loc_module;


    return OPAL_SUCCESS;
}


static int module_init(void)
{
    /* Note that opal_hwloc_topology has not yet been set when this
       function is called.  Nothing to do here. */

    return OPAL_SUCCESS;
}

static void check_for_cores(void)
{
    int num_cores, num_pus;
    static bool already_been_here = false;

    if (already_been_here) {
        return;
    }

    if (NULL == opal_hwloc_topology) {
        return;
    }
    already_been_here = true;

    /* Special workaround for some POWER processors that report PUs
       but not COREs (on these machines, the PUs are architecturally
       "hardware threads", but they don't share resources with other
       PUs, so they're effectively the same as cores, from OMPI's
       perspective).  If hwloc found 0 cores, then change our query
       term from HWLOC_OBJ_PU to HWLOC_OBJ_CORE. */
    num_cores = (int) hwloc_get_nbobjs_by_type(opal_hwloc_topology,
                                               HWLOC_OBJ_CORE);
    num_pus = (int) hwloc_get_nbobjs_by_type(opal_hwloc_topology,
                                             HWLOC_OBJ_PU);
    if (0 == num_cores && num_pus > 0) {
        core_type = HWLOC_OBJ_PU;
    }
}


/*
 * Bind this process to a set of PHYSICAL processor IDs.
 *
 * Per comment in the beginning of this file, the input mask to this
 * function will be a set of hwloc logical core IDs.  We need to
 * convert it to a bitmap of hwloc physical PU IDs.  Specifically, for
 * any hwloc (logical) core ID in the output mask, set all hwloc
 * physical PU IDs in are in that core in the mask that we use to
 * bind.  Then bind to that.
 */
static int module_set(opal_paffinity_base_cpu_set_t mask)
{
    int ret = OPAL_SUCCESS;
    hwloc_bitmap_t set = NULL, tmp = NULL, tmp2 = NULL;
    hwloc_obj_t core;

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_SUPPORTED;
    }

    check_for_cores();

    set = hwloc_bitmap_alloc();
    if (NULL == set) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    hwloc_bitmap_zero(set);

    tmp = hwloc_bitmap_alloc();
    if (NULL == tmp) {
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto out;
    }
    tmp2 = hwloc_bitmap_alloc();
    if (NULL == tmp2) {
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto out;
    }

    /* Iterate through the cores */
    for (core = hwloc_get_obj_by_type(opal_hwloc_topology, core_type, 0);
         core && core->logical_index < OPAL_PAFFINITY_BITMASK_CPU_MAX;
         core = core->next_cousin) {
        if (OPAL_PAFFINITY_CPU_ISSET(core->logical_index, mask)) {
            /* This is a core that's in the input mask.  Yay!  Get the
               actually-available PUs (i.e., (online & allowed)) */
            hwloc_bitmap_and(tmp, core->online_cpuset, core->allowed_cpuset);
            /* OR those PUs with the set of PUs that we already have */
            hwloc_bitmap_or(tmp2, set, tmp);
            /* Now copy that bitmap from the temp output back to the main set */
            hwloc_bitmap_copy(set, tmp2);
        }
    }

    if (0 != hwloc_set_cpubind(opal_hwloc_topology, set, 0)) {
        ret = OPAL_ERR_IN_ERRNO;
    }

 out:
    if (NULL != set) {
        hwloc_bitmap_free(set);
    }
    if (NULL != tmp) {
        hwloc_bitmap_free(tmp);
    }
    if (NULL != tmp2) {
        hwloc_bitmap_free(tmp2);
    }

    return ret;
}


/*
 * Return the set of PHYSICAL processor IDs to which this process is bound.
 *
 * Per the comment at the top of this file, we need to return a bitmap
 * of hwloc logical core IDs.  So we have to get the binding from
 * hwloc (which returns a bitmap of physical PU IDs) and then convert
 * it to a bitmap of hwloc logical core IDs.
 *
 * Also see https://svn.open-mpi.org/trac/ompi/ticket/3085.
 */
static int module_get(opal_paffinity_base_cpu_set_t *mask)
{
    int ret = OPAL_SUCCESS;
    hwloc_bitmap_t set = NULL;
    hwloc_topology_t *t;
    hwloc_obj_t pu, core;

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_SUPPORTED;
    }
    t = &opal_hwloc_topology;

    if (NULL == mask) {
        return OPAL_ERR_BAD_PARAM;
    }

    check_for_cores();

    set = hwloc_bitmap_alloc();
    if (NULL == set) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Get the physical bitmap representing the binding */
    if (0 != hwloc_get_cpubind(*t, set, 0)) {
        ret = OPAL_ERR_IN_ERRNO;
        goto out;
    } 

    /* Now convert that bitmap of physical PU IDs to *logical* core
       IDs */
    OPAL_PAFFINITY_CPU_ZERO(*mask);
    for (pu = hwloc_get_obj_by_type(*t, HWLOC_OBJ_PU, 0);
         pu && pu->logical_index < OPAL_PAFFINITY_BITMASK_CPU_MAX;
         pu = pu->next_cousin) {
        if (hwloc_bitmap_isset(set, pu->os_index)) {
            /* This PU is set. */

            /* See module_init(): if hwloc found cores, then search
               for the parent core.  If hwloc found no cores (and only
               found PUs), then there's no need to find the parent. */

            /* We have cores -- so find the parent */
            if (HWLOC_OBJ_CORE == core_type) {
                core = pu->parent;
                while (NULL != core && HWLOC_OBJ_CORE != core->type) {
                    core = core->parent;
                }

                if (NULL == core) {
                    /* If hwloc didn't report the parent core, then give
                       up */
                    ret = OPAL_ERR_NOT_FOUND;
                    goto out;
                } else {
                    /* Otherwise, save this core's logical index in the
                       output mask */
                    OPAL_PAFFINITY_CPU_SET(core->logical_index, *mask);
                }
            }

            /* We have no cores -- just use the PU logical_index */
            else {
                OPAL_PAFFINITY_CPU_SET(pu->logical_index, *mask);
            }
        }
    }

 out:
    if (NULL != set) {
        hwloc_bitmap_free(set);
    }

    return ret;
}

/*
 * Returns mapping of PHYSICAL socket:core -> PHYSICAL processor id.
 *
 * If the socket/core tuple is valid (which are both hwloc logical
 * values), simply return the core value -- this is a unity operation.
 */
static int module_map_to_processor_id(int socket, int core, int *processor_id)
{
    hwloc_topology_t *t;
    hwloc_obj_t core_obj;

    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_module_map_to_processor_id: IN: socket=%d, core=%d", socket, core);
    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_SUPPORTED;
    }
    t = &opal_hwloc_topology;

    check_for_cores();

    /* Per comment at the beginning of this file, the "physcial core
       IDs" that this module exposes are actually hwloc core logical
       IDs, which are unique in hwloc.  So we can just look up that
       hwloc core ID directly. */
    core_obj = hwloc_get_obj_by_type(*t, core_type, core);
    if (NULL == core_obj) {
        opal_output_verbose(10, opal_paffinity_base_output,
                            "hwloc_module_map_to_processor_id: OUT: Didn't find core %d", core);
        return OPAL_ERR_NOT_FOUND;
    }

    /* Now that we've validated the core, the operation is actually
       just a unity -- in this module, physical processor ID's are
       defined to be the same as the hwloc logical core IDs. */
    *processor_id = core;
    return OPAL_SUCCESS;
}

/*
 * Provides mapping of PHYSICAL processor id -> PHYSICAL socket:core.
 *
 * Remember that in this module, physical processor IDs are defined to
 * be the hwloc core logical IDs (which are unique across all cores).
 * So just take that hwloc logical core ID and find its parent socket
 * logical ID.
 */
static int module_map_to_socket_core(int processor_id, int *socket, int *core)
{
    hwloc_obj_t obj;
    hwloc_topology_t *t;

    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_module_map_to_socket_core: IN: proc_id = %d", processor_id);

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_SUPPORTED;
    }
    t = &opal_hwloc_topology;

    check_for_cores();

    /* Per comment at the beginning of this file, the "physcial core
       IDs" that this module exposes are actually hwloc core logical
       IDs, which are unique in hwloc.  So we can just look up that
       hwloc core ID directly. */
    obj = hwloc_get_obj_by_type(*t, core_type, processor_id);
    if (NULL == obj) {
        opal_output_verbose(10, opal_paffinity_base_output,
                            "hwloc_module_map_to_socket_core: OUT: Didn't find core %d", 
                            processor_id);
        return OPAL_ERR_NOT_FOUND;
    }

    /* Now that we've validated the core, the operation is actually
       just a unity -- in this module, physical processor ID's are
       defined to be the same as the hwloc logical core IDs. */
    *core = processor_id;

    /* Now find the parent socket and get its logical ID, too */
    while (NULL != obj && HWLOC_OBJ_SOCKET != obj->type) {
        obj = obj->parent;
    }
    if (NULL == obj) {
        return OPAL_ERR_NOT_FOUND;
    } else {
        *socket = obj->logical_index;
        return OPAL_SUCCESS;
    }
}

/*
 * Provides number of LOGICAL processors in a host.  Since paffinity
 * does not currently understand hardware threads, we interpret
 * "processors" to mean "cores".
 */
static int module_get_processor_info(int *num_processors)
{
    hwloc_topology_t *t;

    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_get_processor_info: IN");

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_SUPPORTED;
    }
    t = &opal_hwloc_topology;

    check_for_cores();

    *num_processors = (int) hwloc_get_nbobjs_by_type(*t, core_type);

    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_get_processor_info: OUT: returning %d processors (cores)", *num_processors);
    return OPAL_SUCCESS;
}

/*
 * Provides the number of LOGICAL sockets in a host.
 */
static int module_get_socket_info(int *num_sockets)
{
    hwloc_topology_t *t;

    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_module_get_socket_info: IN");

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_SUPPORTED;
    }
    t = &opal_hwloc_topology;

    *num_sockets = (int) hwloc_get_nbobjs_by_type(*t, HWLOC_OBJ_SOCKET);

    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_module_get_socket_info: OUT: returning %d sockets", *num_sockets);
    return OPAL_SUCCESS;
}

/*
 * Provides the number of LOGICAL cores in a PHYSICAL socket. 
 */
static int module_get_core_info(int socket, int *num_cores)
{
    hwloc_obj_t obj;
    hwloc_topology_t *t;

    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_module_get_core_info: IN: socket=%d", socket);

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_SUPPORTED;
    }
    t = &opal_hwloc_topology;

    check_for_cores();

    /* Find the socket */
    obj = hwloc_get_obj_by_type(*t, HWLOC_OBJ_SOCKET, socket);
    if (NULL == obj) {
        return OPAL_ERR_NOT_FOUND;
    }

    /* Ok, we found the right socket.  Browse its descendants looking
       for all cores. */
    *num_cores = dfs_count_type(obj, core_type);
    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_module_get_core_info: OUT: socket=%d, num_cores=%d", socket, *num_cores);
    return OPAL_SUCCESS;
}

/*
 * Provide the PHYSICAL processor id that corresponds to the given
 * LOGICAL processor id.  
 *
 * Remember: paffinity does not understand hardware threads, so
 * "processor" here [usually] means "core" -- except that on some
 * platforms, hwloc won't find any cores; it'll only find PUs (!).  On
 * such platforms, then do the same calculation but with PUs instead
 * of COREs.
 */
static int module_get_physical_processor_id(int logical_processor_id)
{
    hwloc_obj_t obj;
    hwloc_topology_t *t;

    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_module_get_physical_processor_id: INOUT: logical proc %d (unity)", logical_processor_id);

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_SUPPORTED;
    }
    t = &opal_hwloc_topology;

    check_for_cores();

    /* Ensure that logical_processor_id exists */
    obj = hwloc_get_obj_by_type(*t, core_type, logical_processor_id);
    if (NULL == obj) {
        return OPAL_ERR_NOT_FOUND;
    }

    /* Ok, the processor exists.  Return it */
    return logical_processor_id;
}

/*
 * Provide the PHYSICAL socket id that corresponds to the given
 * LOGICAL socket id
 */
static int module_get_physical_socket_id(int logical_socket_id)
{
    hwloc_obj_t obj;
    hwloc_topology_t *t;

    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_module_get_physical_processor_id: INOUT: logical socket %d (unity)", logical_socket_id);

    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_SUPPORTED;
    }
    t = &opal_hwloc_topology;

    /* Ensure that logical_socket_id exists */
    obj = hwloc_get_obj_by_type(*t, HWLOC_OBJ_SOCKET, logical_socket_id);
    if (NULL == obj) {
        return OPAL_ERR_NOT_FOUND;
    }

    /* Ok, the socket exists.  Return it */
    return logical_socket_id;
}

/*
 * Provide the PHYSICAL core id that corresponds to the given LOGICAL
 * core id on the given PHYSICAL socket id.
 *
 * In this case, the caller will be asking about a specific socket,
 * but a logical core *under that specific socket*.  So we need to
 * return the overall hwloc core logical ID for that core.
 */
static int module_get_physical_core_id(int physical_socket_id, 
                                       int logical_core_id)
{
    unsigned count = 0;
    hwloc_obj_t obj;
    hwloc_topology_t *t;

    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_module_get_physical_core_id: IN: phys socket=%d, logical core=%d",
                        physical_socket_id, logical_core_id);
    /* bozo check */
    if (NULL == opal_hwloc_topology) {
        return OPAL_ERR_NOT_SUPPORTED;
    }
    t = &opal_hwloc_topology;

    check_for_cores();

    obj = hwloc_get_obj_by_type(*t, HWLOC_OBJ_SOCKET, physical_socket_id);
    if (NULL == obj) {
        return OPAL_ERR_NOT_FOUND;
    }

    /* Note that we can't look at hwloc's logical_index here -- hwloc
       counts logically across *all* cores.  We only want to find the
       Nth logical core under this particular socket. */
    obj = dfs_find_nth_item(obj, core_type, &count, logical_core_id);
    if (NULL == obj) {
        return OPAL_ERR_NOT_FOUND;
    }
    opal_output_verbose(10, opal_paffinity_base_output,
                        "hwloc_module_get_physical_core_id: OUT: phys socket=%d, logical core=%d: return %d",
                        physical_socket_id, logical_core_id, obj->logical_index);
    return obj->logical_index;
}
