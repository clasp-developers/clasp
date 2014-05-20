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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * maffinity (memory affinity) framework component interface
 * definitions.
 *
 * Intent
 *
 * Simple component to set memory affinity for pages.  Note that this
 * framework assumes that processor affinity is being used (it doesn't
 * make much sense to use memory affinity unless processes are bound
 * to specific processors, otherwise the processes may move around and
 * the memory may end up being remote).
 *
 * maffinity components are typically used with shared memory
 * operations, but can be used elsewhere as well.  The idea is to get
 * memory physically located with the process that is going to use it.
 * For memory allocated to a processor-bound process, functions such
 * as malloc() do this naturally.  However, when working with memory
 * shared by multiple processes on a NUMA machine, it can be extremely
 * advantageous to ensure that pages containing the data structures
 * for a given process are physically local to the processor where
 * that process is bound.
 *
 * One process will allocate a large shared memory block and all will
 * need to participate to make pages local to specific processors.
 *
 * There is one main module function
 * (opal_maffinity_base_module_set_fn_t) that takes an array of
 * segment descriptions within the block.  Each process will get a
 * different set of segment descriptions (i.e., the segments belonging
 * to that process).  Components then do whatever is necessary to make
 * pages local to their respective processes (i.e., the processors
 * where the processes are running).
 */

#ifndef OPAL_MAFFINITY_H
#define OPAL_MAFFINITY_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/maffinity/maffinity_types.h"

/**
 * Module initialization function.  Should return OPAL_SUCCESS.
 *
 * This function should act on the value of the MCA parameter
 * maffinity_base_alloc_policy (stored in the global
 * opal_maffinity_base_map, declared in
 * opal/mca/maffinity/base/base.h).
 */
typedef int (*opal_maffinity_base_module_init_1_0_0_fn_t)(void);


/**
 * Module function to set memory affinity.  Take an array of
 * maffinity_base_segment_t instances to describe which memory should
 * physically reside with which process.
 *
 * This function is intended to be invoked by each process immediately
 * after they mmap / attach the shared memory.  In some cases, it will
 * be a no-op for some processes (i.e., machines where a single
 * function call in the creating process sets the memory affinity for
 * the entire region), but in other cases all processes will need to
 * participate (e.g., the first_use component, each each process will
 * "touch" the pages that are supposed to be local to them).
 */
typedef int (*opal_maffinity_base_module_set_fn_t)
    (opal_maffinity_base_segment_t *segments, size_t num_segments);

/**
 * translate memory node name (such as "mem0") to memory node id
 */
typedef int (*opal_maffinity_base_module_node_name_to_id_fn_t)
    (char *node_name, int *node_id);

/**
 * bind memory to node
 */
typedef int (*opal_maffinity_base_module_bind_fn_t)
    (opal_maffinity_base_segment_t *segments, size_t num_segments, int node_id);

/**
 * Structure for maffinity components.
 */
struct opal_maffinity_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;
};
/**
 * Convenience typedef
 */
typedef struct opal_maffinity_base_component_2_0_0_t opal_maffinity_base_component_2_0_0_t;


/**
 * Structure for maffinity modules
 */
struct opal_maffinity_base_module_1_0_0_t {
    /** Module initialization function */
    opal_maffinity_base_module_init_1_0_0_fn_t maff_module_init;

    /** Set memory affinity */
    opal_maffinity_base_module_set_fn_t maff_module_set;
    opal_maffinity_base_module_node_name_to_id_fn_t maff_module_name_to_id;
    opal_maffinity_base_module_bind_fn_t maff_module_bind;
};
/**
 * Convenience typedef
 */
typedef struct opal_maffinity_base_module_1_0_0_t opal_maffinity_base_module_1_0_0_t;


/*
 * Macro for use in components that are of type maffinity
 */
#define OPAL_MAFFINITY_BASE_VERSION_2_0_0 \
    MCA_BASE_VERSION_2_0_0, \
    "maffinity", 2, 0, 0

#endif /* OPAL_MAFFINITY_H */
