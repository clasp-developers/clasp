/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Most of the description of the data layout is in the
 * coll_sm_module.c file.
 */

#include "ompi_config.h"

#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "orte/util/show_help.h"
#include "coll_sm.h"


/*
 * Public string showing the coll ompi_sm component version number
 */
const char *mca_coll_sm_component_version_string =
    "Open MPI sm collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */
static int sm_register(void);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_coll_sm_component_t mca_coll_sm_component = {

    /* First, fill in the super */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */
        
        {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */
            "sm",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component functions */
            NULL, /* open */
            NULL, /* close */
            NULL, /* query */
            sm_register
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */
        
        mca_coll_sm_init_query,
        mca_coll_sm_comm_query,
    },

    /* sm-component specifc information */

    /* (default) priority */
    /* JMS temporarily lowered until we can get more testing */
    0,

    /* (default) control size (bytes) */
    4096,

    /* (default) number of "in use" flags for each communicator's area
       in the per-communicator shmem segment */
    2,

    /* (default) number of segments for each communicator in the
       per-communicator shmem segment */
    8,

    /* (default) fragment size */
    8192,

    /* (default) degree of tree for tree-based operations (must be <=
       control unit size) */
    4,

    /* (default) number of processes in coll_sm_shared_mem_size
       information variable */
    4,

    /* default values for non-MCA parameters */
    /* Not specifying values here gives us all 0's */
};


/*
 * Register MCA params
 */
static int sm_register(void)
{
    size_t size;
    mca_base_component_t *c = &mca_coll_sm_component.super.collm_version;
    mca_coll_sm_component_t *cs = &mca_coll_sm_component;

    /* If we want to be selected (i.e., all procs on one node), then
       we should have a high priority */

    mca_base_param_reg_int(c, "priority", 
                           "Priority of the sm coll component",
                           false, false, 
                           cs->sm_priority,
                           &cs->sm_priority);

    mca_base_param_reg_int(c, "control_size",
                           "Length of the control data -- should usually be either the length of a cache line on most SMPs, or the size of a page on machines that support direct memory affinity page placement (in bytes)",
                           false, false,
                           cs->sm_control_size,
                           &cs->sm_control_size);

    mca_base_param_reg_int(c, "fragment_size",
                           "Fragment size (in bytes) used for passing data through shared memory (will be rounded up to the nearest control_size size)",
                           false, false,
                           cs->sm_fragment_size,
                           &cs->sm_fragment_size);
    if (0 != (cs->sm_fragment_size % cs->sm_control_size)) {
        cs->sm_fragment_size += cs->sm_control_size - 
            (cs->sm_fragment_size % cs->sm_control_size);
    }

    mca_base_param_reg_int(c, "comm_in_use_flags",
                           "Number of \"in use\" flags, used to mark a message passing area segment as currently being used or not (must be >= 2 and <= comm_num_segments)",
                           false, false,
                           cs->sm_comm_num_in_use_flags,
                           &cs->sm_comm_num_in_use_flags);
    if (cs->sm_comm_num_in_use_flags < 2) {
        cs->sm_comm_num_in_use_flags = 2;
    }

    mca_base_param_reg_int(c, "comm_num_segments",
                           "Number of segments in each communicator's shared memory message passing area (must be >= 2, and must be a multiple of comm_in_use_flags)",
                           false, false,
                           cs->sm_comm_num_segments,
                           &cs->sm_comm_num_segments);
    if (cs->sm_comm_num_segments < cs->sm_comm_num_in_use_flags) {
        cs->sm_comm_num_segments = cs->sm_comm_num_in_use_flags;
    }
    if (0 != (cs->sm_comm_num_segments % cs->sm_comm_num_in_use_flags)) {
        cs->sm_comm_num_segments += cs->sm_comm_num_in_use_flags - 
            (cs->sm_comm_num_segments % cs->sm_comm_num_in_use_flags);
    }
    cs->sm_segs_per_inuse_flag = 
        cs->sm_comm_num_segments / cs->sm_comm_num_in_use_flags;

    mca_base_param_reg_int(c, "tree_degree",
                           "Degree of the tree for tree-based operations (must be => 1 and <= min(control_size, 255))",
                           false, false,
                           cs->sm_tree_degree,
                           &cs->sm_tree_degree);
    if (cs->sm_tree_degree > cs->sm_control_size) {
        orte_show_help("help-mpi-coll-sm.txt", 
                       "tree-degree-larger-than-control", true,
                       cs->sm_tree_degree, cs->sm_control_size);
        cs->sm_tree_degree = cs->sm_control_size;
    }
    if (cs->sm_tree_degree > 255) {
        orte_show_help("help-mpi-coll-sm.txt", 
                       "tree-degree-larger-than-255", true,
                       cs->sm_tree_degree);
        cs->sm_tree_degree = 255;
    }

    /* INFO: Calculate how much space we need in the per-communicator
       shmem data segment.  This formula taken directly from
       coll_sm_module.c. */
    mca_base_param_reg_int(c, "info_num_procs",
                           "Number of processes to use for the calculation of the shared_mem_size MCA information parameter (must be => 2)",
                           false, false,
                           cs->sm_info_comm_size,
                           &cs->sm_info_comm_size);

    size = 4 * cs->sm_control_size +
        (cs->sm_comm_num_in_use_flags * cs->sm_control_size) +
        (cs->sm_comm_num_segments * (cs->sm_info_comm_size * cs->sm_control_size * 2)) +
        (cs->sm_comm_num_segments * (cs->sm_info_comm_size * cs->sm_fragment_size));
    mca_base_param_reg_int(c, "shared_mem_used_data",
                           "Amount of shared memory used, per communicator, in the shared memory data area for info_num_procs processes (in bytes)",
                           false, true,
                           (int)size, NULL);

    return OMPI_SUCCESS;
}
