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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics. Since linkers generally pull in symbols by object fules,
 * keeping these symbols as the only symbols in this file prevents
 * utility programs such as "ompi_info" from having to import entire
 * modules just to query their version and parameters
 */

#include "ompi_config.h"
#include "ompi/mca/topo/unity/topo_unity.h"

/*
 * Public string showing the topo unity module version number
 */

const char *mca_topo_unity_component_version_string = 
    "Open MPI unity topology MCA component version" OMPI_VERSION;

/*
 * *******************************************************************
 * ****** this is the structure that defines the component **************
 * *******************************************************************
 * this structure contains the component version information along with
 * some meta data and function pointers which allow a component to 
 * interact with the MCA framework. component open() and close() are 
 * called during MPI_INIT and MPI_FINALIZE respectively and query()
 * and finalize() are called during creation/destruction of a comm
 * *******************************************************************
 */
mca_topo_base_component_2_0_0_t mca_topo_unity_component = 
{
    {
        MCA_TOPO_BASE_VERSION_2_0_0,

        "unity",                      /* component name */
        OMPI_MAJOR_VERSION, /* major version */
        OMPI_MINOR_VERSION, /* minor version */
        OMPI_RELEASE_VERSION, /* release version */
        NULL,   /* fp to open the component */
        NULL    /* fp to close the component */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    mca_topo_unity_component_init_query,      /* get thread level */
    mca_topo_unity_component_comm_query,      /* get priority and actions */
    mca_topo_unity_component_comm_unquery     /* undo what was done by previous function */
};
