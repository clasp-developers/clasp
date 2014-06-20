/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
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


#include <stdio.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/mca/topo/base/base.h"
#include "ompi/mca/topo/unity/topo_unity.h"

/*
 * *******************************************************************
 * ************************ actions structure ************************
 * *******************************************************************
 */
static mca_topo_base_module_1_0_0_t unity =  {
    mca_topo_unity_module_init, /* initalise after being selected */
    mca_topo_unity_module_finalize, /* close a module on a communicator */
    NULL, /* topo_cart_coords */
    NULL, /* topo_cart_create */
    NULL, /* topo_cart_get */
    NULL, /* topo_cartdim_get */
    mca_topo_unity_cart_map,
    NULL, /* topo_cart_rank */
    NULL, /* topo_cart_shift */
    NULL, /* topo_cart_sub */
    NULL, /* topo_graph_create */
    NULL, /* topo_graph_get */
    mca_topo_unity_graph_map,
    NULL, /* topo_graphdims_get */
    NULL, /* topo_graph_neighbors */
    NULL /* topo_graph_neighbors_count */
};
/*
 * *******************************************************************
 * ************************* structure ends **************************
 * *******************************************************************
 */

int mca_topo_unity_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads)
{
    /* Nothing to do */
   
   return OMPI_SUCCESS;
}      

struct mca_topo_base_module_1_0_0_t *
mca_topo_unity_component_comm_query (int *priority)
{
   /* this is the lowest module on the totem pole */
   *priority = 0;

   /* the check as to whether this is an inter communicator 
    * or and intra communicator has to be done before reaching
    * here. this is my solemn opinion. Therefore I am ignoring 
    * the checks here */
   return &unity;
}

int mca_topo_unity_component_comm_unquery (struct ompi_communicator_t *comm)
{    
   /* This function might be needed for some purposes later. for now it
    * does not have anything to do since there are no steps which need 
    * to be undone if this module is not selected */

   return OMPI_SUCCESS;
}

int mca_topo_unity_module_init (struct ompi_communicator_t *comm)
{
    /* Nothing to do -- the setup is done in communicator/comm.c
       (setup the comm->c_topo_comm data) */

    return OMPI_SUCCESS;
}

   
int mca_topo_unity_module_finalize (struct ompi_communicator_t *comm) 
{
    /* Nothing to do -- the teardown is done in
       communicator/comm_init.c (free the comm->c_topo_comm data) */

    return OMPI_SUCCESS;
}
