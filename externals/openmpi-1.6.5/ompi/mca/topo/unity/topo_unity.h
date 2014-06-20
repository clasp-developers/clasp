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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_TOPO_UNTIY_H
#define MCA_TOPO_UNTIY_H

#include "ompi_config.h"
#include "ompi/mca/topo/topo.h"

/*
 * ******************************************************************
 * ******** functions which provide MCA interface comppliance *******
 * ******************************************************************
 * These functions are:
 *       - mca_topo_unity_module_open
 *       - mca_topo_unity_module_close
 *       - mca_topo_unity_module_query
 *       - mca_topo_unity_module_finalize
 * These functions are always found on the mca_topo_unity_module
 * structure. They are the "meta" functions to ensure smooth op.
 * ******************************************************************
 */
BEGIN_C_DECLS

int mca_topo_unity_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads);
struct mca_topo_base_module_1_0_0_t *
    mca_topo_unity_component_comm_query (int *priority);
int mca_topo_unity_component_comm_unquery (struct ompi_communicator_t *comm);

int mca_topo_unity_module_init (struct ompi_communicator_t *comm);
int mca_topo_unity_module_finalize (struct ompi_communicator_t *comm);

OMPI_MODULE_DECLSPEC extern mca_topo_base_component_2_0_0_t mca_topo_unity_component;

/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 * This module defines just 2 functions:
 *      - graph_map
 *      - cart_map
 * rest of the functions are filled in from the "base" module. Authors
 * of other such topology modules are required to define only these 2
 * functions. They are ofcourse free to implement all of them too :-)
 * ******************************************************************
 */ 
int mca_topo_unity_cart_map (struct ompi_communicator_t *comm,
                             int ndims,
                             int *dims,
                             int *periods,
                             int *newrank);

int mca_topo_unity_graph_map (struct ompi_communicator_t *comm,
                              int nnodes,
                              int *index,
                              int *edges,
                              int *newrank);
/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */ 
                                     
END_C_DECLS

#endif /* MCA_TOPO_UNITY_H */
