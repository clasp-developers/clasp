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
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_TOPO_H
#define MCA_TOPO_H

#include "ompi_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "ompi/communicator/communicator.h"

/*
 * ******************************************************************
 * ********** Use in components that are of type topo v2.0.0 ********
 * ******************************************************************
 */
#define MCA_TOPO_BASE_VERSION_2_0_0 \
  MCA_BASE_VERSION_2_0_0, \
  "topo", 2, 0, 0
/*
 * ******************************************************************
 * **************************** Macro ends **************************
 * ******************************************************************
 */

/*
 * These are the component function prototypes. These function pointers
 * go into the component structure. These functions (query() and finalize()
 * are called during topo_base_select(). Each component is query() ied
 * and subsequently, all the unselected components are finalize() 'ed 
 * so that any *stuff* they did during query() can be undone. By
 * similar logic, finalize() is also called on the component which
 * was selected when the communicator is being destroyed.
 *
 * So, to sum it up, every component carries 4 functions:
 * 1. open() - called during MPI_INIT
 * 2. close() - called during MPI_FINALIZE
 * 3. query() - called to select a particular component
 * 4. finalize() - called when actions taken during query have 
 *                 to be undone
 */

/*
 * **************** component struct *******************************
 * *********** These functions go in the component struct **********
 * **************** component struct *******************************
 */ 
typedef int (*mca_topo_base_component_init_query_1_0_0_fn_t)
  (bool enable_progress_threads,
   bool enable_mpi_threads);

typedef struct mca_topo_base_module_1_0_0_t*
  (*mca_topo_base_component_comm_query_1_0_0_fn_t) (int *priority);
                                            
                            
typedef int (*mca_topo_base_component_comm_unquery_1_0_0_fn_t) 
  (struct ompi_communicator_t *comm);

/*
 * ****************** component struct ******************************
 * Structure for topo v2.0.0 components.This is chained to MCA v2.0.0
 * ****************** component struct ******************************
 */  
struct mca_topo_base_component_2_0_0_t {
    mca_base_component_t topom_version;
    mca_base_component_data_t topom_data;

    mca_topo_base_component_init_query_1_0_0_fn_t topom_init_query;
    mca_topo_base_component_comm_query_1_0_0_fn_t topom_comm_query;
    mca_topo_base_component_comm_unquery_1_0_0_fn_t topom_comm_unquery;
};
typedef struct mca_topo_base_component_2_0_0_t mca_topo_base_component_2_0_0_t;       
typedef mca_topo_base_component_2_0_0_t mca_topo_base_component_t;

/*
 * ******************************************************************
 * *********************** component struct ends here ***************
 * ******************************************************************
 */ 
/*
 * ******************************************************************
 * *********************** information structure  *******************
 * Note for component authors:
 * If you find that this is not the most convinient form of
 * representing your topology, then please feel free to define your
 * own structure in which this struct is the first element. That way,
 * type casting can be used to communicate between 2 different topo
 * components. Note that this representation must be filled up no
 * matter what the actual topo structure might be.
 * ******************************************************************
 */ 

struct mca_topo_base_comm_1_0_0_t {

    /* The first section represents data which is passed on to the 
     * structure by the user when creating the topology. This info
     * is cached on so that if required another component can create the
     * topology again when comm_dup fails to pick the same component */

     int mtc_ndims_or_nnodes; /**< Number of cart dimensions or graph nodes */
     int *mtc_dims_or_index; /**< Cart dimensions or graph indices */
     int *mtc_periods_or_edges; /**< whether this was a periodic cart or graph */
     bool mtc_reorder; /**< Whether the re-ordering is allowed */

    /* The second section is used by the unity component since it does not 
     * hang its own structure off the communicator. Any component which wishes
     * to use the base/topo_base* functions to fill in their unimplemented
     * functions should and must fill this portion up */

     int *mtc_coords; /**< Cart coordinates */
};
typedef struct mca_topo_base_comm_1_0_0_t mca_topo_base_comm_1_0_0_t;
typedef mca_topo_base_comm_1_0_0_t mca_topo_base_comm_t;

/*
 * ******************************************************************
 * *********************** information structure   ******************
 * ******************************************************************
 */ 

struct ompi_proc_t;

/*
 * ***********************************************************************
 * ************************  Interface function definitions **************
 * These are the typedefs for the function pointers to various topology
 * backend functions which will be used by the various topology components
 * ***********************************************************************
 */
typedef int (*mca_topo_base_module_init_1_0_0_fn_t)
  (struct ompi_communicator_t *comm);
            
typedef int (*mca_topo_base_module_finalize_1_0_0_fn_t)
  (struct ompi_communicator_t *comm);

typedef int (*mca_topo_base_module_cart_coords_fn_t) 
                    (struct ompi_communicator_t *comm, 
                     int rank, 
                     int maxdims, 
                     int *coords);

typedef int (*mca_topo_base_module_cart_create_fn_t)
                    (mca_topo_base_comm_t *topo_data,
                     int *proc_count, 
                     struct ompi_proc_t **proc_pointers,
                     int *new_rank,
                     int ndims, 
                     int *dims, 
                     int *periods, 
                     bool redorder);

typedef int (*mca_topo_base_module_cart_get_fn_t)
                    (struct ompi_communicator_t *comm, 
                     int maxdims, 
                     int *dims,
                     int *periods, 
                     int *coords);

typedef int (*mca_topo_base_module_cartdim_get_fn_t)
                    (struct ompi_communicator_t *comm,
                     int *ndims);

typedef int (*mca_topo_base_module_cart_map_fn_t)
                    (struct ompi_communicator_t *comm, 
                     int ndims, 
                     int *dims,
                     int *periods, 
                     int *newrank);

typedef int (*mca_topo_base_module_cart_rank_fn_t)
                    (struct ompi_communicator_t *comm, 
                     int *coords, 
                     int *rank);

typedef int (*mca_topo_base_module_cart_shift_fn_t)
                    (struct ompi_communicator_t *comm, 
                     int direction, 
                     int disp,
                     int *rank_source, 
                     int *rank_dest);

typedef int (*mca_topo_base_module_cart_sub_fn_t)
                    (struct ompi_communicator_t *comm, 
                     int *remain_dims, 
                     struct ompi_communicator_t ** new_comm);

typedef int (*mca_topo_base_module_graph_create_fn_t)
                    (mca_topo_base_comm_t *topo_data, 
                     int *proc_count,
                     struct ompi_proc_t **proc_pointers,
                     int *new_rank,
                     int nnodes, 
                     int *index, 
                     int *edges, 
                     bool reorder);

typedef int (*mca_topo_base_module_graph_get_fn_t)
                    (struct ompi_communicator_t *comm, 
                     int maxindex, 
                     int maxedges, 
                     int *index, 
                     int *edges);

typedef int (*mca_topo_base_module_graph_map_fn_t)
                    (struct ompi_communicator_t *comm, 
                     int nnodes, 
                     int *index, 
                     int *edges, 
                     int *newrank);

typedef int (*mca_topo_base_module_graphdims_get_fn_t)
                    (struct ompi_communicator_t *comm, 
                     int *nnodes, 
                     int *nnedges);

typedef int (*mca_topo_base_module_graph_neighbors_fn_t)
                    (struct ompi_communicator_t *comm, 
                     int rank, 
                     int maxneighbors, 
                     int *neighbors);

typedef int (*mca_topo_base_module_graph_neighbors_count_fn_t)
                    (struct ompi_communicator_t *comm, 
                     int rank, 
                     int *nneighbors);

/*
 * ***********************************************************************
 * ******************** Interface function definitions end  **************
 * ***********************************************************************
 */ 

/*
 * ***********************************************************************
 * ***************************  module structure *************************
 * ***********************************************************************
 */ 
struct mca_topo_base_module_1_0_0_t {
    /* 
     * Per-communicator initialization function. This is called only
     * on the module which is selected. The finalize corresponding to
     * this function is present on the component struct above 
     */
    mca_topo_base_module_init_1_0_0_fn_t topo_module_init;
    mca_topo_base_module_finalize_1_0_0_fn_t topo_module_finalize;

    /* Graph related functions */
    mca_topo_base_module_cart_coords_fn_t topo_cart_coords;
    mca_topo_base_module_cart_create_fn_t topo_cart_create;
    mca_topo_base_module_cart_get_fn_t topo_cart_get;
    mca_topo_base_module_cartdim_get_fn_t topo_cartdim_get;
    mca_topo_base_module_cart_map_fn_t topo_cart_map;
    mca_topo_base_module_cart_rank_fn_t topo_cart_rank;
    mca_topo_base_module_cart_shift_fn_t topo_cart_shift;
    mca_topo_base_module_cart_sub_fn_t topo_cart_sub;
    mca_topo_base_module_graph_create_fn_t topo_graph_create;
    mca_topo_base_module_graph_get_fn_t topo_graph_get;
    mca_topo_base_module_graph_map_fn_t topo_graph_map;
    mca_topo_base_module_graphdims_get_fn_t topo_graphdims_get;
    mca_topo_base_module_graph_neighbors_fn_t topo_graph_neighbors;
    mca_topo_base_module_graph_neighbors_count_fn_t topo_graph_neighbors_count;
};
typedef struct mca_topo_base_module_1_0_0_t mca_topo_base_module_1_0_0_t;
typedef mca_topo_base_module_1_0_0_t mca_topo_base_module_t;
/*
 * ***********************************************************************
 * *******************  component actions structure ends *****************
 * ***********************************************************************
 */ 

    
#endif /* MCA_TOPO_H */
