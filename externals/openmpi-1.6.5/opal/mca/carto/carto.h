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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * The carto framework suplies an information of the the host structure and connection between the 
 * host components i.e memory nodes,CPUs, Ethernet port and inifiniband ports.
 */

#ifndef OPAL_CARTO_H
#define OPAL_CARTO_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/class/opal_graph.h"

/**
 * type for carto graph
 */
typedef opal_graph_t opal_carto_graph_t;

/* A structure of carto node */ 
struct opal_carto_base_node_t {
    opal_graph_vertex_t *vertex; /* the parent of a node is a graph vertex */
    char *node_name;
    char *node_type;     /* the type of the node */
    bool is_cpu;
};

/**
 * A definition of carto node type.
 */
typedef struct opal_carto_base_node_t opal_carto_base_node_t; 


/**
 * A structure of node distance from some other node
 */
struct opal_carto_node_distance_t {
    opal_carto_base_node_t *node; /* The node */
    uint32_t node_distance;       /* and the distance */
};

/**
 * A definition of node distance type.
 */
typedef struct opal_carto_node_distance_t opal_carto_node_distance_t;



/**
 * Module initialization function.  Should return OPAL_SUCCESS.
 */
typedef int (*opal_carto_base_module_init_1_0_0_fn_t)(void);

/**
 * Get the local host graph. you can reduce the graph for only
 * the nodes that interst you using the node type.
 */
typedef int (*opal_carto_base_get_host_graph_fn_t)
        (opal_carto_graph_t **graph, const char *graph_type);

/**
 * Frre a graph
 */
typedef void (*opal_carto_base_free_graph_fn_t)
             (opal_carto_graph_t *graph);

/**
 * Get the distance (weight) from a start node to all other
 * nodes. you can reduce the list to the list to the node types
 * that intersts you.
 */
typedef int (*opal_carto_base_get_nodes_distance_fn_t)
            (opal_carto_graph_t *graph, opal_carto_base_node_t *start, const char *node_type, opal_value_array_t *distance_); 

/**
 * find the distance between two nodes.
 */
typedef uint32_t (*opal_carto_base_spf_fn_t)
            (opal_carto_graph_t *graph,opal_carto_base_node_t *start, opal_carto_base_node_t *end);

/**
 * Find a node in the graph
 */
typedef opal_carto_base_node_t *(*opal_carto_base_find_node_fn_t)
            (opal_carto_graph_t *graph, const char *node_name);


/**
 * Module finalize function.  Invoked by the base on the selected
 * module when the carto framework is being shut down.
 */
typedef int (*opal_carto_base_module_finalize_fn_t)(void);


/**
 * Structure for carto components.
 */
struct opal_carto_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;
};
/**
 * Convenience typedef
 */
typedef struct opal_carto_base_component_2_0_0_t opal_carto_base_component_2_0_0_t;
typedef struct opal_carto_base_component_2_0_0_t opal_carto_base_component_t;


/**
 * Structure for carto modules
 */
struct opal_carto_base_module_1_0_0_t {
    /** Module initialization function */
    opal_carto_base_module_init_1_0_0_fn_t      carto_module_init;
    /** Get host graph */
    opal_carto_base_get_host_graph_fn_t         get_host_graph;
    /** free graph */
    opal_carto_base_free_graph_fn_t             free_graph;
    /** Get the distance from one node to all other nodes */
    opal_carto_base_get_nodes_distance_fn_t     get_nodes_distance;
    /** Find the distance between two nodes */
    opal_carto_base_spf_fn_t                    spf;
    /** Find a node in the graph */
    opal_carto_base_find_node_fn_t              find_node;
    /** Shut down this module */
    opal_carto_base_module_finalize_fn_t        carto_module_finalize;
};
/**
 * Convenience typedef
 */
typedef struct opal_carto_base_module_1_0_0_t opal_carto_base_module_1_0_0_t;
typedef struct opal_carto_base_module_1_0_0_t opal_carto_base_module_t;


/*
 * Macro for use in components that are of type carto
 */
#define OPAL_CARTO_BASE_VERSION_2_0_0 \
    MCA_BASE_VERSION_2_0_0, \
    "carto", 2, 0, 0

#endif /* OPAL_CARTO_H */
