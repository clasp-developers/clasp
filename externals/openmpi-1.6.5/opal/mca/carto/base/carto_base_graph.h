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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#ifndef OPAL_CARTO_BASE_GRAPH_H
#define OPAL_CARTO_BASE_GRAPH_H

/* THIS FILE CONTAINS THE ACTUAL BASE FUNCTION IMPLEMENTATIONS FOR
 * THE CARTO FRAMEWORK - THESE FUNCTIONS ARE TO BE CALLED STRICTLY
 * FROM THE VARIOUS COMPONENTS, AS PASSED THROUGH BY THE PUBLIC
 * BASE FUNCTIONS
 *
 * DO -NOT- CALL THESE FUNCTIONS DIRECTLY!
 */

#include "opal_config.h"

#include "opal/mca/carto/carto.h"

BEGIN_C_DECLS

OPAL_DECLSPEC extern opal_carto_graph_t *opal_carto_base_common_host_graph;

/**
 * Create new carto graph.
 * 
 * @param graph an empty graph pointer
 */
OPAL_DECLSPEC void opal_carto_base_graph_create_fn(opal_carto_graph_t **graph);

/**
 * Add a node to carto graph.
 * 
 * @param graph the carto graph to add the node to.
 * @param node the node to add.
 */
OPAL_DECLSPEC void
opal_carto_base_graph_add_node_fn(opal_carto_graph_t *graph, opal_carto_base_node_t *node);

/**
 * Free a carto graph
 * @param graph the graph we want to free.
 */
OPAL_DECLSPEC void opal_carto_base_free_graph_fn(opal_carto_graph_t *graph);

/**
 * Connect two nodes by adding an edge to the graph.
 * 
 * @param graph the graph that the nodes belongs to.
 * @param start the start node
 * @param end the end node
 * @param weight the weight of the connection
 * 
 * @return int success or error (if one of the nodes does not
 *         belong to the graph.
 */
OPAL_DECLSPEC int
opal_carto_base_connect_nodes_fn(opal_carto_graph_t *graph, opal_carto_base_node_t *start,
                              opal_carto_base_node_t *end, uint32_t weight);

/**
 * Duplicate a carto graph and reduce the new graph to contain
 * nodes from a ceratin type(s)
 * 
 * @param destination The new graph.
 * @param source the original graph.
 * @param node_type the node type(s) that the new graph will
 *                  include.
 */
OPAL_DECLSPEC void
opal_carto_base_duplicate_graph_fn(opal_carto_graph_t **destination, const opal_carto_graph_t *source,
                                const char *node_type);


/**
 * opal_carto_base_get_nodes_distance - returns the distance of
 * all the nodes from the reference node.
 * 
 * @param graph
 * @param reference_node
 * @param node_type the type of the nodes in the returned array
 * @param dist_array
 * 
 * @return int number of nodes in the returned array.
 */
OPAL_DECLSPEC int
opal_carto_base_get_nodes_distance_fn(opal_carto_graph_t *graph, opal_carto_base_node_t *reference_node, 
                                   const char *node_type, opal_value_array_t *dist_array);

/**
 * Find the shortest path between two nodes in the graph
 * 
 * @param graph the graph that the nodes belongs to.
 * @param node1 first node.
 * @param node2 second node.
 * 
 * @return uint32_t he distance between the nodes.
 */
OPAL_DECLSPEC uint32_t
opal_carto_base_graph_spf_fn(opal_carto_graph_t *graph, opal_carto_base_node_t *node1, 
                          opal_carto_base_node_t *node2);

/**
 * Find a node in the graph according to its name.
 * 
 * @param graph the graph in which we are searching.
 * @param node_name the node name.
 * 
 * @return opal_carto_base_node_t* the node with the name -if
 *         found or NULL.
 */
OPAL_DECLSPEC opal_carto_base_node_t
*opal_carto_base_graph_find_node_fn(opal_carto_graph_t *graph, const char *node_name);

/**
 * Get the host cartography graph.
 * 
 * @param graph an unallocated pointer to a graph.
 * @param graph_type the type of nodes we want the returned
 *                   graph will contain.
 * 
 * @return int success or error
 */
OPAL_DECLSPEC int opal_carto_base_graph_get_host_graph_fn(opal_carto_graph_t **graph, const char * graph_type);

END_C_DECLS

#endif
