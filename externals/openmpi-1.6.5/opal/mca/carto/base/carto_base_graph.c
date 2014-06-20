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
# Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 * 
 * This file is an implementation of the carto graph base on the graph class.
 * This file is common to all the carto components.
 */

#include "opal_config.h"


#include "opal/class/opal_graph.h"
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/carto_base_graph.h"



/*
 * Globals
 */

static void opal_carto_base_free_node(void *node);
static void opal_carto_base_copy_nodes(void **dst, void *src);
static void *opal_carto_base_alloc_node(void);
static int opal_carto_compare_nodes(void *node1, void *node2);
static char* opal_carto_print_node(void* node);



/*
 * Functions
 */

/**
 * A function to print a node. this function allocates buffer
 * and prints in it the node type and the node name. this
 * function will be assign to the print_vertex function pointer
 * when building a new node.
 * 
 * @param node The node we want to print
 * 
 * @return char* the string to print.
 */
static char* opal_carto_print_node(void* node)
{
    char *print_str;
    char cpu_str[] = "(CPU)";
    opal_carto_base_node_t *tmp_node = (opal_carto_base_node_t *)node;

    if (false == tmp_node->is_cpu) {
        cpu_str[0] = 0;
    }
    asprintf(&print_str,"%s %5s -%s", tmp_node->node_type, cpu_str, tmp_node->node_name);

    return print_str;
}

/**
 * A function to free node. this function will be assigned to
 * the free_vertex_data pointer
 * 
 * @param node the node to free.
 */
static void opal_carto_base_free_node(void *node)
{
    opal_carto_base_node_t *tmp_node = (opal_carto_base_node_t *)node;
    /* free the node name string */
    free(tmp_node->node_name);
    free(tmp_node->node_type);
    /* free the node */
    free(tmp_node);    
}

/**
 * A function to copy a node. this function will be assign in
 * the copy_vertex_data function pointer.
 * 
 * @param dst the destination node.
 * @param src the source node.
 */
static void opal_carto_base_copy_nodes(void **dst, void *src)
{
    opal_carto_base_node_t *src_node = (opal_carto_base_node_t *)src,
                           *dst_node = (opal_carto_base_node_t *)*dst;

    /* duplicate the node name */
    dst_node->node_name = strdup(src_node->node_name);
    /* copy the node type */
    dst_node->node_type = strdup(src_node->node_type);
    dst_node->is_cpu = src_node->is_cpu;
    /* If the nodes vertex was copied, get the copied vertex */ 
    dst_node->vertex = src_node->vertex->sibling;
}

/**
 * Allocate memory for node. this function will be assign to the
 * alloc_vertex_data function pointer.
 * 
 * @return void*
 */
static void *opal_carto_base_alloc_node(void)
{
    opal_carto_base_node_t *node;

    /* allocate memory fore node */
    node = (opal_carto_base_node_t *)malloc(sizeof(opal_carto_base_node_t));
    /*Init the node fields */
    node->node_name = NULL;
    node->node_type = NULL;
    node->is_cpu = false;
    node->vertex = NULL;

    return (void*)node;
}

/**
 * Compare two nodes. in our case we needs to compare only the
 * node name. this function will be assign to the compare vertex
 * data function pointer.
 * 
 * @param node1
 * @param node2
 * 
 * @return int 0-equal, 1-the first is bigger, -1-the first is
 *         smaller.
 */
static int opal_carto_compare_nodes(void *node1, void *node2)
{
    opal_carto_base_node_t *tmp_node1 = (opal_carto_base_node_t *)node1,
                           *tmp_node2 = (opal_carto_base_node_t *)node2;

    /* use str compare to compare the node names */
    return strcmp(tmp_node1->node_name, tmp_node2->node_name);
}

/**
 * Create new carto graph.
 * 
 * @param graph an empty graph pointer
 */
void opal_carto_base_graph_create_fn(opal_carto_graph_t **graph)
{
    *graph = (opal_carto_graph_t *)OBJ_NEW(opal_graph_t);
}

/**
 * Add a node to carto graph.
 * 
 * @param graph the carto graph to add the node to.
 * @param node the node to add.
 */
void opal_carto_base_graph_add_node_fn(opal_carto_graph_t *graph, opal_carto_base_node_t *node)
{
    /* construct new vertex */
    node->vertex = OBJ_NEW(opal_graph_vertex_t);
    /* assign the node as the vertex data */
    node->vertex->vertex_data = (void *)node;
    /* assign the vertex function pointers */
    node->vertex->free_vertex_data = opal_carto_base_free_node;
    node->vertex->copy_vertex_data = opal_carto_base_copy_nodes;
    node->vertex->alloc_vertex_data = opal_carto_base_alloc_node;
    node->vertex->compare_vertex = opal_carto_compare_nodes;
    node->vertex->print_vertex = opal_carto_print_node;
    /* add the new node to the carto graph by adding the nodes vertex to the graph */
    opal_graph_add_vertex((opal_graph_t *)graph, node->vertex);
}

/**
 * Free a carto graph
 * @param graph the graph we want to free.
 */
void opal_carto_base_free_graph_fn(opal_carto_graph_t *graph)
{
    int i, graph_order;
    opal_carto_base_node_t *node;
    opal_pointer_array_t *graph_vertices;
    opal_graph_vertex_t *vertex;

    graph_vertices = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(graph_vertices, 20, INT_MAX, 20);
    /* get all the graph vertices */
    graph_order = opal_graph_get_graph_vertices(graph, graph_vertices);
    /* for all the vertices in the graph, free the nodes (and distract the vertices) */
    for (i = 0; i < graph_order; i++) {
        vertex = (opal_graph_vertex_t *)opal_pointer_array_get_item(graph_vertices, i);
        node = vertex->vertex_data;
        opal_carto_base_free_node((void *)node);
    }
    OBJ_RELEASE(graph_vertices);
    /* destruct the graph */
    OBJ_RELEASE(graph);
}

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
int opal_carto_base_connect_nodes_fn(opal_carto_graph_t *graph, opal_carto_base_node_t *start, opal_carto_base_node_t *end, uint32_t weight)
{
    opal_graph_edge_t *edge;

    /* construct anew edge */
    edge = OBJ_NEW(opal_graph_edge_t);
    /* assigne the start and the end nodes vertices to the new edge */
    edge->start = start->vertex;
    edge->end = end->vertex;
    /* assign the weight to the edge */
    edge->weight = weight;
    /* add the edge to the graph */
    return opal_graph_add_edge((opal_graph_t *)graph, edge);
}


/**
 * Duplicate a carto graph and reduce the new graph to contain
 * nodes from a ceratin type(s)
 * 
 * @param destination The new graph.
 * @param source the original graph.
 * @param node_type the node type(s) that the new graph will
 *                  include.
 */
void opal_carto_base_duplicate_graph_fn(opal_carto_graph_t **destination, const opal_carto_graph_t *source, const char *node_type)
{
    opal_pointer_array_t *vertices;
    int i, graph_order; 
    opal_carto_base_node_t *node;
    opal_graph_vertex_t *vertex;

    /* duplicate the graph */
    opal_graph_duplicate((opal_graph_t **)destination, (opal_graph_t *)source);
    /* if there is no need for reduction, return */
    if (NULL == node_type) {
        return;
    }
    vertices = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(vertices, 20, INT_MAX, 20);
    /* get all the vertices of the new graph */
    graph_order  = opal_graph_get_graph_vertices(*destination, vertices);
    /* remove all the nodes that are not in the required type */
    for (i = 0; i < graph_order; i++ ) {
        vertex = (opal_graph_vertex_t *)opal_pointer_array_get_item(vertices, i);
        node = vertex->vertex_data;
        if (!(0 == strcmp(node_type, node->node_type) || node->is_cpu)) {
            opal_graph_remove_vertex(*destination, vertex);
        }
    }
    /* free the vertices array */
    OBJ_RELEASE(vertices);
}

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
int opal_carto_base_get_nodes_distance_fn(opal_carto_graph_t *graph, opal_carto_base_node_t *reference_node, 
                                       const char *node_type, opal_value_array_t *dist_array)
{
    opal_value_array_t *distance_array;
    vertex_distance_from_t *vertex_distance;
    opal_carto_base_node_t *node;
    uint32_t i, graph_order;
    int distance_array_size;
    opal_carto_node_distance_t node_distance;


    distance_array = OBJ_NEW(opal_value_array_t);
    opal_value_array_init(distance_array, sizeof(vertex_distance_from_t));
    opal_value_array_reserve(distance_array,50);
    /* use dijkstra algorithm to receive the distance of all the nodes from the referenced node */
    graph_order = opal_graph_dijkstra(graph, reference_node->vertex, distance_array);
    /* for all the nodes in the dijkstra array */
    for (i = 0, distance_array_size = 0; i < graph_order; i++) {
        vertex_distance = opal_value_array_get_item(distance_array, i);
        node = vertex_distance->vertex->vertex_data;
        /* check if the node is in the correct type */
        if (NULL == node_type || 0 == strcmp(node->node_type, node_type)) {
            /* assigne the result distance array */
            node_distance.node = vertex_distance->vertex->vertex_data;
            node_distance.node_distance = vertex_distance->weight;
            opal_value_array_append_item(dist_array, (void *)&node_distance);
        }
    }
    /* return the result distance array */
    return distance_array_size;
}

/**
 * Find the shortest path between two nodes in the graph
 * 
 * @param graph the graph that the nodes belongs to.
 * @param node1 first node.
 * @param node2 second node.
 * 
 * @return uint32_t he distance between the nodes.
 */
uint32_t opal_carto_base_graph_spf_fn(opal_carto_graph_t *graph, opal_carto_base_node_t *node1, opal_carto_base_node_t *node2)
{
    return opal_graph_spf((opal_graph_t *)graph, node1->vertex, node2->vertex);
}

/**
 * Find a node in the graph according to its name.
 * 
 * @param graph the graph in which we are searching.
 * @param node_name the node name.
 * 
 * @return opal_carto_base_node_t* the node with the name -if
 *         found or NULL.
 */
opal_carto_base_node_t *opal_carto_base_graph_find_node_fn(opal_carto_graph_t *graph, const char *node_name)
{
    opal_carto_base_node_t node;
    opal_graph_vertex_t    *vertex;

    /* build a temporary node */
    node.node_name = strdup(node_name);
    /**
     * find a vertex in the graph. the find_vertex uses the
     * compare_vertex_data method. in our case, compare_vertex_data
     * is assigned to opal_carto_compare_nodes that compares two
     * nodes names.
     */
    vertex = opal_graph_find_vertex((opal_graph_t *)graph, (void *)&node);
    free(node.node_name);
    if (NULL != vertex) {
        /* return the fund vertex data (node) */ 
        return vertex->vertex_data;
    }
    /* if not found, return NULL */
    return NULL;
}

/**
 * Get the host cartography graph.
 * 
 * @param graph an unallocated pointer to a graph.
 * @param graph_type the type of nodes we want the returned
 *                   graph will contain.
 * 
 * @return int success or error
 */
int opal_carto_base_graph_get_host_graph_fn(opal_carto_graph_t **graph, const char *graph_type)
{
    /* duplicate the host graph and delete all the relevant nodes */
    opal_carto_base_duplicate_graph_fn(graph, opal_carto_base_common_host_graph, graph_type);
    return OPAL_SUCCESS;
}



