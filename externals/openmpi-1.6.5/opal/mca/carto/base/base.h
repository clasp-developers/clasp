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

#ifndef OPAL_CARTO_BASE_H
#define OPAL_CARTO_BASE_H

#include "opal_config.h"

#include "opal/mca/carto/carto.h"

/*
 * Global functions for MCA overall carto open and close
 */

BEGIN_C_DECLS

/**
 * Initialize the carto MCA framework
 *
 * @retval OPAL_SUCCESS Upon success
 * @retval OPAL_ERROR Upon failure
 *
 * This must be the first function invoked in the carto MCA
 * framework.  It initializes the carto MCA framework, finds
 * and opens carto components, etc.
 *
 * This function is invoked during opal_init().
 * 
 * This function fills in the internal global variable
 * opal_carto_base_components_opened, which is a list of all
 * carto components that were successfully opened.  This
 * variable should \em only be used by other carto base
 * functions -- it is not considered a public interface member --
 * and is only mentioned here for completeness.
 */
OPAL_DECLSPEC int opal_carto_base_open(void);

/**
 * Select an available component.
 *
 * @return OPAL_SUCCESS Upon success.
 * @return OPAL_NOT_FOUND If no component can be selected.
 * @return OPAL_ERROR Upon other failure.
 *
 * This function invokes the selection process for carto components,
 * which works as follows:
 *
 * - If the \em carto MCA parameter is not specified, the
 *   selection set is all available carto components.
 * - If the \em carto MCA parameter is specified, the
 *   selection set is just that component.
 * - All components in the selection set are queried to see if
 *   they want to run.  All components that want to run are ranked
 *   by their priority and the highest priority component is
 *   selected.  All non-selected components have their "close"
 *   function invoked to let them know that they were not selected.
 * - The selected component will have its "init" function invoked to
 *   let it know that it was selected.
 *
 * If we fall through this entire process and no component is
 * selected, then return OPAL_NOT_FOUND (this is not a fatal
 * error).
 *
 * At the end of this process, we'll either have a single
 * component that is selected and initialized, or no component was
 * selected.  If no component was selected, subsequent invocation
 * of the carto wrapper functions will return an error.
 */
OPAL_DECLSPEC int opal_carto_base_select(void);

/**
 * Shut down the carto MCA framework.
 *
 * @retval OPAL_SUCCESS Always
 *
 * This function shuts down everything in the carto MCA
 * framework, and is called during opal_finalize().
 *
 * It must be the last function invoked on the carto MCA
 * framework.
 */
OPAL_DECLSPEC int opal_carto_base_close(void);

/**
 * Indication of whether a component was successfully selected or
 * not
 */
OPAL_DECLSPEC extern bool opal_carto_base_selected;

/**
 * Global component struct for the selected component
 */
OPAL_DECLSPEC extern const opal_carto_base_component_2_0_0_t 
*opal_carto_base_component;

/**
 * Global module struct for the selected module
 */
OPAL_DECLSPEC extern const opal_carto_base_module_1_0_0_t 
*opal_carto_base_module;

/**
 * Indicator as to whether the list of opened carto components
 * is valid or not.
 */
OPAL_DECLSPEC extern bool opal_carto_base_components_opened_valid;

/**
 * List of all opened components; created when the carto
 * framework is initialized and destroyed when we reduce the list
 * to all available carto components.
 */
OPAL_DECLSPEC extern opal_list_t opal_carto_base_components_opened;


/**
 * Get the local host graph. you can reduce the graph for only
 * the nodes that interst you using the node type.
 * 
 * @param graph
 * @param graph_type
 * 
 * @return OPAL_DECLSPEC int
 */
OPAL_DECLSPEC int opal_carto_base_get_host_graph(opal_carto_graph_t **graph, const char *graph_type);


/**
 * Frre a graph
 * 
 * @param graph
 * 
 * @return OPAL_DECLSPEC void
 */
OPAL_DECLSPEC void opal_carto_base_free_graph(opal_carto_graph_t *graph);

/**
 * Get the distance (weight) from a start node to all other
 * nodes. you can reduce the list to the list to the node types
 * that intersts you.
 * 
 * @param graph
 * @param start
 * @param node_type
 * @param distance_
 * 
 * @return OPAL_DECLSPEC int
 */
OPAL_DECLSPEC int opal_carto_base_get_nodes_distance(opal_carto_graph_t *graph, opal_carto_base_node_t *start, const char *node_type, opal_value_array_t *distance_);

/**
 * find the distance between two nodes.
 * 
 * @param graph
 * @param start
 * @param end
 * 
 * @return OPAL_DECLSPEC uint32_t
 */
OPAL_DECLSPEC uint32_t opal_carto_base_spf(opal_carto_graph_t *graph,opal_carto_base_node_t *start, opal_carto_base_node_t *end);

/**
 * Find a node in the graph
 * 
 * @param graph
 * @param node_name
 * 
 * @return OPAL_DECLSPEC opal_carto_base_node_t
 */
OPAL_DECLSPEC opal_carto_base_node_t *opal_carto_base_find_node(opal_carto_graph_t *graph, const char *node_name);


/**
 * Debugging output stream
 */
OPAL_DECLSPEC extern int opal_carto_base_output;

OPAL_DECLSPEC extern opal_carto_graph_t *opal_carto_base_common_host_graph;

OPAL_DECLSPEC extern opal_carto_base_module_1_0_0_t opal_carto_default_module;


END_C_DECLS

#endif /* OPAL_BASE_CARTO_H */
