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
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"


int opal_carto_base_get_host_graph(opal_carto_graph_t **graph, const char *graph_type)
{
    if (!opal_carto_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_carto_base_module->get_host_graph(graph, graph_type);
}

void opal_carto_base_free_graph(opal_carto_graph_t *graph)
{
    if (!opal_carto_base_selected) {
        return ;
    }
    opal_carto_base_module->free_graph(graph);
}


int opal_carto_base_get_nodes_distance(opal_carto_graph_t *graph, opal_carto_base_node_t *start, const char *node_type, opal_value_array_t *distance_)
{
    if (!opal_carto_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_carto_base_module->get_nodes_distance(graph, start, node_type, distance_);
}

uint32_t opal_carto_base_spf(opal_carto_graph_t *graph,opal_carto_base_node_t *start, opal_carto_base_node_t *end)
{
    if (!opal_carto_base_selected) {
        return OPAL_ERR_NOT_FOUND;
    }
    return opal_carto_base_module->spf(graph, start, end);
}

opal_carto_base_node_t *opal_carto_base_find_node(opal_carto_graph_t *graph, const char *node_name)
{
    if (!opal_carto_base_selected) {
        return NULL;
    }
    return opal_carto_base_module->find_node(graph, node_name);
}





