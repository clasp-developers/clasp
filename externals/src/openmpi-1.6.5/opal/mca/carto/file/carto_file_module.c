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
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

/* This component will only be compiled on File, where we are
   guaranteed to have <unistd.h> and friends */
#include <stdio.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif  /* HAVE_STDLIB_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif  /* HAVE_ERRNO_H */

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"
#include "opal/mca/carto/base/carto_base_graph.h"
#include "carto_file.h"
#include "carto_file_lex.h"



char *carto_file_path;

static int opal_carto_file_init(void);
static int opal_carto_file_finalize(void);
static int opal_carto_file_parse(const char *cartofile);


/*
 * File carto module
 */
static const opal_carto_base_module_1_0_0_t loc_module = {
    opal_carto_file_init,
    opal_carto_base_graph_get_host_graph_fn,
    opal_carto_base_free_graph_fn,
    opal_carto_base_get_nodes_distance_fn,
    opal_carto_base_graph_spf_fn,
    opal_carto_base_graph_find_node_fn,
    opal_carto_file_finalize,
};


int opal_carto_file_component_query(mca_base_module_t **module, int *priority)
{
    int param;

    param = mca_base_param_find("carto", "file", "priority");
    mca_base_param_lookup_int(param, priority);

    param = mca_base_param_find("carto", "file", "path");
    mca_base_param_lookup_string(param, &carto_file_path);

    *module = (mca_base_module_t *)&loc_module;

    return OPAL_SUCCESS;
}


/**
 * Init the carto file module.
 * 
 * @return int success or error
 */
static int opal_carto_file_init(void)
{
    int rc;

    /* create an empty graph */
    if (NULL == opal_carto_base_common_host_graph) {
        opal_carto_base_graph_create_fn(&opal_carto_base_common_host_graph);
    }
    if (NULL == carto_file_path) {
        return OPAL_ERROR;
    }
    /* parse the carto file and add nodes and connections to the graph */
    rc = opal_carto_file_parse(carto_file_path);
    return rc;
}

/**
 * finalize the carto file module.
 * 
 * @return int success or error
 */
static int opal_carto_file_finalize(void)
{
    /* free the host cartography graph. */
    if (NULL != opal_carto_base_common_host_graph) {
        opal_carto_base_free_graph_fn(opal_carto_base_common_host_graph);
    }
    return OPAL_SUCCESS;
}


/**
 * Parse the carto file and file the host graph with nodes and
 * connections.
 * 
 * @param cartofile the path to the carto file.
 * 
 * @return int success or error.
 */
static int opal_carto_file_parse(const char *cartofile)
{
    int token;
    opal_carto_base_node_t *node, *end_node;
    uint32_t weight;
    char *node1_name, *node2_name, *value, **argv;
    int cnt, line_number = 1;
    char *token_to_string[] = {
        "New-line",
        "File-error",
        "Node deceleration",
        "Connection deceleration",
        "bi directional connection",
        "Integer",
        "Name",
        "Node connection",
        "Undefined"
        "Undefined"
        "Undefined"
    };

    /* set the done flag to false. at the end of file the the lexical analyzer will set it to true */
    carto_file_done = false;
    carto_file_in = fopen(cartofile, "r"); /* open the carto file */
    /* if the file not found, return an error */
    if (NULL == carto_file_in) {
        opal_show_help("help-opal-carto-file.txt", "file not found", true, cartofile);
        return OPAL_ERR_NOT_FOUND;
    }
    while (!carto_file_done) {
        token = carto_file_lex();
        switch (token) {
        case OPAL_CARTO_FILE_NEWLINE:
            line_number++;
            break;
        case OPAL_CARTO_FILE_NODE_DECELERATION:
                token = carto_file_lex();
                switch (token) {
                case OPAL_CARTO_FILE_NAME:
                    node = (opal_carto_base_node_t *)malloc(sizeof(opal_carto_base_node_t));
                    node->node_type = strdup(carto_file_value.sval);
                    if (0 == strcmp("socket",node->node_type)) {
                        node->is_cpu = true;
                    }
                    else {
                        node->is_cpu = false;
                    }
                    token = carto_file_lex();
                    switch (token) {
                    case OPAL_CARTO_FILE_NAME:
                        node->node_name = strdup(carto_file_value.sval);
                        opal_carto_base_graph_add_node_fn(opal_carto_base_common_host_graph, node);
                        break;
                    default:
                        free(node);
                        opal_show_help("help-opal-carto-file.txt", "expected node name", 
                                       true, cartofile, line_number, token_to_string[token]);
                        goto error;
                    }
                    break;
                default:
                    opal_show_help("help-opal-carto-file.txt", "expected node type", 
                                   true, cartofile, line_number, token_to_string[token]);
                    goto error;
                }
            break;
        case OPAL_CARTO_FILE_CONNECTION_DECELERATION:
            token = carto_file_lex();
            switch (token) {
            case OPAL_CARTO_FILE_NAME:
                node1_name = strdup(carto_file_value.sval);
                while (OPAL_CARTO_FILE_NEWLINE != token) {
                    token = carto_file_lex();
                    switch (token) {
                    case OPAL_CARTO_FILE_NODE_CONNECTION:
                        value = carto_file_value.sval;
                        argv = opal_argv_split (value, ':');
                        cnt = opal_argv_count (argv);
                        if (2 == cnt) {
                            node2_name = strdup(argv[0]);
                            weight = atoi(argv[1]);
                        } else {
                            opal_show_help("help-opal-carto-file.txt", "incorrect connection", true, cartofile, line_number, value);
                            opal_argv_free (argv);
                            free(node1_name);
                            goto error;
                        }
                        opal_argv_free (argv);
                        /* find the start node of the connection */
                        node = opal_carto_base_graph_find_node_fn(opal_carto_base_common_host_graph,node1_name);
                        if (NULL == node) {
                            opal_show_help("help-opal-carto-file.txt", "vertex not found", true, cartofile, line_number, node1_name);
                            free(node1_name);
                            free(node2_name);
                            goto error;
                        }
                        /* find the end node of the connection */
                        end_node = opal_carto_base_graph_find_node_fn(opal_carto_base_common_host_graph,node2_name);
                        if (NULL == end_node) {
                            opal_show_help("help-opal-carto-file.txt", "vertex not found", true, cartofile, line_number, node2_name);
                            free(node1_name);
                            free(node2_name);
                            goto error;
                        }
                        opal_carto_base_connect_nodes_fn(opal_carto_base_common_host_graph, node, end_node, weight);
                        free(node2_name);
                        break;
                    case OPAL_CARTO_FILE_NEWLINE:
                        line_number++;
                        break;
                    default:
                        opal_show_help("help-opal-carto-file.txt", "expected Connection", 
                                       true, cartofile, line_number, token_to_string[token]);
                        free(node1_name);
                        goto error;
                    }
                }
                free(node1_name);
                break;
            default:
                opal_show_help("help-opal-carto-file.txt", "expected node name", 
                               true, cartofile, line_number, token_to_string[token]);
                goto error;
            }
            break;
        case OPAL_CARTO_FILE_BIDIRECTION_CONNECTION:
            token = carto_file_lex();
            switch (token) {
            case OPAL_CARTO_FILE_NAME:
                node1_name = strdup(carto_file_value.sval);
                while (OPAL_CARTO_FILE_NEWLINE != token) {
                    token = carto_file_lex();
                    switch (token) {
                    case OPAL_CARTO_FILE_NODE_CONNECTION:
                        value = carto_file_value.sval;
                        argv = opal_argv_split (value, ':');
                        cnt = opal_argv_count (argv);
                        if (2 == cnt) {
                            node2_name = strdup(argv[0]);
                            weight = atoi(argv[1]);
                        } else {
                            opal_show_help("help-opal-carto-file.txt", "incorrect connection", true, cartofile, line_number, value);
                            opal_argv_free (argv);
                            free(node1_name);
                            goto error;
                        }
                        opal_argv_free (argv);
                        /* find the start node of the connection */
                        node = opal_carto_base_graph_find_node_fn(opal_carto_base_common_host_graph,node1_name);
                        if (NULL == node) {
                            opal_show_help("help-opal-carto-file.txt", "vertex not found", true, cartofile, line_number, node1_name);
                            free(node1_name);
                            free(node2_name);
                            goto error;
                        }
                        /* find the end node of the connection */
                        end_node = opal_carto_base_graph_find_node_fn(opal_carto_base_common_host_graph,node2_name);
                        if (NULL == end_node) {
                            opal_show_help("help-opal-carto-file.txt", "vertex not found", true, cartofile, line_number, node2_name);
                            free(node1_name);
                            free(node2_name);
                            goto error;
                        }
                        opal_carto_base_connect_nodes_fn(opal_carto_base_common_host_graph, node, end_node, weight);
                        opal_carto_base_connect_nodes_fn(opal_carto_base_common_host_graph, end_node, node, weight);
                        free(node2_name);
                        break;
                    case OPAL_CARTO_FILE_NEWLINE:
                        line_number++;
                        break;
                    default:
                        opal_show_help("help-opal-carto-file.txt", "expected Connection", 
                                       true, cartofile, line_number, token_to_string[token]);
                        free(node1_name);
                        goto error;
                    }
                }
                free(node1_name);
                break;
            default:
                opal_show_help("help-opal-carto-file.txt", "expected node name", 
                               true, cartofile, line_number, token_to_string[token]);
                goto error;
            }
            break;
        default:
            opal_show_help("help-opal-carto-file.txt", "expected deceleration", 
                           true, cartofile, line_number, token_to_string[token]);
            goto error;

        }
    }
    return OPAL_SUCCESS;
error:
    fclose(carto_file_in);
    return OPAL_ERR_BAD_PARAM;
}




