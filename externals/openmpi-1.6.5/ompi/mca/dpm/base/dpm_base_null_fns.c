/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 University of Houston. All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007      Cisco Systems, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <stdio.h>

#include "ompi/mca/dpm/dpm.h"
#include "ompi/mca/dpm/base/base.h"


int ompi_dpm_base_null_connect_accept (ompi_communicator_t *comm, int root,
                                       char *port_string, bool send_first,
                                       ompi_communicator_t **newcomm)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

void ompi_dpm_base_null_disconnect(ompi_communicator_t *comm)
{
    return;
}

int ompi_dpm_base_null_spawn(int count, char **array_of_commands,
                             char ***array_of_argv,
                             int *array_of_maxprocs,
                             MPI_Info *array_of_info,
                             char *port_name)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_dpm_base_null_dyn_init(void)
{
    return OMPI_SUCCESS;
}

int ompi_dpm_base_null_dyn_finalize (void)
{
    return OMPI_SUCCESS;
}

void ompi_dpm_base_null_mark_dyncomm (ompi_communicator_t *comm)
{
    return;
}

int ompi_dpm_base_null_open_port(char *port_name, orte_rml_tag_t given_tag)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_dpm_base_null_parse_port(char *port_name, 
                                  char **hnp_uri, char **rml_uri, orte_rml_tag_t *tag)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_dpm_base_null_route_to_port(char *rml_uri, orte_process_name_t *rproc)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_dpm_base_null_close_port(char *port_name)
{
    return OMPI_ERR_NOT_SUPPORTED;
}
