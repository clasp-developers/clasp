/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/mpiext/example/mpiext_example_c.h"

static const char FUNC_NAME[] = "OMPI_Progress";


int OMPI_Progress(char * stmt) 
{
    printf("%s!!!\n", stmt);

    return MPI_SUCCESS;
}

