/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"

#include "ompi/mca/pubsub/pubsub.h"
#include "ompi/mca/pubsub/base/base.h"

int ompi_pubsub_base_null_publish(char *service, ompi_info_t *info, char *port)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

int ompi_pubsub_base_null_unpublish(char *service, ompi_info_t *info)
{
    return OMPI_ERR_NOT_SUPPORTED;
}

char* ompi_pubsub_base_null_lookup(char *service, ompi_info_t *info)
{
    return NULL;
}
