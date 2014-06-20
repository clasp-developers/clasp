/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <sys/time.h>
#include <time.h>
#include "ompi/types.h"
#include "mtl_psm.h"
#include "mtl_psm_types.h"
#include "mtl_psm_endpoint.h" 

/*
 * Initialize state of the endpoint instance.
 *
 */

static void mca_mtl_psm_endpoint_construct(mca_mtl_psm_endpoint_t* endpoint)
{
    endpoint->mtl_psm_module = NULL;
}

/*
 * Destroy a endpoint
 *
 */

static void mca_mtl_psm_endpoint_destruct(mca_mtl_psm_endpoint_t* endpoint)
{
}


OBJ_CLASS_INSTANCE(
    mca_mtl_psm_endpoint_t, 
    opal_list_item_t, 
    mca_mtl_psm_endpoint_construct, 
    mca_mtl_psm_endpoint_destruct);

