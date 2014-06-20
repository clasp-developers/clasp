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
#include "btl_mx.h"
#include "btl_mx_endpoint.h" 
#include "btl_mx_proc.h"
#include "btl_mx_frag.h"


/*
 * Initialize state of the endpoint instance.
 *
 */
static void mca_btl_mx_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{
    endpoint->endpoint_btl  = NULL;
    endpoint->endpoint_proc = NULL;
    endpoint->status        = MCA_BTL_MX_NOT_CONNECTED;
}

/*
 * Destroy a endpoint
 *
 */
static void mca_btl_mx_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
}


OBJ_CLASS_INSTANCE(
    mca_btl_mx_endpoint_t, 
    opal_list_item_t, 
    mca_btl_mx_endpoint_construct, 
    mca_btl_mx_endpoint_destruct);

