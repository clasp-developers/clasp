/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"


#include "mtl_mx.h"
#include "mtl_mx_types.h"
#include "mtl_mx_request.h"

int
ompi_mtl_mx_cancel(struct mca_mtl_base_module_t* mtl, 
                   struct mca_mtl_request_t *mtl_request, 
                   int flag) 
{
    mca_mtl_mx_request_t *mtl_mx_request =
        (mca_mtl_mx_request_t*) mtl_request;
    uint32_t result;

    mx_cancel(ompi_mtl_mx.mx_endpoint, 
              &mtl_mx_request->mx_request,
              &result);

    /* If MX canceled the request, mark it as canceled and complete
       it.  Otherwise, keep on going */
    if (result) {
        mtl_request->ompi_req->req_status._cancelled = true;
        mtl_mx_request->super.completion_callback(&mtl_mx_request->super);
    }

    return OMPI_SUCCESS;
}
