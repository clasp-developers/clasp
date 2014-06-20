/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_rdma_sendreq.h"

#include "opal/datatype/opal_convertor.h"


int
ompi_osc_rdma_sendreq_alloc_init(ompi_osc_rdma_req_type_t req_type,
                                  void *origin_addr, int origin_count,
                                  struct ompi_datatype_t *origin_dt,
                                  int target, OPAL_PTRDIFF_TYPE target_disp, 
				  int target_count,
                                  struct ompi_datatype_t *target_dt,
                                  ompi_osc_rdma_module_t *module,
                                  ompi_osc_rdma_sendreq_t **sendreq)
{
    int ret;

    /* allocate a sendreq */
    ret = ompi_osc_rdma_sendreq_alloc(module, target,
                                       sendreq);
    if (OMPI_SUCCESS != ret) return ret;

    /* initialize local side of sendreq */
    ret = ompi_osc_rdma_sendreq_init_origin(*sendreq,
                                             req_type,
                                             origin_addr,
                                             origin_count,
                                             origin_dt);
    if (OMPI_SUCCESS != ret) {
        ompi_osc_rdma_sendreq_free(*sendreq);
        return ret;
    }

    /* initialize remote side of sendreq */
    ret = ompi_osc_rdma_sendreq_init_target(*sendreq,
                                             target_disp,
                                             target_count,
                                             target_dt);
    if (OMPI_SUCCESS != ret) {
        ompi_osc_rdma_sendreq_free(*sendreq);
        return ret;
    }

    return OMPI_SUCCESS;
}


static void ompi_osc_rdma_sendreq_construct(ompi_osc_rdma_sendreq_t *req)
{
    req->super.req_type = OMPI_REQUEST_WIN;
    req->super.req_free = NULL;
    req->super.req_cancel = NULL;
    OBJ_CONSTRUCT(&(req->req_origin_convertor), opal_convertor_t);
}


static void ompi_osc_rdma_sendreq_destruct(ompi_osc_rdma_sendreq_t *req)
{
    OBJ_DESTRUCT(&(req->req_origin_convertor));
}


OBJ_CLASS_INSTANCE(ompi_osc_rdma_sendreq_t, ompi_request_t,
                   ompi_osc_rdma_sendreq_construct, 
                   ompi_osc_rdma_sendreq_destruct);
