/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MTL_MXM_REQUEST_H
#define OMPI_MTL_MXM_REQUEST_H

#include "opal/datatype/opal_convertor.h"
#include "mtl_mxm.h"


struct mca_mtl_mxm_request_t {
    struct mca_mtl_request_t super;
    union {
        mxm_req_base_t base;
        mxm_send_req_t send;
        mxm_recv_req_t recv;
    } mxm;
#if MXM_API >= MXM_VERSION(2,0)
    int is_send;
#endif
    /* mxm_segment_t mxm_segment[1]; */
    void *buf;
    size_t length;
    struct opal_convertor_t *convertor;
    bool free_after;
};
typedef struct mca_mtl_mxm_request_t mca_mtl_mxm_request_t;

#endif
