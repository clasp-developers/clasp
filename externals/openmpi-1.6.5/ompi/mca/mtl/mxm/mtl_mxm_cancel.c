/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mtl_mxm.h"
#include "mtl_mxm_request.h"

int ompi_mtl_mxm_cancel(struct mca_mtl_base_module_t* mtl,
                        struct mca_mtl_request_t *mtl_request, int flag)
{
    mca_mtl_mxm_request_t *mtl_mxm_request = (mca_mtl_mxm_request_t*) mtl_request;
    mxm_error_t err;

#if MXM_API >= MXM_VERSION(2,0)
    if (mtl_mxm_request->is_send) {
        err = mxm_req_cancel_send(&mtl_mxm_request->mxm.send);
    } else {
        err = mxm_req_cancel_recv(&mtl_mxm_request->mxm.recv);
    }
#else
    err = mxm_req_cancel(&mtl_mxm_request->mxm.base);
#endif
    if ((err != MXM_OK) && (err != MXM_ERR_NO_PROGRESS)) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
