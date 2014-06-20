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
#include "mtl_mxm_types.h"

#include "ompi/communicator/communicator.h"

int ompi_mtl_mxm_iprobe(struct mca_mtl_base_module_t* mtl,
                        struct ompi_communicator_t *comm, int src, int tag,
                        int *flag, struct ompi_status_public_t *status)
{
    mxm_error_t err;
    mxm_recv_req_t req;

    req.base.state = MXM_REQ_NEW;
    ompi_mtl_mxm_set_recv_envelope(&req, comm, src, tag);

    err = mxm_req_probe(&req);
    if (MXM_OK == err) {
        *flag = 1;
        if (MPI_STATUS_IGNORE != status) {
            ompi_mtl_mxm_to_mpi_status(err, status);
            status->MPI_SOURCE = req.completion.sender_imm;
            status->MPI_TAG    = req.completion.sender_tag;
            status->_ucount    = req.completion.sender_len;
        }
        return OMPI_SUCCESS;
    } else if (MXM_ERR_NO_MESSAGE == err) {
        *flag = 0;
        return OMPI_SUCCESS;
    } else {
        return OMPI_ERROR;
    }
}
