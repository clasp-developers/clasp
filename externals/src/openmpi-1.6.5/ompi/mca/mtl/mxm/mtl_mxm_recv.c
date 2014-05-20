/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

#include "orte/util/show_help.h"

#include "mtl_mxm.h"
#include "mtl_mxm_types.h"
#include "mtl_mxm_request.h"


static void ompi_mtl_mxm_recv_completion_cb(void *context)
{
	mca_mtl_mxm_request_t *req = (mca_mtl_mxm_request_t *) context;
    struct ompi_request_t *ompi_req = req->super.ompi_req;
    mxm_recv_req_t *mxm_recv_req = &req->mxm.recv;

    /* Set completion status and envelope */
    ompi_mtl_mxm_to_mpi_status(mxm_recv_req->base.error, &ompi_req->req_status);
    ompi_req->req_status.MPI_TAG    = mxm_recv_req->completion.sender_tag;
    ompi_req->req_status.MPI_SOURCE = mxm_recv_req->completion.sender_imm;
    ompi_req->req_status._ucount    = mxm_recv_req->completion.actual_len;

    req->super.completion_callback(&req->super);
}

static size_t ompi_mtl_mxm_stream_unpack(void *buffer, size_t length,
                                         size_t offset, void *context)
{
    struct iovec iov;
    uint32_t iov_count = 1;

   mca_mtl_mxm_request_t *mtl_mxm_request = (mca_mtl_mxm_request_t *) context;
    opal_convertor_t *convertor = mtl_mxm_request->convertor;

    iov.iov_len = length;
    iov.iov_base = buffer;

    opal_convertor_set_position(convertor, &offset);
    opal_convertor_unpack(convertor, &iov, &iov_count, &length);

    return length;
}

 static inline __opal_attribute_always_inline__ int
               ompi_mtl_mxm_choose_recv_datatype(mca_mtl_mxm_request_t *mtl_mxm_request)
{
    void **buffer = &mtl_mxm_request->buf;
    size_t *buffer_len = &mtl_mxm_request->length;

    mxm_recv_req_t *mxm_recv_req = &mtl_mxm_request->mxm.recv;
    opal_convertor_t *convertor = mtl_mxm_request->convertor;

    opal_convertor_get_packed_size(convertor, buffer_len);

    if (0 == *buffer_len) {
        *buffer = NULL;
        *buffer_len = 0;

        mxm_recv_req->base.data_type = MXM_REQ_DATA_BUFFER;

        return OMPI_SUCCESS;
    }

    if (opal_convertor_need_buffers(convertor)) {
        mxm_recv_req->base.data_type = MXM_REQ_DATA_STREAM;
        mxm_recv_req->base.data.stream.length = *buffer_len;
        mxm_recv_req->base.data.stream.cb = ompi_mtl_mxm_stream_unpack;

        return OMPI_SUCCESS;
    }

    mxm_recv_req->base.data_type = MXM_REQ_DATA_BUFFER;

    *buffer = convertor->pBaseBuf +
            convertor->use_desc->desc[convertor->use_desc->used].end_loop.first_elem_disp;

    mxm_recv_req->base.data.buffer.ptr     = *buffer;
    mxm_recv_req->base.data.buffer.length  = *buffer_len;

    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_irecv(struct mca_mtl_base_module_t* mtl,
                       struct ompi_communicator_t *comm, int src, int tag,
                       struct opal_convertor_t *convertor,
                       struct mca_mtl_request_t *mtl_request)
{
    mca_mtl_mxm_request_t * mtl_mxm_request;
    mxm_error_t err;
    mxm_recv_req_t *mxm_recv_req;
    int ret;

    mtl_mxm_request = (mca_mtl_mxm_request_t*) mtl_request;

    mtl_mxm_request->convertor = convertor;
    ret = ompi_mtl_mxm_choose_recv_datatype(mtl_mxm_request);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }


    /* prepare a receive request embedded in the MTL request */
    mxm_recv_req = &mtl_mxm_request->mxm.recv;
#if MXM_API >= MXM_VERSION(2,0)
    mtl_mxm_request->is_send = 0;
#endif

    mxm_recv_req->base.state               = MXM_REQ_NEW;
    ompi_mtl_mxm_set_recv_envelope(mxm_recv_req, comm, src, tag);

#if MXM_API < MXM_VERSION(2,0)
    mxm_recv_req->base.flags               = 0;
#endif

#if MXM_API < MXM_VERSION(1,5)
    mxm_recv_req->base.data.buffer.mkey    = MXM_MKEY_NONE;
#else
    mxm_recv_req->base.data.buffer.memh    = MXM_INVALID_MEM_HANDLE;
#endif

    mxm_recv_req->base.context             = mtl_mxm_request;
    mxm_recv_req->base.completed_cb        = ompi_mtl_mxm_recv_completion_cb;

    /* post-recv */
    err = mxm_req_recv(mxm_recv_req);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "error posting receive", true,
                       mxm_error_string(err), mtl_mxm_request->buf, mtl_mxm_request->length);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

