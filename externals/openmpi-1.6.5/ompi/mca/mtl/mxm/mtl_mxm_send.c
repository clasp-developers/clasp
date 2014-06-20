/* * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/pml/pml.h"
#include "opal/datatype/opal_convertor.h"
#include "orte/util/show_help.h"

#include "mtl_mxm.h"
#include "mtl_mxm_types.h"
#include "mtl_mxm_request.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

static inline  __opal_attribute_always_inline__ 
              size_t ompi_mtl_mxm_stream_pack(opal_convertor_t *convertor, void *buffer,
                                              size_t length, size_t offset)
{
    struct iovec iov;
    uint32_t iov_count = 1;

    iov.iov_len = length;
    iov.iov_base = buffer;

    opal_convertor_set_position(convertor, &offset);
    opal_convertor_pack(convertor, &iov, &iov_count, &length);

    return length;
}

static size_t ompi_mtl_mxm_stream_isend(void *buffer, size_t length, size_t offset, void *context)
{
    mca_mtl_mxm_request_t *mtl_mxm_request = (mca_mtl_mxm_request_t *) context;
    opal_convertor_t *convertor = mtl_mxm_request->convertor;

    return ompi_mtl_mxm_stream_pack(convertor, buffer, length, offset);
}

static size_t ompi_mtl_mxm_stream_send(void *buffer, size_t length, size_t offset, void *context)
{
    opal_convertor_t *convertor = (opal_convertor_t *) context;

    return ompi_mtl_mxm_stream_pack(convertor, buffer, length, offset);
}

static inline __opal_attribute_always_inline__ int
               ompi_mtl_mxm_choose_send_datatype(mxm_send_req_t *mxm_send_req,
                                                 opal_convertor_t *convertor,
                                                 mxm_stream_cb_t stream_cb)
{
    struct iovec iov;
    uint32_t iov_count = 1;

    size_t *buffer_len = &mxm_send_req->base.data.buffer.length;

    opal_convertor_get_packed_size(convertor, buffer_len);
    if (0 == *buffer_len) {
        mxm_send_req->base.data.buffer.ptr = NULL;
        mxm_send_req->base.data_type = MXM_REQ_DATA_BUFFER;

        return OMPI_SUCCESS;
    }

    if (opal_convertor_need_buffers(convertor)) {
        mxm_send_req->base.data_type = MXM_REQ_DATA_STREAM;
        mxm_send_req->base.data.stream.length = *buffer_len;
        mxm_send_req->base.data.stream.cb = stream_cb;

        return OMPI_SUCCESS;
    }

    mxm_send_req->base.data_type = MXM_REQ_DATA_BUFFER;

    iov.iov_base = NULL;
    iov.iov_len = *buffer_len;

    opal_convertor_pack(convertor, &iov, &iov_count, buffer_len);
    mxm_send_req->base.data.buffer.ptr = iov.iov_base;

    return OMPI_SUCCESS;
}

static void ompi_mtl_mxm_send_completion_cb(void *context)
{
    mca_mtl_mxm_request_t *mtl_mxm_request = context;

    ompi_mtl_mxm_to_mpi_status(mtl_mxm_request->mxm.base.error,
                               &mtl_mxm_request->super.ompi_req->req_status);
    mtl_mxm_request->super.completion_callback(&mtl_mxm_request->super);
}

static void ompi_mtl_mxm_send_progress_cb(void *user_data)
{
    opal_progress();
}

int ompi_mtl_mxm_send(struct mca_mtl_base_module_t* mtl,
                      struct ompi_communicator_t* comm, int dest, int tag,
                      struct opal_convertor_t *convertor,
                      mca_pml_base_send_mode_t mode)
{
    mxm_send_req_t mxm_send_req;
    mxm_wait_t wait;
    mxm_error_t err;
    int ret;

    /* prepare local send request */
    mxm_send_req.base.state         = MXM_REQ_NEW;
    mxm_send_req.base.mq            = ompi_mtl_mxm_mq_lookup(comm);
    mxm_send_req.base.conn          = ompi_mtl_mxm_conn_lookup(comm, dest);
    mxm_send_req.base.context       = convertor;
    mxm_send_req.base.completed_cb  = NULL;

    ret = ompi_mtl_mxm_choose_send_datatype(&mxm_send_req, convertor,
                                            ompi_mtl_mxm_stream_send);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

#if MXM_API < MXM_VERSION(1,5)
    mxm_send_req.base.data.buffer.mkey   = MXM_MKEY_NONE;
#else
    mxm_send_req.base.data.buffer.memh   = MXM_INVALID_MEM_HANDLE;
#endif

    mxm_send_req.op.send.tag             = tag;
    mxm_send_req.op.send.imm_data        = ompi_comm_rank(comm);
#if MXM_API < MXM_VERSION(2,0)
    mxm_send_req.base.flags              = MXM_REQ_FLAG_BLOCKING;
    mxm_send_req.opcode                  = MXM_REQ_OP_SEND;
    if (mode == MCA_PML_BASE_SEND_SYNCHRONOUS) {
        mxm_send_req.base.flags          |= MXM_REQ_FLAG_SEND_SYNC;
    }
#else
    mxm_send_req.flags                   = MXM_REQ_SEND_FLAG_BLOCKING;
    if (mode == MCA_PML_BASE_SEND_SYNCHRONOUS) {
        mxm_send_req.opcode              = MXM_REQ_OP_SEND_SYNC;
    } else {
        mxm_send_req.opcode              = MXM_REQ_OP_SEND;
    }
#endif

    /* post-send */
    err = mxm_req_send(&mxm_send_req);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "error posting send", true, 0, mxm_error_string(err));
        return OMPI_ERROR;
    }

    /* wait for request completion */
    wait.req          = &mxm_send_req.base;
    wait.state        = MXM_REQ_COMPLETED;
    wait.progress_cb  = ompi_mtl_mxm_send_progress_cb;
    wait.progress_arg = NULL;
    mxm_wait(&wait);

    return OMPI_SUCCESS;
}

int ompi_mtl_mxm_isend(struct mca_mtl_base_module_t* mtl,
                       struct ompi_communicator_t* comm, int dest, int tag,
                       struct opal_convertor_t *convertor,
                       mca_pml_base_send_mode_t mode, bool blocking,
                       mca_mtl_request_t * mtl_request)
{
    mca_mtl_mxm_request_t *mtl_mxm_request = (mca_mtl_mxm_request_t *) mtl_request;
    mxm_send_req_t *mxm_send_req;
    mxm_error_t err;
    int ret;

    assert(mtl == &ompi_mtl_mxm.super);

    mtl_mxm_request->convertor = convertor;

    mxm_send_req = &mtl_mxm_request->mxm.send;
#if MXM_API >= MXM_VERSION(2,0)
    mtl_mxm_request->is_send = 1;
#endif

    /* prepare a send request embedded in the MTL request */
    mxm_send_req->base.state               = MXM_REQ_NEW;
    mxm_send_req->base.mq                  = ompi_mtl_mxm_mq_lookup(comm);
    mxm_send_req->base.conn                = ompi_mtl_mxm_conn_lookup(comm, dest);

    ret = ompi_mtl_mxm_choose_send_datatype(mxm_send_req, convertor,
                                            ompi_mtl_mxm_stream_isend);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    mtl_mxm_request->buf    = mxm_send_req->base.data.buffer.ptr;
    mtl_mxm_request->length = mxm_send_req->base.data.buffer.length;

#if MXM_API < MXM_VERSION(1,5)
    mxm_send_req->base.data.buffer.mkey    = MXM_MKEY_NONE;
#else
    mxm_send_req->base.data.buffer.memh    = MXM_INVALID_MEM_HANDLE;
#endif
    mxm_send_req->base.context             = mtl_mxm_request;
    mxm_send_req->base.completed_cb        = ompi_mtl_mxm_send_completion_cb;

#if MXM_API < MXM_VERSION(2,0)
    mxm_send_req->base.flags               = 0;
    mxm_send_req->opcode                   = MXM_REQ_OP_SEND;
    if (mode == MCA_PML_BASE_SEND_SYNCHRONOUS) {
        mxm_send_req->base.flags           |= MXM_REQ_FLAG_SEND_SYNC;
    } else {
    }
#else
    mxm_send_req->flags                    = 0;
    if (mode == MCA_PML_BASE_SEND_SYNCHRONOUS) {
        mxm_send_req->opcode               = MXM_REQ_OP_SEND_SYNC;
    } else {
        mxm_send_req->opcode               = MXM_REQ_OP_SEND;
    }
#endif
    mxm_send_req->op.send.tag              = tag;
    mxm_send_req->op.send.imm_data         = ompi_comm_rank(comm);

    /* post-send */
    err = mxm_req_send(mxm_send_req);
    if (MXM_OK != err) {
        orte_show_help("help-mtl-mxm.txt", "error posting send", true, 1, mxm_error_string(err));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
