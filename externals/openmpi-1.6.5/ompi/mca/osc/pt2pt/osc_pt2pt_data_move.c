/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_pt2pt.h"
#include "osc_pt2pt_sendreq.h"
#include "osc_pt2pt_header.h"
#include "osc_pt2pt_data_move.h"
#include "osc_pt2pt_buffer.h"

#include "opal/util/arch.h"
#include "opal/util/output.h"
#include "opal/sys/atomic.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "ompi/memchecker.h"


static inline int32_t
create_send_tag(ompi_osc_pt2pt_module_t *module)
{
#if OPAL_HAVE_THREAD_SUPPORT && OPAL_HAVE_ATOMIC_CMPSET_32
    int32_t newval, oldval;
    do {
        oldval = module->p2p_tag_counter;
        newval = (oldval + 1) % mca_pml.pml_max_tag;
    } while (0 == opal_atomic_cmpset_32(&module->p2p_tag_counter, oldval, newval));
    return newval;
#else
    int32_t ret;
    /* no compare and swap - have to lock the module */
    OPAL_THREAD_LOCK(&module->p2p_lock);
    module->p2p_tag_counter = (module->p2p_tag_counter + 1) % mca_pml.pml_max_tag;
    ret = module->p2p_tag_counter;
    OPAL_THREAD_UNLOCK(&module->p2p_lock);
    return ret;
#endif
}


static inline void
inmsg_mark_complete(ompi_osc_pt2pt_module_t *module)
{
    int32_t count;
    bool need_unlock = false;

    OPAL_THREAD_LOCK(&module->p2p_lock);
    count = (module->p2p_num_pending_in -= 1);
    if ((0 != module->p2p_lock_status) &&
        (opal_list_get_size(&module->p2p_unlocks_pending) != 0)) {
        need_unlock = true;
    }
    OPAL_THREAD_UNLOCK(&module->p2p_lock);

    MEMCHECKER(
        /* Here we need restore the initial states of memory. */
        opal_memchecker_base_mem_defined( module->p2p_win->w_baseptr, module->p2p_win->w_size);
    );
    if (0 == count) {
        if (need_unlock) ompi_osc_pt2pt_passive_unlock_complete(module);
        opal_condition_broadcast(&module->p2p_cond);        
    }
}


/**********************************************************************
 *
 * Sending a sendreq to target
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_sendreq_send_long_cb(ompi_osc_pt2pt_mpireq_t *mpireq)
{
    ompi_osc_pt2pt_longreq_t *longreq = 
        (ompi_osc_pt2pt_longreq_t*) mpireq;
    ompi_osc_pt2pt_sendreq_t *sendreq = 
        (ompi_osc_pt2pt_sendreq_t*) longreq->mpireq.cbdata;
    int32_t count;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                         "%d completed long sendreq to %d",
                         ompi_comm_rank(sendreq->req_module->p2p_comm),
                         sendreq->req_target_rank));

    OPAL_THREAD_LOCK(&sendreq->req_module->p2p_lock);
    count = (sendreq->req_module->p2p_num_pending_out -= 1);
    OPAL_THREAD_UNLOCK(&sendreq->req_module->p2p_lock);

    ompi_osc_pt2pt_longreq_free(longreq);
    ompi_osc_pt2pt_sendreq_free(sendreq);

    if (0 == count) opal_condition_broadcast(&sendreq->req_module->p2p_cond);
}


static void
ompi_osc_pt2pt_sendreq_send_cb(ompi_osc_pt2pt_mpireq_t *mpireq)
{
    ompi_osc_pt2pt_buffer_t *buffer = 
        (ompi_osc_pt2pt_buffer_t*) mpireq;
    ompi_osc_pt2pt_sendreq_t *sendreq = 
        (ompi_osc_pt2pt_sendreq_t*) mpireq->cbdata;
    ompi_osc_pt2pt_send_header_t *header =
        (ompi_osc_pt2pt_send_header_t*) buffer->payload;
    int32_t count;

    /* have to look at header, and not the sendreq because in the case
       of get, it's possible that the sendreq has been freed already
       (if the remote side replies before we get our send completion
       callback) and already allocated to another request.  We don't
       wait for this completion before exiting a synchronization point
       in the case of get, as we really don't care when it completes -
       only when the data arrives. */
    if (OMPI_OSC_PT2PT_HDR_GET != header->hdr_base.hdr_type) {
#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
            OMPI_OSC_PT2PT_SEND_HDR_NTOH(*header);
        }
#endif
        /* do we need to post a send? */
        if (header->hdr_msg_length != 0) {
            /* sendreq is done.  Mark it as so and get out of here */
            OPAL_THREAD_LOCK(&sendreq->req_module->p2p_lock);
            count = (sendreq->req_module->p2p_num_pending_out -= 1);
            OPAL_THREAD_UNLOCK(&sendreq->req_module->p2p_lock);
            ompi_osc_pt2pt_sendreq_free(sendreq);
            if (0 == count) opal_condition_broadcast(&sendreq->req_module->p2p_cond);
        }
    }
    
    /* release the buffer */
    OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                          &mpireq->super);
}


/* create the initial fragment, pack header, datatype, and payload (if
   size fits) and send */
int
ompi_osc_pt2pt_sendreq_send(ompi_osc_pt2pt_module_t *module,
                            ompi_osc_pt2pt_sendreq_t *sendreq)
{
    int ret = OMPI_SUCCESS;
    opal_free_list_item_t *item = NULL;
    ompi_osc_pt2pt_send_header_t *header = NULL;
    ompi_osc_pt2pt_buffer_t *buffer = NULL;
    size_t written_data = 0;
    size_t needed_len = sizeof(ompi_osc_pt2pt_send_header_t);
    const void *packed_ddt;
    size_t packed_ddt_len = ompi_datatype_pack_description_length(sendreq->req_target_datatype);

    /* we always need to send the ddt */
    needed_len += packed_ddt_len;
    if (OMPI_OSC_PT2PT_GET != sendreq->req_type) {
        needed_len += sendreq->req_origin_bytes_packed;
    }

    /* verify at least enough space for header */
    if (mca_osc_pt2pt_component.p2p_c_eager_size
        < sizeof(ompi_osc_pt2pt_send_header_t) + packed_ddt_len) {
        ret = MPI_ERR_TRUNCATE;
        goto cleanup;
    }

    /* Get a buffer */
    OPAL_FREE_LIST_GET(&mca_osc_pt2pt_component.p2p_c_buffers,
                       item, ret);
    if (NULL == item) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    buffer = (ompi_osc_pt2pt_buffer_t*) item;

    /* setup buffer */
    buffer->mpireq.cbfunc = ompi_osc_pt2pt_sendreq_send_cb;
    buffer->mpireq.cbdata = (void*) sendreq;

    /* pack header */
    header = (ompi_osc_pt2pt_send_header_t*) buffer->payload;
    written_data += sizeof(ompi_osc_pt2pt_send_header_t);
    header->hdr_base.hdr_flags = 0;
    header->hdr_origin = ompi_comm_rank(sendreq->req_module->p2p_comm);
    header->hdr_origin_sendreq.pval = (void*) sendreq;
    header->hdr_origin_tag = 0;
    header->hdr_target_disp = sendreq->req_target_disp;
    header->hdr_target_count = sendreq->req_target_count;

    switch (sendreq->req_type) {
    case OMPI_OSC_PT2PT_PUT:
        header->hdr_base.hdr_type = OMPI_OSC_PT2PT_HDR_PUT;
#if OPAL_ENABLE_MEM_DEBUG
        header->hdr_target_op = 0;
#endif
        break;

    case OMPI_OSC_PT2PT_ACC:
        header->hdr_base.hdr_type = OMPI_OSC_PT2PT_HDR_ACC;
        header->hdr_target_op = sendreq->req_op_id;
        break;

    case OMPI_OSC_PT2PT_GET:
        header->hdr_base.hdr_type = OMPI_OSC_PT2PT_HDR_GET;
#if OPAL_ENABLE_MEM_DEBUG
        header->hdr_target_op = 0;
#endif
        break;
    }

    /* Set datatype id and / or pack datatype */
    ret = ompi_datatype_get_pack_description(sendreq->req_target_datatype, &packed_ddt);
    if (OMPI_SUCCESS != ret) goto cleanup;
    memcpy((unsigned char*) buffer->payload + written_data,
           packed_ddt, packed_ddt_len);
    written_data += packed_ddt_len;
 
    if (OMPI_OSC_PT2PT_GET != sendreq->req_type) {
        /* if sending data and it fits, pack payload */
        if (mca_osc_pt2pt_component.p2p_c_eager_size >=
            written_data + sendreq->req_origin_bytes_packed) {
            struct iovec iov;
            uint32_t iov_count = 1;
            size_t max_data = sendreq->req_origin_bytes_packed;

            iov.iov_len = max_data;
            iov.iov_base = (IOVBASE_TYPE*)((unsigned char*) buffer->payload + written_data);
            MEMCHECKER(
                memchecker_convertor_call(&opal_memchecker_base_mem_defined,
                                          &sendreq->req_origin_convertor);
            );
            ret = opal_convertor_pack(&sendreq->req_origin_convertor, &iov, &iov_count,
                                      &max_data );
            MEMCHECKER(
                memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                          &sendreq->req_origin_convertor);
            );
            if (ret < 0) {
                ret = OMPI_ERR_FATAL;
                goto cleanup;
            }

            assert(max_data == sendreq->req_origin_bytes_packed);
            written_data += max_data;

            header->hdr_msg_length = sendreq->req_origin_bytes_packed;
        } else {
            header->hdr_msg_length = 0;
            header->hdr_origin_tag = create_send_tag(module);
        }
    } else {
        header->hdr_msg_length = 0;
    }

    buffer->len = written_data;

#ifdef WORDS_BIGENDIAN
    header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
#elif OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if (sendreq->req_target_proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
        header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
        OMPI_OSC_PT2PT_SEND_HDR_HTON(*header);
    }
#endif

    /* send fragment */
    OPAL_OUTPUT_VERBOSE((51, ompi_osc_base_output,
                         "%d sending sendreq to %d",
                         ompi_comm_rank(sendreq->req_module->p2p_comm),
                         sendreq->req_target_rank));
    ret = MCA_PML_CALL(isend(buffer->payload,
                             buffer->len,
                             MPI_BYTE,
                             sendreq->req_target_rank,
                             CONTROL_MSG_TAG,
                             MCA_PML_BASE_SEND_STANDARD,
                             module->p2p_comm,
                             &buffer->mpireq.request));

    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    opal_list_append(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                     &buffer->mpireq.super.super);
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    /* Need to be fixed.
     * The payload is made undefined due to the isend call.
     */
    MEMCHECKER(
        opal_memchecker_base_mem_defined(buffer->payload, buffer->len);
    );
    if (OMPI_OSC_PT2PT_GET != sendreq->req_type && 
        header->hdr_msg_length == 0) {
        ompi_osc_pt2pt_longreq_t *longreq;
        ompi_osc_pt2pt_longreq_alloc(&longreq);

        longreq->mpireq.cbfunc = ompi_osc_pt2pt_sendreq_send_long_cb;
        longreq->mpireq.cbdata = sendreq;
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "%d starting long sendreq to %d (%d)",
                             ompi_comm_rank(sendreq->req_module->p2p_comm),
                             sendreq->req_target_rank,
                             header->hdr_origin_tag));

        mca_pml.pml_isend(sendreq->req_origin_convertor.pBaseBuf,
                          sendreq->req_origin_convertor.count,
                          sendreq->req_origin_datatype,
                          sendreq->req_target_rank,
                          header->hdr_origin_tag,
                          MCA_PML_BASE_SEND_STANDARD,
                          sendreq->req_module->p2p_comm,
                          &(longreq->mpireq.request));

        /* put the send request in the waiting list */
        OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
        opal_list_append(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                         &(longreq->mpireq.super.super));
        OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    }

    goto done;

 cleanup:
    if (item != NULL) {
        OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                              item);
    }

 done:
    return ret;
}


/**********************************************************************
 *
 * Sending a replyreq back to origin
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_replyreq_send_long_cb(ompi_osc_pt2pt_mpireq_t *mpireq)
{
    ompi_osc_pt2pt_longreq_t *longreq = 
        (ompi_osc_pt2pt_longreq_t*) mpireq;
    ompi_osc_pt2pt_replyreq_t *replyreq = 
        (ompi_osc_pt2pt_replyreq_t*) mpireq->cbdata;

    inmsg_mark_complete(replyreq->rep_module);

    ompi_osc_pt2pt_longreq_free(longreq);
    ompi_osc_pt2pt_replyreq_free(replyreq);
}


static void
ompi_osc_pt2pt_replyreq_send_cb(ompi_osc_pt2pt_mpireq_t *mpireq)
{
    ompi_osc_pt2pt_buffer_t *buffer = 
        (ompi_osc_pt2pt_buffer_t*) mpireq;
    ompi_osc_pt2pt_replyreq_t *replyreq = 
        (ompi_osc_pt2pt_replyreq_t*) mpireq->cbdata;
    ompi_osc_pt2pt_reply_header_t *header =
        (ompi_osc_pt2pt_reply_header_t*) buffer->payload;

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        if (header->hdr_base.hdr_flags & OMPI_OSC_PT2PT_HDR_FLAG_NBO) {
            OMPI_OSC_PT2PT_REPLY_HDR_NTOH(*header);
        }
#endif

    /* do we need to post a send? */
    if (header->hdr_msg_length != 0) {
        /* sendreq is done.  Mark it as so and get out of here */
        inmsg_mark_complete(replyreq->rep_module);
        ompi_osc_pt2pt_replyreq_free(replyreq);
    }
    
    /* release the descriptor and replyreq */
    OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                          &mpireq->super);
}


int
ompi_osc_pt2pt_replyreq_send(ompi_osc_pt2pt_module_t *module,
                             ompi_osc_pt2pt_replyreq_t *replyreq)
{
    int ret = OMPI_SUCCESS;
    opal_free_list_item_t *item;
    ompi_osc_pt2pt_buffer_t *buffer = NULL;
    ompi_osc_pt2pt_reply_header_t *header = NULL;
    size_t written_data = 0;

    /* Get a buffer */
    OPAL_FREE_LIST_GET(&mca_osc_pt2pt_component.p2p_c_buffers,
                       item, ret);
    if (NULL == item) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    buffer = (ompi_osc_pt2pt_buffer_t*) item;

    /* verify at least enough space for header */
    if (mca_osc_pt2pt_component.p2p_c_eager_size < sizeof(ompi_osc_pt2pt_reply_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup buffer */
    buffer->mpireq.cbfunc = ompi_osc_pt2pt_replyreq_send_cb;
    buffer->mpireq.cbdata = (void*) replyreq;

    /* pack header */
    header = (ompi_osc_pt2pt_reply_header_t*) buffer->payload;
    written_data += sizeof(ompi_osc_pt2pt_reply_header_t);
    header->hdr_base.hdr_type = OMPI_OSC_PT2PT_HDR_REPLY;
    header->hdr_base.hdr_flags = 0;
    header->hdr_origin_sendreq = replyreq->rep_origin_sendreq;
    header->hdr_target_tag = 0;

    /* if sending data fits, pack payload */
    if (mca_osc_pt2pt_component.p2p_c_eager_size >=
        written_data + replyreq->rep_target_bytes_packed) {
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data = replyreq->rep_target_bytes_packed;

        iov.iov_len  = max_data;
        iov.iov_base = (IOVBASE_TYPE*)((unsigned char*) buffer->payload + written_data);
        /* 
         * Before copy to the target buffer, make the target part 
         * accessable.
         */
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_defined,
                                      &replyreq->rep_target_convertor);
        );
        ret = opal_convertor_pack(&replyreq->rep_target_convertor, &iov, &iov_count,
                                  &max_data );
        /* Copy finished, make the target buffer unaccessable. */
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                      &replyreq->rep_target_convertor);
        );

        if (ret < 0) {
            ret = OMPI_ERR_FATAL;
            goto cleanup;
        }

        assert(max_data == replyreq->rep_target_bytes_packed);
        written_data += max_data;

        header->hdr_msg_length = replyreq->rep_target_bytes_packed;
    } else {
        header->hdr_msg_length = 0;
        header->hdr_target_tag = create_send_tag(module);
    }

    buffer->len = written_data;

#ifdef WORDS_BIGENDIAN
    header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
#elif OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if (replyreq->rep_origin_proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
        header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
        OMPI_OSC_PT2PT_REPLY_HDR_HTON(*header);
    }
#endif

    /* send fragment */
    ret = MCA_PML_CALL(isend(buffer->payload,
                             buffer->len,
                             MPI_BYTE,
                             replyreq->rep_origin_rank,
                             CONTROL_MSG_TAG,
                             MCA_PML_BASE_SEND_STANDARD,
                             module->p2p_comm,
                             &buffer->mpireq.request));
    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    opal_list_append(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                     &buffer->mpireq.super.super);
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);

    /* Need to be fixed.
     * The payload is made undefined due to the isend call.
     */
    MEMCHECKER(
        opal_memchecker_base_mem_defined(buffer->payload, buffer->len);
    );
    if (header->hdr_msg_length == 0) {
        ompi_osc_pt2pt_longreq_t *longreq;
        ompi_osc_pt2pt_longreq_alloc(&longreq);

        longreq->mpireq.cbfunc = ompi_osc_pt2pt_replyreq_send_long_cb;
        longreq->mpireq.cbdata = replyreq;

        mca_pml.pml_isend(replyreq->rep_target_convertor.pBaseBuf,
                          replyreq->rep_target_convertor.count,
                          replyreq->rep_target_datatype,
                          replyreq->rep_origin_rank,
                          header->hdr_target_tag,
                          MCA_PML_BASE_SEND_STANDARD,
                          module->p2p_comm,
                          &(longreq->mpireq.request));

        /* put the send request in the waiting list */
        OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
        opal_list_append(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                         &longreq->mpireq.super.super);
        OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    }
    goto done;

 cleanup:
    if (item != NULL) {
        OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                              item);
    }

 done:
    return ret;
}


/**********************************************************************
 *
 * Receive a put on the target side
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_sendreq_recv_put_long_cb(ompi_osc_pt2pt_mpireq_t *mpireq)
{
    ompi_osc_pt2pt_longreq_t *longreq =
        (ompi_osc_pt2pt_longreq_t*) mpireq;

    OBJ_RELEASE(longreq->req_datatype);
    ompi_osc_pt2pt_longreq_free(longreq);

    inmsg_mark_complete(longreq->req_module);
}


int
ompi_osc_pt2pt_sendreq_recv_put(ompi_osc_pt2pt_module_t *module,
                                ompi_osc_pt2pt_send_header_t *header,
                                void *inbuf)
{
    int ret = OMPI_SUCCESS;
    void *target = (unsigned char*) module->p2p_win->w_baseptr + 
        ((unsigned long)header->hdr_target_disp * module->p2p_win->w_disp_unit);    
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->p2p_comm, header->hdr_origin );
    struct ompi_datatype_t *datatype = 
        ompi_osc_base_datatype_create(proc, &inbuf);

    if (NULL == datatype) {
        opal_output(ompi_osc_base_output,
                    "Error recreating datatype.  Aborting.");
        ompi_mpi_abort(module->p2p_comm, 1, false);
    }

    if (header->hdr_msg_length > 0) {
        opal_convertor_t convertor;
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data;
        ompi_proc_t *proc;

        /* create convertor */
        OBJ_CONSTRUCT(&convertor, opal_convertor_t);

        /* initialize convertor */
        proc = ompi_comm_peer_lookup(module->p2p_comm, header->hdr_origin);
        opal_convertor_copy_and_prepare_for_recv(proc->proc_convertor,
                                                 &(datatype->super),
                                                 header->hdr_target_count,
                                                 target,
                                                 0,
                                                 &convertor);
        iov.iov_len = header->hdr_msg_length;
        iov.iov_base = (IOVBASE_TYPE*)inbuf;
        max_data = iov.iov_len;
        /* 
         * Before copy to the user buffer, make the target part 
         * accessable.
         */
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_defined,
                                      &convertor);
        );
        opal_convertor_unpack(&convertor, 
                              &iov,
                              &iov_count,
                              &max_data );
        /* Copy finished, make the user buffer unaccessable. */
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                      &convertor);
        );
        OBJ_DESTRUCT(&convertor);
        OBJ_RELEASE(datatype);
        inmsg_mark_complete(module);
    } else {
            ompi_osc_pt2pt_longreq_t *longreq;
            ompi_osc_pt2pt_longreq_alloc(&longreq);

            longreq->mpireq.cbfunc = ompi_osc_pt2pt_sendreq_recv_put_long_cb;
            longreq->mpireq.cbdata = NULL;
            longreq->req_datatype = datatype;
            longreq->req_module = module;

            ret = mca_pml.pml_irecv(target,
                                    header->hdr_target_count,
                                    datatype,
                                    header->hdr_origin,
                                    header->hdr_origin_tag,
                                    module->p2p_comm,
                                    &(longreq->mpireq.request));

            /* put the send request in the waiting list */
            OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
            opal_list_append(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                             &(longreq->mpireq.super.super));
            OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    }

    return ret;
}


/**********************************************************************
 *
 * Receive an accumulate on the target side
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_sendreq_recv_accum_long_cb(ompi_osc_pt2pt_mpireq_t *mpireq)
{
    ompi_osc_pt2pt_longreq_t *longreq =
        (ompi_osc_pt2pt_longreq_t*) mpireq;
    ompi_osc_pt2pt_module_t *module = longreq->req_module;
    ompi_osc_pt2pt_send_header_t *header = 
        (ompi_osc_pt2pt_send_header_t*) mpireq->cbdata;
    void *payload = (void*) (header + 1);
    int ret;
    void *target = (unsigned char*) module->p2p_win->w_baseptr + 
        ((unsigned long)header->hdr_target_disp * module->p2p_win->w_disp_unit);    

    /* lock the window for accumulates */
    OPAL_THREAD_LOCK(&longreq->req_module->p2p_acc_lock);

    if (longreq->req_op == &ompi_mpi_op_replace.op) {
        opal_convertor_t convertor;
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data;

        /* create convertor */
        OBJ_CONSTRUCT(&convertor, opal_convertor_t);

        /* initialize convertor */
        opal_convertor_copy_and_prepare_for_recv(ompi_proc_local()->proc_convertor,
                                                 &(longreq->req_datatype->super),
                                                 header->hdr_target_count,
                                                 target,
                                                 0,
                                                 &convertor);

        iov.iov_len = header->hdr_msg_length;
        iov.iov_base = (IOVBASE_TYPE*) payload;
        max_data = iov.iov_len;
        opal_convertor_unpack(&convertor, 
                              &iov,
                              &iov_count,
                              &max_data);
        OBJ_DESTRUCT(&convertor);
    } else {        
        /* 
         * Before copy to the user buffer, make the target part 
         * accessable.
         */
        MEMCHECKER(
            opal_memchecker_base_mem_defined( target, header->hdr_msg_length );
        );
        /* copy the data from the temporary buffer into the user window */
        ret = ompi_osc_base_process_op(target,
                                       payload,
                                       header->hdr_msg_length,
                                       longreq->req_datatype,
                                       header->hdr_target_count,
                                       longreq->req_op);
        /* Copy finished, make the user buffer unaccessable. */
        MEMCHECKER(
            opal_memchecker_base_mem_noaccess( target, header->hdr_msg_length );
        );
    }

    /* unlock the window for accumulates */
    OPAL_THREAD_UNLOCK(&longreq->req_module->p2p_acc_lock);
    
    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                         "%d finished receiving long accum message from %d",
                         ompi_comm_rank(longreq->req_module->p2p_comm),
                         header->hdr_origin));

    /* free the temp buffer */
    free(mpireq->cbdata);

    /* Release datatype & op */
    OBJ_RELEASE(longreq->req_datatype);
    OBJ_RELEASE(longreq->req_op);

    inmsg_mark_complete(longreq->req_module);

    ompi_osc_pt2pt_longreq_free(longreq);
}


int
ompi_osc_pt2pt_sendreq_recv_accum(ompi_osc_pt2pt_module_t *module,
                                  ompi_osc_pt2pt_send_header_t *header,
                                  void *payload)
{
    int ret = OMPI_SUCCESS;
    struct ompi_op_t *op = ompi_osc_base_op_create(header->hdr_target_op);
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->p2p_comm, header->hdr_origin );
    struct ompi_datatype_t *datatype = 
        ompi_osc_base_datatype_create(proc, &payload);
    void *target = (unsigned char*) module->p2p_win->w_baseptr + 
        ((unsigned long)header->hdr_target_disp * module->p2p_win->w_disp_unit);    

    if (NULL == datatype) {
        opal_output(ompi_osc_base_output,
                    "Error recreating datatype.  Aborting.");
        ompi_mpi_abort(module->p2p_comm, 1, false);
    }

    if (header->hdr_msg_length > 0) {
        /* lock the window for accumulates */
        OPAL_THREAD_LOCK(&module->p2p_acc_lock);

        if (op == &ompi_mpi_op_replace.op) {
            opal_convertor_t convertor;
            struct iovec iov;
            uint32_t iov_count = 1;
            size_t max_data;

            /* create convertor */
            OBJ_CONSTRUCT(&convertor, opal_convertor_t);

            /* initialize convertor */
            opal_convertor_copy_and_prepare_for_recv(proc->proc_convertor,
                                                     &(datatype->super),
                                                     header->hdr_target_count,
                                                     target,
                                                     0,
                                                     &convertor);

            iov.iov_len = header->hdr_msg_length;
            iov.iov_base = (IOVBASE_TYPE*)payload;
            max_data = iov.iov_len;
            opal_convertor_unpack(&convertor, 
                                  &iov,
                                  &iov_count,
                                  &max_data);
            OBJ_DESTRUCT(&convertor);
        } else {
            void *buffer = NULL;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (proc->proc_arch != ompi_proc_local()->proc_arch) {
                opal_convertor_t convertor;
                struct iovec iov;
                uint32_t iov_count = 1;
                size_t max_data;
                struct ompi_datatype_t *primitive_datatype = NULL;
                uint32_t primitive_count;
                size_t buflen;

                ompi_osc_base_get_primitive_type_info(datatype, &primitive_datatype, &primitive_count);
                primitive_count *= header->hdr_target_count;

                /* figure out how big a buffer we need */
                ompi_datatype_type_size(primitive_datatype, &buflen);
                buflen *= primitive_count;

                /* create convertor */
                OBJ_CONSTRUCT(&convertor, opal_convertor_t);

                buffer = (void*) malloc(buflen);
                if (NULL == buffer) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

                /* initialize convertor */
                opal_convertor_copy_and_prepare_for_recv(proc->proc_convertor,
                                                         &(primitive_datatype->super),
                                                         primitive_count,
                                                         buffer,
                                                         0,
                                                         &convertor);

                iov.iov_len = header->hdr_msg_length;
                iov.iov_base = (IOVBASE_TYPE*)payload;
                max_data = iov.iov_len;
                opal_convertor_unpack(&convertor, 
                                      &iov,
                                      &iov_count,
                                      &max_data);
                OBJ_DESTRUCT(&convertor);
            } else {
                buffer = payload;
            }
#else
            buffer = payload;
#endif
            /* 
             * Before copy to the user buffer, make the target part 
             * accessable.
             */
            MEMCHECKER(
                opal_memchecker_base_mem_defined( target, header->hdr_msg_length );
            );
            /* copy the data from the temporary buffer into the user window */
            ret = ompi_osc_base_process_op(target,
                                           buffer,
                                           header->hdr_msg_length,
                                           datatype,
                                           header->hdr_target_count,
                                           op);
            /* Copy finished, make the user buffer unaccessable. */
            MEMCHECKER(
                opal_memchecker_base_mem_noaccess( target, header->hdr_msg_length );
            );

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (proc->proc_arch != ompi_proc_local()->proc_arch) {
                if (NULL == buffer) free(buffer);
            }
#endif
        }

        /* unlock the window for accumulates */
        OPAL_THREAD_UNLOCK(&module->p2p_acc_lock);

        /* Release datatype & op */
        OBJ_RELEASE(datatype);
        OBJ_RELEASE(op);

        inmsg_mark_complete(module);

        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "%d received accum message from %d",
                             ompi_comm_rank(module->p2p_comm),
                             header->hdr_origin));
    } else {
        ompi_osc_pt2pt_longreq_t *longreq;
        size_t buflen;
        struct ompi_datatype_t *primitive_datatype = NULL;
        uint32_t primitive_count;

        /* get underlying type... */
        ompi_osc_base_get_primitive_type_info(datatype, &primitive_datatype, &primitive_count);
        primitive_count *= header->hdr_target_count;

        /* figure out how big a buffer we need */
        ompi_datatype_type_size(primitive_datatype, &buflen);
        buflen *= primitive_count;

        /* get a longreq and fill it in */
        ompi_osc_pt2pt_longreq_alloc(&longreq);

        longreq->mpireq.cbfunc = ompi_osc_pt2pt_sendreq_recv_accum_long_cb;
        longreq->req_datatype = datatype;
        longreq->req_op = op;
        longreq->req_module = module;

        /* allocate a buffer to receive into ... */
        longreq->mpireq.cbdata = malloc(buflen + sizeof(ompi_osc_pt2pt_send_header_t));
        
        if (NULL == longreq->mpireq.cbdata) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        /* fill in tmp header */
        memcpy(longreq->mpireq.cbdata, header,
               sizeof(ompi_osc_pt2pt_send_header_t));
        ((ompi_osc_pt2pt_send_header_t*) longreq->mpireq.cbdata)->hdr_msg_length = buflen;

        ret = mca_pml.pml_irecv(((char*) longreq->mpireq.cbdata) + sizeof(ompi_osc_pt2pt_send_header_t),
                                primitive_count,
                                primitive_datatype,
                                header->hdr_origin,
                                header->hdr_origin_tag,
                                module->p2p_comm,
                                &(longreq->mpireq.request));

        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "%d started long recv accum message from %d (%d)",
                             ompi_comm_rank(module->p2p_comm),
                             header->hdr_origin,
                             header->hdr_origin_tag));

        /* put the send request in the waiting list */
        OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
        opal_list_append(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                         &(longreq->mpireq.super.super));
        OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    }

    return ret;
}


/**********************************************************************
 *
 * Recveive a get on the origin side
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_replyreq_recv_long_cb(ompi_osc_pt2pt_mpireq_t *mpireq)
{
    ompi_osc_pt2pt_longreq_t *longreq = 
        (ompi_osc_pt2pt_longreq_t*) mpireq;
    ompi_osc_pt2pt_sendreq_t *sendreq =
        (ompi_osc_pt2pt_sendreq_t*) longreq->mpireq.cbdata;
    int32_t count;

    OPAL_THREAD_LOCK(&sendreq->req_module->p2p_lock);
    count = (sendreq->req_module->p2p_num_pending_out -= 1);
    OPAL_THREAD_UNLOCK(&sendreq->req_module->p2p_lock);

    ompi_osc_pt2pt_longreq_free(longreq);
    ompi_osc_pt2pt_sendreq_free(sendreq);

    if (0 == count) opal_condition_broadcast(&sendreq->req_module->p2p_cond);
}

int
ompi_osc_pt2pt_replyreq_recv(ompi_osc_pt2pt_module_t *module,
                             ompi_osc_pt2pt_sendreq_t *sendreq,
                             ompi_osc_pt2pt_reply_header_t *header,
                             void *payload)
{
    int ret = OMPI_SUCCESS;
    int32_t count;

    /* receive into user buffer */
    if (header->hdr_msg_length > 0) {
        /* short message.  woo! */

        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data;

        iov.iov_len  = header->hdr_msg_length;
        iov.iov_base = (IOVBASE_TYPE*)payload;
        max_data     = iov.iov_len;
        /* 
         * Before copy to the target buffer, make the target part 
         * accessable.
         */
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_defined,
                                      &sendreq->req_origin_convertor);
        );
        opal_convertor_unpack(&sendreq->req_origin_convertor,
                              &iov,
                              &iov_count,
                              &max_data );
        /*
         * Copy finished, make the target buffer unaccessable.(Or just leave it accessable?)
         */
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                      &sendreq->req_origin_convertor);
        );

        OPAL_THREAD_LOCK(&module->p2p_lock);
        count = (sendreq->req_module->p2p_num_pending_out -= 1);
        OPAL_THREAD_UNLOCK(&module->p2p_lock);

        ompi_osc_pt2pt_sendreq_free(sendreq);

        if (0 == count) opal_condition_broadcast(&module->p2p_cond);

    } else {
        ompi_osc_pt2pt_longreq_t *longreq;
        ompi_osc_pt2pt_longreq_alloc(&longreq);

        longreq->mpireq.cbfunc = ompi_osc_pt2pt_replyreq_recv_long_cb;
        longreq->mpireq.cbdata = sendreq;
        longreq->req_module = module;

        /* BWB - FIX ME -  George is going to kill me for this */
        ret = mca_pml.pml_irecv(sendreq->req_origin_convertor.pBaseBuf,
                                sendreq->req_origin_convertor.count,
                                sendreq->req_origin_datatype,
                                sendreq->req_target_rank,
                                header->hdr_target_tag,
                                module->p2p_comm,
                                &(longreq->mpireq.request));

        /* put the send request in the waiting list */
        OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
        opal_list_append(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                         &(longreq->mpireq.super.super));
        OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    }

    return ret;
}


/**********************************************************************
 *
 * Control message communication
 *
 **********************************************************************/
static void
ompi_osc_pt2pt_control_send_cb(ompi_osc_pt2pt_mpireq_t *mpireq)
{
    /* release the descriptor and sendreq */
    OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                          &mpireq->super);
}


int
ompi_osc_pt2pt_control_send(ompi_osc_pt2pt_module_t *module,
                            ompi_proc_t *proc,
                            uint8_t type, int32_t value0, int32_t value1)
{
    int ret = OMPI_SUCCESS;
    opal_free_list_item_t *item;
    ompi_osc_pt2pt_buffer_t *buffer = NULL;
    ompi_osc_pt2pt_control_header_t *header = NULL;
    int rank = -1, i;

    /* find the rank */
    for (i = 0 ; i < ompi_comm_size(module->p2p_comm) ; ++i) {
        if (proc == ompi_comm_peer_lookup(module->p2p_comm, i)) {
            rank = i;
        }
    }

    /* Get a buffer */
    OPAL_FREE_LIST_GET(&mca_osc_pt2pt_component.p2p_c_buffers,
                       item, ret);
    if (NULL == item) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }
    buffer = (ompi_osc_pt2pt_buffer_t*) item;

    /* verify at least enough space for header */
    if (mca_osc_pt2pt_component.p2p_c_eager_size < sizeof(ompi_osc_pt2pt_control_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup buffer */
    buffer->mpireq.cbfunc = ompi_osc_pt2pt_control_send_cb;
    buffer->mpireq.cbdata = NULL;
    buffer->len = sizeof(ompi_osc_pt2pt_control_header_t);

    /* pack header */
    header = (ompi_osc_pt2pt_control_header_t*) buffer->payload;
    header->hdr_base.hdr_type = type;
    header->hdr_base.hdr_flags = 0;
    header->hdr_value[0] = value0;
    header->hdr_value[1] = value1;

#ifdef WORDS_BIGENDIAN
    header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
#elif OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if (proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
        header->hdr_base.hdr_flags |= OMPI_OSC_PT2PT_HDR_FLAG_NBO;
        OMPI_OSC_PT2PT_CONTROL_HDR_HTON(*header);
    }
#endif

    /* send fragment */
    ret = MCA_PML_CALL(isend(buffer->payload,
                             buffer->len,
                             MPI_BYTE,
                             rank,
                             CONTROL_MSG_TAG,
                             MCA_PML_BASE_SEND_STANDARD,
                             module->p2p_comm,
                             &buffer->mpireq.request));
    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.p2p_c_lock);
    opal_list_append(&mca_osc_pt2pt_component.p2p_c_pending_requests,
                     &(buffer->mpireq.super.super));
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.p2p_c_lock);

    goto done;

 cleanup:
    if (item != NULL) {
        OPAL_FREE_LIST_RETURN(&mca_osc_pt2pt_component.p2p_c_buffers,
                              item);
    }

 done:
    return ret;
}
