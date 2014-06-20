/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2009-2011 Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_rdma.h"
#include "osc_rdma_sendreq.h"
#include "osc_rdma_header.h"
#include "osc_rdma_data_move.h"
#include "osc_rdma_obj_convert.h"

#include "opal/util/arch.h"
#include "opal/util/output.h"
#include "opal/sys/atomic.h"
#include "opal/align.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/op/op.h"
#include "ompi/memchecker.h"

static inline int32_t
create_send_tag(ompi_osc_rdma_module_t *module)
{
#if OPAL_HAVE_THREAD_SUPPORT && OPAL_HAVE_ATOMIC_CMPSET_32
    int32_t newval, oldval;
    do {
        oldval = module->m_tag_counter;
        newval = (oldval + 1) % mca_pml.pml_max_tag;
    } while (0 == opal_atomic_cmpset_32(&module->m_tag_counter, oldval, newval));
    return newval;
#else
    int32_t ret;
    /* no compare and swap - have to lock the module */
    OPAL_THREAD_LOCK(&module->m_lock);
    module->m_tag_counter = (module->m_tag_counter + 1) % mca_pml.pml_max_tag;
    ret = module->m_tag_counter;
    OPAL_THREAD_UNLOCK(&module->m_lock);
    return ret;
#endif
}


static inline void
inmsg_mark_complete(ompi_osc_rdma_module_t *module)
{
    int32_t count;
    bool need_unlock = false;

    OPAL_THREAD_LOCK(&module->m_lock);
    count = (module->m_num_pending_in -= 1);
    if ((0 != module->m_lock_status) &&
        (opal_list_get_size(&module->m_unlocks_pending) != 0)) {
        need_unlock = true;
    }
    OPAL_THREAD_UNLOCK(&module->m_lock);

    if (0 == count) {
        if (need_unlock) ompi_osc_rdma_passive_unlock_complete(module);
        opal_condition_broadcast(&module->m_cond);        
    }
}

/**********************************************************************
 *
 * Multi-buffer support
 *
 **********************************************************************/
static int
send_multi_buffer(ompi_osc_rdma_module_t *module, int rank)
{
    ompi_osc_rdma_base_header_t *header = (ompi_osc_rdma_base_header_t*)
        ((char*) module->m_pending_buffers[rank].descriptor->des_src[0].seg_addr.pval +
         module->m_pending_buffers[rank].descriptor->des_src[0].seg_len);

    header->hdr_type = OMPI_OSC_RDMA_HDR_MULTI_END;
    header->hdr_flags = 0;

    module->m_pending_buffers[rank].descriptor->des_src[0].seg_len +=
        sizeof(ompi_osc_rdma_base_header_t);
    mca_bml_base_send(module->m_pending_buffers[rank].bml_btl, 
                      module->m_pending_buffers[rank].descriptor, 
                      MCA_BTL_TAG_OSC_RDMA);

    module->m_pending_buffers[rank].descriptor = NULL;
    module->m_pending_buffers[rank].bml_btl = NULL;
    module->m_pending_buffers[rank].remain_len = 0;

    return OMPI_SUCCESS;
}


int
ompi_osc_rdma_flush(ompi_osc_rdma_module_t *module)
{
    int i;

    for (i = 0 ; i < ompi_comm_size(module->m_comm) ; ++i) {
        if (module->m_pending_buffers[i].descriptor != NULL) {
            send_multi_buffer(module, i);
        }
    }

    return OMPI_SUCCESS;
}


/**********************************************************************
 *
 * RDMA data transfers (put / get)
 *
 **********************************************************************/
static void
rdma_cb(struct mca_btl_base_module_t* btl,
        struct mca_btl_base_endpoint_t* endpoint,
        struct mca_btl_base_descriptor_t* descriptor,
        int status)
{
    ompi_osc_rdma_sendreq_t *sendreq = 
        (ompi_osc_rdma_sendreq_t*) descriptor->des_cbdata;
    int32_t out_count, rdma_count;

    assert(OMPI_SUCCESS == status);

    OPAL_THREAD_LOCK(&sendreq->req_module->m_lock);
    out_count = (sendreq->req_module->m_num_pending_out -= 1);
    rdma_count = (sendreq->req_module->m_rdma_num_pending -= 1);
    OPAL_THREAD_UNLOCK(&sendreq->req_module->m_lock);

    btl->btl_free(btl, descriptor);
    ompi_osc_rdma_sendreq_free(sendreq);

    if ((0 == out_count) || (0 == rdma_count)) {
        opal_condition_broadcast(&sendreq->req_module->m_cond);
    }
}


static int
ompi_osc_rdma_sendreq_rdma(ompi_osc_rdma_module_t *module,
                           ompi_osc_rdma_sendreq_t *sendreq)
{
    mca_btl_base_descriptor_t* descriptor;
    ompi_osc_rdma_btl_t *rdma_btl = NULL;
    mca_btl_base_module_t* btl;
    size_t size = sendreq->req_origin_bytes_packed;
    int index, target, ret;

    target = sendreq->req_target_rank;

    if (module->m_peer_info[target].peer_num_btls > 0) {

        index = ++(module->m_peer_info[target].peer_index_btls);
        if (index >= module->m_peer_info[target].peer_num_btls) {
            module->m_peer_info[target].peer_index_btls = 0;
            index = 0;
        }

        rdma_btl = &(module->m_peer_info[target].peer_btls[index]);
        btl = rdma_btl->bml_btl->btl;

        if (sendreq->req_type == OMPI_OSC_RDMA_PUT) {
            mca_bml_base_prepare_src(rdma_btl->bml_btl, NULL,
                    &sendreq->req_origin_convertor, rdma_btl->rdma_order,
                    0, &size, 0, &descriptor);

            assert(NULL != descriptor);

            descriptor->des_dst = sendreq->remote_segs;
            descriptor->des_dst_cnt = 1;
            descriptor->des_dst[0].seg_addr.lval = 
                module->m_peer_info[target].peer_base + 
                ((unsigned long)sendreq->req_target_disp * module->m_win->w_disp_unit);
            descriptor->des_dst[0].seg_len = 
                sendreq->req_origin_bytes_packed;
            descriptor->des_dst[0].seg_key.key64 = 
                rdma_btl->peer_seg_key;
#if 0
            opal_output(0, "putting to %d: 0x%lx(%d), %d, %d",
                        target, descriptor->des_dst[0].seg_addr.lval,
                        descriptor->des_dst[0].seg_len,
                        rdma_btl->rdma_order,
                        descriptor->order);
#endif
            descriptor->des_cbdata = sendreq;
            descriptor->des_cbfunc = rdma_cb;

            ret = btl->btl_put(btl, rdma_btl->bml_btl->btl_endpoint,
                               descriptor);
        } else {
            mca_bml_base_prepare_dst(rdma_btl->bml_btl,
                    NULL, &sendreq->req_origin_convertor, rdma_btl->rdma_order,
                    0, &size, 0, &descriptor);

            assert(NULL != descriptor);

            descriptor->des_src = sendreq->remote_segs;
            descriptor->des_src_cnt = 1;
            descriptor->des_src[0].seg_addr.lval = 
                module->m_peer_info[target].peer_base + 
                ((unsigned long)sendreq->req_target_disp * module->m_win->w_disp_unit);
            descriptor->des_src[0].seg_len = 
                sendreq->req_origin_bytes_packed;
            descriptor->des_src[0].seg_key.key64 = 
                rdma_btl->peer_seg_key;

            descriptor->des_cbdata = sendreq;
            descriptor->des_cbfunc = rdma_cb;

            ret = btl->btl_get(btl, rdma_btl->bml_btl->btl_endpoint,
                               descriptor);
        }
        rdma_btl->rdma_order = descriptor->order;

        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            return ret;
        } else {
            OPAL_THREAD_LOCK(&module->m_lock);
            rdma_btl->num_sent++;
            sendreq->req_module->m_rdma_num_pending += 1;
            OPAL_THREAD_UNLOCK(&module->m_lock);
        }
    } else {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    return OMPI_SUCCESS;
}


/**********************************************************************
 *
 * Sending a sendreq to target
 *
 **********************************************************************/
static void
ompi_osc_rdma_sendreq_send_long_cb(ompi_osc_rdma_longreq_t *longreq)
{
    ompi_osc_rdma_sendreq_t *sendreq = 
        (ompi_osc_rdma_sendreq_t*) longreq->cbdata;
    int32_t count;

    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                         "%d completed long sendreq to %d",
                         ompi_comm_rank(sendreq->req_module->m_comm),
                         sendreq->req_target_rank));

    OPAL_THREAD_LOCK(&sendreq->req_module->m_lock);
    count = (sendreq->req_module->m_num_pending_out -= 1);
    OPAL_THREAD_UNLOCK(&sendreq->req_module->m_lock);

    ompi_osc_rdma_longreq_free(longreq);
    ompi_osc_rdma_sendreq_free(sendreq);

    if (0 == count) opal_condition_broadcast(&sendreq->req_module->m_cond);
}


static void
ompi_osc_rdma_sendreq_send_cb(struct mca_btl_base_module_t* btl, 
                           struct mca_btl_base_endpoint_t *endpoint,
                           struct mca_btl_base_descriptor_t* descriptor,
                           int status)
{
    ompi_osc_rdma_send_header_t *header =
        (ompi_osc_rdma_send_header_t*) descriptor->des_src[0].seg_addr.pval;
    ompi_osc_rdma_sendreq_t *sendreq = NULL;
    ompi_osc_rdma_module_t *module = NULL;
    int32_t count;
    bool done = false;

    if (OMPI_SUCCESS != status) {
        /* requeue and return */
        /* BWB - FIX ME - figure out where to put this bad boy */
        abort();
        return;
    }

    if (header->hdr_base.hdr_type == OMPI_OSC_RDMA_HDR_MULTI_END) {
        done = true;
    }

    while (!done) {
        sendreq = (ompi_osc_rdma_sendreq_t*) header->hdr_origin_sendreq.pval;
        module = sendreq->req_module;

        /* have to look at header, and not the sendreq because in the
           case of get, it's possible that the sendreq has been freed
           already (if the remote side replies before we get our send
           completion callback) and already allocated to another
           request.  We don't wait for this completion before exiting
           a synchronization point in the case of get, as we really
           don't care when it completes - only when the data
           arrives. */
        if (OMPI_OSC_RDMA_HDR_GET != header->hdr_base.hdr_type) {
#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
                OMPI_OSC_RDMA_SEND_HDR_NTOH(*header);
            }
#endif
            /* do we need to post a send? */
            if (header->hdr_msg_length != 0) {
                /* sendreq is done.  Mark it as so and get out of here */
                OPAL_THREAD_LOCK(&sendreq->req_module->m_lock);
                count = sendreq->req_module->m_num_pending_out -= 1;
                OPAL_THREAD_UNLOCK(&sendreq->req_module->m_lock);
                ompi_osc_rdma_sendreq_free(sendreq);
                if (0 == count) {
                    opal_condition_broadcast(&sendreq->req_module->m_cond);
                }
            } else {
                ompi_osc_rdma_longreq_t *longreq;
                ompi_osc_rdma_longreq_alloc(&longreq);
                
                longreq->cbfunc = ompi_osc_rdma_sendreq_send_long_cb;
                longreq->cbdata = sendreq;
                OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                                     "%d starting long sendreq to %d (%d)",
                                     ompi_comm_rank(sendreq->req_module->m_comm),
                                     sendreq->req_target_rank,
                                     header->hdr_origin_tag));
                        
                mca_pml.pml_isend(sendreq->req_origin_convertor.pBaseBuf,
                                  sendreq->req_origin_convertor.count,
                                  sendreq->req_origin_datatype,
                                  sendreq->req_target_rank,
                                  header->hdr_origin_tag,
                                  MCA_PML_BASE_SEND_STANDARD,
                                  sendreq->req_module->m_comm,
                                  &(longreq->request));

                /* put the send request in the waiting list */
                OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
                opal_list_append(&mca_osc_rdma_component.c_pending_requests,
                                 &(longreq->super.super));
                OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
            }
        } else {
            ompi_osc_rdma_sendreq_free(sendreq);
        }

        if (0 == (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_MULTI)) {
            done = true;
        } else {
            /* Find starting point for next header.  Note that the last part
             * added in to compute the starting point for the next header is
             * extra padding that may have been inserted. */
            header = (ompi_osc_rdma_send_header_t*)
                (((char*) header) + 
                 sizeof(ompi_osc_rdma_send_header_t) + 
                 ompi_datatype_pack_description_length(sendreq->req_target_datatype) +
                 header->hdr_msg_length +
                 (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_ALIGN_MASK));

            if (header->hdr_base.hdr_type == OMPI_OSC_RDMA_HDR_MULTI_END) {
                done = true;
            }
        }
    }
    
    /* release the descriptor and sendreq */
    btl->btl_free(btl, descriptor);

    if (opal_list_get_size(&module->m_queued_sendreqs) > 0) {
        opal_list_item_t *item;
        int ret, i, len;

        len = opal_list_get_size(&module->m_queued_sendreqs);
        OPAL_OUTPUT_VERBOSE((40, ompi_osc_base_output,
                             "%d items in restart queue",
                             len));
        for (i = 0 ; i < len ; ++i) {
            OPAL_THREAD_LOCK(&module->m_lock);
            item = opal_list_remove_first(&module->m_queued_sendreqs);
            OPAL_THREAD_UNLOCK(&module->m_lock);
            if (NULL == item) break;

            ret = ompi_osc_rdma_sendreq_send(module, (ompi_osc_rdma_sendreq_t*) item);
            if (OMPI_SUCCESS != ret) {
                OPAL_THREAD_LOCK(&module->m_lock);
                opal_list_append(&(module->m_queued_sendreqs), item);
                OPAL_THREAD_UNLOCK(&module->m_lock);
            }
        }

        /* flush so things actually get sent out and resources restored */
        ompi_osc_rdma_flush(module);
    }
}


/* create the initial fragment, pack header, datatype, and payload (if
   size fits) and send */
int
ompi_osc_rdma_sendreq_send(ompi_osc_rdma_module_t *module,
                            ompi_osc_rdma_sendreq_t *sendreq)
{
    int ret = OMPI_SUCCESS;
    mca_bml_base_endpoint_t *endpoint = NULL;
    mca_bml_base_btl_t *bml_btl = NULL;
    mca_btl_base_module_t* btl = NULL;
    mca_btl_base_descriptor_t *descriptor = NULL;
    ompi_osc_rdma_send_header_t *header = NULL;
    size_t written_data = 0;
    size_t offset;
    size_t needed_len = sizeof(ompi_osc_rdma_send_header_t);
    const void *packed_ddt;
    size_t packed_ddt_len, remain;

    if ((module->m_eager_send_active) && 
        (module->m_use_rdma) &&
        (ompi_datatype_is_contiguous_memory_layout(sendreq->req_target_datatype,
                                              sendreq->req_target_count)) &&
        (!opal_convertor_need_buffers(&sendreq->req_origin_convertor)) &&
        (sendreq->req_type != OMPI_OSC_RDMA_ACC)) {
        ret = ompi_osc_rdma_sendreq_rdma(module, sendreq);
        if (OPAL_LIKELY(OMPI_SUCCESS == ret)) return ret;
    }

    /* we always need to send the ddt */
    packed_ddt_len = ompi_datatype_pack_description_length(sendreq->req_target_datatype);
    needed_len += packed_ddt_len;
    if (OMPI_OSC_RDMA_GET != sendreq->req_type) {
        needed_len += sendreq->req_origin_bytes_packed;
    }

    /* Reuse the buffer if:
     *   - The whole message will fit
     *   - The header and datatype will fit AND the payload would be long anyway
     * Note that if the datatype is too big for an eager, we'll fall
     * through and return an error out of the new buffer case */
    if ((module->m_pending_buffers[sendreq->req_target_rank].remain_len >= needed_len) ||
        ((sizeof(ompi_osc_rdma_send_header_t) + packed_ddt_len <
          module->m_pending_buffers[sendreq->req_target_rank].remain_len) && 
         (needed_len > module->m_pending_buffers[sendreq->req_target_rank].bml_btl->btl->btl_eager_limit))) {
        bml_btl = module->m_pending_buffers[sendreq->req_target_rank].bml_btl;
        descriptor = module->m_pending_buffers[sendreq->req_target_rank].descriptor;
        remain = module->m_pending_buffers[sendreq->req_target_rank].remain_len;
    } else {
        /* send the existing buffer */
        if (module->m_pending_buffers[sendreq->req_target_rank].descriptor) {
            send_multi_buffer(module, sendreq->req_target_rank);
        }

        /* get a buffer... */
        endpoint = (mca_bml_base_endpoint_t*) sendreq->req_target_proc->proc_bml;
        bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
        btl = bml_btl->btl;
        mca_bml_base_alloc(bml_btl, &descriptor, MCA_BTL_NO_ORDER,
                           module->m_use_buffers ? btl->btl_eager_limit :
                           needed_len < btl->btl_eager_limit ? needed_len :
                           btl->btl_eager_limit, MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
        if (NULL == descriptor) {
            ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            goto cleanup;
        }

        /* verify at least enough space for header */
        if (descriptor->des_src[0].seg_len < sizeof(ompi_osc_rdma_send_header_t) + packed_ddt_len) {
            ret = MPI_ERR_TRUNCATE;
            goto cleanup;
        }

        /* setup descriptor */
        descriptor->des_cbfunc = ompi_osc_rdma_sendreq_send_cb;

        module->m_pending_buffers[sendreq->req_target_rank].bml_btl = bml_btl;
        module->m_pending_buffers[sendreq->req_target_rank].descriptor = descriptor;
        module->m_pending_buffers[sendreq->req_target_rank].remain_len = descriptor->des_src[0].seg_len - sizeof(ompi_osc_rdma_base_header_t);
        remain = module->m_pending_buffers[sendreq->req_target_rank].remain_len;
        descriptor->des_src[0].seg_len = 0;
    }

    /* pack header */
    header = (ompi_osc_rdma_send_header_t*) 
        ((char*) descriptor->des_src[0].seg_addr.pval + descriptor->des_src[0].seg_len);
    written_data += sizeof(ompi_osc_rdma_send_header_t);
    header->hdr_base.hdr_flags = 0;
    header->hdr_windx = ompi_comm_get_cid(sendreq->req_module->m_comm);
    header->hdr_origin = ompi_comm_rank(sendreq->req_module->m_comm);
    header->hdr_origin_sendreq.pval = (void*) sendreq;
    header->hdr_origin_tag = 0;
    header->hdr_target_disp = sendreq->req_target_disp;
    header->hdr_target_count = sendreq->req_target_count;

    switch (sendreq->req_type) {
    case OMPI_OSC_RDMA_PUT:
        header->hdr_base.hdr_type = OMPI_OSC_RDMA_HDR_PUT;
#if OPAL_ENABLE_MEM_DEBUG
        header->hdr_target_op = 0;
#endif
        break;

    case OMPI_OSC_RDMA_ACC:
        header->hdr_base.hdr_type = OMPI_OSC_RDMA_HDR_ACC;
        header->hdr_target_op = sendreq->req_op_id;
        break;

    case OMPI_OSC_RDMA_GET:
        header->hdr_base.hdr_type = OMPI_OSC_RDMA_HDR_GET;
#if OPAL_ENABLE_MEM_DEBUG
        header->hdr_target_op = 0;
#endif
        sendreq->req_refcount++;
        break;
    }

    /* Set datatype id and / or pack datatype */
    ret = ompi_datatype_get_pack_description(sendreq->req_target_datatype, &packed_ddt);
    if (OMPI_SUCCESS != ret) goto cleanup;
    memcpy((unsigned char*) descriptor->des_src[0].seg_addr.pval + descriptor->des_src[0].seg_len + written_data,
           packed_ddt, packed_ddt_len);
    written_data += packed_ddt_len;

    if (OMPI_OSC_RDMA_GET != sendreq->req_type) {
        /* if sending data and it fits, pack payload */
        if (remain >= written_data + sendreq->req_origin_bytes_packed) {
            struct iovec iov;
            uint32_t iov_count = 1;
            size_t max_data = sendreq->req_origin_bytes_packed;

            iov.iov_len = max_data;
            iov.iov_base = (IOVBASE_TYPE*)((unsigned char*) descriptor->des_src[0].seg_addr.pval + descriptor->des_src[0].seg_len + written_data);

            ret = opal_convertor_pack(&sendreq->req_origin_convertor, &iov, &iov_count,
                                      &max_data );
            if (ret < 0) {
                ret = OMPI_ERR_FATAL;
                goto cleanup;
            }

            written_data += max_data;
            descriptor->des_src[0].seg_len += written_data;

            header->hdr_msg_length = sendreq->req_origin_bytes_packed;
        } else {
            descriptor->des_src[0].seg_len += written_data;

            header->hdr_msg_length = 0;
            header->hdr_origin_tag = create_send_tag(module);
        }
    } else {
        descriptor->des_src[0].seg_len += written_data;
        header->hdr_msg_length = 0;
    }
    module->m_pending_buffers[sendreq->req_target_rank].remain_len -= written_data;

    if (module->m_use_buffers) {
        header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_MULTI;

        /* When putting multiple messages in a single buffer, the
         * starting point for the next message needs to be aligned with
         * pointer addresses.  Therefore, the pointer, amount written
         * and space remaining are adjusted forward so that the
         * starting position for the next message is aligned properly.
         * The amount of this alignment is embedded in the hdr_flags
         * field so the callback completion and receiving side can
         * also know how much to move the pointer to find the starting
         * point of the next header.  This strict alignment is
         * required by certain platforms like SPARC.  Without it,
         * bus errors can occur.  Keeping things aligned also may
         * offer some performance improvements on other platforms.
         */
        offset = OPAL_ALIGN_PAD_AMOUNT(descriptor->des_src[0].seg_len, sizeof(uint64_t));
        if (0 != offset) {
            header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_ALIGN_MASK & offset;
            descriptor->des_src[0].seg_len += offset;
            written_data += offset;
            module->m_pending_buffers[sendreq->req_target_rank].remain_len -= offset;
        }

#ifdef WORDS_BIGENDIAN
        header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
#elif OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        if (sendreq->req_target_proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
            header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
            OMPI_OSC_RDMA_SEND_HDR_HTON(*header);
        }
#endif

        if (module->m_pending_buffers[sendreq->req_target_rank].remain_len <
            sizeof(ompi_osc_rdma_send_header_t) + 128) {
            /* not enough space left - send now */
            ret = send_multi_buffer(module, sendreq->req_target_rank);
        } else {
            ret = OMPI_SUCCESS;
        }

        goto done;
    } else {
#ifdef WORDS_BIGENDIAN
        header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
#elif OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        if (sendreq->req_target_proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
            header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
            OMPI_OSC_RDMA_SEND_HDR_HTON(*header);
        }
#endif

        /* send fragment */
        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "%d sending sendreq to %d",
                             ompi_comm_rank(sendreq->req_module->m_comm),
                             sendreq->req_target_rank));

        module->m_pending_buffers[sendreq->req_target_rank].bml_btl = NULL;
        module->m_pending_buffers[sendreq->req_target_rank].descriptor = NULL;
        module->m_pending_buffers[sendreq->req_target_rank].remain_len = 0;
        
        ret = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_OSC_RDMA);
        if (1 == ret) ret = OMPI_SUCCESS;
        goto done;
    }

 cleanup:
    if (descriptor != NULL) {
        mca_bml_base_free(bml_btl, descriptor);
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
ompi_osc_rdma_replyreq_send_long_cb(ompi_osc_rdma_longreq_t *longreq)
{
    ompi_osc_rdma_replyreq_t *replyreq = 
        (ompi_osc_rdma_replyreq_t*) longreq->cbdata;

    inmsg_mark_complete(replyreq->rep_module);

    ompi_osc_rdma_longreq_free(longreq);
    ompi_osc_rdma_replyreq_free(replyreq);
}


static void
ompi_osc_rdma_replyreq_send_cb(struct mca_btl_base_module_t* btl, 
                             struct mca_btl_base_endpoint_t *endpoint,
                             struct mca_btl_base_descriptor_t* descriptor,
                             int status)
{
    ompi_osc_rdma_replyreq_t *replyreq = 
        (ompi_osc_rdma_replyreq_t*) descriptor->des_cbdata;
    ompi_osc_rdma_reply_header_t *header =
        (ompi_osc_rdma_reply_header_t*) descriptor->des_src[0].seg_addr.pval;

    if (OMPI_SUCCESS != status) {
        /* requeue and return */
        /* BWB - FIX ME - figure out where to put this bad boy */
        abort();
        return;
    }

#if !defined(WORDS_BIGENDIAN) && OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        if (header->hdr_base.hdr_flags & OMPI_OSC_RDMA_HDR_FLAG_NBO) {
            OMPI_OSC_RDMA_REPLY_HDR_NTOH(*header);
        }
#endif

    /* do we need to post a send? */
    if (header->hdr_msg_length != 0) {
        /* sendreq is done.  Mark it as so and get out of here */
        inmsg_mark_complete(replyreq->rep_module);
        ompi_osc_rdma_replyreq_free(replyreq);
    } else {
            ompi_osc_rdma_longreq_t *longreq;
            ompi_osc_rdma_longreq_alloc(&longreq);

            longreq->cbfunc = ompi_osc_rdma_replyreq_send_long_cb;
            longreq->cbdata = replyreq;

            mca_pml.pml_isend(replyreq->rep_target_convertor.pBaseBuf,
                              replyreq->rep_target_convertor.count,
                              replyreq->rep_target_datatype,
                              replyreq->rep_origin_rank,
                              header->hdr_target_tag,
                              MCA_PML_BASE_SEND_STANDARD,
                              replyreq->rep_module->m_comm,
                              &(longreq->request));

            /* put the send request in the waiting list */
            OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
            opal_list_append(&mca_osc_rdma_component.c_pending_requests,
                             &(longreq->super.super));
            OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
    }
    
    /* release the descriptor and replyreq */
    btl->btl_free(btl, descriptor);
}


int
ompi_osc_rdma_replyreq_send(ompi_osc_rdma_module_t *module,
                             ompi_osc_rdma_replyreq_t *replyreq)
{
    int ret = OMPI_SUCCESS;
    mca_bml_base_endpoint_t *endpoint = NULL;
    mca_bml_base_btl_t *bml_btl = NULL;
    mca_btl_base_descriptor_t *descriptor = NULL;
    ompi_osc_rdma_reply_header_t *header = NULL;
    size_t written_data = 0;
        
    /* Get a BTL and a fragment to go with it */
    endpoint = (mca_bml_base_endpoint_t*) replyreq->rep_origin_proc->proc_bml;
    bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
    mca_bml_base_alloc(bml_btl, &descriptor, MCA_BTL_NO_ORDER,
            bml_btl->btl->btl_eager_limit, MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if (NULL == descriptor) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* verify at least enough space for header */
    if (descriptor->des_src[0].seg_len < sizeof(ompi_osc_rdma_reply_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup descriptor */
    descriptor->des_cbfunc = ompi_osc_rdma_replyreq_send_cb;
    descriptor->des_cbdata = (void*) replyreq;

    /* pack header */
    header = (ompi_osc_rdma_reply_header_t*) descriptor->des_src[0].seg_addr.pval;
    written_data += sizeof(ompi_osc_rdma_reply_header_t);
    header->hdr_base.hdr_type = OMPI_OSC_RDMA_HDR_REPLY;
    header->hdr_base.hdr_flags = 0;
    header->hdr_origin_sendreq = replyreq->rep_origin_sendreq;
    header->hdr_target_tag = 0;

    /* if sending data fits, pack payload */
    if (descriptor->des_src[0].seg_len >=
        written_data + replyreq->rep_target_bytes_packed) {
        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data = replyreq->rep_target_bytes_packed;

        iov.iov_len = max_data;
        iov.iov_base = (IOVBASE_TYPE*)((unsigned char*) descriptor->des_src[0].seg_addr.pval + written_data);

        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_defined,
                                      &replyreq->rep_target_convertor);
        );
        ret = opal_convertor_pack(&replyreq->rep_target_convertor, &iov, &iov_count,
                                  &max_data );
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
        descriptor->des_src[0].seg_len = written_data;

        header->hdr_msg_length = replyreq->rep_target_bytes_packed;
    } else {
        header->hdr_msg_length = 0;
        header->hdr_target_tag = create_send_tag(module);
    }

#ifdef WORDS_BIGENDIAN
    header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
#elif OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if (replyreq->rep_origin_proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
        header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
        OMPI_OSC_RDMA_REPLY_HDR_HTON(*header);
    }
#endif

    /* send fragment */
    ret = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_OSC_RDMA);
    if (1 == ret) ret = OMPI_SUCCESS;
    goto done;

 cleanup:
    if (descriptor != NULL) {
        mca_bml_base_free(bml_btl, descriptor);
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
ompi_osc_rdma_sendreq_recv_put_long_cb(ompi_osc_rdma_longreq_t *longreq)
{
    OBJ_RELEASE(longreq->req_datatype);
    ompi_osc_rdma_longreq_free(longreq);
    
    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                         "%d finished receiving long put message",
                         ompi_comm_rank(longreq->req_module->m_comm))); 

    inmsg_mark_complete(longreq->req_module);
}


int
ompi_osc_rdma_sendreq_recv_put(ompi_osc_rdma_module_t *module,
                               ompi_osc_rdma_send_header_t *header,
                               void **inbuf)
{
    int ret = OMPI_SUCCESS;
    void *target = (unsigned char*) module->m_win->w_baseptr + 
        ((unsigned long)header->hdr_target_disp * module->m_win->w_disp_unit);    
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->m_comm, header->hdr_origin );
    struct ompi_datatype_t *datatype = 
        ompi_osc_base_datatype_create(proc, inbuf);

    if (NULL == datatype) {
        opal_output(ompi_osc_base_output,
                    "Error recreating datatype.  Aborting.");
        ompi_mpi_abort(module->m_comm, 1, false);
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
        proc         = ompi_comm_peer_lookup(module->m_comm, header->hdr_origin);
        opal_convertor_copy_and_prepare_for_recv(proc->proc_convertor,
                                                 &(datatype->super),
                                                 header->hdr_target_count,
                                                 target,
                                                 0,
                                                 &convertor);
        iov.iov_len  = header->hdr_msg_length;
        iov.iov_base = (IOVBASE_TYPE*)*inbuf;
        max_data     = iov.iov_len;
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_defined, &convertor);
        );
        opal_convertor_unpack(&convertor, 
                              &iov,
                              &iov_count,
                              &max_data );
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_noaccess, &convertor);
        );
        OBJ_DESTRUCT(&convertor);
        OBJ_RELEASE(datatype);
        inmsg_mark_complete(module);
        *inbuf = ((char*) *inbuf) + header->hdr_msg_length;

        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "%d received put message from %d",
                             ompi_comm_rank(module->m_comm),
                             header->hdr_origin));

    } else {
        ompi_osc_rdma_longreq_t *longreq;
        ompi_osc_rdma_longreq_alloc(&longreq);

        longreq->cbfunc = ompi_osc_rdma_sendreq_recv_put_long_cb;
        longreq->cbdata = NULL;
        longreq->req_datatype = datatype;
        longreq->req_module = module;

        ret = mca_pml.pml_irecv(target,
                                header->hdr_target_count,
                                datatype,
                                header->hdr_origin,
                                header->hdr_origin_tag,
                                module->m_comm,
                                &(longreq->request));

        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "%d started long recv put message from %d (%d)",
                             ompi_comm_rank(module->m_comm),
                             header->hdr_origin,
                             header->hdr_origin_tag));

        /* put the send request in the waiting list */
        OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
        opal_list_append(&mca_osc_rdma_component.c_pending_requests,
                         &(longreq->super.super));
        OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
    }

    return ret;
}




/**********************************************************************
 *
 * Receive an accumulate on the target side
 *
 **********************************************************************/


static void
ompi_osc_rdma_sendreq_recv_accum_long_cb(ompi_osc_rdma_longreq_t *longreq)
{
    ompi_osc_rdma_send_header_t *header = 
        (ompi_osc_rdma_send_header_t*) longreq->cbdata;
    void *payload = (void*) (header + 1);
    int ret;
    ompi_osc_rdma_module_t *module = longreq->req_module;
    unsigned char *target_buffer =
        (unsigned char*) module->m_win->w_baseptr + 
        ((unsigned long)header->hdr_target_disp * module->m_win->w_disp_unit);

    /* lock the window for accumulates */
    OPAL_THREAD_LOCK(&longreq->req_module->m_acc_lock);

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
                                                 target_buffer,
                                                 0,
                                                 &convertor);

        iov.iov_len = header->hdr_msg_length;
        iov.iov_base = (IOVBASE_TYPE*) payload;
        max_data = iov.iov_len;
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_defined,
                                      &convertor);
        );
        opal_convertor_unpack(&convertor, 
                              &iov,
                              &iov_count,
                              &max_data);
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                      &convertor);
        );
        OBJ_DESTRUCT(&convertor);
    } else {
        /* copy the data from the temporary buffer into the user window */
        ret = ompi_osc_base_process_op(target_buffer,
                                       payload,
                                       header->hdr_msg_length,
                                       longreq->req_datatype,
                                       header->hdr_target_count,
                                       longreq->req_op);
    }

    /* unlock the window for accumulates */
    OPAL_THREAD_UNLOCK(&longreq->req_module->m_acc_lock);
    
    OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                         "%d finished receiving long accum message from %d",
                         ompi_comm_rank(longreq->req_module->m_comm), 
                         header->hdr_origin));

    /* free the temp buffer */
    free(longreq->cbdata);

    /* Release datatype & op */
    OBJ_RELEASE(longreq->req_datatype);
    OBJ_RELEASE(longreq->req_op);

    inmsg_mark_complete(longreq->req_module);

    ompi_osc_rdma_longreq_free(longreq);
}


int
ompi_osc_rdma_sendreq_recv_accum(ompi_osc_rdma_module_t *module,
                                  ompi_osc_rdma_send_header_t *header,
                                  void **payload)
{
    int ret = OMPI_SUCCESS;
    struct ompi_op_t *op = ompi_osc_base_op_create(header->hdr_target_op);
    ompi_proc_t *proc = ompi_comm_peer_lookup( module->m_comm, header->hdr_origin );
    struct ompi_datatype_t *datatype = 
        ompi_osc_base_datatype_create(proc, payload);

    if (NULL == datatype) {
        opal_output(ompi_osc_base_output,
                    "Error recreating datatype.  Aborting.");
        ompi_mpi_abort(module->m_comm, 1, false);
    }

    if (header->hdr_msg_length > 0) {
        unsigned char *target_buffer;

        target_buffer = (unsigned char*) module->m_win->w_baseptr + 
            ((unsigned long)header->hdr_target_disp * module->m_win->w_disp_unit);

        /* lock the window for accumulates */
        OPAL_THREAD_LOCK(&module->m_acc_lock);

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
                                                     target_buffer,
                                                     0,
                                                     &convertor);

            iov.iov_len  = header->hdr_msg_length;
            iov.iov_base = (IOVBASE_TYPE*)*payload;
            max_data     = iov.iov_len;
            MEMCHECKER(
                memchecker_convertor_call(&opal_memchecker_base_mem_defined, &convertor);
            );
            opal_convertor_unpack(&convertor, 
                                  &iov,
                                  &iov_count,
                                  &max_data);
            MEMCHECKER(
                memchecker_convertor_call(&opal_memchecker_base_mem_noaccess, &convertor);
            );
            OBJ_DESTRUCT(&convertor);
        } else {
            void *buffer = NULL;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (proc->proc_arch != ompi_proc_local()->proc_arch) {
                opal_convertor_t convertor;
                struct iovec iov;
                uint32_t iov_count = 1;
                size_t max_data;
                ompi_datatype_t *primitive_datatype = NULL;
                uint32_t primitive_count;
                size_t buflen;

                ompi_osc_base_get_primitive_type_info(datatype, &primitive_datatype, &primitive_count);
                primitive_count *= header->hdr_target_count;

                /* figure out how big a buffer we need */
                ompi_datatype_type_size(primitive_datatype, &buflen);
                buflen *= primitive_count;

                /* create convertor */
                OBJ_CONSTRUCT(&convertor, opal_convertor_t);

                payload = (void*) malloc(buflen);

                /* initialize convertor */
                opal_convertor_copy_and_prepare_for_recv(proc->proc_convertor,
                                                         &(primitive_datatype->super),
                                                         primitive_count,
                                                         buffer,
                                                         0,
                                                         &convertor);

                iov.iov_len  = header->hdr_msg_length;
                iov.iov_base = (IOVBASE_TYPE*)*payload;
                max_data     = iov.iov_len;
                MEMCHECKER(
                    memchecker_convertor_call(&opal_memchecker_base_mem_defined, &convertor);
                );
                opal_convertor_unpack(&convertor, 
                                      &iov,
                                      &iov_count,
                                      &max_data);
                MEMCHECKER(
                    memchecker_convertor_call(&opal_memchecker_base_mem_noaccess, &convertor);
                );
                OBJ_DESTRUCT(&convertor);
            } else {
                buffer = *payload;
            }
#else
            buffer = *payload;
#endif
            /* copy the data from the temporary buffer into the user window */
            ret = ompi_osc_base_process_op(target_buffer,
                                           buffer,
                                           header->hdr_msg_length,
                                           datatype,
                                           header->hdr_target_count,
                                           op);

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
            if (proc->proc_arch != ompi_proc_local()->proc_arch) {
                if (NULL == buffer) free(buffer);
            }
#endif
        }

        /* unlock the window for accumulates */
        OPAL_THREAD_UNLOCK(&module->m_acc_lock);

        /* Release datatype & op */
        OBJ_RELEASE(datatype);
        OBJ_RELEASE(op);

        inmsg_mark_complete(module);

        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "%d received accum message from %d",
                             ompi_comm_rank(module->m_comm),
                             header->hdr_origin));
        *payload = ((char*) *payload) + header->hdr_msg_length;

    } else {
        ompi_osc_rdma_longreq_t *longreq;
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
        ompi_osc_rdma_longreq_alloc(&longreq);

        longreq->cbfunc = ompi_osc_rdma_sendreq_recv_accum_long_cb;
        longreq->req_datatype = datatype;
        longreq->req_op = op;
        longreq->req_module = module;

        /* allocate a buffer to receive into ... */
        longreq->cbdata = malloc(buflen + sizeof(ompi_osc_rdma_send_header_t));
        
        if (NULL == longreq->cbdata) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        /* fill in tmp header */
        memcpy(longreq->cbdata, header,
               sizeof(ompi_osc_rdma_send_header_t));
        ((ompi_osc_rdma_send_header_t*) longreq->cbdata)->hdr_msg_length = buflen;

        ret = mca_pml.pml_irecv(((char*) longreq->cbdata) + sizeof(ompi_osc_rdma_send_header_t),
                                primitive_count,
                                primitive_datatype,
                                header->hdr_origin,
                                header->hdr_origin_tag,
                                module->m_comm,
                                &(longreq->request));

        OPAL_OUTPUT_VERBOSE((50, ompi_osc_base_output,
                             "%d started long recv accum message from %d (%d)",
                             ompi_comm_rank(module->m_comm),
                             header->hdr_origin,
                             header->hdr_origin_tag));

        /* put the send request in the waiting list */
        OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
        opal_list_append(&mca_osc_rdma_component.c_pending_requests,
                         &(longreq->super.super));
        OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
    }

    return ret;
}


/**********************************************************************
 *
 * Recveive a get on the origin side
 *
 **********************************************************************/
static void
ompi_osc_rdma_replyreq_recv_long_cb(ompi_osc_rdma_longreq_t *longreq)
{
    ompi_osc_rdma_sendreq_t *sendreq =
        (ompi_osc_rdma_sendreq_t*) longreq->cbdata;
    int32_t count;

    OPAL_THREAD_LOCK(&sendreq->req_module->m_lock);
    count = (sendreq->req_module->m_num_pending_out -= 1);
    OPAL_THREAD_UNLOCK(&sendreq->req_module->m_lock);

    ompi_osc_rdma_longreq_free(longreq);
    ompi_osc_rdma_sendreq_free(sendreq);

    if (0 == count) opal_condition_broadcast(&sendreq->req_module->m_cond);
}


int
ompi_osc_rdma_replyreq_recv(ompi_osc_rdma_module_t *module,
                             ompi_osc_rdma_sendreq_t *sendreq,
                             ompi_osc_rdma_reply_header_t *header,
                             void **payload)
{
    int ret = OMPI_SUCCESS;

    /* receive into user buffer */
    if (header->hdr_msg_length > 0) {
        /* short message.  woo! */

        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data;
        int32_t count;

        iov.iov_len  = header->hdr_msg_length;
        iov.iov_base = (IOVBASE_TYPE*)*payload;
        max_data     = iov.iov_len;
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_defined,
                                      &sendreq->req_origin_convertor);
        );
        opal_convertor_unpack(&sendreq->req_origin_convertor,
                              &iov,
                              &iov_count,
                              &max_data );
        MEMCHECKER(
            memchecker_convertor_call(&opal_memchecker_base_mem_noaccess,
                                      &sendreq->req_origin_convertor);
        );

        count = sendreq->req_module->m_num_pending_out -= 1;
        ompi_osc_rdma_sendreq_free(sendreq);
        *payload = ((char*) *payload) + header->hdr_msg_length;

        if (0 == count) opal_condition_broadcast(&sendreq->req_module->m_cond);
    } else {
        ompi_osc_rdma_longreq_t *longreq;
        ompi_osc_rdma_longreq_alloc(&longreq);

        longreq->cbfunc = ompi_osc_rdma_replyreq_recv_long_cb;
        longreq->cbdata = sendreq;
        longreq->req_module = module;

        /* BWB - FIX ME -  George is going to kill me for this */
        ret = mca_pml.pml_irecv(sendreq->req_origin_convertor.pBaseBuf,
                                sendreq->req_origin_convertor.count,
                                sendreq->req_origin_datatype,
                                sendreq->req_target_rank,
                                header->hdr_target_tag,
                                module->m_comm,
                                &(longreq->request));

        /* put the send request in the waiting list */
        OPAL_THREAD_LOCK(&mca_osc_rdma_component.c_lock);
        opal_list_append(&mca_osc_rdma_component.c_pending_requests,
                         &(longreq->super.super));
        OPAL_THREAD_UNLOCK(&mca_osc_rdma_component.c_lock);
    }

    return ret;
}


/**********************************************************************
 *
 * Control message communication
 *
 **********************************************************************/
static void
ompi_osc_rdma_control_send_cb(struct mca_btl_base_module_t* btl, 
                               struct mca_btl_base_endpoint_t *endpoint,
                               struct mca_btl_base_descriptor_t* descriptor,
                               int status)
{
    ompi_osc_rdma_control_header_t *header = NULL;

    header = (ompi_osc_rdma_control_header_t*) descriptor->des_src[0].seg_addr.pval;

    /* release the descriptor and sendreq */
    btl->btl_free(btl, descriptor);
}


int
ompi_osc_rdma_control_send(ompi_osc_rdma_module_t *module,
                            ompi_proc_t *proc,
                            uint8_t type, int32_t value0, int32_t value1)
{
    int ret = OMPI_SUCCESS;
    mca_bml_base_endpoint_t *endpoint = NULL;
    mca_bml_base_btl_t *bml_btl = NULL;
    mca_btl_base_descriptor_t *descriptor = NULL;
    ompi_osc_rdma_control_header_t *header = NULL;
        
    /* Get a BTL and a fragment to go with it */
    endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml;
    bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
    mca_bml_base_alloc(bml_btl, &descriptor, MCA_BTL_NO_ORDER,
            sizeof(ompi_osc_rdma_control_header_t),
            MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if (NULL == descriptor) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* verify at least enough space for header */
    if (descriptor->des_src[0].seg_len < sizeof(ompi_osc_rdma_control_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup descriptor */
    descriptor->des_cbfunc = ompi_osc_rdma_control_send_cb;
    descriptor->des_cbdata = NULL;
    descriptor->des_src[0].seg_len = sizeof(ompi_osc_rdma_control_header_t);

    /* pack header */
    header = (ompi_osc_rdma_control_header_t*) descriptor->des_src[0].seg_addr.pval;
    header->hdr_base.hdr_type = type;
    header->hdr_base.hdr_flags = 0;
    header->hdr_value[0] = value0;
    header->hdr_value[1] = value1;
    header->hdr_windx = ompi_comm_get_cid(module->m_comm);

#ifdef WORDS_BIGENDIAN
    header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
#elif OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if (proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
        header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
        OMPI_OSC_RDMA_CONTROL_HDR_HTON(*header);
    }
#endif

    /* send fragment */
    ret = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_OSC_RDMA);
    if (1 == ret) ret = OMPI_SUCCESS;
    goto done;

 cleanup:
    if (descriptor != NULL) {
        mca_bml_base_free(bml_btl, descriptor);
    }

 done:
    return ret;
}


int
ompi_osc_rdma_rdma_ack_send(ompi_osc_rdma_module_t *module,
                            ompi_proc_t *proc,
                            ompi_osc_rdma_btl_t *rdma_btl)
{
    int ret = OMPI_SUCCESS;
    mca_bml_base_btl_t *bml_btl = rdma_btl->bml_btl;
    mca_btl_base_descriptor_t *descriptor = NULL;
    ompi_osc_rdma_control_header_t *header = NULL;
        
    /* Get a BTL and a fragment to go with it */
    mca_bml_base_alloc(bml_btl, &descriptor, rdma_btl->rdma_order,
            sizeof(ompi_osc_rdma_control_header_t),
            MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if (NULL == descriptor) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* verify at least enough space for header */
    if (descriptor->des_src[0].seg_len < sizeof(ompi_osc_rdma_control_header_t)) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* setup descriptor */
    descriptor->des_cbfunc = ompi_osc_rdma_control_send_cb;
    descriptor->des_cbdata = NULL;
    descriptor->des_src[0].seg_len = sizeof(ompi_osc_rdma_control_header_t);

    /* pack header */
    header = (ompi_osc_rdma_control_header_t*) descriptor->des_src[0].seg_addr.pval;
    header->hdr_base.hdr_type = OMPI_OSC_RDMA_HDR_RDMA_COMPLETE;
    header->hdr_base.hdr_flags = 0;
    header->hdr_value[0] = rdma_btl->num_sent;
    header->hdr_value[1] = 0;
    header->hdr_windx = ompi_comm_get_cid(module->m_comm);

#ifdef WORDS_BIGENDIAN
    header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
#elif OPAL_ENABLE_HETEROGENEOUS_SUPPORT
    if (proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) {
        header->hdr_base.hdr_flags |= OMPI_OSC_RDMA_HDR_FLAG_NBO;
        OMPI_OSC_RDMA_CONTROL_HDR_HTON(*header);
    }
#endif

    assert(header->hdr_base.hdr_flags == 0);

    /* send fragment */
    ret = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_OSC_RDMA);
    if (1 == ret) ret = OMPI_SUCCESS;
    goto done;

 cleanup:
    if (descriptor != NULL) {
        mca_bml_base_free(bml_btl, descriptor);
    }

 done:
    return ret;
}
