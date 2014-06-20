/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "opal/prefetch.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/mpool.h" 
#include "pml_ob1.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_rdmafrag.h"
#include "pml_ob1_recvreq.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/memchecker.h"

OBJ_CLASS_INSTANCE(mca_pml_ob1_send_range_t, ompi_free_list_item_t,
        NULL, NULL);

void mca_pml_ob1_send_request_process_pending(mca_bml_base_btl_t *bml_btl)
{
    int i, s = opal_list_get_size(&mca_pml_ob1.send_pending);

    /* advance pending requests */
    for(i = 0; i < s; i++) {
        mca_pml_ob1_send_pending_t pending_type = MCA_PML_OB1_SEND_PENDING_NONE;
        mca_pml_ob1_send_request_t* sendreq;
        mca_bml_base_btl_t *send_dst;

        sendreq = get_request_from_send_pending(&pending_type);
        if(OPAL_UNLIKELY(NULL == sendreq))
            break;

        switch(pending_type) {
        case MCA_PML_OB1_SEND_PENDING_SCHEDULE:
            if(mca_pml_ob1_send_request_schedule_exclusive(sendreq) ==
                    OMPI_ERR_OUT_OF_RESOURCE) {
                return;
            }
            break;
        case MCA_PML_OB1_SEND_PENDING_START:
            send_dst = mca_bml_base_btl_array_find(
                    &sendreq->req_endpoint->btl_eager, bml_btl->btl);
            /* reset the converter */
            MCA_PML_OB1_SEND_REQUEST_RESET(sendreq);
            if( (NULL == send_dst) ||
                (mca_pml_ob1_send_request_start_btl(sendreq, send_dst) ==
                 OMPI_ERR_OUT_OF_RESOURCE) ) {
                /* prepend to the pending list to minimize reordering in case
                 * send_dst != 0 */
                add_request_to_send_pending(sendreq,
                        MCA_PML_OB1_SEND_PENDING_START, NULL == send_dst);
                /* if no destination try next request otherwise give up,
                 * no more resources on this btl */
                if(send_dst != NULL)
                    return;
            }
            break;
        default:
            opal_output(0, "[%s:%d] wrong send request type\n",
                    __FILE__, __LINE__);
            break;
        }
    }
}

/*
 * The free call mark the final stage in a request life-cycle. Starting from this
 * point the request is completed at both PML and user level, and can be used
 * for others p2p communications. Therefore, in the case of the OB1 PML it should
 * be added to the free request list.
 */
static int mca_pml_ob1_send_request_free(struct ompi_request_t** request)
{
    mca_pml_ob1_send_request_t* sendreq = *(mca_pml_ob1_send_request_t**)request;
    
    assert( false == sendreq->req_send.req_base.req_free_called );

    OPAL_THREAD_LOCK(&ompi_request_lock);
    sendreq->req_send.req_base.req_free_called = true;

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_NOTIFY,
                             &(sendreq->req_send.req_base), PERUSE_SEND );

    if( true == sendreq->req_send.req_base.req_pml_complete ) {
        /* make buffer defined when the request is compeleted,
           and before releasing the objects. */
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );

        MCA_PML_OB1_SEND_REQUEST_RETURN( sendreq );
    }

    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_pml_ob1_send_request_cancel(struct ompi_request_t* request, int complete)
{
    /* we dont cancel send requests by now */
    return OMPI_SUCCESS;
}

static void mca_pml_ob1_send_request_construct(mca_pml_ob1_send_request_t* req)
{
    req->req_send.req_base.req_type = MCA_PML_REQUEST_SEND;
    req->req_send.req_base.req_ompi.req_free = mca_pml_ob1_send_request_free;
    req->req_send.req_base.req_ompi.req_cancel = mca_pml_ob1_send_request_cancel;
    req->req_rdma_cnt = 0;
    req->req_throttle_sends = false;
    OBJ_CONSTRUCT(&req->req_send_ranges, opal_list_t);
    OBJ_CONSTRUCT(&req->req_send_range_lock, opal_mutex_t);
}

static void mca_pml_ob1_send_request_destruct(mca_pml_ob1_send_request_t* req)
{
    OBJ_DESTRUCT(&req->req_send_ranges);
    OBJ_DESTRUCT(&req->req_send_range_lock);
}

OBJ_CLASS_INSTANCE( mca_pml_ob1_send_request_t,
                    mca_pml_base_send_request_t,
                    mca_pml_ob1_send_request_construct,
                    mca_pml_ob1_send_request_destruct );

/**
 * Completion of a short message - nothing left to schedule.
 */

static inline void
mca_pml_ob1_match_completion_free_request( mca_bml_base_btl_t* bml_btl,  
                                           mca_pml_ob1_send_request_t* sendreq )
{
    if( sendreq->req_send.req_bytes_packed > 0 ) {
        PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                                 &(sendreq->req_send.req_base), PERUSE_SEND );
    }

    /* signal request completion */
    send_request_pml_complete(sendreq);

    /* check for pending requests */
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

static void
mca_pml_ob1_match_completion_free( struct mca_btl_base_module_t* btl,  
                                   struct mca_btl_base_endpoint_t* ep,
                                   struct mca_btl_base_descriptor_t* des,
                                   int status )
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)des->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context; 

    /* check completion status */
    if( OPAL_UNLIKELY(OMPI_SUCCESS != status) ) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }
    mca_pml_ob1_match_completion_free_request( bml_btl, sendreq );
}

static inline void
mca_pml_ob1_rndv_completion_request( mca_bml_base_btl_t* bml_btl,
                                     mca_pml_ob1_send_request_t* sendreq,
                                     size_t req_bytes_delivered )
{
    if( sendreq->req_send.req_bytes_packed > 0 ) {
        PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                                 &(sendreq->req_send.req_base), PERUSE_SEND );
    }

    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered, req_bytes_delivered);

    /* advance the request */
    OPAL_THREAD_ADD32(&sendreq->req_state, -1);

    send_request_pml_complete_check(sendreq);

    /* check for pending requests */
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

/*
 *  Completion of the first fragment of a long message that 
 *  requires an acknowledgement
 */
static void
mca_pml_ob1_rndv_completion( mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* ep,
                             struct mca_btl_base_descriptor_t* des,
                             int status )
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)des->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    size_t req_bytes_delivered = 0;

    /* check completion status */
    if( OPAL_UNLIKELY(OMPI_SUCCESS != status) ) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }

    /* count bytes of user data actually delivered. As the rndv completion only
     * happens in one thread, the increase of the req_bytes_delivered does not
     * have to be atomic.
     */
    MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( des->des_src,
                                        des->des_src_cnt,
                                        sizeof(mca_pml_ob1_rendezvous_hdr_t),
                                        req_bytes_delivered );

    mca_pml_ob1_rndv_completion_request( bml_btl, sendreq, req_bytes_delivered );
}


/**
 * Completion of a get request.
 */

static void
mca_pml_ob1_rget_completion( mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* ep,
                             struct mca_btl_base_descriptor_t* des,
                             int status )
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)des->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    size_t req_bytes_delivered = 0;

    /* count bytes of user data actually delivered and check for request completion */
    if (OPAL_LIKELY(OMPI_SUCCESS == status)) {
        MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( des->des_src, des->des_src_cnt,
                                            0, req_bytes_delivered );
    }
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered, req_bytes_delivered);

    send_request_pml_complete_check(sendreq);
    /* free the descriptor */
    mca_bml_base_free(bml_btl, des);
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}


/**
 * Completion of a control message - return resources.
 */

static void
mca_pml_ob1_send_ctl_completion( mca_btl_base_module_t* btl,
                                 struct mca_btl_base_endpoint_t* ep,
                                 struct mca_btl_base_descriptor_t* des,
                                 int status )
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context; 

    /* check for pending requests */
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

/**
 * Completion of additional fragments of a large message - may need
 * to schedule additional fragments.
 */

static void
mca_pml_ob1_frag_completion( mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* ep,
                             struct mca_btl_base_descriptor_t* des,
                             int status )
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)des->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context;
    size_t req_bytes_delivered = 0;

    /* check completion status */
    if( OPAL_UNLIKELY(OMPI_SUCCESS != status) ) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }

    /* count bytes of user data actually delivered */
    MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( des->des_src,
                                        des->des_src_cnt,
                                        sizeof(mca_pml_ob1_frag_hdr_t),
                                        req_bytes_delivered );

    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth, -1);
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered, req_bytes_delivered);

    if(send_request_pml_complete_check(sendreq) == false)
        mca_pml_ob1_send_request_schedule(sendreq);

    /* check for pending requests */
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

/**
 *  Buffer the entire message and mark as complete.
 */

int mca_pml_ob1_send_request_start_buffered(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data, req_bytes_delivered;
    int rc;

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &des, 
                       MCA_BTL_NO_ORDER,
                       sizeof(mca_pml_ob1_rendezvous_hdr_t) + size,
                       MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
    if( OPAL_UNLIKELY(NULL == des) ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = des->des_src;

    /* pack the data into the BTL supplied buffer */
    iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)segment->seg_addr.pval + 
                                    sizeof(mca_pml_ob1_rendezvous_hdr_t));
    iov.iov_len = size;
    iov_count = 1;
    max_data = size;
    if((rc = opal_convertor_pack( &sendreq->req_send.req_base.req_convertor,
                                  &iov,
                                  &iov_count,
                                  &max_data)) < 0) {
        mca_bml_base_free(bml_btl, des);
        return rc;
    }
    req_bytes_delivered = max_data;

    /* build rendezvous header */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_rndv.hdr_src_req.pval = sendreq;

    ob1_hdr_hton(hdr, MCA_PML_OB1_HDR_TYPE_RNDV,
                 sendreq->req_send.req_base.req_proc);

    /* update lengths */
    segment->seg_len = sizeof(mca_pml_ob1_rendezvous_hdr_t) + max_data;

    des->des_cbfunc = mca_pml_ob1_rndv_completion;
    des->des_cbdata = sendreq;

    /* buffer the remainder of the message */
    rc = mca_pml_base_bsend_request_alloc((ompi_request_t*)sendreq);
    if( OPAL_UNLIKELY(OMPI_SUCCESS != rc) ) {
        mca_bml_base_free(bml_btl, des);
        return rc;
    }

    iov.iov_base = (IOVBASE_TYPE*)(((unsigned char*)sendreq->req_send.req_addr) + max_data);
    iov.iov_len = max_data = sendreq->req_send.req_bytes_packed - max_data;

    if((rc = opal_convertor_pack( &sendreq->req_send.req_base.req_convertor,
                                  &iov,
                                  &iov_count,
                                  &max_data)) < 0) {
        mca_bml_base_free(bml_btl, des);
        return rc;
    }

    /* re-init convertor for packed data */
    opal_convertor_prepare_for_send( &sendreq->req_send.req_base.req_convertor,
                                     &(ompi_mpi_byte.dt.super),
                                     sendreq->req_send.req_bytes_packed,
                                     sendreq->req_send.req_addr );
   
    /* wait for ack and completion */
    sendreq->req_state = 2;

    /* request is complete at mpi level */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    MCA_PML_OB1_SEND_REQUEST_MPI_COMPLETE(sendreq, true);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    /* send */
    rc = mca_bml_base_send(bml_btl, des, MCA_PML_OB1_HDR_TYPE_RNDV);
    if( OPAL_LIKELY( rc >= 0 ) ) {
        if( OPAL_LIKELY( 1 == rc ) ) {
            mca_pml_ob1_rndv_completion_request( bml_btl, sendreq, req_bytes_delivered);
        }
        return OMPI_SUCCESS;
    }
    mca_bml_base_free(bml_btl, des );
    return rc;
}


/**
 *  We work on a buffered request with a size smaller than the eager size 
 *  or the BTL is not able to send the data IN_PLACE. Request a segment
 *  that is used for initial hdr and any eager data. This is used only
 *  from the _START macro.
 */
int mca_pml_ob1_send_request_start_copy( mca_pml_ob1_send_request_t* sendreq,
                                         mca_bml_base_btl_t* bml_btl,
                                         size_t size )
{
    mca_btl_base_descriptor_t* des = NULL;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data = size;
    int rc;

    if(NULL != bml_btl->btl->btl_sendi) {
        mca_pml_ob1_match_hdr_t match;
        match.hdr_common.hdr_flags = 0;
        match.hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;
        match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
        match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        match.hdr_tag = sendreq->req_send.req_base.req_tag;
        match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
        
        ob1_hdr_hton(&match, MCA_PML_OB1_HDR_TYPE_MATCH,
                     sendreq->req_send.req_base.req_proc);

        /* try to send immediately */
        rc = mca_bml_base_sendi( bml_btl, &sendreq->req_send.req_base.req_convertor,
                                 &match, OMPI_PML_OB1_MATCH_HDR_LEN, 
                                 size, MCA_BTL_NO_ORDER, 
                                 MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP,
                                 MCA_PML_OB1_HDR_TYPE_MATCH, 
                                 &des);
        if( OPAL_LIKELY(OMPI_SUCCESS == rc) ) {
            /* signal request completion */
            send_request_pml_complete(sendreq);

            /* check for pending requests */
            MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
            return OMPI_SUCCESS;
        }
    } else { 
        /* allocate descriptor */
        mca_bml_base_alloc( bml_btl, &des,
                            MCA_BTL_NO_ORDER,
                            OMPI_PML_OB1_MATCH_HDR_LEN + size,
                            MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
    }
    if( OPAL_UNLIKELY(NULL == des) ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    segment = des->des_src;

    if(size > 0) {
        /* pack the data into the supplied buffer */
        iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)segment->seg_addr.pval +
                                       OMPI_PML_OB1_MATCH_HDR_LEN);
        iov.iov_len  = size;
        iov_count    = 1;
        /*
         * Before copy the user buffer, make the target part 
         * accessible.
         */
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
        (void)opal_convertor_pack( &sendreq->req_send.req_base.req_convertor,
                                   &iov, &iov_count, &max_data );
         /*
          *  Packing finished, make the user buffer unaccessable.
          */
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_noaccess,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
    }

    
    /* build match header */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;

    ob1_hdr_hton(hdr, MCA_PML_OB1_HDR_TYPE_MATCH,
                 sendreq->req_send.req_base.req_proc);

    /* update lengths */
    segment->seg_len = OMPI_PML_OB1_MATCH_HDR_LEN + max_data;

    /* short message */
    des->des_cbdata = sendreq;
    des->des_cbfunc = mca_pml_ob1_match_completion_free;

    /* send */
    rc = mca_bml_base_send_status(bml_btl, des, MCA_PML_OB1_HDR_TYPE_MATCH);
    if( OPAL_LIKELY( rc >= OMPI_SUCCESS ) ) {
        if( OPAL_LIKELY( 1 == rc ) ) {
            mca_pml_ob1_match_completion_free_request( bml_btl, sendreq );
        }
        return OMPI_SUCCESS;
    }

    if (OMPI_ERR_RESOURCE_BUSY == rc) {
        /* No more resources. Allow the upper level to queue the send */
        rc = OMPI_ERR_OUT_OF_RESOURCE;
    }

    mca_bml_base_free (bml_btl, des);

    return rc;
}

/**
 *  BTL can send directly from user buffer so allow the BTL
 *  to prepare the segment list. Start sending a small message.
 */

int mca_pml_ob1_send_request_start_prepare( mca_pml_ob1_send_request_t* sendreq,
                                            mca_bml_base_btl_t* bml_btl,
                                            size_t size )
{
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    int rc;

    /* prepare descriptor */
    mca_bml_base_prepare_src( bml_btl,
                              NULL,
                              &sendreq->req_send.req_base.req_convertor,
                              MCA_BTL_NO_ORDER,
                              OMPI_PML_OB1_MATCH_HDR_LEN,
                              &size,
                              MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP,
                              &des );
    if( OPAL_UNLIKELY(NULL == des) ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    segment = des->des_src;

    /* build match header */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;

    ob1_hdr_hton(hdr, MCA_PML_OB1_HDR_TYPE_MATCH,
                 sendreq->req_send.req_base.req_proc);

    /* short message */
    des->des_cbfunc = mca_pml_ob1_match_completion_free;
    des->des_cbdata = sendreq;

    /* send */
    rc = mca_bml_base_send(bml_btl, des, MCA_PML_OB1_HDR_TYPE_MATCH); 
    if( OPAL_LIKELY( rc >= 0 ) ) {
        if( OPAL_LIKELY( 1 == rc ) ) {
            mca_pml_ob1_match_completion_free_request( bml_btl, sendreq );
        }
        return OMPI_SUCCESS;
    }
    mca_bml_base_free(bml_btl, des );
    return rc;
}


/**
 *  We have contigous data that is registered - schedule across
 *  available nics.
 */

int mca_pml_ob1_send_request_start_rdma( mca_pml_ob1_send_request_t* sendreq,
                                         mca_bml_base_btl_t* bml_btl,
                                         size_t size )
{
    /*
     * When req_rdma array is constructed the first element of the array always
     * assigned different btl in round robin fashion (if there are more than
     * one RDMA capable BTLs). This way round robin distribution of RDMA
     * operation is achieved.
     */

    mca_btl_base_descriptor_t* des, *src = NULL;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    bool need_local_cb = false;
    int rc;

    bml_btl = sendreq->req_rdma[0].bml_btl;
    if((sendreq->req_rdma_cnt == 1) && (bml_btl->btl_flags & MCA_BTL_FLAGS_GET)) {
        mca_mpool_base_registration_t* reg = sendreq->req_rdma[0].btl_reg;
        size_t i;
        size_t old_position = sendreq->req_send.req_base.req_convertor.bConverted;

        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
        /* prepare source descriptor/segment(s) */
        /* PML owns this descriptor and will free it in */
        /*  get_completion */
        mca_bml_base_prepare_src( bml_btl, 
                                  reg,
                                  &sendreq->req_send.req_base.req_convertor,
                                  MCA_BTL_NO_ORDER,
                                  0,
                                  &size,
                                  0,
                                  &src );
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_noaccess,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
        if( OPAL_UNLIKELY(NULL == src) ) {
            opal_convertor_set_position(&sendreq->req_send.req_base.req_convertor,
                                        &old_position);
            return OMPI_ERR_OUT_OF_RESOURCE;
        } 

        src->des_cbfunc = mca_pml_ob1_rget_completion;
        src->des_cbdata = sendreq;

        /* allocate space for get hdr + segment list */
        mca_bml_base_alloc(bml_btl, &des, MCA_BTL_NO_ORDER,
                           sizeof(mca_pml_ob1_rget_hdr_t) +
                           (sizeof(mca_btl_base_segment_t) * (src->des_src_cnt-1)),
                           MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
        if( OPAL_UNLIKELY(NULL == des) ) {
            opal_convertor_set_position( &sendreq->req_send.req_base.req_convertor,
                                         &old_position );
            mca_bml_base_free(bml_btl, src);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        segment = des->des_src;

        /* build match header */
        hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
        hdr->hdr_common.hdr_flags = MCA_PML_OB1_HDR_FLAGS_CONTIG|MCA_PML_OB1_HDR_FLAGS_PIN;
        hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RGET;
        hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
        hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
        hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
        hdr->hdr_rndv.hdr_src_req.pval = sendreq;
        hdr->hdr_rget.hdr_des.pval = src;
        hdr->hdr_rget.hdr_seg_cnt = src->des_src_cnt;

        ob1_hdr_hton(hdr, MCA_PML_OB1_HDR_TYPE_RGET,
                     sendreq->req_send.req_base.req_proc);

        for( i = 0; i < src->des_src_cnt; i++ ) {
            hdr->hdr_rget.hdr_segs[i].seg_addr.lval = ompi_ptr_ptol(src->des_src[i].seg_addr.pval);
            hdr->hdr_rget.hdr_segs[i].seg_len       = src->des_src[i].seg_len;
            hdr->hdr_rget.hdr_segs[i].seg_key.key64 = src->des_src[i].seg_key.key64;
        }

        des->des_cbfunc = mca_pml_ob1_send_ctl_completion;

        /**
         * Well, it's a get so we will not know when the peer get the data anyway.
         * If we generate the PERUSE event here, at least we will know when do we
         * sent the GET message ...
         */
        if( sendreq->req_send.req_bytes_packed > 0 ) {
            PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                                     &(sendreq->req_send.req_base), PERUSE_SEND );
        }

    } else {

        /* allocate a rendezvous header - dont eager send any data 
         * receiver will schedule rdma put(s) of the entire message
         */

        mca_bml_base_alloc(bml_btl, &des, 
                           MCA_BTL_NO_ORDER,
                           sizeof(mca_pml_ob1_rendezvous_hdr_t),
                           MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
        if( OPAL_UNLIKELY(NULL == des)) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        segment = des->des_src;
            
        /* build hdr */
        hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
        hdr->hdr_common.hdr_flags = MCA_PML_OB1_HDR_FLAGS_CONTIG|MCA_PML_OB1_HDR_FLAGS_PIN;
        hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
        hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
        hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
        hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
        hdr->hdr_rndv.hdr_src_req.pval = sendreq;

        ob1_hdr_hton(hdr, MCA_PML_OB1_HDR_TYPE_RNDV,
                     sendreq->req_send.req_base.req_proc);

        /* update lengths with number of bytes actually packed */
        segment->seg_len = sizeof(mca_pml_ob1_rendezvous_hdr_t);
    
        /* first fragment of a long message */
        des->des_cbfunc = mca_pml_ob1_rndv_completion;
        need_local_cb = true;

        /* wait for ack and completion */
        sendreq->req_state = 2;
    }

    des->des_cbdata = sendreq;

    /* send */
    rc = mca_bml_base_send(bml_btl, des, hdr->hdr_common.hdr_type);
    if( OPAL_LIKELY( rc >= 0 ) ) {
        if( OPAL_LIKELY( 1 == rc ) && (true == need_local_cb)) {
            mca_pml_ob1_rndv_completion_request( bml_btl, sendreq, 0 );
        }
        return OMPI_SUCCESS;
    }
    mca_bml_base_free(bml_btl, des);

    if (NULL != src) {
        mca_bml_base_free (bml_btl, src);
    }

    return rc;
}


/**
 *  Rendezvous is required. Not doing rdma so eager send up to
 *  the btls eager limit.
 */

int mca_pml_ob1_send_request_start_rndv( mca_pml_ob1_send_request_t* sendreq,
                                         mca_bml_base_btl_t* bml_btl,
                                         size_t size,
                                         int flags )
{
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    int rc;

    /* prepare descriptor */
    if(size == 0) {
        mca_bml_base_alloc( bml_btl, 
                            &des, 
                            MCA_BTL_NO_ORDER,
                            sizeof(mca_pml_ob1_rendezvous_hdr_t),
                            MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP ); 
    } else {
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
        mca_bml_base_prepare_src( bml_btl, 
                                  NULL,
                                  &sendreq->req_send.req_base.req_convertor,
                                  MCA_BTL_NO_ORDER,
                                  sizeof(mca_pml_ob1_rendezvous_hdr_t),
                                  &size,
                                  MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP,
                                  &des );
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_noaccess,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
    }

    if( OPAL_UNLIKELY(NULL == des) ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = des->des_src;

    /* build hdr */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_rndv.hdr_src_req.pval = sendreq;

    ob1_hdr_hton(hdr, MCA_PML_OB1_HDR_TYPE_RNDV,
            sendreq->req_send.req_base.req_proc);

    /* first fragment of a long message */
    des->des_cbdata = sendreq;
    des->des_cbfunc = mca_pml_ob1_rndv_completion;

    /* wait for ack and completion */
    sendreq->req_state = 2;

    /* send */
    rc = mca_bml_base_send(bml_btl, des, MCA_PML_OB1_HDR_TYPE_RNDV);
    if( OPAL_LIKELY( rc >= 0 ) ) {
        if( OPAL_LIKELY( 1 == rc ) ) {
            mca_pml_ob1_rndv_completion_request( bml_btl, sendreq, size );
        }
        return OMPI_SUCCESS;
    }
    mca_bml_base_free(bml_btl, des );
    return rc;
}

void mca_pml_ob1_send_request_copy_in_out( mca_pml_ob1_send_request_t *sendreq,
                                           uint64_t send_offset,
                                           uint64_t send_length )
{
    mca_pml_ob1_send_range_t *sr;
    ompi_free_list_item_t *i;
    mca_bml_base_endpoint_t* bml_endpoint = sendreq->req_endpoint;
    int num_btls = mca_bml_base_btl_array_get_size(&bml_endpoint->btl_send);
    int rc = OMPI_SUCCESS, n;
    double weight_total = 0;

    if( OPAL_UNLIKELY(0 == send_length) )
        return;

    OMPI_FREE_LIST_WAIT(&mca_pml_ob1.send_ranges, i, rc);

    sr = (mca_pml_ob1_send_range_t*)i;

    sr->range_send_offset = send_offset;
    sr->range_send_length = send_length;
    sr->range_btl_idx = 0;

    for(n = 0; n < num_btls && n < mca_pml_ob1.max_send_per_range; n++) {
        sr->range_btls[n].bml_btl =
            mca_bml_base_btl_array_get_next(&bml_endpoint->btl_send);
        weight_total += sr->range_btls[n].bml_btl->btl_weight;
    }

    sr->range_btl_cnt = n;
    mca_pml_ob1_calc_weighted_length(sr->range_btls, n, send_length,
            weight_total);

    OPAL_THREAD_LOCK(&sendreq->req_send_range_lock);
    opal_list_append(&sendreq->req_send_ranges, (opal_list_item_t*)sr);
    OPAL_THREAD_UNLOCK(&sendreq->req_send_range_lock);
}

static inline mca_pml_ob1_send_range_t *
get_send_range_nolock(mca_pml_ob1_send_request_t* sendreq)
{
    opal_list_item_t *item;

    item = opal_list_get_first(&sendreq->req_send_ranges);

    if(opal_list_get_end(&sendreq->req_send_ranges) == item)
        return NULL;

    return (mca_pml_ob1_send_range_t*)item;
}

static inline mca_pml_ob1_send_range_t *
get_send_range(mca_pml_ob1_send_request_t* sendreq)
{
    mca_pml_ob1_send_range_t *range;

    OPAL_THREAD_LOCK(&sendreq->req_send_range_lock);
    range = get_send_range_nolock(sendreq);
    OPAL_THREAD_UNLOCK(&sendreq->req_send_range_lock);

    return range;
}

static inline mca_pml_ob1_send_range_t *
get_next_send_range(mca_pml_ob1_send_request_t* sendreq,
        mca_pml_ob1_send_range_t *range)
{
    OPAL_THREAD_LOCK(&sendreq->req_send_range_lock);
    opal_list_remove_item(&sendreq->req_send_ranges, (opal_list_item_t *)range);
    OMPI_FREE_LIST_RETURN(&mca_pml_ob1.send_ranges, &range->base);
    range = get_send_range_nolock(sendreq);
    OPAL_THREAD_UNLOCK(&sendreq->req_send_range_lock);

    return range;
}

/**
 *  Schedule pipeline of send descriptors for the given request.
 *  Up to the rdma threshold. If this is a send based protocol,
 *  the rdma threshold is the end of the message. Otherwise, schedule
 *  fragments up to the threshold to overlap initial registration/setup
 *  costs of the rdma. Only one thread can be inside this function.
 */

int
mca_pml_ob1_send_request_schedule_once(mca_pml_ob1_send_request_t* sendreq)
{ 
    size_t prev_bytes_remaining = 0;
    mca_pml_ob1_send_range_t *range;
    int num_fail = 0;

    /* check pipeline_depth here before attempting to get any locks */
    if(true == sendreq->req_throttle_sends &&
            sendreq->req_pipeline_depth >= mca_pml_ob1.send_pipeline_depth)
        return OMPI_SUCCESS;

    range = get_send_range(sendreq);

    while(range && (false == sendreq->req_throttle_sends ||
            sendreq->req_pipeline_depth < mca_pml_ob1.send_pipeline_depth)) {
        mca_pml_ob1_frag_hdr_t* hdr;
        mca_btl_base_descriptor_t* des;
        int rc, btl_idx;
        size_t size, offset, data_remaining = 0;
        mca_bml_base_btl_t* bml_btl;

        assert(range->range_send_length != 0);

        if(prev_bytes_remaining == range->range_send_length)
            num_fail++;
        else
            num_fail = 0;

        prev_bytes_remaining = range->range_send_length;

        if( OPAL_UNLIKELY(num_fail == range->range_btl_cnt) ) {
            assert(sendreq->req_pending == MCA_PML_OB1_SEND_PENDING_NONE);
            add_request_to_send_pending(sendreq,
                    MCA_PML_OB1_SEND_PENDING_SCHEDULE, true);
            /* Note that request remains locked. send_request_process_pending()
             * function will call shedule_exclusive() directly without taking
             * the lock */
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

cannot_pack:
        do {
            btl_idx = range->range_btl_idx;
            if(++range->range_btl_idx == range->range_btl_cnt)
                range->range_btl_idx = 0;
        } while(!range->range_btls[btl_idx].length);

        bml_btl = range->range_btls[btl_idx].bml_btl;
        /* If there is a remaining data from another BTL that was too small
         * for converter to pack then send it through another BTL */
        range->range_btls[btl_idx].length += data_remaining;
        size = range->range_btls[btl_idx].length;

        /* makes sure that we don't exceed BTL max send size */
        if(bml_btl->btl->btl_max_send_size != 0) {
            size_t max_send_size = bml_btl->btl->btl_max_send_size -
                sizeof(mca_pml_ob1_frag_hdr_t);

            if (size > max_send_size) {
                size = max_send_size;
            }
        }
            
        /* pack into a descriptor */
        offset = (size_t)range->range_send_offset;
        opal_convertor_set_position(&sendreq->req_send.req_base.req_convertor, 
                                    &offset);
        range->range_send_offset = (uint64_t)offset;

        data_remaining = size;
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
        mca_bml_base_prepare_src(bml_btl, NULL,
                                 &sendreq->req_send.req_base.req_convertor,
                                 MCA_BTL_NO_ORDER,
                                 sizeof(mca_pml_ob1_frag_hdr_t),
                                 &size, MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK, &des);
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_noaccess,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );

        if( OPAL_UNLIKELY(des == NULL || size == 0) ) {
            if(des) {
                /* Converter can't pack this chunk. Append to another chunk
                 * from other BTL */
                mca_bml_base_free(bml_btl, des);
                range->range_btls[btl_idx].length -= data_remaining;
                goto cannot_pack;
            }   
            continue;
        }

        des->des_cbfunc = mca_pml_ob1_frag_completion;
        des->des_cbdata = sendreq;

        /* setup header */
        hdr = (mca_pml_ob1_frag_hdr_t*)des->des_src->seg_addr.pval;
        hdr->hdr_common.hdr_flags = 0;
        hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_FRAG;
        hdr->hdr_frag_offset = range->range_send_offset;
        hdr->hdr_src_req.pval = sendreq;
        hdr->hdr_dst_req = sendreq->req_recv;

        ob1_hdr_hton(hdr, MCA_PML_OB1_HDR_TYPE_FRAG,
                sendreq->req_send.req_base.req_proc);

#if OMPI_WANT_PERUSE
         PERUSE_TRACE_COMM_OMPI_EVENT(PERUSE_COMM_REQ_XFER_CONTINUE,
                 &(sendreq->req_send.req_base), size, PERUSE_SEND);
#endif  /* OMPI_WANT_PERUSE */

        /* initiate send - note that this may complete before the call returns */
        rc = mca_bml_base_send(bml_btl, des, MCA_PML_OB1_HDR_TYPE_FRAG);
        if( OPAL_LIKELY(rc >= 0) ) {
            /* update state */
            range->range_btls[btl_idx].length -= size;
            range->range_send_length -= size;
            range->range_send_offset += size;
            OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth, 1);
            if(range->range_send_length == 0) {
                range = get_next_send_range(sendreq, range);
                prev_bytes_remaining = 0;
            }
        } else { 
            mca_bml_base_free(bml_btl,des);
        }
    }

    return OMPI_SUCCESS;
} 


/**
 *  An RDMA put operation has completed:
 *  (1) Update request status and if required set completed
 *  (2) Send FIN control message to the destination 
 */

static void mca_pml_ob1_put_completion( mca_btl_base_module_t* btl,
                                        struct mca_btl_base_endpoint_t* ep,
                                        struct mca_btl_base_descriptor_t* des,
                                        int status )
{
    mca_pml_ob1_rdma_frag_t* frag = (mca_pml_ob1_rdma_frag_t*)des->des_cbdata;
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)frag->rdma_req;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context;

    /* check completion status */
    if( OPAL_UNLIKELY(OMPI_SUCCESS != status) ) {
        /* TSW - FIX */
        ORTE_ERROR_LOG(status);
        orte_errmgr.abort(-1, NULL);
    }

    mca_pml_ob1_send_fin(sendreq->req_send.req_base.req_proc, 
                         bml_btl,
                         frag->rdma_hdr.hdr_rdma.hdr_des,
                         des->order, 0);
    
    /* check for request completion */
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered, frag->rdma_length);

    send_request_pml_complete_check(sendreq);

    MCA_PML_OB1_RDMA_FRAG_RETURN(frag);

    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

int mca_pml_ob1_send_request_put_frag( mca_pml_ob1_rdma_frag_t* frag )
{
    mca_mpool_base_registration_t* reg = NULL;
    mca_bml_base_btl_t* bml_btl = frag->rdma_bml;
    mca_btl_base_descriptor_t* des;
    size_t save_size = frag->rdma_length;
    int rc;

    /* setup descriptor */
    mca_bml_base_prepare_src( bml_btl, 
                              reg,
                              &frag->convertor, 
                              MCA_BTL_NO_ORDER,
                              0,
                              &frag->rdma_length,
                              MCA_BTL_DES_FLAGS_BTL_OWNERSHIP,
                              &des );
    
    if( OPAL_UNLIKELY(NULL == des) ) {
        if(frag->retries < mca_pml_ob1.rdma_put_retries_limit) {
            size_t offset = (size_t)frag->rdma_hdr.hdr_rdma.hdr_rdma_offset;
            frag->rdma_length = save_size; 
            opal_convertor_set_position(&frag->convertor, &offset);
            OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
            opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        } else {
            mca_pml_ob1_send_request_t *sendreq =
                (mca_pml_ob1_send_request_t*)frag->rdma_req;

            /* tell receiver to unregister memory */
            mca_pml_ob1_send_fin(sendreq->req_send.req_base.req_proc,
                    bml_btl, frag->rdma_hdr.hdr_rdma.hdr_des,
                    MCA_BTL_NO_ORDER, 1);

            /* send fragment by copy in/out */
            mca_pml_ob1_send_request_copy_in_out(sendreq,
                    frag->rdma_hdr.hdr_rdma.hdr_rdma_offset, frag->rdma_length);
            /* if a pointer to a receive request is not set it means that
             * ACK was not yet received. Don't schedule sends before ACK */
            if(NULL != sendreq->req_recv.pval)
                mca_pml_ob1_send_request_schedule(sendreq);
        }
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    des->des_dst = frag->rdma_segs;
    des->des_dst_cnt = frag->rdma_hdr.hdr_rdma.hdr_seg_cnt;
    des->des_cbfunc = mca_pml_ob1_put_completion;
    des->des_cbdata = frag;

    PERUSE_TRACE_COMM_OMPI_EVENT( PERUSE_COMM_REQ_XFER_CONTINUE,
                                  &(((mca_pml_ob1_send_request_t*)frag->rdma_req)->req_send.req_base), save_size, PERUSE_SEND );

    rc = mca_bml_base_put(bml_btl, des);
    if( OPAL_UNLIKELY(OMPI_SUCCESS != rc) ) {
        mca_bml_base_free(bml_btl, des);
        frag->rdma_length = save_size;
        if(OMPI_ERR_OUT_OF_RESOURCE == rc) {
            OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
            opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        } else {
            /* TSW - FIX */
            ORTE_ERROR_LOG(rc);
            orte_errmgr.abort(-1, NULL);
        }
    }
    return OMPI_SUCCESS;
}

/**
 *  Receiver has scheduled an RDMA operation:
 *  (1) Allocate an RDMA fragment to maintain the state of the operation
 *  (2) Call BTL prepare_src to pin/prepare source buffers
 *  (3) Queue the RDMA put 
 */

void mca_pml_ob1_send_request_put( mca_pml_ob1_send_request_t* sendreq,
                                   mca_btl_base_module_t* btl, 
                                   mca_pml_ob1_rdma_hdr_t* hdr )
{
    mca_bml_base_endpoint_t *bml_endpoint = sendreq->req_endpoint;
    mca_pml_ob1_rdma_frag_t* frag;
    int rc;
    size_t i, size = 0;

    if(hdr->hdr_common.hdr_flags & MCA_PML_OB1_HDR_TYPE_ACK) { 
        OPAL_THREAD_ADD32(&sendreq->req_state, -1);
    }

    sendreq->req_recv.pval = hdr->hdr_recv_req.pval;

    MCA_PML_OB1_RDMA_FRAG_ALLOC(frag, rc); 

    if( OPAL_UNLIKELY(NULL == frag) ) {
        /* TSW - FIX */
        ORTE_ERROR_LOG(rc);
        orte_errmgr.abort(-1, NULL);
    }

    /* setup fragment */
    for( i = 0; i < hdr->hdr_seg_cnt; i++ ) {
        frag->rdma_segs[i].seg_addr.lval = hdr->hdr_segs[i].seg_addr.lval;
        frag->rdma_segs[i].seg_len       = hdr->hdr_segs[i].seg_len;
        frag->rdma_segs[i].seg_key.key64 = hdr->hdr_segs[i].seg_key.key64;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        if ((sendreq->req_send.req_base.req_proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) !=
            (ompi_proc_local()->proc_arch & OPAL_ARCH_ISBIGENDIAN)) {
            size += opal_swap_bytes4(frag->rdma_segs[i].seg_len);
        } else 
#endif
        {
            size += frag->rdma_segs[i].seg_len;
        }
    }

    frag->rdma_bml = mca_bml_base_btl_array_find(&bml_endpoint->btl_rdma, btl);
    frag->rdma_hdr.hdr_rdma = *hdr;
    frag->rdma_req = sendreq; 
    frag->rdma_ep = bml_endpoint;
    frag->rdma_length = size;
    frag->rdma_state = MCA_PML_OB1_RDMA_PUT;
    frag->reg = NULL;
    frag->retries = 0;

    /* lookup the corresponding registration */
    for(i=0; i<sendreq->req_rdma_cnt; i++) {
       if(sendreq->req_rdma[i].bml_btl == frag->rdma_bml) {
           frag->reg = sendreq->req_rdma[i].btl_reg;
           break;
       }
    } 

    /*  RDMA writes may proceed in parallel to send and to each other, so
     *  create clone of the convertor for each RDMA fragment
     */
    size = hdr->hdr_rdma_offset;
    opal_convertor_clone_with_position(&sendreq->req_send.req_base.req_convertor,
            &frag->convertor, 0, &size);

    mca_pml_ob1_send_request_put_frag(frag);
}

