/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h" 
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/mpool/mpool.h" 
#include "pml_bfo_comm.h"
#include "pml_bfo_recvreq.h"
#include "pml_bfo_recvfrag.h"
#include "pml_bfo_sendreq.h"
#include "pml_bfo_rdmafrag.h"
#include "ompi/mca/bml/base/base.h" 
#include "orte/mca/errmgr/errmgr.h"
#include "opal/util/arch.h"
#include "ompi/memchecker.h"
#if PML_BFO
#include "pml_bfo_failover.h"
#endif /* PML_BFO */

void mca_pml_bfo_recv_request_process_pending(void)
{
    mca_pml_bfo_recv_request_t* recvreq;
    int rc, i, s = (int)opal_list_get_size(&mca_pml_bfo.recv_pending);

    for(i = 0; i < s; i++) {
        OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
        recvreq = (mca_pml_bfo_recv_request_t*)
            opal_list_remove_first(&mca_pml_bfo.recv_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
        if( OPAL_UNLIKELY(NULL == recvreq) )
            break;
        recvreq->req_pending = false;
        rc = mca_pml_bfo_recv_request_schedule_exclusive(recvreq, NULL);
        if(OMPI_ERR_OUT_OF_RESOURCE == rc)
            break;
    }
}

static int mca_pml_bfo_recv_request_free(struct ompi_request_t** request)
{
    mca_pml_bfo_recv_request_t* recvreq = *(mca_pml_bfo_recv_request_t**)request; 

    assert( false == recvreq->req_recv.req_base.req_free_called );

    OPAL_THREAD_LOCK(&ompi_request_lock);
    recvreq->req_recv.req_base.req_free_called = true;

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_NOTIFY,
                             &(recvreq->req_recv.req_base), PERUSE_RECV );

    if( true == recvreq->req_recv.req_base.req_pml_complete ) {
        /* make buffer defined when the request is compeleted,
           and before releasing the objects. */
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            recvreq->req_recv.req_base.req_addr,
                            recvreq->req_recv.req_base.req_count,
                            recvreq->req_recv.req_base.req_datatype);
        );

        MCA_PML_BFO_RECV_REQUEST_RETURN( recvreq );
    }

    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
} 

static int mca_pml_bfo_recv_request_cancel(struct ompi_request_t* ompi_request, int complete)
{
    mca_pml_bfo_recv_request_t* request = (mca_pml_bfo_recv_request_t*)ompi_request;
    mca_pml_bfo_comm_t* comm = request->req_recv.req_base.req_comm->c_pml_comm;

    if( true == ompi_request->req_complete ) { /* way to late to cancel this one */
        /*
         * Receive request completed, make user buffer accessable.
         */
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            request->req_recv.req_base.req_addr,
                            request->req_recv.req_base.req_count,
                            request->req_recv.req_base.req_datatype);
        );
        return OMPI_SUCCESS;
    }

    /* The rest should be protected behind the match logic lock */
    OPAL_THREAD_LOCK(&comm->matching_lock);
    if( OMPI_ANY_TAG == ompi_request->req_status.MPI_TAG ) { /* the match has not been already done */
       if( request->req_recv.req_base.req_peer == OMPI_ANY_SOURCE ) {
          opal_list_remove_item( &comm->wild_receives, (opal_list_item_t*)request );
       } else {
          mca_pml_bfo_comm_proc_t* proc = comm->procs + request->req_recv.req_base.req_peer;
          opal_list_remove_item(&proc->specific_receives, (opal_list_item_t*)request);
       }
       PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_REMOVE_FROM_POSTED_Q,
                                &(request->req_recv.req_base), PERUSE_RECV );
       /**
        * As now the PML is done with this request we have to force the pml_complete
        * to true. Otherwise, the request will never be freed.
        */
       request->req_recv.req_base.req_pml_complete = true;
    }
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
    
    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request->req_status._cancelled = true;
    /* This macro will set the req_complete to true so the MPI Test/Wait* functions
     * on this request will be able to complete. As the status is marked as
     * cancelled the cancel state will be detected.
     */
    MCA_PML_BFO_RECV_REQUEST_MPI_COMPLETE(request);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    /*
     * Receive request cancelled, make user buffer accessable.
     */
    MEMCHECKER(
        memchecker_call(&opal_memchecker_base_mem_defined,
                        request->req_recv.req_base.req_addr,
                        request->req_recv.req_base.req_count,
                        request->req_recv.req_base.req_datatype);
    );
    return OMPI_SUCCESS;
}

static void mca_pml_bfo_recv_request_construct(mca_pml_bfo_recv_request_t* request)
{
    request->req_recv.req_base.req_type = MCA_PML_REQUEST_RECV;
    request->req_recv.req_base.req_ompi.req_free = mca_pml_bfo_recv_request_free;
    request->req_recv.req_base.req_ompi.req_cancel = mca_pml_bfo_recv_request_cancel;
    request->req_rdma_cnt = 0;
    OBJ_CONSTRUCT(&request->lock, opal_mutex_t);
}

OBJ_CLASS_INSTANCE(
    mca_pml_bfo_recv_request_t,
    mca_pml_base_recv_request_t,
    mca_pml_bfo_recv_request_construct,
    NULL);


/*
 * Release resources.
 */

static void mca_pml_bfo_recv_ctl_completion( mca_btl_base_module_t* btl,
                                             struct mca_btl_base_endpoint_t* ep,
                                             struct mca_btl_base_descriptor_t* des,
                                             int status )
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;

#if PML_BFO
    if (btl->btl_flags & MCA_BTL_FLAGS_FAILOVER_SUPPORT) {
        mca_pml_bfo_check_recv_ctl_completion_status(btl, des, status);
    }
    MCA_PML_BFO_CHECK_RECVREQ_EAGER_BML_BTL_RECV_CTL(bml_btl, btl, des);
#endif /* PML_BFO */
    MCA_PML_BFO_PROGRESS_PENDING(bml_btl);
}

/*
 * Put operation has completed remotely - update request status
 */

static void mca_pml_bfo_put_completion( mca_btl_base_module_t* btl,
                                        struct mca_btl_base_endpoint_t* ep,
                                        struct mca_btl_base_descriptor_t* des,
                                        int status )
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    mca_pml_bfo_recv_request_t* recvreq = (mca_pml_bfo_recv_request_t*)des->des_cbdata;
    size_t bytes_received = 0;

    if( OPAL_LIKELY(status == OMPI_SUCCESS) ) {
        MCA_PML_BFO_COMPUTE_SEGMENT_LENGTH( des->des_dst, des->des_dst_cnt,
                                            0, bytes_received );
    }
    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_pipeline_depth,-1);

#if PML_BFO
    btl->btl_free(btl, des);
    MCA_PML_BFO_ERROR_CHECK_ON_FIN_FOR_PUT(recvreq);
    MCA_PML_BFO_CHECK_RECVREQ_EAGER_BML_BTL(bml_btl, btl, recvreq, "PUT");
#else /* PML_BFO */
    mca_bml_base_free(bml_btl, des);
#endif /* PML_BFO */

    /* check completion status */
    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, bytes_received);
    if(recv_request_pml_complete_check(recvreq) == false &&
            recvreq->req_rdma_offset < recvreq->req_send_offset) {
        /* schedule additional rdma operations */
        mca_pml_bfo_recv_request_schedule(recvreq, bml_btl);
    }
    MCA_PML_BFO_PROGRESS_PENDING(bml_btl);
}

/*
 *
 */

int mca_pml_bfo_recv_request_ack_send_btl(
        ompi_proc_t* proc, mca_bml_base_btl_t* bml_btl,
        uint64_t hdr_src_req, void *hdr_dst_req, uint64_t hdr_send_offset,
        bool nordma)
{
    mca_btl_base_descriptor_t* des;
    mca_pml_bfo_ack_hdr_t* ack;
    int rc;

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &des, MCA_BTL_NO_ORDER,
                       sizeof(mca_pml_bfo_ack_hdr_t),
                       MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if( OPAL_UNLIKELY(NULL == des) ) {
        return OMPI_ERR_OUT_OF_RESOURCE; 
    }

    /* fill out header */
    ack = (mca_pml_bfo_ack_hdr_t*)des->des_src->seg_addr.pval;
    ack->hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_ACK;
    ack->hdr_common.hdr_flags = nordma ? MCA_PML_BFO_HDR_FLAGS_NORDMA : 0;
    ack->hdr_src_req.lval = hdr_src_req;
    ack->hdr_dst_req.pval = hdr_dst_req;
    ack->hdr_send_offset = hdr_send_offset;

    bfo_hdr_hton(ack, MCA_PML_BFO_HDR_TYPE_ACK, proc);

    /* initialize descriptor */
    des->des_cbfunc = mca_pml_bfo_recv_ctl_completion;
#if PML_BFO
    des->des_cbdata = hdr_dst_req;
#endif /* PML_BFO */

    rc = mca_bml_base_send(bml_btl, des, MCA_PML_BFO_HDR_TYPE_ACK);
    if( OPAL_LIKELY( rc >= 0 ) ) {
#if PML_BFO
        if ((bml_btl->btl_flags & MCA_BTL_FLAGS_FAILOVER_SUPPORT) &&
            (des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK)) {
            ((mca_pml_bfo_recv_request_t *)hdr_dst_req)->req_events++;
        }
#endif /* PML_BFO */
        return OMPI_SUCCESS;
    }
    mca_bml_base_free(bml_btl, des);
    return OMPI_ERR_OUT_OF_RESOURCE; 
}

static int mca_pml_bfo_recv_request_ack(
    mca_pml_bfo_recv_request_t* recvreq,
    mca_pml_bfo_rendezvous_hdr_t* hdr, 
    size_t bytes_received)
{
    ompi_proc_t* proc = (ompi_proc_t*)recvreq->req_recv.req_base.req_proc;
    mca_bml_base_endpoint_t* bml_endpoint = NULL;

    bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml; 

    /* by default copy everything */
    recvreq->req_send_offset = bytes_received;
    if(hdr->hdr_msg_length > bytes_received) {
        size_t rdma_num = mca_bml_base_btl_array_get_size(&bml_endpoint->btl_rdma);
        /*
         * lookup request buffer to determine if memory is already
         * registered. 
         */

        if(opal_convertor_need_buffers(&recvreq->req_recv.req_base.req_convertor) == 0 &&
           hdr->hdr_match.hdr_common.hdr_flags & MCA_PML_BFO_HDR_FLAGS_CONTIG &&
           rdma_num != 0) {
            unsigned char *base;
            opal_convertor_get_current_pointer( &recvreq->req_recv.req_base.req_convertor, (void**)&(base) );
           
            if(hdr->hdr_match.hdr_common.hdr_flags & MCA_PML_BFO_HDR_FLAGS_PIN)
                recvreq->req_rdma_cnt = mca_pml_bfo_rdma_btls(bml_endpoint,
                        base, recvreq->req_recv.req_bytes_packed,
                        recvreq->req_rdma );
            else
                recvreq->req_rdma_cnt = 0;

            /* memory is already registered on both sides */
            if (recvreq->req_rdma_cnt != 0) {
                recvreq->req_send_offset = hdr->hdr_msg_length;
                /* are rdma devices available for long rdma protocol */
            } else if(bml_endpoint->btl_send_limit < hdr->hdr_msg_length) {
                /* use convertor to figure out the rdma offset for this request */
                recvreq->req_send_offset = hdr->hdr_msg_length - 
                    bml_endpoint->btl_pipeline_send_length;

                if(recvreq->req_send_offset < bytes_received)
                    recvreq->req_send_offset = bytes_received;

                /* use converter to figure out the rdma offset for this
                 * request */
                opal_convertor_set_position(&recvreq->req_recv.req_base.req_convertor,
                        &recvreq->req_send_offset);

                recvreq->req_rdma_cnt =
                    mca_pml_bfo_rdma_pipeline_btls(bml_endpoint,
                            recvreq->req_send_offset - bytes_received,
                            recvreq->req_rdma);
            }
        }
        /* nothing to send by copy in/out - no need to ack */
        if(recvreq->req_send_offset == hdr->hdr_msg_length)
            return OMPI_SUCCESS;
    }
    /* let know to shedule function there is no need to put ACK flag */
    recvreq->req_ack_sent = true;
    return mca_pml_bfo_recv_request_ack_send(proc, hdr->hdr_src_req.lval,
                                             recvreq, recvreq->req_send_offset,
                                             recvreq->req_send_offset == bytes_received);
}

/**
 * Return resources used by the RDMA
 */

static void mca_pml_bfo_rget_completion( mca_btl_base_module_t* btl,
                                         struct mca_btl_base_endpoint_t* ep,
                                         struct mca_btl_base_descriptor_t* des,
                                         int status )
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    mca_pml_bfo_rdma_frag_t* frag = (mca_pml_bfo_rdma_frag_t*)des->des_cbdata;
    mca_pml_bfo_recv_request_t* recvreq = (mca_pml_bfo_recv_request_t*)frag->rdma_req;

#if PML_BFO
    if (btl->btl_flags & MCA_BTL_FLAGS_FAILOVER_SUPPORT) {
        recvreq->req_events--;
    }
#endif /* PML_BFO */
    /* check completion status */
    if( OPAL_UNLIKELY(OMPI_SUCCESS != status) ) {
#if PML_BFO
        MCA_PML_BFO_ERROR_CHECK_ON_RDMA_READ_COMPLETION(recvreq);
#else /* PML_BFO */
        /* TSW - FIX */
        ORTE_ERROR_LOG(status);
        orte_errmgr.abort(-1, NULL);
#endif /* PML_BFO */
    }
#if PML_BFO
    MCA_PML_BFO_SECOND_ERROR_CHECK_ON_RDMA_READ_COMPLETION(recvreq, status, btl);
    MCA_PML_BFO_CHECK_RECVREQ_RDMA_BML_BTL(bml_btl, btl, recvreq, "RDMA write");
#endif /* PML_BFO */

    mca_pml_bfo_send_fin(recvreq->req_recv.req_base.req_proc,
                         bml_btl,
                         frag->rdma_hdr.hdr_rget.hdr_des,
#if PML_BFO
                         des->order, 0, (uint16_t)recvreq->req_msgseq, recvreq->req_restartseq,
                         recvreq->req_recv.req_base.req_comm->c_contextid,
                         recvreq->req_recv.req_base.req_comm->c_my_rank);
#else /* PML_BFO */
                         des->order, 0); 
#endif /* PML_BFO */

    /* is receive request complete */
    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, frag->rdma_length);
    recv_request_pml_complete_check(recvreq);

    MCA_PML_BFO_RDMA_FRAG_RETURN(frag);

    MCA_PML_BFO_PROGRESS_PENDING(bml_btl);
}


/*
 *
 */
int mca_pml_bfo_recv_request_get_frag( mca_pml_bfo_rdma_frag_t* frag )
{
    mca_pml_bfo_recv_request_t* recvreq = (mca_pml_bfo_recv_request_t*)frag->rdma_req;
    mca_bml_base_btl_t* bml_btl = frag->rdma_bml;
    mca_btl_base_descriptor_t* descriptor;
    size_t save_size = frag->rdma_length;
    int rc;

    /* prepare descriptor */
    mca_bml_base_prepare_dst( bml_btl, 
                              NULL,
                              &recvreq->req_recv.req_base.req_convertor,
                              MCA_BTL_NO_ORDER,
                              0,
                              &frag->rdma_length,
                              MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK,
                              &descriptor );
    if( OPAL_UNLIKELY(NULL == descriptor) ) {
        frag->rdma_length = save_size;
        OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
        opal_list_append(&mca_pml_bfo.rdma_pending, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    descriptor->des_src = frag->rdma_segs;
    descriptor->des_src_cnt = frag->rdma_hdr.hdr_rdma.hdr_seg_cnt;
    descriptor->des_cbfunc = mca_pml_bfo_rget_completion;
    descriptor->des_cbdata = frag;

    PERUSE_TRACE_COMM_OMPI_EVENT(PERUSE_COMM_REQ_XFER_CONTINUE,
                                 &(recvreq->req_recv.req_base),
                                 frag->rdma_length, PERUSE_RECV);

    /* queue up get request */
    rc = mca_bml_base_get(bml_btl,descriptor);
    if( OPAL_UNLIKELY(OMPI_SUCCESS != rc) ) {
        if(OMPI_ERR_OUT_OF_RESOURCE == rc) {
            mca_bml_base_free(bml_btl, descriptor);
            OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
            opal_list_append(&mca_pml_bfo.rdma_pending,
                    (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        } else {
            ORTE_ERROR_LOG(rc);
            orte_errmgr.abort(-1, NULL);
        }
    }
#if PML_BFO
    if ((bml_btl->btl_flags & MCA_BTL_FLAGS_FAILOVER_SUPPORT) &&
        (descriptor->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK)) {
        recvreq->req_events++;
    }
#endif /* PML_BFO */

    return OMPI_SUCCESS;
}




/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */

void mca_pml_bfo_recv_request_progress_frag( mca_pml_bfo_recv_request_t* recvreq,
                                             mca_btl_base_module_t* btl,
                                             mca_btl_base_segment_t* segments,
                                             size_t num_segments )
{
    size_t bytes_received = 0, data_offset = 0;
    size_t bytes_delivered __opal_attribute_unused__; /* is being set to zero in MCA_PML_BFO_RECV_REQUEST_UNPACK */
    mca_pml_bfo_hdr_t* hdr = (mca_pml_bfo_hdr_t*)segments->seg_addr.pval;

    MCA_PML_BFO_COMPUTE_SEGMENT_LENGTH( segments, num_segments,
                                        0, bytes_received );
    bytes_received -= sizeof(mca_pml_bfo_frag_hdr_t);
    data_offset     = hdr->hdr_frag.hdr_frag_offset;
    /*
     *  Make user buffer accessable(defined) before unpacking.
     */
    MEMCHECKER(
               memchecker_call(&opal_memchecker_base_mem_defined,
                               recvreq->req_recv.req_base.req_addr,
                               recvreq->req_recv.req_base.req_count,
                               recvreq->req_recv.req_base.req_datatype);
               );
    MCA_PML_BFO_RECV_REQUEST_UNPACK( recvreq,
                                     segments,
                                     num_segments,
                                     sizeof(mca_pml_bfo_frag_hdr_t),
                                     data_offset,
                                     bytes_received,
                                     bytes_delivered );
    /*
     *  Unpacking finished, make the user buffer unaccessable again.
     */
    MEMCHECKER(
               memchecker_call(&opal_memchecker_base_mem_noaccess,
                               recvreq->req_recv.req_base.req_addr,
                               recvreq->req_recv.req_base.req_count,
                               recvreq->req_recv.req_base.req_datatype);
               );
    
    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, bytes_received);
    /* check completion status */
    if(recv_request_pml_complete_check(recvreq) == false &&
            recvreq->req_rdma_offset < recvreq->req_send_offset) {
        /* schedule additional rdma operations */
        mca_pml_bfo_recv_request_schedule(recvreq, NULL);
    }
}

/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */

void mca_pml_bfo_recv_request_progress_rget( mca_pml_bfo_recv_request_t* recvreq,
                                             mca_btl_base_module_t* btl,
                                             mca_btl_base_segment_t* segments,
                                             size_t num_segments )
{
    size_t bytes_received = 0;
    mca_pml_bfo_rget_hdr_t* hdr = (mca_pml_bfo_rget_hdr_t*)segments->seg_addr.pval;
    mca_bml_base_endpoint_t* bml_endpoint = NULL;
    mca_pml_bfo_rdma_frag_t* frag;
    size_t i, size = 0;
    int rc;

    MCA_PML_BFO_COMPUTE_SEGMENT_LENGTH( segments, num_segments,
                                        0, bytes_received );
    recvreq->req_recv.req_bytes_packed = hdr->hdr_rndv.hdr_msg_length;

#if PML_BFO
    recvreq->remote_req_send = hdr->hdr_rndv.hdr_src_req;
#endif /* PML_BFO */
    MCA_PML_BFO_RECV_REQUEST_MATCHED(recvreq, &hdr->hdr_rndv.hdr_match);
    
    /* if receive buffer is not contiguous we can't just RDMA read into it, so
     * fall back to copy in/out protocol. It is a pity because buffer on the
     * sender side is already registered. We need to be smarter here, perhaps
     * do couple of RDMA reads */
    if(opal_convertor_need_buffers(&recvreq->req_recv.req_base.req_convertor) == true) {
        mca_pml_bfo_recv_request_ack(recvreq, &hdr->hdr_rndv, 0);
        return;
    }
    
    MCA_PML_BFO_RDMA_FRAG_ALLOC(frag,rc);
    if( OPAL_UNLIKELY(NULL == frag) ) {
        /* GLB - FIX */
         ORTE_ERROR_LOG(rc);
         orte_errmgr.abort(-1, NULL);
    }

    /* lookup bml datastructures */
    bml_endpoint = (mca_bml_base_endpoint_t*)recvreq->req_recv.req_base.req_proc->proc_bml; 

    /* allocate/initialize a fragment */
    for(i = 0; i < hdr->hdr_seg_cnt; i++) {
        frag->rdma_segs[i] = hdr->hdr_segs[i];
#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        if ((recvreq->req_recv.req_base.req_proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) !=
            (ompi_proc_local()->proc_arch & OPAL_ARCH_ISBIGENDIAN)) {
            size += opal_swap_bytes4(hdr->hdr_segs[i].seg_len);
        } else 
#endif
        {
            size += hdr->hdr_segs[i].seg_len;
        }
    }
#if PML_BFO
    frag->rdma_btl = btl;
#endif /* PML_BFO */
    frag->rdma_bml = mca_bml_base_btl_array_find(&bml_endpoint->btl_rdma, btl);
    if( OPAL_UNLIKELY(NULL == frag->rdma_bml) ) {
        opal_output(0, "[%s:%d] invalid bml for rdma get", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }
    frag->rdma_hdr.hdr_rget = *hdr;
    frag->rdma_req = recvreq;
    frag->rdma_ep = bml_endpoint;
    frag->rdma_length = size;
    frag->rdma_state = MCA_PML_BFO_RDMA_GET;
    frag->reg = NULL;

    mca_pml_bfo_recv_request_get_frag(frag);
    return;
}

/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */

void mca_pml_bfo_recv_request_progress_rndv( mca_pml_bfo_recv_request_t* recvreq,
                                             mca_btl_base_module_t* btl,
                                             mca_btl_base_segment_t* segments,
                                             size_t num_segments )
{
    size_t bytes_received = 0;
    size_t bytes_delivered __opal_attribute_unused__; /* is being set to zero in MCA_PML_BFO_RECV_REQUEST_UNPACK */
    size_t data_offset = 0;
    mca_pml_bfo_hdr_t* hdr = (mca_pml_bfo_hdr_t*)segments->seg_addr.pval;

    MCA_PML_BFO_COMPUTE_SEGMENT_LENGTH( segments, num_segments,
                                        0, bytes_received );
    
    bytes_received -= sizeof(mca_pml_bfo_rendezvous_hdr_t);
    recvreq->req_recv.req_bytes_packed = hdr->hdr_rndv.hdr_msg_length;
    recvreq->remote_req_send = hdr->hdr_rndv.hdr_src_req;
    recvreq->req_rdma_offset = bytes_received;
    MCA_PML_BFO_RECV_REQUEST_MATCHED(recvreq, &hdr->hdr_match);
    mca_pml_bfo_recv_request_ack(recvreq, &hdr->hdr_rndv, bytes_received);
    /**
     * The PUT protocol do not attach any data to the original request.
     * Therefore, we might want to avoid unpacking if there is nothing to
     * unpack.
     */
    if( 0 < bytes_received ) {
        MEMCHECKER(
                   memchecker_call(&opal_memchecker_base_mem_defined,
                                   recvreq->req_recv.req_base.req_addr,
                                   recvreq->req_recv.req_base.req_count,
                                   recvreq->req_recv.req_base.req_datatype);
                   );
        MCA_PML_BFO_RECV_REQUEST_UNPACK( recvreq,
                                         segments,
                                         num_segments,
                                         sizeof(mca_pml_bfo_rendezvous_hdr_t),
                                         data_offset,
                                         bytes_received,
                                         bytes_delivered );
        MEMCHECKER(
                   memchecker_call(&opal_memchecker_base_mem_noaccess,
                                   recvreq->req_recv.req_base.req_addr,
                                   recvreq->req_recv.req_base.req_count,
                                   recvreq->req_recv.req_base.req_datatype);
                   );
    }
    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, bytes_received);
    /* check completion status */
    if(recv_request_pml_complete_check(recvreq) == false &&
       recvreq->req_rdma_offset < recvreq->req_send_offset) {
        /* schedule additional rdma operations */
        mca_pml_bfo_recv_request_schedule(recvreq, NULL);
    }
}

/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */
void mca_pml_bfo_recv_request_progress_match( mca_pml_bfo_recv_request_t* recvreq,
                                              mca_btl_base_module_t* btl,
                                              mca_btl_base_segment_t* segments,
                                              size_t num_segments )
{
    size_t bytes_received = 0, data_offset = 0;
    size_t bytes_delivered __opal_attribute_unused__; /* is being set to zero in MCA_PML_BFO_RECV_REQUEST_UNPACK */
    mca_pml_bfo_hdr_t* hdr = (mca_pml_bfo_hdr_t*)segments->seg_addr.pval;

    MCA_PML_BFO_COMPUTE_SEGMENT_LENGTH( segments, num_segments,
                                        0, bytes_received );
    bytes_received -= OMPI_PML_BFO_MATCH_HDR_LEN;
    recvreq->req_recv.req_bytes_packed = bytes_received;
    
    MCA_PML_BFO_RECV_REQUEST_MATCHED(recvreq, &hdr->hdr_match);
    /*
     *  Make user buffer accessable(defined) before unpacking.
     */
    MEMCHECKER(
               memchecker_call(&opal_memchecker_base_mem_defined,
                               recvreq->req_recv.req_base.req_addr,
                               recvreq->req_recv.req_base.req_count,
                               recvreq->req_recv.req_base.req_datatype);
               );
    MCA_PML_BFO_RECV_REQUEST_UNPACK( recvreq,
                                     segments,
                                     num_segments,
                                     OMPI_PML_BFO_MATCH_HDR_LEN,
                                     data_offset,
                                     bytes_received,
                                     bytes_delivered);
    /*
     *  Unpacking finished, make the user buffer unaccessable again.
     */
    MEMCHECKER(
               memchecker_call(&opal_memchecker_base_mem_noaccess,
                               recvreq->req_recv.req_base.req_addr,
                               recvreq->req_recv.req_base.req_count,
                               recvreq->req_recv.req_base.req_datatype);
               );
    
    /*
     * No need for atomic here, as we know there is only one fragment
     * for this request.
     */
    recvreq->req_bytes_received += bytes_received;
    recv_request_pml_complete(recvreq);
}


/**
 * Handle completion of a probe request
 */

void mca_pml_bfo_recv_request_matched_probe( mca_pml_bfo_recv_request_t* recvreq,
                                             mca_btl_base_module_t* btl,
                                             mca_btl_base_segment_t* segments,
                                             size_t num_segments )
{
    size_t bytes_packed = 0;
    mca_pml_bfo_hdr_t* hdr = (mca_pml_bfo_hdr_t*)segments->seg_addr.pval;

    switch(hdr->hdr_common.hdr_type) {
        case MCA_PML_BFO_HDR_TYPE_MATCH:

            MCA_PML_BFO_COMPUTE_SEGMENT_LENGTH( segments, num_segments,
                                                OMPI_PML_BFO_MATCH_HDR_LEN,
                                                bytes_packed );
            break;

        case MCA_PML_BFO_HDR_TYPE_RNDV:
        case MCA_PML_BFO_HDR_TYPE_RGET:

            bytes_packed = hdr->hdr_rndv.hdr_msg_length;
            break;
    }

    /* set completion status */
    recvreq->req_recv.req_base.req_ompi.req_status.MPI_TAG = hdr->hdr_match.hdr_tag;
    recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE = hdr->hdr_match.hdr_src;
    recvreq->req_bytes_received = bytes_packed;
    recvreq->req_bytes_expected = bytes_packed;
    recv_request_pml_complete(recvreq);
}


/*
 * Schedule RDMA protocol.
 *
*/

int mca_pml_bfo_recv_request_schedule_once( mca_pml_bfo_recv_request_t* recvreq,
                                            mca_bml_base_btl_t *start_bml_btl )
{
    mca_bml_base_btl_t* bml_btl; 
    int num_tries = recvreq->req_rdma_cnt, num_fail = 0;
    size_t i, prev_bytes_remaining = 0;
    size_t bytes_remaining = recvreq->req_send_offset -
        recvreq->req_rdma_offset;

    /* if starting bml_btl is provided schedule next fragment on it first */
    if(start_bml_btl != NULL) {
        for(i = 0; i < recvreq->req_rdma_cnt; i++) {
            if(recvreq->req_rdma[i].bml_btl != start_bml_btl)
                continue;
            /* something left to be send? */
            if( OPAL_LIKELY(recvreq->req_rdma[i].length) )
                recvreq->req_rdma_idx = i;
            break;
        }
    }

    while(bytes_remaining > 0 &&
           recvreq->req_pipeline_depth < mca_pml_bfo.recv_pipeline_depth) {
        size_t hdr_size;
        size_t size;
        mca_pml_bfo_rdma_hdr_t* hdr;
        mca_btl_base_descriptor_t* dst;
        mca_btl_base_descriptor_t* ctl;
        mca_mpool_base_registration_t * reg = NULL;
        mca_btl_base_module_t* btl;
        int rc, rdma_idx;

        if(prev_bytes_remaining == bytes_remaining) {
            if(++num_fail == num_tries) {
                OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
                if(false == recvreq->req_pending) {
                    opal_list_append(&mca_pml_bfo.recv_pending,
                            (opal_list_item_t*)recvreq);
                    recvreq->req_pending = true;
                }
                OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        } else {
            num_fail = 0;
            prev_bytes_remaining = bytes_remaining;
        }

        do {
            rdma_idx = recvreq->req_rdma_idx;
            bml_btl = recvreq->req_rdma[rdma_idx].bml_btl;
            reg = recvreq->req_rdma[rdma_idx].btl_reg;
            size = recvreq->req_rdma[rdma_idx].length;
            if(++recvreq->req_rdma_idx >= recvreq->req_rdma_cnt)
                recvreq->req_rdma_idx = 0;
        } while(!size);
        btl = bml_btl->btl;

        /* makes sure that we don't exceed BTL max rdma size
         * if memory is not pinned already */
        if( (NULL == reg) && (btl->btl_rdma_pipeline_frag_size != 0) &&
                             (size > btl->btl_rdma_pipeline_frag_size)) {
            size = btl->btl_rdma_pipeline_frag_size;
        }

        /* take lock to protect converter against concurrent access
         * from unpack */
        OPAL_THREAD_LOCK(&recvreq->lock);
        opal_convertor_set_position( &recvreq->req_recv.req_base.req_convertor,
                                     &recvreq->req_rdma_offset );

        /* prepare a descriptor for RDMA */
        mca_bml_base_prepare_dst(bml_btl, reg, 
                                 &recvreq->req_recv.req_base.req_convertor,
                                 MCA_BTL_NO_ORDER, 0, &size, MCA_BTL_DES_FLAGS_BTL_OWNERSHIP, &dst);
        OPAL_THREAD_UNLOCK(&recvreq->lock);

        if(OPAL_UNLIKELY(dst == NULL)) {
            continue;
        }

        dst->des_cbfunc = mca_pml_bfo_put_completion;
        dst->des_cbdata = recvreq;

        /* prepare a descriptor for rdma control message */
        hdr_size = sizeof(mca_pml_bfo_rdma_hdr_t);
        if(dst->des_dst_cnt > 1) {
            hdr_size += (sizeof(mca_btl_base_segment_t) *
                    (dst->des_dst_cnt-1));
        }

        mca_bml_base_alloc(bml_btl, &ctl, MCA_BTL_NO_ORDER, hdr_size,
                           MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);

        if( OPAL_UNLIKELY(NULL == ctl) ) {
            mca_bml_base_free(bml_btl,dst);
            continue;
        }
        ctl->des_cbfunc = mca_pml_bfo_recv_ctl_completion;
#if PML_BFO
        ctl->des_cbdata = recvreq;
#endif /* PML_BFO */
        
        /* fill in rdma header */
        hdr = (mca_pml_bfo_rdma_hdr_t*)ctl->des_src->seg_addr.pval;
        hdr->hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_PUT;
        hdr->hdr_common.hdr_flags =
            (!recvreq->req_ack_sent) ? MCA_PML_BFO_HDR_TYPE_ACK : 0;
        hdr->hdr_req = recvreq->remote_req_send;
#if PML_BFO
        hdr->hdr_dst_req.pval = recvreq; /* only needed in the first put message */
#endif /* PML_BFO */
        hdr->hdr_des.pval = dst;
        hdr->hdr_rdma_offset = recvreq->req_rdma_offset;
        hdr->hdr_seg_cnt = dst->des_dst_cnt;

        for( i = 0; i < dst->des_dst_cnt; i++ ) {
            hdr->hdr_segs[i].seg_addr.lval = ompi_ptr_ptol(dst->des_dst[i].seg_addr.pval);
            hdr->hdr_segs[i].seg_len       = dst->des_dst[i].seg_len;
            hdr->hdr_segs[i].seg_key.key64 = dst->des_dst[i].seg_key.key64;
        }

        if(!recvreq->req_ack_sent)
            recvreq->req_ack_sent = true;
        bfo_hdr_hton(hdr, MCA_PML_BFO_HDR_TYPE_PUT, recvreq->req_recv.req_base.req_proc);

        PERUSE_TRACE_COMM_OMPI_EVENT( PERUSE_COMM_REQ_XFER_CONTINUE,
                                      &(recvreq->req_recv.req_base), size,
                                      PERUSE_RECV);

        /* send rdma request to peer */
        rc = mca_bml_base_send(bml_btl, ctl, MCA_PML_BFO_HDR_TYPE_PUT);
        if( OPAL_LIKELY( rc >= 0 ) ) {
#if PML_BFO
            if ((btl->btl_flags & MCA_BTL_FLAGS_FAILOVER_SUPPORT) &&
                 (ctl->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK)) {
                 recvreq->req_events++;
            }
#endif /* PML_BFO */
            /* update request state */
            recvreq->req_rdma_offset += size;
            OPAL_THREAD_ADD_SIZE_T(&recvreq->req_pipeline_depth, 1);
            recvreq->req_rdma[rdma_idx].length -= size;
            bytes_remaining -= size;
        } else {
            mca_bml_base_free(bml_btl,ctl);
            mca_bml_base_free(bml_btl,dst);
        }
    }

    return OMPI_SUCCESS;
}

#define IS_PROB_REQ(R) \
    ((MCA_PML_REQUEST_IPROBE == (R)->req_recv.req_base.req_type) || \
     (MCA_PML_REQUEST_PROBE == (R)->req_recv.req_base.req_type))

static inline void append_recv_req_to_queue(opal_list_t *queue,
        mca_pml_bfo_recv_request_t *req)
{
    if(OPAL_UNLIKELY(req->req_recv.req_base.req_type == MCA_PML_REQUEST_IPROBE))
        return;

    opal_list_append(queue, (opal_list_item_t*)req);

    /**
     * We don't want to generate this kind of event for MPI_Probe. Hopefully,
     * the compiler will optimize out the empty if loop in the case where PERUSE
     * support is not required by the user.
     */
    if(req->req_recv.req_base.req_type != MCA_PML_REQUEST_PROBE) {
        PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_REQ_INSERT_IN_POSTED_Q,
                                &(req->req_recv.req_base), PERUSE_RECV);
    }
}

/*
 *  this routine tries to match a posted receive.  If a match is found,
 *  it places the request in the appropriate matched receive list. This
 *  function has to be called with the communicator matching lock held.
*/
static mca_pml_bfo_recv_frag_t*
recv_req_match_specific_proc( const mca_pml_bfo_recv_request_t *req,
                              mca_pml_bfo_comm_proc_t *proc )
{
    opal_list_t* unexpected_frags = &proc->unexpected_frags;
    opal_list_item_t *i;
    mca_pml_bfo_recv_frag_t* frag;
    int tag = req->req_recv.req_base.req_tag;

    if(opal_list_get_size(unexpected_frags) == 0)
        return NULL;

    if( OMPI_ANY_TAG == tag ) {
        for (i =  opal_list_get_first(unexpected_frags);
             i != opal_list_get_end(unexpected_frags);
             i =  opal_list_get_next(i)) {
            frag = (mca_pml_bfo_recv_frag_t*)i;
            
            if( frag->hdr.hdr_match.hdr_tag >= 0 )
                return frag;
        }
    } else {
        for (i =  opal_list_get_first(unexpected_frags);
             i != opal_list_get_end(unexpected_frags);
             i =  opal_list_get_next(i)) {
            frag = (mca_pml_bfo_recv_frag_t*)i;
            
            if( frag->hdr.hdr_match.hdr_tag == tag )
                return frag;
        }
    }
    return NULL;
}

/*
 * this routine is used to try and match a wild posted receive - where
 * wild is determined by the value assigned to the source process
*/
static mca_pml_bfo_recv_frag_t*
recv_req_match_wild( mca_pml_bfo_recv_request_t* req,
                     mca_pml_bfo_comm_proc_t **p)
{
    mca_pml_bfo_comm_t* comm = req->req_recv.req_base.req_comm->c_pml_comm;
    mca_pml_bfo_comm_proc_t* proc = comm->procs;
    size_t proc_count = comm->num_procs, i;

    /*
     * Loop over all the outstanding messages to find one that matches.
     * There is an outer loop over lists of messages from each
     * process, then an inner loop over the messages from the
     * process.
     */
    for (i = 0; i < proc_count; i++) {
        mca_pml_bfo_recv_frag_t* frag;

        /* loop over messages from the current proc */
        if((frag = recv_req_match_specific_proc(req, &proc[i]))) {
            *p = &proc[i];
            req->req_recv.req_base.req_proc = proc[i].ompi_proc;
            prepare_recv_req_converter(req);
            return frag; /* match found */
        }
    }

    *p = NULL;
    return NULL;
}


void mca_pml_bfo_recv_req_start(mca_pml_bfo_recv_request_t *req)
{
    mca_pml_bfo_comm_t* comm = req->req_recv.req_base.req_comm->c_pml_comm;
    mca_pml_bfo_comm_proc_t* proc;
    mca_pml_bfo_recv_frag_t* frag;
    opal_list_t *queue;
    mca_pml_bfo_hdr_t* hdr;

    /* init/re-init the request */
    req->req_lock = 0;
    req->req_pipeline_depth = 0;
    req->req_bytes_received = 0;
    req->req_bytes_expected = 0;
    /* What about req_rdma_cnt ? */
#if PML_BFO
    req->req_rdma_cnt = 0;
    req->req_events = 0;
    req->req_restartseq = 0;
    req->req_errstate = 0;
#endif /* PML_BFO */
    req->req_rdma_idx = 0;
    req->req_pending = false;
    req->req_ack_sent = false;

    MCA_PML_BASE_RECV_START(&req->req_recv.req_base);

    OPAL_THREAD_LOCK(&comm->matching_lock);
    /**
     * The laps of time between the ACTIVATE event and the SEARCH_UNEX one include
     * the cost of the request lock.
     */
    PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_SEARCH_UNEX_Q_BEGIN,
                            &(req->req_recv.req_base), PERUSE_RECV);

    /* assign sequence number */
    req->req_recv.req_base.req_sequence = comm->recv_sequence++;

    /* attempt to match posted recv */
    if(req->req_recv.req_base.req_peer == OMPI_ANY_SOURCE) {
        frag = recv_req_match_wild(req, &proc);
        queue = &comm->wild_receives;
#if !OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        /* As we are in a homogeneous environment we know that all remote
         * architectures are exactly the same as the local one. Therefore,
         * we can safely construct the convertor based on the proc
         * information of rank 0.
         */
        if( NULL == frag ) {
            req->req_recv.req_base.req_proc = ompi_proc_local_proc;
            prepare_recv_req_converter(req);
        }
#endif  /* !OPAL_ENABLE_HETEROGENEOUS_SUPPORT */
    } else {
        proc = &comm->procs[req->req_recv.req_base.req_peer];
        req->req_recv.req_base.req_proc = proc->ompi_proc;
        frag = recv_req_match_specific_proc(req, proc);
        queue = &proc->specific_receives;
        /* wild cardrecv will be prepared on match */ 
        prepare_recv_req_converter(req);
    }

    if(OPAL_UNLIKELY(NULL == frag)) {
        PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_SEARCH_UNEX_Q_END,
                                &(req->req_recv.req_base), PERUSE_RECV);
        /* We didn't find any matches.  Record this irecv so we can match
           it when the message comes in. */
        append_recv_req_to_queue(queue, req);
        req->req_match_received = false;
        OPAL_THREAD_UNLOCK(&comm->matching_lock);
    } else {
        if(OPAL_LIKELY(!IS_PROB_REQ(req))) {
            PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_REQ_MATCH_UNEX,
                                    &(req->req_recv.req_base), PERUSE_RECV);

            hdr = (mca_pml_bfo_hdr_t*)frag->segments->seg_addr.pval;
            PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_MSG_REMOVE_FROM_UNEX_Q,
                                   req->req_recv.req_base.req_comm,
                                   hdr->hdr_match.hdr_src,
                                   hdr->hdr_match.hdr_tag,
                                   PERUSE_RECV);

            PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_SEARCH_UNEX_Q_END,
                                    &(req->req_recv.req_base), PERUSE_RECV);

            opal_list_remove_item(&proc->unexpected_frags,
                                  (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&comm->matching_lock);
            
            switch(hdr->hdr_common.hdr_type) {
            case MCA_PML_BFO_HDR_TYPE_MATCH:
                mca_pml_bfo_recv_request_progress_match(req, frag->btl, frag->segments,
                                                        frag->num_segments);
                break;
            case MCA_PML_BFO_HDR_TYPE_RNDV:
                mca_pml_bfo_recv_request_progress_rndv(req, frag->btl, frag->segments,
                                                       frag->num_segments);
                break;
            case MCA_PML_BFO_HDR_TYPE_RGET:
                mca_pml_bfo_recv_request_progress_rget(req, frag->btl, frag->segments,
                                                       frag->num_segments);
                break;
            default:
                assert(0);
            }
            
            MCA_PML_BFO_RECV_FRAG_RETURN(frag);

        } else {
            OPAL_THREAD_UNLOCK(&comm->matching_lock);
            mca_pml_bfo_recv_request_matched_probe(req, frag->btl,
                                                   frag->segments, frag->num_segments);
        }
    }
}
