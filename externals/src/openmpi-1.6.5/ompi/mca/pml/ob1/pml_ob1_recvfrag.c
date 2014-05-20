/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2006-2008 University of Houston.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 */

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "opal/threads/mutex.h"
#include "opal/prefetch.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "pml_ob1.h"
#include "pml_ob1_comm.h"
#include "pml_ob1_recvfrag.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_hdr.h"
#include "ompi/peruse/peruse-internal.h"
#include "ompi/memchecker.h"


OBJ_CLASS_INSTANCE( mca_pml_ob1_buffer_t,
                    ompi_free_list_item_t,
                    NULL,
                    NULL );

OBJ_CLASS_INSTANCE( mca_pml_ob1_recv_frag_t,
                    opal_list_item_t,
                    NULL,
                    NULL );

/**
 * Static functions.
 */

/**
 * Append a unexpected descriptor to a queue. This function will allocate and
 * initialize the fragment (if necessary) and then will add it to the specified
 * queue. The allocated fragment is not returned to the caller.
 */
static void
append_frag_to_list(opal_list_t *queue, mca_btl_base_module_t *btl,
                    mca_pml_ob1_match_hdr_t *hdr, mca_btl_base_segment_t* segments,
                    size_t num_segments, mca_pml_ob1_recv_frag_t* frag)
{
    int rc;

    if(NULL == frag) {
        MCA_PML_OB1_RECV_FRAG_ALLOC(frag, rc);
        MCA_PML_OB1_RECV_FRAG_INIT(frag, hdr, segments, num_segments, btl);
    }
    opal_list_append(queue, (opal_list_item_t*)frag);
}

/**
 * Match incoming recv_frags against posted receives.  
 * Supports out of order delivery.
 * 
 * @param frag_header (IN)          Header of received recv_frag.
 * @param frag_desc (IN)            Received recv_frag descriptor.
 * @param match_made (OUT)          Flag indicating wether a match was made.
 * @param additional_matches (OUT)  List of additional matches 
 * @return                          OMPI_SUCCESS or error status on failure.
 */
static int mca_pml_ob1_recv_frag_match( mca_btl_base_module_t *btl, 
                                        mca_pml_ob1_match_hdr_t *hdr,
                                        mca_btl_base_segment_t* segments,
                                        size_t num_segments,
                                        int type);
 
static mca_pml_ob1_recv_request_t*
match_one(mca_btl_base_module_t *btl,
          mca_pml_ob1_match_hdr_t *hdr, mca_btl_base_segment_t* segments,
          size_t num_segments, ompi_communicator_t *comm_ptr,
          mca_pml_ob1_comm_proc_t *proc,
          mca_pml_ob1_recv_frag_t* frag);
 
void mca_pml_ob1_recv_frag_callback_match(mca_btl_base_module_t* btl, 
                                          mca_btl_base_tag_t tag,
                                          mca_btl_base_descriptor_t* des,
                                          void* cbdata )
{ 
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_ob1_match_hdr_t* hdr = (mca_pml_ob1_match_hdr_t*)segments->seg_addr.pval;
    ompi_communicator_t *comm_ptr;
    mca_pml_ob1_recv_request_t *match = NULL;
    mca_pml_ob1_comm_t *comm;
    mca_pml_ob1_comm_proc_t *proc;
    size_t num_segments = des->des_dst_cnt;
    size_t bytes_received = 0;
    
    if( OPAL_UNLIKELY(segments->seg_len < OMPI_PML_OB1_MATCH_HDR_LEN) ) {
        return;
    }
    ob1_hdr_ntoh(((mca_pml_ob1_hdr_t*) hdr), MCA_PML_OB1_HDR_TYPE_MATCH);
    
    /* communicator pointer */
    comm_ptr = ompi_comm_lookup(hdr->hdr_ctx);
    if(OPAL_UNLIKELY(NULL == comm_ptr)) {
        /* This is a special case. A message for a not yet existing
         * communicator can happens. Instead of doing a matching we
         * will temporarily add it the a pending queue in the PML.
         * Later on, when the communicator is completely instantiated,
         * this pending queue will be searched and all matching fragments
         * moved to the right communicator.
         */
        append_frag_to_list( &mca_pml_ob1.non_existing_communicator_pending,
                             btl, hdr, segments, num_segments, NULL );
        return;
    }
    comm = (mca_pml_ob1_comm_t *)comm_ptr->c_pml_comm;
    
    /* source sequence number */
    proc = &comm->procs[hdr->hdr_src];
 
    /* We generate the MSG_ARRIVED event as soon as the PML is aware
     * of a matching fragment arrival. Independing if it is received
     * on the correct order or not. This will allow the tools to
     * figure out if the messages are not received in the correct
     * order (if multiple network interfaces).
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_MSG_ARRIVED, comm_ptr,
                           hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);
 
    /* get next expected message sequence number - if threaded
     * run, lock to make sure that if another thread is processing
     * a frag from the same message a match is made only once.
     * Also, this prevents other posted receives (for a pair of
     * end points) from being processed, and potentially "loosing"
     * the fragment.
     */
    OPAL_THREAD_LOCK(&comm->matching_lock);
    
     /* get sequence number of next message that can be processed */
    if(OPAL_UNLIKELY((((uint16_t) hdr->hdr_seq) != ((uint16_t) proc->expected_sequence)) ||
                     (opal_list_get_size(&proc->frags_cant_match) > 0 ))) {
        goto slow_path;
    }
    
    /* This is the sequence number we were expecting, so we can try
     * matching it to already posted receives.
     */
    
    /* We're now expecting the next sequence number. */
    proc->expected_sequence++;

    /* We generate the SEARCH_POSTED_QUEUE only when the message is
     * received in the correct sequence. Otherwise, we delay the event
     * generation until we reach the correct sequence number.
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_SEARCH_POSTED_Q_BEGIN, comm_ptr,
                            hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);
    
    match = match_one(btl, hdr, segments, num_segments, comm_ptr, proc, NULL);
    
    /* The match is over. We generate the SEARCH_POSTED_Q_END here,
     * before going into the mca_pml_ob1_check_cantmatch_for_match so
     * we can make a difference for the searching time for all
     * messages.
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_SEARCH_POSTED_Q_END, comm_ptr,
                           hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);
    
    /* release matching lock before processing fragment */
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
    
    if(OPAL_LIKELY(match)) {
        bytes_received = segments->seg_len - OMPI_PML_OB1_MATCH_HDR_LEN;
        match->req_recv.req_bytes_packed = bytes_received;
        
        MCA_PML_OB1_RECV_REQUEST_MATCHED(match, hdr);
        if(match->req_bytes_expected > 0) { 
            struct iovec iov[2];
            uint32_t iov_count = 1;
            
            /*
             *  Make user buffer accessable(defined) before unpacking.
             */
            MEMCHECKER(
                       memchecker_call(&opal_memchecker_base_mem_defined,
                                       match->req_recv.req_base.req_addr,
                                       match->req_recv.req_base.req_count,
                                       match->req_recv.req_base.req_datatype);
                       );
            
            iov[0].iov_len = bytes_received;
            iov[0].iov_base = (IOVBASE_TYPE*)((unsigned char*)segments->seg_addr.pval +
                                              OMPI_PML_OB1_MATCH_HDR_LEN);
            while (iov_count < num_segments) {
                bytes_received += segments[iov_count].seg_len;
                iov[iov_count].iov_len = segments[iov_count].seg_len;
                iov[iov_count].iov_base = (IOVBASE_TYPE*)((unsigned char*)segments[iov_count].seg_addr.pval);
                iov_count++;
            }
            opal_convertor_unpack( &match->req_recv.req_base.req_convertor,
                                   iov,
                                   &iov_count,
                                   &bytes_received );
            match->req_bytes_received = bytes_received;
            /*
             *  Unpacking finished, make the user buffer unaccessable again.
             */
            MEMCHECKER(
                       memchecker_call(&opal_memchecker_base_mem_noaccess,
                                       match->req_recv.req_base.req_addr,
                                       match->req_recv.req_base.req_count,
                                       match->req_recv.req_base.req_datatype);
                       );
        }
        
        /* no need to check if complete we know we are.. */
        /*  don't need a rmb as that is for checking */
        recv_request_pml_complete(match);
    }
    return;
    
 slow_path:
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
    mca_pml_ob1_recv_frag_match(btl, hdr, segments,
                                num_segments, MCA_PML_OB1_HDR_TYPE_MATCH);
}


void mca_pml_ob1_recv_frag_callback_rndv(mca_btl_base_module_t* btl, 
                                         mca_btl_base_tag_t tag,
                                         mca_btl_base_descriptor_t* des,
                                         void* cbdata )
{ 
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;
    
    if( OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t)) ) {
        return;
    }
    ob1_hdr_ntoh(hdr, MCA_PML_OB1_HDR_TYPE_RNDV);
    mca_pml_ob1_recv_frag_match(btl, &hdr->hdr_match, segments,
                                des->des_dst_cnt, MCA_PML_OB1_HDR_TYPE_RNDV);
    return;
}

void mca_pml_ob1_recv_frag_callback_rget(mca_btl_base_module_t* btl, 
                                         mca_btl_base_tag_t tag,
                                         mca_btl_base_descriptor_t* des,
                                         void* cbdata )
{ 
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;
    
    if( OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t)) ) {
        return;
    }
    ob1_hdr_ntoh(hdr, MCA_PML_OB1_HDR_TYPE_RGET);
    mca_pml_ob1_recv_frag_match(btl, &hdr->hdr_match, segments,
                                des->des_dst_cnt, MCA_PML_OB1_HDR_TYPE_RGET);
    return;
}

  

void mca_pml_ob1_recv_frag_callback_ack(mca_btl_base_module_t* btl, 
                                        mca_btl_base_tag_t tag,
                                        mca_btl_base_descriptor_t* des,
                                        void* cbdata )
{ 
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;
    mca_pml_ob1_send_request_t* sendreq;
    
    if( OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t)) ) {
         return;
    }
    
    ob1_hdr_ntoh(hdr, MCA_PML_OB1_HDR_TYPE_ACK);
    sendreq = (mca_pml_ob1_send_request_t*)hdr->hdr_ack.hdr_src_req.pval;
    sendreq->req_recv = hdr->hdr_ack.hdr_dst_req;
    
    /* if the request should be delivered entirely by copy in/out
     * then throttle sends */
    if(hdr->hdr_common.hdr_flags & MCA_PML_OB1_HDR_FLAGS_NORDMA)
        sendreq->req_throttle_sends = true;
    
    mca_pml_ob1_send_request_copy_in_out(sendreq,
                                         hdr->hdr_ack.hdr_send_offset,
                                         sendreq->req_send.req_bytes_packed -
                                         hdr->hdr_ack.hdr_send_offset);

    if (sendreq->req_state != 0) {
        /* Typical receipt of an ACK message causes req_state to be
         * decremented. However, a send request that started as an
         * RGET request can become a RNDV. For example, when the
         * receiver determines that its receive buffer is not
         * contiguous and therefore cannot support the RGET
         * protocol. A send request that started with the RGET
         * protocol has req_state == 0 and as such should not be
         * decremented.
         */
        OPAL_THREAD_ADD32(&sendreq->req_state, -1);
    }

    if(send_request_pml_complete_check(sendreq) == false)
        mca_pml_ob1_send_request_schedule(sendreq);
    
    return;
}

void mca_pml_ob1_recv_frag_callback_frag(mca_btl_base_module_t* btl, 
                                         mca_btl_base_tag_t tag,
                                         mca_btl_base_descriptor_t* des,
                                         void* cbdata ) { 
     mca_btl_base_segment_t* segments = des->des_dst;
     mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;
     mca_pml_ob1_recv_request_t* recvreq;
     
     if( OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t)) ) {
         return;
     }
     ob1_hdr_ntoh(hdr, MCA_PML_OB1_HDR_TYPE_FRAG);
     recvreq = (mca_pml_ob1_recv_request_t*)hdr->hdr_frag.hdr_dst_req.pval;
     mca_pml_ob1_recv_request_progress_frag(recvreq,btl,segments,des->des_dst_cnt);
     
     return;
}


void mca_pml_ob1_recv_frag_callback_put(mca_btl_base_module_t* btl, 
                                        mca_btl_base_tag_t tag,
                                        mca_btl_base_descriptor_t* des,
                                        void* cbdata ) { 
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;
    mca_pml_ob1_send_request_t* sendreq;
    
    if( OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t)) ) {
        return;
    }
    
    ob1_hdr_ntoh(hdr, MCA_PML_OB1_HDR_TYPE_PUT);
    sendreq = (mca_pml_ob1_send_request_t*)hdr->hdr_rdma.hdr_req.pval;
    mca_pml_ob1_send_request_put(sendreq,btl,&hdr->hdr_rdma);
    
    return;
}


void mca_pml_ob1_recv_frag_callback_fin(mca_btl_base_module_t* btl, 
                                        mca_btl_base_tag_t tag,
                                        mca_btl_base_descriptor_t* des,
                                        void* cbdata ) { 
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;
    mca_btl_base_descriptor_t* rdma;
    
    if( OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t)) ) {
        return;
    }
    
    ob1_hdr_ntoh(hdr, MCA_PML_OB1_HDR_TYPE_FIN);
    rdma = (mca_btl_base_descriptor_t*)hdr->hdr_fin.hdr_des.pval;
    rdma->des_cbfunc(btl, NULL, rdma,
                     hdr->hdr_fin.hdr_fail ? OMPI_ERROR : OMPI_SUCCESS);
    
    return;
}



#define PML_MAX_SEQ ~((mca_pml_sequence_t)0);

static inline mca_pml_ob1_recv_request_t* get_posted_recv(opal_list_t *queue)
{
    if(opal_list_get_size(queue) == 0)
        return NULL;

    return (mca_pml_ob1_recv_request_t*)opal_list_get_first(queue);
}

static inline mca_pml_ob1_recv_request_t* get_next_posted_recv(
        opal_list_t *queue,
        mca_pml_ob1_recv_request_t* req)
{
    opal_list_item_t *i = opal_list_get_next((opal_list_item_t*)req);

    if(opal_list_get_end(queue) == i)
        return NULL;

    return (mca_pml_ob1_recv_request_t*)i;
}

static mca_pml_ob1_recv_request_t *match_incomming(
        mca_pml_ob1_match_hdr_t *hdr, mca_pml_ob1_comm_t *comm,
        mca_pml_ob1_comm_proc_t *proc)
{
    mca_pml_ob1_recv_request_t *specific_recv, *wild_recv;
    mca_pml_sequence_t wild_recv_seq, specific_recv_seq;
    int tag = hdr->hdr_tag;

    specific_recv = get_posted_recv(&proc->specific_receives);
    wild_recv = get_posted_recv(&comm->wild_receives);

    wild_recv_seq = wild_recv ?
        wild_recv->req_recv.req_base.req_sequence : PML_MAX_SEQ;
    specific_recv_seq = specific_recv ?
        specific_recv->req_recv.req_base.req_sequence : PML_MAX_SEQ;

    /* they are equal only if both are PML_MAX_SEQ */
    while(wild_recv_seq != specific_recv_seq) {
        mca_pml_ob1_recv_request_t **match;
        opal_list_t *queue;
        int req_tag;
        mca_pml_sequence_t *seq;

        if (OPAL_UNLIKELY(wild_recv_seq < specific_recv_seq)) {
            match = &wild_recv;
            queue = &comm->wild_receives;
            seq = &wild_recv_seq;
        } else {
            match = &specific_recv;
            queue = &proc->specific_receives;
            seq = &specific_recv_seq;
        }

        req_tag = (*match)->req_recv.req_base.req_tag;
        if(req_tag == tag || (req_tag == OMPI_ANY_TAG && tag >= 0)) {
            opal_list_remove_item(queue, (opal_list_item_t*)(*match));
            PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_REQ_REMOVE_FROM_POSTED_Q,
                    &((*match)->req_recv.req_base), PERUSE_RECV);
            return *match;
        }

        *match = get_next_posted_recv(queue, *match);
        *seq = (*match) ? (*match)->req_recv.req_base.req_sequence : PML_MAX_SEQ;
    }

    return NULL;
}

static mca_pml_ob1_recv_request_t*
match_one(mca_btl_base_module_t *btl,
          mca_pml_ob1_match_hdr_t *hdr, mca_btl_base_segment_t* segments,
          size_t num_segments, ompi_communicator_t *comm_ptr,
          mca_pml_ob1_comm_proc_t *proc,
          mca_pml_ob1_recv_frag_t* frag)
{
    mca_pml_ob1_recv_request_t *match;
    mca_pml_ob1_comm_t *comm = (mca_pml_ob1_comm_t *)comm_ptr->c_pml_comm;

    do {
        match = match_incomming(hdr, comm, proc);

        /* if match found, process data */
        if(OPAL_LIKELY(NULL != match)) {
            match->req_recv.req_base.req_proc = proc->ompi_proc;

            if(OPAL_UNLIKELY(MCA_PML_REQUEST_PROBE == match->req_recv.req_base.req_type)) {
                /* complete the probe */
                mca_pml_ob1_recv_request_matched_probe(match, btl, segments,
                                                       num_segments);
                /* attempt to match actual request */
                continue;
            }

            PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_MSG_MATCH_POSTED_REQ,
                                    &(match->req_recv.req_base), PERUSE_RECV);
            return match;
        }

        /* if no match found, place on unexpected queue */
        append_frag_to_list(&proc->unexpected_frags, btl, hdr, segments,
                            num_segments, frag);
        PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_MSG_INSERT_IN_UNEX_Q, comm_ptr,
                               hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);
        return NULL;
    } while(true);
}

static mca_pml_ob1_recv_frag_t* check_cantmatch_for_match(mca_pml_ob1_comm_proc_t *proc)
{
    mca_pml_ob1_recv_frag_t *frag;

    /* search the list for a fragment from the send with sequence
     * number next_msg_seq_expected
     */
    for(frag = (mca_pml_ob1_recv_frag_t*)opal_list_get_first(&proc->frags_cant_match);
        frag != (mca_pml_ob1_recv_frag_t*)opal_list_get_end(&proc->frags_cant_match);
        frag = (mca_pml_ob1_recv_frag_t*)opal_list_get_next(frag))
    {
        mca_pml_ob1_match_hdr_t* hdr = &frag->hdr.hdr_match;
        /*
         * If the message has the next expected seq from that proc...
         */
        if(hdr->hdr_seq != proc->expected_sequence)
            continue;

        opal_list_remove_item(&proc->frags_cant_match, (opal_list_item_t*)frag);
        return frag;
    }

    return NULL;
}

/**
 * RCS/CTS receive side matching
 *
 * @param hdr list of parameters needed for matching
 *                    This list is also embeded in frag,
 *                    but this allows to save a memory copy when
 *                    a match is made in this routine. (IN)
 * @param frag   pointer to receive fragment which we want
 *                    to match (IN/OUT).  If a match is not made,
 *                    hdr is copied to frag.
 * @param match_made  parameter indicating if we matched frag/
 *                    hdr (OUT)
 * @param additional_matches  if a match is made with frag, we
 *                    may be able to match fragments that previously
 *                    have arrived out-of-order.  If this is the
 *                    case, the associated fragment descriptors are
 *                    put on this list for further processing. (OUT)
 *
 * @return OMPI error code
 *
 * This routine is used to try and match a newly arrived message fragment
 *   to pre-posted receives.  The following assumptions are made
 *   - fragments are received out of order
 *   - for long messages, e.g. more than one fragment, a RTS/CTS algorithm
 *       is used.
 *   - 2nd and greater fragments include a receive descriptor pointer
 *   - fragments may be dropped
 *   - fragments may be corrupt
 *   - this routine may be called simultaneously by more than one thread
 */
static int mca_pml_ob1_recv_frag_match( mca_btl_base_module_t *btl, 
                                        mca_pml_ob1_match_hdr_t *hdr,
                                        mca_btl_base_segment_t* segments,
                                        size_t num_segments, 
                                        int type)
{
    /* local variables */
    uint16_t next_msg_seq_expected, frag_msg_seq;
    ompi_communicator_t *comm_ptr;
    mca_pml_ob1_recv_request_t *match = NULL;
    mca_pml_ob1_comm_t *comm;
    mca_pml_ob1_comm_proc_t *proc;
    mca_pml_ob1_recv_frag_t* frag = NULL;

    /* communicator pointer */
    comm_ptr = ompi_comm_lookup(hdr->hdr_ctx);
    if(OPAL_UNLIKELY(NULL == comm_ptr)) {
        /* This is a special case. A message for a not yet existing
         * communicator can happens. Instead of doing a matching we
         * will temporarily add it the a pending queue in the PML.
         * Later on, when the communicator is completely instantiated,
         * this pending queue will be searched and all matching fragments
         * moved to the right communicator.
         */
        append_frag_to_list( &mca_pml_ob1.non_existing_communicator_pending,
                             btl, hdr, segments, num_segments, NULL );
        return OMPI_SUCCESS;
    }
    comm = (mca_pml_ob1_comm_t *)comm_ptr->c_pml_comm;

    /* source sequence number */
    frag_msg_seq = hdr->hdr_seq;
    proc = &comm->procs[hdr->hdr_src];

    /**
     * We generate the MSG_ARRIVED event as soon as the PML is aware of a matching
     * fragment arrival. Independing if it is received on the correct order or not.
     * This will allow the tools to figure out if the messages are not received in the
     * correct order (if multiple network interfaces).
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_MSG_ARRIVED, comm_ptr,
                           hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);

    /* get next expected message sequence number - if threaded
     * run, lock to make sure that if another thread is processing 
     * a frag from the same message a match is made only once.
     * Also, this prevents other posted receives (for a pair of
     * end points) from being processed, and potentially "loosing"
     * the fragment.
     */
    OPAL_THREAD_LOCK(&comm->matching_lock);

    /* get sequence number of next message that can be processed */
    next_msg_seq_expected = (uint16_t)proc->expected_sequence;
    if(OPAL_UNLIKELY(frag_msg_seq != next_msg_seq_expected))
        goto wrong_seq;

    /*
     * This is the sequence number we were expecting,
     * so we can try matching it to already posted
     * receives.
     */

out_of_order_match:
    /* We're now expecting the next sequence number. */
    proc->expected_sequence++;

    /**
     * We generate the SEARCH_POSTED_QUEUE only when the message is received
     * in the correct sequence. Otherwise, we delay the event generation until
     * we reach the correct sequence number.
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_SEARCH_POSTED_Q_BEGIN, comm_ptr,
                            hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);

    match = match_one(btl, hdr, segments, num_segments, comm_ptr, proc, frag);

    /**
     * The match is over. We generate the SEARCH_POSTED_Q_END here, before going
     * into the mca_pml_ob1_check_cantmatch_for_match so we can make a difference
     * for the searching time for all messages.
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_SEARCH_POSTED_Q_END, comm_ptr,
                            hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);

    /* release matching lock before processing fragment */
    OPAL_THREAD_UNLOCK(&comm->matching_lock);

    if(OPAL_LIKELY(match)) {
        switch(type) { 
        case MCA_PML_OB1_HDR_TYPE_MATCH:
            mca_pml_ob1_recv_request_progress_match(match, btl, segments, num_segments);
            break;
        case MCA_PML_OB1_HDR_TYPE_RNDV:
            mca_pml_ob1_recv_request_progress_rndv(match, btl, segments, num_segments);
            break;
        case MCA_PML_OB1_HDR_TYPE_RGET:
            mca_pml_ob1_recv_request_progress_rget(match, btl, segments, num_segments);
            break;
        }
        
        if(OPAL_UNLIKELY(frag))
            MCA_PML_OB1_RECV_FRAG_RETURN(frag);
    }
    
    /* 
     * Now that new message has arrived, check to see if
     * any fragments on the c_c_frags_cant_match list
     * may now be used to form new matchs
     */
    if(OPAL_UNLIKELY(opal_list_get_size(&proc->frags_cant_match) > 0)) {
        OPAL_THREAD_LOCK(&comm->matching_lock);
        if((frag = check_cantmatch_for_match(proc))) {
            hdr = &frag->hdr.hdr_match;
            segments = frag->segments;
            num_segments = frag->num_segments;
            btl = frag->btl;
            type = hdr->hdr_common.hdr_type;
            goto out_of_order_match;
        }
        OPAL_THREAD_UNLOCK(&comm->matching_lock);
    }

    return OMPI_SUCCESS;
wrong_seq:
    /*
     * This message comes after the next expected, so it
     * is ahead of sequence.  Save it for later.
     */
    append_frag_to_list(&proc->frags_cant_match, btl, hdr, segments,
                        num_segments, NULL);
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
    return OMPI_SUCCESS;
}

