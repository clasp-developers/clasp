/*
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 * Functions that implement failover capabilities.
 */
                                                                                                                                                 
#ifndef MCA_PML_BFO_FAILOVER_H
#define MCA_PML_BFO_FAILOVER_H

#include "ompi/mca/btl/btl.h"
#include "pml_bfo_hdr.h"

BEGIN_C_DECLS

bool mca_pml_bfo_is_duplicate_msg(mca_pml_bfo_comm_proc_t* proc,
                                  mca_pml_bfo_match_hdr_t *hdr);
bool mca_pml_bfo_is_duplicate_fin(mca_pml_bfo_hdr_t* hdr, mca_btl_base_descriptor_t* rdma,
                                  mca_btl_base_module_t* btl);

mca_pml_bfo_recv_request_t* mca_pml_bfo_get_request(mca_pml_bfo_match_hdr_t *hdr);

void mca_pml_bfo_send_request_restart(mca_pml_bfo_send_request_t* sendreq,
                                      bool repost, mca_btl_base_tag_t tag);
void mca_pml_bfo_send_request_rndvrestartnotify(mca_pml_bfo_send_request_t* sendreq,
                                      bool repost, mca_btl_base_tag_t tag, int status,
                                      mca_btl_base_module_t* btl);

void
mca_pml_bfo_rndvrestartnotify_completion(mca_btl_base_module_t* btl,
                                         struct mca_btl_base_endpoint_t* ep,
                                         struct mca_btl_base_descriptor_t* des,
                                         int status);
void
mca_pml_bfo_check_recv_ctl_completion_status(mca_btl_base_module_t* btl,
                                             struct mca_btl_base_descriptor_t* des,
                                             int status);

/* Reset a receive request to the beginning */
void mca_pml_bfo_recv_request_reset(mca_pml_bfo_recv_request_t* recvreq);
/* Notify sender that receiver detected an error */
void mca_pml_bfo_recv_request_recverrnotify(mca_pml_bfo_recv_request_t* recvreq,
                                            mca_btl_base_tag_t tag, int status);
/* Ack the RNDVRESTARTNOTIFY message */
void mca_pml_bfo_recv_request_rndvrestartack(mca_pml_bfo_recv_request_t* recvreq,
                                             mca_btl_base_tag_t tag, int status,
                                             mca_btl_base_module_t* btl);
/* Nack the RNDVRESTARTNOTIFY message */
void mca_pml_bfo_recv_request_rndvrestartnack(mca_btl_base_descriptor_t* olddes,
                                              ompi_proc_t* ompi_proc, bool repost);

void mca_pml_bfo_recv_restart_completion(mca_btl_base_module_t* btl,
                                         struct mca_btl_base_endpoint_t* ep,
                                         struct mca_btl_base_descriptor_t* des,
                                         int status);
void mca_pml_bfo_failover_error_handler(struct mca_btl_base_module_t* btl,
                                        int32_t flags, ompi_proc_t *errproc, char *btlname);
void mca_pml_bfo_repost_match_fragment(struct mca_btl_base_descriptor_t* des);
void mca_pml_bfo_repost_fin(struct mca_btl_base_descriptor_t* des);

void mca_pml_bfo_map_out_btl(struct mca_btl_base_module_t* btl,
                             ompi_proc_t *errproc, char *btlname);

extern void mca_pml_bfo_map_out( mca_btl_base_module_t *btl,
                                 mca_btl_base_tag_t tag,
                                 mca_btl_base_descriptor_t* descriptor,
                                 void* cbdata );

int mca_pml_bfo_register_callbacks(void);

void mca_pml_bfo_update_rndv_fields(mca_pml_bfo_hdr_t* hdr,
                                    mca_pml_bfo_send_request_t*, char *type);

void mca_pml_bfo_update_bml_btl(mca_bml_base_btl_t** bml_btl, mca_btl_base_module_t* btl,
				struct mca_btl_base_descriptor_t* des);

void mca_pml_bfo_find_recvreq_eager_bml_btl(mca_bml_base_btl_t** bml_btl,
                                            mca_btl_base_module_t* btl,
                                            mca_pml_bfo_recv_request_t* recvreq,
                                            char* type);

void mca_pml_bfo_find_sendreq_eager_bml_btl(mca_bml_base_btl_t** bml_btl,
                                            mca_btl_base_module_t* btl,
                                            mca_pml_bfo_send_request_t* sendreq,
                                            char* type);

void mca_pml_bfo_find_sendreq_rdma_bml_btl(mca_bml_base_btl_t** bml_btl,
                                           mca_btl_base_module_t* btl,
                                           mca_pml_bfo_send_request_t* sendreq,
                                           char* type);

void mca_pml_bfo_update_eager_bml_btl_recv_ctl(mca_bml_base_btl_t** bml_btl,
                                               mca_btl_base_module_t* btl,
                                               struct mca_btl_base_descriptor_t* des);
void mca_pml_bfo_find_recvreq_rdma_bml_btl(mca_bml_base_btl_t** bml_btl,
                                           mca_btl_base_module_t* btl,
                                           mca_pml_bfo_recv_request_t* recvreq,
                                           char* type);

bool mca_pml_bfo_rndv_completion_status_error(struct mca_btl_base_descriptor_t* des,
                                              mca_pml_bfo_send_request_t* sendreq);
void mca_pml_bfo_send_ctl_completion_status_error(struct mca_btl_base_descriptor_t* des);


void mca_pml_bfo_completion_sendreq_has_error(mca_pml_bfo_send_request_t* sendreq,
					      int status,
					      mca_btl_base_module_t* btl,
					      int type,
					      char *description);
/**
 * Four new callbacks for the four new message types.
 */
extern void mca_pml_bfo_recv_frag_callback_rndvrestartnotify( mca_btl_base_module_t *btl,
                                                              mca_btl_base_tag_t tag,
                                                              mca_btl_base_descriptor_t* descriptor,
                                                              void* cbdata );

extern void mca_pml_bfo_recv_frag_callback_rndvrestartack( mca_btl_base_module_t *btl,
                                                           mca_btl_base_tag_t tag,
                                                           mca_btl_base_descriptor_t* descriptor,
                                                           void* cbdata );

extern void mca_pml_bfo_recv_frag_callback_rndvrestartnack( mca_btl_base_module_t *btl,
                                                            mca_btl_base_tag_t tag,
                                                            mca_btl_base_descriptor_t* descriptor,
                                                            void* cbdata );

extern void mca_pml_bfo_recv_frag_callback_recverrnotify( mca_btl_base_module_t *btl,
                                                          mca_btl_base_tag_t tag,
                                                          mca_btl_base_descriptor_t* descriptor,
                                                          void* cbdata );

/**
 * A bunch of macros to help isolate failover code from regular ob1 code.
 */

/* Drop any ACK fragments if request is in error state.  Do not want
 * to initiate any more activity. */
#define MCA_PML_BFO_ERROR_CHECK_ON_ACK_CALLBACK(sendreq)                          \
    if( OPAL_UNLIKELY((sendreq)->req_error)) {                                    \
         opal_output_verbose(20, mca_pml_bfo_output,                              \
                             "ACK: received: dropping because request in error, " \
                             "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",   \
                             (uint16_t)(sendreq)->req_send.req_base.req_sequence, \
                             (sendreq)->req_restartseq,                           \
                             (void *)(sendreq), (sendreq)->req_recv.pval,         \
                             (sendreq)->req_send.req_base.req_peer);              \
        return;                                                                   \
    }

/* Drop any FRAG fragments if request is in error state.  Do not want
 * to initiate any more activity. */
#define MCA_PML_BFO_ERROR_CHECK_ON_FRAG_CALLBACK(recvreq)                                \
    if( OPAL_UNLIKELY((recvreq)->req_errstate)) {                                        \
        opal_output_verbose(20, mca_pml_bfo_output,                                      \
                            "FRAG: received: dropping because request in error, "        \
                            "PML=%d, src_req=%p, dst_req=%p, peer=%d, offset=%d",        \
                            (uint16_t)(recvreq)->req_msgseq,                             \
                            (recvreq)->remote_req_send.pval,                             \
                            (void *)(recvreq),                                           \
                            (recvreq)->req_recv.req_base.req_ompi.req_status.MPI_SOURCE, \
                            (int)hdr->hdr_frag.hdr_frag_offset);                         \
        return;                                                                          \
    }

/* Drop any PUT fragments if request is in error state.  Do not want
 * to initiate any more activity. */
#define MCA_PML_BFO_ERROR_CHECK_ON_PUT_CALLBACK(sendreq)                          \
    if( OPAL_UNLIKELY((sendreq)->req_error)) {                                    \
         opal_output_verbose(20, mca_pml_bfo_output,                              \
                             "PUT: received: dropping because request in error, " \
                             "PML=%d, src_req=%p, dst_req=%p, peer=%d",           \
                             (uint16_t)(sendreq)->req_send.req_base.req_sequence, \
                             (void *)(sendreq), (sendreq)->req_recv.pval,         \
                             (sendreq)->req_send.req_base.req_peer);              \
        return;                                                                   \
    }

/**
 * Macros for pml_bfo_recvreq.c file.
 */

/* This can happen if a FIN message arrives after the request was
 * marked in error.  So, just drop the message.  Note that the status
 * field is not being checked.  That is because the status field is the
 * value returned in the FIN hdr.hdr_fail field and may be used for
 * other things.  Note that we allow the various fields to be updated
 * in case this actually completes the request and the sending side
 * thinks it is done. */
#define MCA_PML_BFO_ERROR_CHECK_ON_FIN_FOR_PUT(recvreq)                                   \
    if( OPAL_UNLIKELY((recvreq)->req_errstate)) {                                         \
        opal_output_verbose(20, mca_pml_bfo_output,                                       \
                            "FIN: received on broken request, skipping, "                 \
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",            \
                            (recvreq)->req_msgseq, (recvreq)->req_restartseq,             \
                            (recvreq)->remote_req_send.pval, (void *)(recvreq),           \
                            (recvreq)->req_recv.req_base.req_ompi.req_status.MPI_SOURCE); \
        /* Even though in error, it still might complete.  */                             \
        recv_request_pml_complete_check(recvreq);                                         \
        return;                                                                           \
    }

#define MCA_PML_BFO_ERROR_CHECK_ON_RDMA_READ_COMPLETION(recvreq)                            \
    if ((recvreq)->req_errstate) {                                                          \
        opal_output_verbose(30, mca_pml_bfo_output,                                         \
			    "RDMA read: completion failed, error already seen, "            \
			    "PML=%d, RQS=%d, src_req=%lx, dst_req=%lx, peer=%d",            \
			    (recvreq)->req_msgseq, (recvreq)->req_restartseq,               \
			    (unsigned long)(recvreq)->remote_req_send.pval,                 \
			    (unsigned long)(recvreq),                                       \
			    (recvreq)->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);   \
	return;                                                                             \
    } else {                                                                                \
	opal_output_verbose(30, mca_pml_bfo_output,                                         \
			    "RDMA read: completion failed, sending RECVERRNOTIFY to "       \
			    "sender, PML=%d, RQS=%d, src_req=%lx, dst_req=%lx, peer=%d",    \
			    (recvreq)->req_msgseq, (recvreq)->req_restartseq,               \
			    (unsigned long)(recvreq)->remote_req_send.pval,                 \
			    (unsigned long)(recvreq),                                       \
			    (recvreq)->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);   \
	mca_pml_bfo_recv_request_recverrnotify(recvreq, MCA_PML_BFO_HDR_TYPE_RGET, status); \
    }

#define MCA_PML_BFO_SECOND_ERROR_CHECK_ON_RDMA_READ_COMPLETION(recvreq, status, btl)        \
    /* See if the request has received a RNDVRESTARTNOTIFY */                               \
    if( OPAL_UNLIKELY(recvreq->req_errstate)) {                                             \
        if (recvreq->req_errstate & RECVREQ_RNDVRESTART_RECVED) {                           \
            opal_output_verbose(30, mca_pml_bfo_output,                                     \
                                "RDMA read: completion: recvreq has error, outstanding events=%d " \
                                "PML=%d, RQS=%d, src_req=%lx, dst_req=%lx, status=%d, peer=%d",    \
                                recvreq->req_events, recvreq->req_msgseq, recvreq->req_restartseq, \
                                (unsigned long)recvreq->remote_req_send.pval,               \
                                (unsigned long)recvreq, status,                             \
                                recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE); \
            if (0 == recvreq->req_events) {                                                 \
                mca_pml_bfo_recv_request_rndvrestartack(recvreq, MCA_PML_BFO_HDR_TYPE_RGET, \
                                                        status, btl);                       \
            }                                                                               \
        }                                                                                   \
        MCA_PML_BFO_RDMA_FRAG_RETURN(frag);                                                 \
        return;                                                                             \
    }

/**
 * Macros for pml_bfo_sendreq.c file.
 */

/* This macro is called on the sending side after receiving
 * a PUT message.  There is a chance that this PUT message
 * has shown up and is attempting to modify the state of
 * the req_state, but the req_state is no longer being tracked
 * because the RNDV message has turned into a RGET message
 * because it got an error on the RNDV completion.
 */
#define MCA_PML_BFO_VERIFY_SENDREQ_REQ_STATE_VALUE(sendreq)  \
    if (sendreq->req_state == -1) {                          \
        OPAL_THREAD_ADD32(&sendreq->req_state, 1);           \
    }

/* Now check the error state.  This request can be in error if the
 * RNDV message made it over, but the receiver got an error trying to
 * send the ACK back and therefore sent a RECVERRNOTIFY message.  In
 * that case, we want to start the restart dance as the receiver has
 * matched this message already.  Only restart if there are no
 * outstanding events on send request. */
#define MCA_PML_BFO_RNDV_COMPLETION_SENDREQ_ERROR_CHECK(sendreq, status, btl, type, description) \
    if( OPAL_UNLIKELY ((sendreq)->req_error)) {                                             \
        mca_pml_bfo_completion_sendreq_has_error(sendreq, status,                           \
                                                 btl, type, description);                   \
        return;                                                                             \
    }

/** 
 * This macro is called within the frag completion function in two
 * places.  It is called to see if any errors occur prior to the
 * completion event on the frag.  It is then called a second time
 * after the scheduling routine is called as the scheduling routine
 * may have detected that a BTL that was cached on the request had
 * been removed and therefore marked the request in error.  In that
 * case, the scheduling of fragments can no longer proceed properly,
 * and if there are no outstanding events, iniated the restart dance.
 */
#define MCA_PML_BFO_FRAG_COMPLETION_SENDREQ_ERROR_CHECK(sendreq, status, btl, type, description) \
    if( OPAL_UNLIKELY((sendreq)->req_error)) {                                              \
        mca_pml_bfo_completion_sendreq_has_error(sendreq, status,                           \
                                                 btl, type, description);                   \
        return;                                                                             \
    }

/* This can happen if a FIN message arrives after the request was
 * marked in error.  So, just drop the message.  Note that the status
 * field is not checked here.  That is because that is the value
 * returned in the FIN hdr.hdr_fail field and may be used for other
 * things. */
#define MCA_PML_BFO_RGET_COMPLETION_SENDREQ_ERROR_CHECK(sendreq, btl, des)                 \
    if( OPAL_UNLIKELY(sendreq->req_error)) {                                               \
        opal_output_verbose(30, mca_pml_bfo_output,                                        \
                            "FIN: received on broken request, skipping, "                  \
                            "PML=%d, src_req=%lx, dst_req=%lx, peer=%d",                   \
                            (uint16_t)sendreq->req_send.req_base.req_sequence,             \
                            (unsigned long)sendreq, (unsigned long)sendreq->req_recv.pval, \
                            sendreq->req_send.req_base.req_peer);                          \
        btl->btl_free(btl, des);                                                           \
        return;                                                                            \
    }


/* Check if there has been an error on the send request when we get
 * a completion event on the RDMA write. */
#define MCA_PML_BFO_PUT_COMPLETION_SENDREQ_ERROR_CHECK(sendreq, status, btl)              \
    if ( OPAL_UNLIKELY(sendreq->req_error)) {                                             \
        mca_pml_bfo_completion_sendreq_has_error(sendreq, status, btl,                    \
                                                 MCA_PML_BFO_HDR_TYPE_PUT, "RDMA write"); \
        MCA_PML_BFO_RDMA_FRAG_RETURN(frag);                                               \
        return;                                                                           \
    }

#define MCA_PML_BFO_CHECK_FOR_RNDV_RESTART(hdr, sendreq, type)  \
    if (0 < sendreq->req_restartseq) {                          \
        mca_pml_bfo_update_rndv_fields(hdr, sendreq, type);     \
    }

/* If a bml_btl gets mapped out, then we need to adjust it based
 * on the btl from the callback function.  These macros are called on
 * every callback to make sure things are copacetic.
 */
#define MCA_PML_BFO_CHECK_EAGER_BML_BTL_ON_FIN_COMPLETION(bml_btl, btl, des)               \
    if (bml_btl->btl != btl) {                                                             \
        ompi_proc_t *proc = (ompi_proc_t*) des->des_cbdata;                                \
        mca_bml_base_endpoint_t* bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml; \
        bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_eager, btl);              \
    }                  
#define MCA_PML_BFO_CHECK_SENDREQ_EAGER_BML_BTL(bml_btl, btl, sendreq, type)   \
    if (bml_btl->btl != btl) {                                                 \
        mca_pml_bfo_find_sendreq_eager_bml_btl(&bml_btl, btl, sendreq, type);  \
    }
#define MCA_PML_BFO_CHECK_SENDREQ_RDMA_BML_BTL(bml_btl, btl, sendreq, type)    \
    if (bml_btl->btl != btl) {                                                 \
        mca_pml_bfo_find_sendreq_rdma_bml_btl(&bml_btl, btl, sendreq, type);   \
    }

#define MCA_PML_BFO_CHECK_RECVREQ_EAGER_BML_BTL(bml_btl, btl, recvreq, type)   \
    if (bml_btl->btl != btl) {                                                 \
        mca_pml_bfo_find_recvreq_eager_bml_btl(&bml_btl, btl, recvreq, type);  \
    }

#define MCA_PML_BFO_CHECK_RECVREQ_RDMA_BML_BTL(bml_btl, btl, recvreq, type)    \
    if (bml_btl->btl != btl) {                                                 \
        mca_pml_bfo_find_recvreq_rdma_bml_btl(&bml_btl, btl, recvreq, type);   \
    }

#define MCA_PML_BFO_CHECK_RECVREQ_EAGER_BML_BTL_RECV_CTL(bml_btl, btl, des)    \
    if (bml_btl->btl != btl) {                                                 \
        mca_pml_bfo_update_eager_bml_btl_recv_ctl(&bml_btl, btl, des);         \
    }

#define MCA_PML_BFO_CHECK_FOR_REMOVED_BML(sendreq, frag, btl)                             \
    if( OPAL_UNLIKELY(NULL == frag->rdma_bml) ) {                                         \
        opal_output_verbose(30, mca_pml_bfo_output,                                       \
                            "PUT received: no matching BTL to RDMA write to, oustanding " \
                            "events=%d, PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d", \
                            sendreq->req_events,                                          \
                            (uint16_t)sendreq->req_send.req_base.req_sequence,            \
                            sendreq->req_restartseq, (void *)sendreq,                     \
                            sendreq->req_recv.pval, sendreq->req_send.req_base.req_peer); \
        MCA_PML_BFO_RDMA_FRAG_RETURN(frag);                                               \
        sendreq->req_error++;                                                             \
        if (0 == sendreq->req_events) {                                                   \
            mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,                    \
                                                       MCA_PML_BFO_HDR_TYPE_PUT,          \
                                                       OMPI_ERROR, btl);                  \
        }                                                                                 \
        return;                                                                           \
    }

/* This macro checks to see if the cached number of BTLs in the
 * send request still matches the value from the endpoint.
 * If it does not, this means that a BTL was removed from the
 * available list.  In this case, start the request over.
 */
#define MCA_PML_BFO_CHECK_FOR_REMOVED_BTL(sendreq, range)                       \
    if ((int)mca_bml_base_btl_array_get_size(&sendreq->req_endpoint->btl_send)  \
        != range->range_btl_cnt) {                                              \
        sendreq->req_error++;                                                   \
        return OMPI_ERROR;                                                      \
    }


END_C_DECLS

#endif
