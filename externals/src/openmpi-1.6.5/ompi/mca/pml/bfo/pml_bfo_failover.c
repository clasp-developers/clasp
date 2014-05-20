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
 * Functions that implement failover capabilities.  To utilize the
 * failover feature, one needs to configure the library with
 * --enable-openib-failover.  Then the system that is being used
 * must have two or more openib BTLs in use.   When an error occurs,
 * the BTL will call into this PML to map out the offending BTL and
 * continue using the one that is still working.
 * Most of the differences between the ob1 PML and the bfo PML are
 * contained in this file.
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "opal/class/opal_bitmap.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/pml/base/base.h"
#include "ompi/mca/btl/base/base.h"
#include "pml_bfo.h"
#include "pml_bfo_component.h"
#include "pml_bfo_comm.h"
#include "pml_bfo_hdr.h"
#include "pml_bfo_recvfrag.h"
#include "pml_bfo_sendreq.h"
#include "pml_bfo_recvreq.h"
#include "pml_bfo_rdmafrag.h"
#include "pml_bfo_failover.h"
#include "ompi/mca/bml/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/util/show_help.h"
#include "orte/mca/notifier/notifier.h"

#include "ompi/runtime/ompi_cr.h"

static void mca_pml_bfo_error_pending_packets(mca_btl_base_module_t* btl,
                                              mca_bml_base_endpoint_t* ep);

/**
 * When running with failover enabled, check the PML sequence numbers
 * to see if we have received a duplicate message.  This check is done
 * for for all MATCH fragments.  It is also done for RNDV and RGET
 * fragments that do not have the MCA_PML_BFO_HDR_FLAGS_RESTART flag
 * set.
 * We set the window size to half the total range of sequence numbers.
 * We only enter this code when the seq_num is not the expected one.
 * A few more notes on the algorithm used here.  In normal operation,
 * the expected value will either be equal to or less than the
 * sequence number of the header.  This is because we are using this
 * sequence number to detect packets arriving prior to them being
 * expected.  If we determine that expected is less than header, then
 * make sure this is not a rollover case.  We do that by adding the
 * maxnum to the expected.
 * @param proc Pointer to proc from where message came
 * @param hdr Pointer to header of message 
 */
bool mca_pml_bfo_is_duplicate_msg(mca_pml_bfo_comm_proc_t* proc,
                                  mca_pml_bfo_match_hdr_t *hdr)
{
    const int window = 32768;
    const int maxnum = 65536;
    mca_pml_bfo_recv_frag_t *frag;

#if 0
    opal_output(0, "checking dup, exp=%d, act=%d, type=%d, cant_match=%d\n",
                (uint16_t)proc->expected_sequence,
                hdr->hdr_seq, hdr->hdr_common.hdr_type,
                opal_list_get_size(&proc->frags_cant_match));
#endif

    /* Few cases near end of values where expected may equal 65535 and
     * an out of order shows up that may equal something like 1.  */
    if (OPAL_UNLIKELY((uint16_t)proc->expected_sequence > hdr->hdr_seq)) {
        if (((uint16_t)proc->expected_sequence - hdr->hdr_seq) < window) {
            opal_output_verbose(20, mca_pml_bfo_output,
                                "%s:%d: frag duplicated, exp=%d, act=%d, type=%d\n",
                                __FILE__, __LINE__, (uint16_t)proc->expected_sequence,
                                hdr->hdr_seq, hdr->hdr_common.hdr_type);
            return true;
        }
    } else {
        /* This is the normal flow through this code.  We also need to
         * use the maxnum to ensure that we handle cases where the
         * expected number has rolled over but then a duplicate message
         * shows up that is greater than it. */
        if ((((uint16_t)proc->expected_sequence + maxnum) - hdr->hdr_seq) < window) {
            opal_output_verbose(20, mca_pml_bfo_output,
                "%s:%d: frag duplicated, exp=%d, act=%d, type=%d\n",
                __FILE__, __LINE__, (uint16_t)proc->expected_sequence,
                hdr->hdr_seq, hdr->hdr_common.hdr_type);
            return true;
        }
    }

    /* Need to explicitly check against any out of order fragments. Unfortunately, we
     * always have to do this since we can get a duplicate out of order fragment. */
    if(OPAL_UNLIKELY(opal_list_get_size(&proc->frags_cant_match) > 0)) {
        for(frag = (mca_pml_bfo_recv_frag_t*)opal_list_get_first(&proc->frags_cant_match);
            frag != (mca_pml_bfo_recv_frag_t*)opal_list_get_end(&proc->frags_cant_match);
            frag = (mca_pml_bfo_recv_frag_t*)opal_list_get_next(frag))
            {
                mca_pml_bfo_match_hdr_t* mhdr = &frag->hdr.hdr_match;

                if(mhdr->hdr_seq == hdr->hdr_seq) {
                    opal_output_verbose(20, mca_pml_bfo_output,
                        "%s:%d: frag duplicated on frags_cant_match list, seq=%d, type=%d\n",
                        __FILE__, __LINE__, hdr->hdr_seq, hdr->hdr_common.hdr_type);
                    return true;
                }
            }
    }

    return false;
}

/**
 * This function checks to see if we have received a duplicate FIN
 * message.  This is done by first pulling the pointer of the request
 * that the FIN message is pointing to from the message.  We then
 * check the various fields in the request to the fields in the header
 * and make sure they match.  If they do not, then the request must
 * have been recycled already and this is a duplicate FIN message.  We
 * have to do this check on every FIN message that we receive.
 */
bool mca_pml_bfo_is_duplicate_fin(mca_pml_bfo_hdr_t* hdr, mca_btl_base_descriptor_t* rdma,
                                  mca_btl_base_module_t* btl)
{
    mca_pml_base_request_t* basereq;
    /* When running with failover enabled, need to ensure that this
     * is not a duplicate FIN message.  */
    if (btl->btl_flags & MCA_BTL_FLAGS_FAILOVER_SUPPORT) {
        /* The first check is to make sure the descriptor is pointing
         * to a valid request.  The descriptor may be pointing to NULL
         * if it was freed and not reused yet.  */
        if (NULL == rdma->des_cbdata) {
            opal_output_verbose(20, mca_pml_bfo_output,
                                "FIN: received: dropping because not pointing to valid descriptor "
                                "PML=%d CTX=%d SRC=%d RQS=%d",
                                hdr->hdr_fin.hdr_match.hdr_seq,
                                hdr->hdr_fin.hdr_match.hdr_ctx,
                                hdr->hdr_fin.hdr_match.hdr_src,
                                hdr->hdr_fin.hdr_match.hdr_common.hdr_flags);
            return true;
        }

        basereq = (mca_pml_base_request_t*)rdma->des_cbdata;
        /* Now we know the descriptor is pointing to a non-null request.
         * Does it match what we expect?  To make sure the receiver request
         * matches the FIN message, check the context number, source of the
         * message, and MPI sequence number.  Then make sure that it also
         * matches the internal sequencing number of the requests.  We need
         * to look at the type of request we are pointing at to figure out
         * what fields to access.  */
        if (basereq->req_type == MCA_PML_REQUEST_RECV) {
            mca_pml_bfo_recv_request_t* recvreq = (mca_pml_bfo_recv_request_t*)basereq;
            if ((hdr->hdr_fin.hdr_match.hdr_ctx !=
		 recvreq->req_recv.req_base.req_comm->c_contextid) ||
                (hdr->hdr_fin.hdr_match.hdr_src !=
		 recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE) ||
                (hdr->hdr_fin.hdr_match.hdr_seq != (uint16_t)recvreq->req_msgseq)) {
                opal_output_verbose(5, mca_pml_bfo_output,
                                    "FIN: received on receiver: dropping because no match "
                                    "PML:exp=%d,act=%d CTX:exp=%d,act=%d SRC:exp=%d,act=%d "
                                    "RQS:exp=%d,act=%d, dst_req=%p",
                                    (uint16_t)recvreq->req_msgseq, hdr->hdr_fin.hdr_match.hdr_seq,
                                    recvreq->req_recv.req_base.req_comm->c_contextid,
                                    hdr->hdr_fin.hdr_match.hdr_ctx,
                                    recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE,
                                    hdr->hdr_fin.hdr_match.hdr_src,
                                    recvreq->req_restartseq,
                                    hdr->hdr_fin.hdr_match.hdr_common.hdr_flags,
                                    (void *)recvreq);
                return true;
            }
            if (hdr->hdr_fin.hdr_match.hdr_common.hdr_flags != recvreq->req_restartseq) {
                opal_output_verbose(5, mca_pml_bfo_output,
                                    "FIN: received on receiver: dropping because old "
                                    "PML:exp=%d,act=%d CTX:exp=%d,act=%d SRC:exp=%d,act=%d "
                                    "RQS:exp=%d,act=%d, dst_req=%p",
                                    (uint16_t)recvreq->req_msgseq, hdr->hdr_fin.hdr_match.hdr_seq,
                                    recvreq->req_recv.req_base.req_comm->c_contextid,
                                    hdr->hdr_fin.hdr_match.hdr_ctx,
                                    recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE,
                                    hdr->hdr_fin.hdr_match.hdr_src,
                                    recvreq->req_restartseq,
                                    hdr->hdr_fin.hdr_match.hdr_common.hdr_flags,
                                    (void *)recvreq);
                return true;
            }
        } else if (basereq->req_type == MCA_PML_REQUEST_SEND) {
            mca_pml_bfo_send_request_t* sendreq = (mca_pml_bfo_send_request_t*)basereq;
            if ((hdr->hdr_fin.hdr_match.hdr_ctx !=
		 sendreq->req_send.req_base.req_comm->c_contextid) ||
                (hdr->hdr_fin.hdr_match.hdr_src !=
		 sendreq->req_send.req_base.req_peer) ||
                (hdr->hdr_fin.hdr_match.hdr_seq !=
		 (uint16_t)sendreq->req_send.req_base.req_sequence)) {
                uint16_t seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
                opal_output_verbose(5, mca_pml_bfo_output,
                                    "FIN: received on sender: dropping because no match "
                                    "PML:exp=%d,act=%d CTX:exp=%d,act=%d SRC:exp=%d,act=%d "
                                    "RQS:exp=%d,act=%d, dst_req=%p",
                                    seq, hdr->hdr_fin.hdr_match.hdr_seq,
                                    sendreq->req_send.req_base.req_comm->c_contextid,
                                    hdr->hdr_fin.hdr_match.hdr_ctx,
                                    sendreq->req_send.req_base.req_peer,
                                    hdr->hdr_fin.hdr_match.hdr_src,
                                    sendreq->req_restartseq,
                                    hdr->hdr_fin.hdr_match.hdr_common.hdr_flags,
                                    (void *)sendreq);
                return true;
            }
            if (hdr->hdr_fin.hdr_match.hdr_common.hdr_flags != sendreq->req_restartseq) {
                uint16_t seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
                opal_output_verbose(5, mca_pml_bfo_output,
                                    "FIN: received on sender: dropping because old "
                                    "PML:exp=%d,act=%d CTX:exp=%d,act=%d SRC:exp=%d,act=%d "
                                    "RQS:exp=%d,act=%d, dst_req=%p",
                                    seq, hdr->hdr_fin.hdr_match.hdr_seq,
                                    sendreq->req_send.req_base.req_comm->c_contextid,
                                    hdr->hdr_fin.hdr_match.hdr_ctx,
                                    sendreq->req_send.req_base.req_peer,
                                    hdr->hdr_fin.hdr_match.hdr_src,
                                    sendreq->req_restartseq,
                                    hdr->hdr_fin.hdr_match.hdr_common.hdr_flags,
                                    (void *)sendreq);
                return true;
            }
        } else {
            /* We can get here if the descriptor has been reused, but
             * not as an RDMA descriptor.  In that case, the callback
             * function has been set to something else.  Clearly the
             * descriptor we are interested is gone, so just drop the
             * FIN message. */
            opal_output_verbose(5, mca_pml_bfo_output,
                                "FIN: received: dropping because descriptor has been reused "
                                "PML=%d CTX=%d SRC=%d RQS=%d rdma->des_flags=%d",
                                hdr->hdr_fin.hdr_match.hdr_seq, hdr->hdr_fin.hdr_match.hdr_ctx,
                                hdr->hdr_fin.hdr_match.hdr_src, hdr->hdr_fin.hdr_match.hdr_common.hdr_flags,
                                rdma->des_flags);
            return true;
        }
    }
    return false;
}

/**
 * Repost a FIN message if we get an error on the completion event.
 */
void mca_pml_bfo_repost_fin(struct mca_btl_base_descriptor_t* des) {
    /* In the error case, we will repost the FIN message.  I had
     * considered restarting the request.  The problem is that the
     * request may be already complete when we detect that a FIN
     * message got an error on its completion event.  For example, with
     * the PUT protocol, if the RDMA writes succeed and all the data
     * has been sent, then the request is marked as complete and can be
     * freed.  Therefore, an error on the FIN message has no request to
     * refer back to.  So, we will just repost it.  However, we are also
     * faced with the case where the FIN message has an error but it
     * actually makes it to the other side.  In that case we are now
     * sending a FIN message to a non-existent request on the receiver
     * side.  To handle that, we have added the match information to
     * the FIN message.  That way, we can check on the receiving side
     * to ensure that it is pointing to a valid request. */
    mca_pml_bfo_fin_hdr_t* hdr;
    mca_bml_base_endpoint_t* bml_endpoint;
    ompi_proc_t *proc;
    mca_bml_base_btl_t* bml_btl;

    proc = (ompi_proc_t*) des->des_cbdata;
    bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml;
    hdr = (mca_pml_bfo_fin_hdr_t*)des->des_src->seg_addr.pval;

    opal_output_verbose(20, mca_pml_bfo_output,
                        "REPOST: BFO_HDR_TYPE_FIN: seq=%d,myrank=%d,peer=%d,hdr->hdr_fail=%d,src=%d",
                        hdr->hdr_match.hdr_seq, ORTE_PROC_MY_NAME->vpid, proc->proc_name.vpid,
                        hdr->hdr_fail, hdr->hdr_match.hdr_src);

    bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_eager);

    /* Reconstruct the fin for sending on the other BTL */
    mca_pml_bfo_send_fin(proc, bml_btl,
                         hdr->hdr_des, MCA_BTL_NO_ORDER,
                         hdr->hdr_fail, hdr->hdr_match.hdr_seq,
                         hdr->hdr_match.hdr_common.hdr_flags,
                         hdr->hdr_match.hdr_ctx, hdr->hdr_match.hdr_src);
    return;
}

/**
 * This function is called when a RNDV or RGET is received with the
 * FLAGS_RESTART flag set.  This means this message already has a
 * receive request already associated with it.
 */
mca_pml_bfo_recv_request_t* mca_pml_bfo_get_request(mca_pml_bfo_match_hdr_t *hdr) {
    mca_pml_bfo_recv_request_t *match = NULL;
    mca_pml_bfo_rendezvous_hdr_t * rhdr = (mca_pml_bfo_rendezvous_hdr_t *) hdr;
    match = (mca_pml_bfo_recv_request_t *) rhdr->hdr_dst_req.pval;

    /* Check to see if we have received a duplicate RNDV (or RGET).  This can
     * occur because we got an error when we reposted the RNDV.  Therefore,
     * we make sure that the request has not completed from underneath us
     * and been recycled.  Secondly, make sure we are not getting it a
     * second time for the same request. */
    if ((rhdr->hdr_match.hdr_ctx != match->req_recv.req_base.req_comm->c_contextid) ||
        (rhdr->hdr_match.hdr_src != match->req_recv.req_base.req_ompi.req_status.MPI_SOURCE) ||
        (rhdr->hdr_match.hdr_seq != (uint16_t)match->req_msgseq) ||
        (rhdr->hdr_restartseq == match->req_restartseq)) {
        if (hdr->hdr_common.hdr_type == MCA_PML_BFO_HDR_TYPE_RNDV) {
            opal_output_verbose(20, mca_pml_bfo_output,
                                "RNDV: received with RESTART flag: duplicate, dropping "
                                "PML:exp=%d,act=%d RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                                match->req_msgseq, rhdr->hdr_match.hdr_seq, match->req_restartseq,
                                match->remote_req_send.pval, (void *)match,
                                match->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
        } else {
            opal_output_verbose(20, mca_pml_bfo_output,
                                "RGET: received with RESTART flag: duplicate, dropping "
                                "PML:exp=%d,act=%d RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                                match->req_msgseq, rhdr->hdr_match.hdr_seq, match->req_restartseq,
                                match->remote_req_send.pval, (void *)match,
                                match->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
        }
        return NULL;
    }

    mca_pml_bfo_recv_request_reset(match);
    if (hdr->hdr_common.hdr_type == MCA_PML_BFO_HDR_TYPE_RNDV) {
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RNDV: received with RESTART flag: restarting recv, "
                            "PML:exp=%d,act=%d RQS(new)=%d, src_req=%p, dst_req=%p, peer=%d",
                            match->req_msgseq, rhdr->hdr_match.hdr_seq, match->req_restartseq,
                            match->remote_req_send.pval, (void *)match,
                            match->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
    } else {
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RGET: received with RESTART flag: restarting recv, "
                            "PML:exp=%d,act=%d RQS(new)=%d, src_req=%p, dst_req=%p, peer=%d",
                            match->req_msgseq, rhdr->hdr_match.hdr_seq, match->req_restartseq,
                            match->remote_req_send.pval, (void *)match,
                            match->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
    }
    return match;
}

/**
 * Callback for when a RNDVRESTARTNOTIFY message is received.  A
 * RNDVRESTARTNOTIFY message is sent from the sender to the receiver
 * telling the receiver that the message is going to be started over.
 * The receiver first makes sure that the request being pointed to is
 * still valid.  If it is not, that means the receiver must have
 * completed the request and therefore we need to send a NACK back to
 * the sender.  The receiver then makes sure this is not a duplicate
 * message.  If it is a duplicate, it will just drop it.  Otherwise,
 * it will then send a RNDVRESTARTACK message if there are no
 * outstanding events on the receiver.  Otherwise, it will just change
 * the state of the request and wait for another event to send the
 * RNDVRESTARTACK to the sender.
 */
void mca_pml_bfo_recv_frag_callback_rndvrestartnotify(mca_btl_base_module_t* btl,
                                                      mca_btl_base_tag_t tag,
                                                      mca_btl_base_descriptor_t* des,
                                                      void* cbdata ) {
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_bfo_hdr_t* hdr = (mca_pml_bfo_hdr_t*)segments->seg_addr.pval;
    mca_pml_bfo_recv_request_t* recvreq;
    ompi_proc_t* ompi_proc;
    orte_process_name_t orte_proc;

    bfo_hdr_ntoh(hdr, MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNOTIFY);
    recvreq = (mca_pml_bfo_recv_request_t*)hdr->hdr_restart.hdr_dst_req.pval;

    /* Check to see if the receive request is still valid.  If the
     * request is recycled, that means the original request must have
     * completed and we therefore need to send a NACK back to the sender.
     * Note that when the request is gone, we need to pull some information
     * off the header so that we can figure out where to send the NACK
     * message back to. */
    if ((hdr->hdr_match.hdr_ctx != recvreq->req_recv.req_base.req_comm->c_contextid) ||
        (hdr->hdr_match.hdr_src != recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE) ||
        (hdr->hdr_match.hdr_seq != (uint16_t)recvreq->req_msgseq)) {
        orte_proc.jobid = hdr->hdr_restart.hdr_jobid;
        orte_proc.vpid = hdr->hdr_restart.hdr_vpid;
        ompi_proc = ompi_proc_find(&orte_proc);
        opal_output_verbose(20, mca_pml_bfo_output,
                            "RNDVRESTARTNOTIFY: received: does not match request, sending NACK back "
                            "PML:req=%d,hdr=%d CTX:req=%d,hdr=%d SRC:req=%d,hdr=%d "
                            "RQS:req=%d,hdr=%d src_req=%p, dst_req=%p, peer=%d, hdr->hdr_jobid=%d, "
                            "hdr->hdr_vpid=%d, ompi_proc->proc_hostname=%s",
                            (uint16_t)recvreq->req_msgseq, hdr->hdr_match.hdr_seq,
                            recvreq->req_recv.req_base.req_comm->c_contextid, hdr->hdr_match.hdr_ctx,
                            recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE,
                            hdr->hdr_match.hdr_src, recvreq->req_restartseq,
                            hdr->hdr_restart.hdr_restartseq,
                            recvreq->remote_req_send.pval, (void *)recvreq,
                            recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE,
                            hdr->hdr_restart.hdr_jobid, hdr->hdr_restart.hdr_vpid,
                            ompi_proc->proc_hostname);
        mca_pml_bfo_recv_request_rndvrestartnack(des, ompi_proc, false);
        return;
    }

    /* We know that we have the correct receive request.  Make sure this is not
     * a duplicate RNDVRESTARTNOTIFY on this request. */
    if (hdr->hdr_restart.hdr_restartseq == recvreq->req_restartseq) {
        opal_output_verbose(20, mca_pml_bfo_output,
                            "RNDVRESTARTNOTIFY: received duplicate: dropping RNDVRESTARTNOTIFY "
                            "message PML:req=%d,hdr=%d CTX:req=%d,hdr=%d SRC:req=%d,hdr=%d "
                            "RQS:req=%d,hdr=%d src_req=%p, dst_req=%p, peer=%d",
                            (uint16_t)recvreq->req_msgseq, hdr->hdr_match.hdr_seq,
                            recvreq->req_recv.req_base.req_comm->c_contextid, hdr->hdr_match.hdr_ctx,
                            recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE,
                            hdr->hdr_match.hdr_src, recvreq->req_restartseq,
                            hdr->hdr_restart.hdr_restartseq,
                            recvreq->remote_req_send.pval, (void *)recvreq,
                            recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
        return;
    }

    /* Increment restart number. */
    recvreq->req_restartseq++;
    recvreq->req_errstate |= RECVREQ_RNDVRESTART_RECVED;
    opal_output_verbose(30, mca_pml_bfo_output,
                        "RNDVRESTARTNOTIFY: received: outstanding receive events=%d, "
                        "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                        recvreq->req_events, recvreq->req_msgseq, recvreq->req_restartseq,
                        recvreq->remote_req_send.pval, (void *)recvreq,
                        recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);

    if (0 == recvreq->req_events) {
        mca_pml_bfo_recv_request_rndvrestartack(recvreq, MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNOTIFY,
                                                OMPI_SUCCESS, btl);
    }

    return;
}

/**
 * Callback for when a RNDVRESTARTACK message is received.  This
 * message is sent from the receiver to the sender to acknowledge
 * the receipt of the RNDVRESTARTNOTIFY message.  At this point,
 * the sender can reset the send request and restart the message.
 */
void mca_pml_bfo_recv_frag_callback_rndvrestartack(mca_btl_base_module_t* btl,
                                                   mca_btl_base_tag_t tag,
                                                   mca_btl_base_descriptor_t* des,
                                                   void* cbdata ) {
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_bfo_hdr_t* hdr = (mca_pml_bfo_hdr_t*)segments->seg_addr.pval;
    mca_pml_bfo_send_request_t* sendreq;

    bfo_hdr_ntoh(hdr, MCA_PML_BFO_HDR_TYPE_RNDVRESTARTACK);
    sendreq = (mca_pml_bfo_send_request_t*)hdr->hdr_restart.hdr_src_req.pval;

    /* Check to see if we have received a duplicate message.  The
     * first three comparisons make sure that we are not looking at a
     * recycled request.  The last check makes sure we are not getting
     * a duplicate message for this specific request.  All of this is
     * needed because the receiver might get an error and repost the
     * RNDVRESTARTACK message, but the RNDVRESTARTACK was actually received. */
    if ((hdr->hdr_match.hdr_ctx != sendreq->req_send.req_base.req_comm->c_contextid) ||
        (hdr->hdr_match.hdr_src != sendreq->req_send.req_base.req_peer) ||
        (hdr->hdr_match.hdr_seq != (uint16_t)sendreq->req_send.req_base.req_sequence) ||
        (hdr->hdr_restart.hdr_restartseq != sendreq->req_restartseq)) {
        opal_output_verbose(20, mca_pml_bfo_output,
                            "RNDVRESTARTACK: received: does not match request, dropping "
                            "PML:exp=%d,act=%d CTX:exp=%d,act=%d SRC:exp=%d,act=%d EXP:exp=%d,act=%d "
                            "src_req=%p, dst_req=%p, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence, hdr->hdr_match.hdr_seq,
                            sendreq->req_send.req_base.req_comm->c_contextid, hdr->hdr_match.hdr_ctx,
                            sendreq->req_send.req_base.req_peer, hdr->hdr_match.hdr_src,
                            sendreq->req_restartseq, hdr->hdr_restart.hdr_restartseq,
                            (void *)sendreq, sendreq->req_recv.pval,
                            sendreq->req_send.req_base.req_peer);
        return;
    }

    sendreq->req_restart++;
    if (2 == sendreq->req_restart) {
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RNDVRESTARTACK: received: restarting send "
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                            hdr->hdr_match.hdr_seq, hdr->hdr_restart.hdr_restartseq,
                            (void *)sendreq, sendreq->req_recv.pval,
                            sendreq->req_send.req_base.req_peer);
        mca_pml_bfo_send_request_restart(sendreq, false, 0);
    } else {
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RNDVRESTARTACK received: waiting for RNDVRESTARTNOTIFY completion "
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                            hdr->hdr_match.hdr_seq, hdr->hdr_restart.hdr_restartseq,
                            (void *)sendreq, sendreq->req_recv.pval,    
                            sendreq->req_send.req_base.req_peer);
    }
    return;
}


/**
 * Callback for when a RECVERRNOTIFY message is received.  This message
 * is sent from the receiver to the sender and tells the sender that
 * the receiver has seen an error.  This will trigger the sender
 * to start the request restart sequence.
 */
void mca_pml_bfo_recv_frag_callback_recverrnotify(mca_btl_base_module_t* btl,
                                                  mca_btl_base_tag_t tag,
                                                  mca_btl_base_descriptor_t* des,
                                                  void* cbdata ) {
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_bfo_hdr_t* hdr = (mca_pml_bfo_hdr_t*)segments->seg_addr.pval;
    mca_pml_bfo_send_request_t* sendreq;

    bfo_hdr_ntoh(hdr, MCA_PML_BFO_HDR_TYPE_RECVERRNOTIFY);
    sendreq = (mca_pml_bfo_send_request_t*)hdr->hdr_restart.hdr_src_req.pval;

    /* First make sure that this message is pointing to a valid request.
     * This can be determined if the communicator context, the source of
     * the message, and the MPI sequence number all match. */
    if ((hdr->hdr_match.hdr_ctx != sendreq->req_send.req_base.req_comm->c_contextid) ||
        (hdr->hdr_match.hdr_src != sendreq->req_send.req_base.req_peer) ||
        (hdr->hdr_match.hdr_seq != (uint16_t)sendreq->req_send.req_base.req_sequence)) {
        opal_output_verbose(20, mca_pml_bfo_output,
                            "RECVERRNOTIFY: received: does not match request, dropping "
                            "PML:exp=%d,act=%d CTX:exp=%d,act=%d SRC:exp=%d,act=%d RQS:exp=%d,act=%d "
                            "src_req=%p, dst_req=%p, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence, hdr->hdr_match.hdr_seq,
                            sendreq->req_send.req_base.req_comm->c_contextid, hdr->hdr_match.hdr_ctx,
                            sendreq->req_send.req_base.req_peer, hdr->hdr_match.hdr_src,
                            sendreq->req_restartseq, hdr->hdr_restart.hdr_restartseq,
                            (void *)sendreq, sendreq->req_recv.pval,
                            sendreq->req_send.req_base.req_peer);
        return;
    }

    /* If a good ACK was never received, then the first ACK received
     * might be a RECVERRNOTIFY message.  In that case, the sendreq does not
     * have a valid req_recv pointer in it.  Therefore, check for that
     * case and update the field in the sendreq if necessary. */
    if (NULL == sendreq->req_recv.pval) {
        sendreq->req_recv = hdr->hdr_restart.hdr_dst_req;
    }

    /* Now check to see a restart needs to be issued.  The request
     * sequence number in the header is compared against the current
     * request sequence number in the send request.  If the header
     * sequence number is greater than or equal to the send request
     * number, then a rndvrestartnotify is issued.  There are some cases
     * where a few extra rndvrestartnotifys are issued.  That is OK as
     * it will all work itself out.  The idea is to prevent many
     * restarts unnecessarily.  This still allows multiple restarts to
     * happen.  It could be that sometime later another error occurs
     * which initiates a restart.  That is OK as it will have the new
     * sequence number and all is well. */
    if (hdr->hdr_restart.hdr_restartseq >= sendreq->req_restartseq) {
        assert(sendreq->req_send.req_base.req_ompi.req_state == OMPI_REQUEST_ACTIVE);
        sendreq->req_error++;
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RECVERRNOTIFY: received: sendreq has error, outstanding events=%d, "
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                            sendreq->req_events, (uint16_t)sendreq->req_send.req_base.req_sequence,
                            sendreq->req_restartseq, (void *)sendreq,
                            sendreq->req_recv.pval,
                            sendreq->req_send.req_base.req_peer);

        if (0 == sendreq->req_events) {
            mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,
                                                       MCA_PML_BFO_HDR_TYPE_RECVERRNOTIFY,
                                                       OMPI_SUCCESS, btl);
        }
    } else {
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RECVERRNOTIFY: received: error has already been noted, ignoring "
                            "PML:exp=%d,act=%d RQS:exp=%d,act=%d src_req=%p, dst_req=%p, peer=%d",
                            sendreq->req_restartseq, hdr->hdr_restart.hdr_restartseq,
                            (uint16_t)sendreq->req_send.req_base.req_sequence, hdr->hdr_match.hdr_seq,
                            (void *)sendreq, sendreq->req_recv.pval,
                            sendreq->req_send.req_base.req_peer);
    }
    return;
}

/**
 * Callback for when a RNDVRESTARTNACK message is received.  This message
 * is sent from the receiver to the sender and tells the sender that
 * the receiver has already completed the message and there is nothing
 * else to be done.  The sender should then just make the send request
 * complete.
 */
void mca_pml_bfo_recv_frag_callback_rndvrestartnack(mca_btl_base_module_t* btl,
                                                    mca_btl_base_tag_t tag,
                                                    mca_btl_base_descriptor_t* des,
                                                    void* cbdata ) {

    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_bfo_hdr_t* hdr = (mca_pml_bfo_hdr_t*)segments->seg_addr.pval;
    mca_pml_bfo_send_request_t* sendreq;

    bfo_hdr_ntoh(hdr, MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNACK);
    sendreq = (mca_pml_bfo_send_request_t*)hdr->hdr_restart.hdr_src_req.pval;

    /* Not convinced a RNDVRESTARTNACK that does not match a request can
     * happen, but have the check in here anyways for now */
    if ((hdr->hdr_match.hdr_ctx != sendreq->req_send.req_base.req_comm->c_contextid) ||
        (hdr->hdr_match.hdr_src != sendreq->req_send.req_base.req_peer) ||
        (hdr->hdr_match.hdr_seq != (uint16_t)sendreq->req_send.req_base.req_sequence) ||
        (hdr->hdr_restart.hdr_restartseq != sendreq->req_restartseq)) {
        opal_output_verbose(20, mca_pml_bfo_output,
                            "RNDVRESTARTNACK: received: does not match request, dropping "
                            "PML:exp=%d,act=%d CTX:exp=%d,act=%d SRC:exp=%d,act=%d EXP:exp=%d,act=%d "
                            "src_req=%p, dst_req=%p, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence, hdr->hdr_match.hdr_seq,
                            sendreq->req_send.req_base.req_comm->c_contextid, hdr->hdr_match.hdr_ctx,
                            sendreq->req_send.req_base.req_peer, hdr->hdr_match.hdr_src,
                            sendreq->req_restartseq, hdr->hdr_restart.hdr_restartseq,
                            (void *)sendreq, sendreq->req_recv.pval,
                            sendreq->req_send.req_base.req_peer);
        return;
    }

    opal_output_verbose(20, mca_pml_bfo_output,
                        "RNDVRESTARTNACK: received: marking send request as complete "
                        "PML=%d CTX=%d SRC=%d EXP=%d "
                        "src_req=%p, dst_req=%p, peer=%d",
                        (uint16_t)sendreq->req_send.req_base.req_sequence,
                        sendreq->req_send.req_base.req_comm->c_contextid,
                        sendreq->req_send.req_base.req_peer, sendreq->req_restartseq,
                        (void *)sendreq, sendreq->req_recv.pval,
                        sendreq->req_send.req_base.req_peer);
    /* Mark the sender complete.  This data exchange is over. */
    send_request_pml_complete(sendreq);
    return;
}


/**
 * This function gets called when failover is enabled and an error
 * occurs during the rendezvous protocol.  A message is sent to the
 * receiving side notifying the request that the communication is
 * going to be starting over.  However, none of the information in the
 * send request is reset yet, so that any in flight fragments can
 * still find a home.  Information in the send request gets reset when
 * the completion event for this send occurs AND an ACK has been
 * received back from the receiver.
 */
void mca_pml_bfo_send_request_rndvrestartnotify(mca_pml_bfo_send_request_t* sendreq,
                                                bool repost, mca_btl_base_tag_t tag,
                                                int status, mca_btl_base_module_t* btl)
{
    mca_btl_base_descriptor_t* des;
    mca_pml_bfo_restart_hdr_t* restart;
    int rc;
    mca_bml_base_btl_t* bml_btl;
    ompi_proc_t* proc = (ompi_proc_t*)sendreq->req_send.req_base.req_proc;
    mca_bml_base_endpoint_t* bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml;

    /* If this message is not a repost, then update the sequence number. */
    if (!repost) {
        /* Bump up the rendezvous request sequence number. */
        sendreq->req_restartseq++;
    }

    assert(0 == sendreq->req_events);
    assert(0 != bml_endpoint->btl_eager.arr_size);

    /* In the case that this is started because the receiver has
     * sent us a message, then attempt to use a different BTL than the
     * error message was received on.  This may potentially tickle the
     * error sooner if this side has not seen it yet. */
    bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_eager);
    if (bml_btl->btl == btl) {
        /* If there is more than one BTL left, then we will get a 
         * different one.  If there is only one, we will just get 
         * the same one back again.  That is OK. */
        bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_eager);
    }

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &des, MCA_BTL_NO_ORDER,
                       sizeof(mca_pml_bfo_restart_hdr_t),
                       MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP |
                       MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if( OPAL_UNLIKELY(NULL == des) ) {
        opal_output(0, "%s:%d Our of resources, cannot proceed", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }

    /* fill out header */
    restart = (mca_pml_bfo_restart_hdr_t*)des->des_src->seg_addr.pval;
    restart->hdr_match.hdr_common.hdr_flags = 0;
    restart->hdr_match.hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNOTIFY;
    restart->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    restart->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    restart->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
    restart->hdr_restartseq = sendreq->req_restartseq;
    restart->hdr_src_req.pval = sendreq;
    restart->hdr_dst_req = sendreq->req_recv;
    restart->hdr_dst_rank = sendreq->req_send.req_base.req_peer; /* Needed for NACKs */
    restart->hdr_jobid = ORTE_PROC_MY_NAME->jobid;
    restart->hdr_vpid = ORTE_PROC_MY_NAME->vpid;

    bfo_hdr_hton(restart, MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNOTIFY, proc);

    /* initialize descriptor */
    des->des_cbfunc = mca_pml_bfo_rndvrestartnotify_completion;

    opal_output_verbose(30, mca_pml_bfo_output,
                        "RNDVRESTARTNOTIFY: sent: PML=%d, RQS(new)=%d, CTX=%d, SRC=%d, "
                        "src_req=%p, dst_req=%p, peer=%d",
                        (uint16_t)sendreq->req_send.req_base.req_sequence, sendreq->req_restartseq,
                        restart->hdr_match.hdr_ctx, restart->hdr_match.hdr_src,
                        (void *)sendreq, sendreq->req_recv.pval,
                        sendreq->req_send.req_base.req_peer);

    rc = mca_bml_base_send(bml_btl, des, MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNOTIFY);
    if( OPAL_UNLIKELY( rc < 0 ) ) {
        opal_output(0, "[%s:%d] Cannot send rndvrestartnotify message", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }

}

/**
 * This function restarts a RNDV send request.  When this is called,
 * all the fields in the send request are reset and the send is
 * started over.  The sendreq->req_restartseq will be non-zero which will
 * trigger a special flag in the RNDV header which indicates the match
 * has already happened on the receiving side.
 */
void mca_pml_bfo_send_request_restart(mca_pml_bfo_send_request_t* sendreq,
                                      bool repost, mca_btl_base_tag_t tag)
{
    size_t offset = 0;
    opal_list_item_t *first_item;
    opal_list_item_t *last_item;
    mca_bml_base_endpoint_t* endpoint;
    size_t i;

    /* If the tag is something valid, it was a repost.  We could also
     * check the repost field as well.  Maybe I can drop the
     * repost and have the tag double as it. */
    switch (tag) {
    case MCA_PML_BFO_HDR_TYPE_RNDV:
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RNDV: completion failed, reset and repost: PML=%d, RQS=%d, "
                            "CTX=%d, SRC=%d, src_req=%p, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence, sendreq->req_restartseq,
                            sendreq->req_send.req_base.req_comm->c_contextid,
                            sendreq->req_send.req_base.req_comm->c_my_rank, (void *)sendreq,
                            sendreq->req_send.req_base.req_peer);
        break;
    case MCA_PML_BFO_HDR_TYPE_RGET:
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RGET: completion failed, reset and repost: PML=%d, RQS=%d, "
                            "CTX=%d, SRC=%d, src_req=%p, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence, sendreq->req_restartseq,
                            sendreq->req_send.req_base.req_comm->c_contextid,
                            sendreq->req_send.req_base.req_comm->c_my_rank, (void *)sendreq,
                            sendreq->req_send.req_base.req_peer);
        break;
    default:
        break;
    }

    /* Return mpool resources, they get reacquired when request starts over. */
    mca_pml_bfo_free_rdma_resources(sendreq);

    /* Release any memory in use if this is a buffered send */
    if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED &&
        sendreq->req_send.req_addr != sendreq->req_send.req_base.req_addr) {
        mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);
    }

    /* Clear out any unsent send ranges.  Recreate the functionality 
     * from the get_send_range() and get_next_send_range() functions. */
    OPAL_THREAD_LOCK(&sendreq->req_send_range_lock);
    first_item = opal_list_get_begin(&sendreq->req_send_ranges);
    last_item = opal_list_get_last(&sendreq->req_send_ranges);
    while (first_item != last_item) {
        opal_list_remove_item(&sendreq->req_send_ranges, last_item);
        OMPI_FREE_LIST_RETURN(&mca_pml_bfo.send_ranges, (ompi_free_list_item_t *)last_item);
        last_item = opal_list_get_last(&sendreq->req_send_ranges);
    }
    OPAL_THREAD_UNLOCK(&sendreq->req_send_range_lock);

    /* Reset the converter to the beginning. */
    opal_convertor_set_position(&sendreq->req_send.req_base.req_convertor,
                                &offset);

    /* Bump up internal sequence number to handle possible duplicate
     * RNDV messages.  In the case of reposting a RNDV message, do not
     * increment the value.  That way, a duplicate message can be
     * detected. */
    if (!repost) {
        sendreq->req_restartseq++;
    }

    /* This code here is essentially the same is mca_pml_bfo_send_request_start()
     * but with a few modifications since we are restarting the request, not
     * starting entirely from scratch. */
    endpoint = (mca_bml_base_endpoint_t*)sendreq->req_send.req_base.req_proc->proc_bml;
    sendreq->req_endpoint = endpoint;
    sendreq->req_state = 0;
    sendreq->req_lock = 0;
    sendreq->req_pipeline_depth = 0;
    sendreq->req_bytes_delivered = 0;
    sendreq->req_pending = MCA_PML_BFO_SEND_PENDING_NONE;

    /* Note that we do not reset the following three items.
     * They stay with their original values.
     *     sendreq->req_send.req_base.req_sequence
     *     sendreq->req_restartseq
     *     sendreq->req_recv.pval
     */
    sendreq->req_restart = 0;         /* reset in case we restart again */
    sendreq->req_error = 0;           /* clear error state */
    sendreq->req_events = 0;          /* clear events, probably 0 anyways */

    MCA_PML_BASE_SEND_START( &sendreq->req_send.req_base );

    for(i = 0; i < mca_bml_base_btl_array_get_size(&endpoint->btl_eager); i++) {
        mca_bml_base_btl_t* bml_btl;
        int rc;

        /* select a btl */
        bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
        rc = mca_pml_bfo_send_request_start_btl(sendreq, bml_btl);
        if(OPAL_LIKELY(OMPI_ERR_OUT_OF_RESOURCE != rc))
            return;
    }
    add_request_to_send_pending(sendreq, MCA_PML_BFO_SEND_PENDING_START, true);
}

/**
 * This function will repost a match fragment.  This function has to
 * handle the case where there may not be a request associated with
 * the fragment and just use the information in the fragment to
 * repost the send.
 */
void mca_pml_bfo_repost_match_fragment(struct mca_btl_base_descriptor_t* des)
{
    mca_pml_bfo_send_request_t* sendreq = (mca_pml_bfo_send_request_t*)des->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context;
    struct mca_bml_base_endpoint_t* endpoint;
    int rc;
    size_t offset = 0;

    /* At this point a determination has to be made whether the
     * BFO_HDR_TYPE_MATCH fragment was sent via the sendi interface or
     * via the regular send interface.  This is important because if it
     * was sent via the sendi interface, then the request associated
     * with it has already been completed and released.  This can be
     * determined by looking at the des->des_flags field of the
     * descriptor.  If the ALWAYS_CALLBACK flag is set then it is known
     * that there is a valid send request associated with the fragment
     * and it can be used to extricate information.  If ALWAYS_CALLBACK
     * is not set, then the endpoint information is in the callback
     * data field and where to resend the fragment can be determined
     * from the fragment. */
    if (des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
        endpoint = sendreq->req_endpoint;
        opal_output_verbose(30, mca_pml_bfo_output,
                            "MATCH: repost: src_req=%p",
                            (void *)sendreq);
    } else {
        endpoint = des->des_cbdata;
        opal_output_verbose(30, mca_pml_bfo_output,
                            "MATCH: repost: des=%p (sendi fragment)",
                            (void *)des);
    }

    assert(0 != endpoint->btl_eager.arr_size);
    bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);

    if (des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
        /* Reset the converter to the beginning if the message is
         * not a zero-length message.  In the case of zero-length
         * message, the convertor is not being used. */
        if (0 != sendreq->req_send.req_bytes_packed) {
            opal_convertor_set_position(&sendreq->req_send.req_base.req_convertor,
                                        &offset);
        }
        rc = mca_pml_bfo_send_request_start_btl(sendreq, bml_btl);
        if (OMPI_SUCCESS == rc) {
            return;
        } else if (OMPI_ERR_OUT_OF_RESOURCE == rc) {
            opal_output_verbose(30, mca_pml_bfo_output,
                                "Warning: delaying reposting of BFO_HDR_TYPE_MATCH, btls=%d",
                                (int)sendreq->req_endpoint->btl_eager.arr_size);
            add_request_to_send_pending(sendreq, MCA_PML_BFO_SEND_PENDING_START, true);
            return;
        } else {
            opal_output(0, "%s:%d FATAL ERROR, cannot repost BFO_HDR_TYPE_MATCH",
                        __FILE__, __LINE__);
            orte_errmgr.abort(-1, NULL);
        }
    } else {
        /* No send request available so alloc and repost explicitly */
        mca_btl_base_descriptor_t* newdes = NULL;
        mca_btl_base_segment_t* oldseg;
        mca_btl_base_segment_t* newseg;

        oldseg = des->des_src;
        /* The alloc routine must be called with the MCA_BTL_NO_ORDER
         * flag so that the allocation routine works.  The allocation
         * will fill in the order flag in the descriptor. */
        mca_bml_base_alloc( bml_btl, &newdes,
                            MCA_BTL_NO_ORDER,
                            oldseg->seg_len,
                            MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
        if (OPAL_UNLIKELY(NULL == newdes)) {
            opal_output(0, "%s:%d FATAL ERROR, cannot repost BFO_HDR_TYPE_MATCH",
                        __FILE__, __LINE__);
            orte_errmgr.abort(-1, NULL);
        }
        newseg = newdes->des_src;
        /* Copy over all the data that is actually sent over the wire */
        memcpy(newseg->seg_addr.pval, oldseg->seg_addr.pval, oldseg->seg_len);
        newseg->seg_len = oldseg->seg_len;

        /* This call will either return OMPI_SUCCESS or OMPI_ERROR.  The
         * OMPI_SUCCESS only says that the send request can be freed.
         * It may be that the message was queued up in the BTL. */
        rc = mca_bml_base_send(bml_btl, newdes, MCA_PML_BFO_HDR_TYPE_MATCH);

        /* Some BTLs will set the CALLBACK flag but we do not want that
         * as there is no longer a request associated with this descriptor.
         * Therefore, always make sure it is cleared.  */
        newdes->des_flags &= ~MCA_BTL_DES_SEND_ALWAYS_CALLBACK;

        if( OPAL_LIKELY( rc >= 0 )) {
            /* Just let the normal flow of data free whatever needs
             * to be freed */
            return;
        } else {
            opal_output(0, "%s:%d FATAL ERROR, cannot repost BFO_HDR_TYPE_MATCH",
                        __FILE__, __LINE__);
            orte_errmgr.abort(-1, NULL);
        }
   }
    /* No need to free any descriptors.  The BTLs take care of it since
     * we originally allocated with MCA_BTL_DES_FLAGS_BTL_OWNERSHIP. */
}

/**
 * Completion callback for rndvrestartnotify completion event.  If the
 * RNDVRESTARTACK has already been received, then reset and restart.
 * Otherwise, just update the state and let the RNDVRESTARTACK trigger
 * the reset and restart.
 */
void
mca_pml_bfo_rndvrestartnotify_completion(mca_btl_base_module_t* btl,
                                         struct mca_btl_base_endpoint_t* ep,
                                         struct mca_btl_base_descriptor_t* des,
                                         int status)
{
    mca_pml_bfo_restart_hdr_t* restart;
    mca_pml_bfo_send_request_t* sendreq;

    restart = (mca_pml_bfo_restart_hdr_t*)des->des_src->seg_addr.pval;
    sendreq = (mca_pml_bfo_send_request_t*) restart->hdr_src_req.pval;

    /* Need to resend this message in the case that it fails */
    if( OPAL_UNLIKELY((OMPI_SUCCESS != status))) {
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RNDVRESTARTNOTIFY: completion failed: repost "
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence,
                            sendreq->req_restartseq,
                            (void *)sendreq, sendreq->req_recv.pval,
                            sendreq->req_send.req_base.req_peer);
        /* Repost the message and indicate it is a repost, not a new one. No need
         * to check the req_events as this is the only possible outstanding send
         * event when we have posted this message.  We also know the sendreq is still
         * available because nothing can proceed until this completion event happens
         * successfully as we track the req_restart value. */
        mca_pml_bfo_send_request_rndvrestartnotify(sendreq, true,
                                                   MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNOTIFY,
                                                   status, btl);
        return;
    }

    /* The req_restart value is incremented to indicate completion of
     * the RNDVRESTARTNOTIFY message.  Then (typically) the arrival of the
     * ACK message will cause the request to reset and restart. Need to
     * make sure that RNDVRESTARTNOTIFY callback has been called as well as
     * the ACK back from the receiver prior to resetting and restarting
     * the request.  This is needed in case we get an error on the
     * RNDVRESTARTNOTIFY message, but it actually makes it over. We want
     * to make sure the send request has not restarted yet.  So, keep a
     * counter that counts to 2. */
    sendreq->req_restart++;
    if (2 == sendreq->req_restart) {
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RNDVRESTARTNOTIFY: completion: restarting request "
                            "PML=%d, RQS=%d, CTX=%d, src_req=%p, dst_req=%p, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence,
                            sendreq->req_restartseq,
                            sendreq->req_send.req_base.req_comm->c_contextid,
                            sendreq->req_recv.pval, (void *)sendreq,
                            sendreq->req_send.req_base.req_peer);
        mca_pml_bfo_send_request_restart(sendreq, false, 0);
    } else {
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RNDVRESTARTNOTIFY: completion: waiting for ack "
                            "PML=%d, RQS=%d, CTX=%d, src_req=%p, dst_req=%p, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence,
                            sendreq->req_restartseq,
                            sendreq->req_send.req_base.req_comm->c_contextid,
                            sendreq->req_recv.pval, (void *)sendreq,
                            sendreq->req_send.req_base.req_peer);
    }
}

/**
 * This function is called when an error is detected on a completion
 * event on the receiving side.  This can come from a ACK, PUT, RDMA
 * read (GET) or RECVERRNOTIFY completion event.  When this happens, check
 * the state of the request and decide if the sender needs be notified
 * that a problem was seen.  If no RECVERRNOTIFY message has been sent and
 * no RNDVRESTARTNOTIFY has been received from the sender, then send a
 * message telling the sender an error was seen.
 */
void mca_pml_bfo_recv_request_recverrnotify(mca_pml_bfo_recv_request_t* recvreq,
                                            mca_btl_base_tag_t tag, int status)
{
    mca_btl_base_descriptor_t* des;
    mca_pml_bfo_restart_hdr_t* restart;
    ompi_proc_t* proc = (ompi_proc_t*)recvreq->req_recv.req_base.req_proc;
    mca_bml_base_endpoint_t* bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml;
    mca_bml_base_btl_t* bml_btl;
    int rc;

    assert(0 != bml_endpoint->btl_eager.arr_size);

    bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_eager);

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &des, MCA_BTL_NO_ORDER,
                       sizeof(mca_pml_bfo_restart_hdr_t),
                       MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP |
                       MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if( OPAL_UNLIKELY(NULL == des) ) {
        opal_output(0, "%s:%d Out of resources, cannot proceed", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }

    /* fill out header */
    restart = (mca_pml_bfo_restart_hdr_t*)des->des_src->seg_addr.pval;
    restart->hdr_match.hdr_common.hdr_flags = 0;
    restart->hdr_match.hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_RECVERRNOTIFY;
    restart->hdr_match.hdr_ctx = recvreq->req_recv.req_base.req_comm->c_contextid;
    restart->hdr_match.hdr_src = recvreq->req_recv.req_base.req_comm->c_my_rank;
    restart->hdr_match.hdr_seq = (uint16_t)recvreq->req_msgseq;
    restart->hdr_restartseq = recvreq->req_restartseq;
    restart->hdr_src_req = recvreq->remote_req_send;
    restart->hdr_dst_req.pval = recvreq;

    bfo_hdr_hton(restart, MCA_PML_BFO_HDR_TYPE_RECVERRNOTIFY, proc);

    /* initialize descriptor */
    des->des_cbfunc = mca_pml_bfo_recv_restart_completion;

    opal_output_verbose(30, mca_pml_bfo_output,
                        "RECVERRNOTIFY: sending to sender, "
                        "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d, btl=%p",
                        recvreq->req_msgseq, recvreq->req_restartseq,
                        recvreq->remote_req_send.pval,
                        (void *)recvreq,
                        recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE,
                        (void *)bml_btl->btl);

    rc = mca_bml_base_send(bml_btl, des, MCA_PML_BFO_HDR_TYPE_RECVERRNOTIFY);
    if( OPAL_UNLIKELY( rc < 0 ) ) {
        opal_output(0, "[%s:%d] Cannot send recverrnotify message", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }
    /* Prevent future error messages on this request */
    recvreq->req_errstate |= RECVREQ_RECVERRSENT;
}

/**
 * This function is called when it may be time to send a RNDVRESTARTACK
 * message back to the sending side.  This can happen because we
 * received a RNDVRESTARTNOTIFY message from the sender.  This can
 * also happen if we have noticed that the request has received the
 * RNDVRESTARTNOTIFY message, but has not yet sent out the RNDVRESTARTACK
 * because there were still some pending receive events on the request.
 * That means we can enter this routine from a completion event on a ACK,
 * PUT, or RDMA read as well as from the receipt of a RNDVRESTARTNOTIFY
 * message.  If all is good, we sent the RNDVRESTARTACK message back to
 * the sender.  Then sometime later a message will arrive telling us
 * to reset and restart the receive request.
 */
void mca_pml_bfo_recv_request_rndvrestartack(mca_pml_bfo_recv_request_t* recvreq,
                                            mca_btl_base_tag_t tag, int status,
                                            mca_btl_base_module_t* btl)
{
    mca_btl_base_descriptor_t* des;
    mca_pml_bfo_restart_hdr_t* restart;
    ompi_proc_t* proc = (ompi_proc_t*)recvreq->req_recv.req_base.req_proc;
    mca_bml_base_endpoint_t* bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml;
    mca_bml_base_btl_t* bml_btl;
    int rc;

    assert((recvreq->req_errstate & RECVREQ_RNDVRESTART_RECVED) == RECVREQ_RNDVRESTART_RECVED);
    assert((recvreq->req_errstate & RECVREQ_RNDVRESTART_ACKED) == 0);
    assert(0 != bml_endpoint->btl_eager.arr_size);

    bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_eager);

    /* Attempt to use a different BTL than the error message was
     * received on.  This may potentially tickle the error sooner if
     * this side has not seen it yet. */
    if (bml_btl->btl == btl) {
        /* If there is more than one BTL left, then we will get a 
         * different one.  If there is only one, we will just get 
         * the same one back again.  That is OK. */
        bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_eager);
    }

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &des, MCA_BTL_NO_ORDER,
                       sizeof(mca_pml_bfo_restart_hdr_t),
                       MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP |
                       MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if( OPAL_UNLIKELY(NULL == des) ) {
        opal_output(0, "%s:%d Out of resources, cannot proceed", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }

    /* fill out header */
    restart = (mca_pml_bfo_restart_hdr_t*)des->des_src->seg_addr.pval;
    restart->hdr_match.hdr_common.hdr_flags = 0;
    restart->hdr_match.hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_RNDVRESTARTACK;
    restart->hdr_match.hdr_ctx = recvreq->req_recv.req_base.req_comm->c_contextid;
    restart->hdr_match.hdr_src = recvreq->req_recv.req_base.req_comm->c_my_rank;
    restart->hdr_match.hdr_seq = (uint16_t)recvreq->req_msgseq;
    restart->hdr_restartseq = recvreq->req_restartseq;
    restart->hdr_src_req = recvreq->remote_req_send;
    restart->hdr_dst_req.pval = recvreq;

    bfo_hdr_hton(restart, MCA_PML_BFO_HDR_TYPE_RNDVRESTARTACK, proc);

    /* initialize descriptor */
    des->des_cbfunc = mca_pml_bfo_recv_restart_completion;
    des->des_cbdata = (void *)proc;

    opal_output_verbose(30, mca_pml_bfo_output,
                        "RNDVRESTARTACK: due to PML tag=%d completion, sending to "
                        "sender, PML=%d, RQS=%d, src_req=%p, dst_req=%p, status=%d, "
                        "peer=%d, btl=%p",
                        tag, recvreq->req_msgseq, recvreq->req_restartseq,
                        recvreq->remote_req_send.pval, (void *)recvreq, status,
                        recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE,
                        (void *)bml_btl->btl);

    rc = mca_bml_base_send(bml_btl, des, MCA_PML_BFO_HDR_TYPE_RNDVRESTARTACK);
    if( OPAL_UNLIKELY( rc < 0 ) ) {
        opal_output(0, "[%s:%d] Cannot send rndvrestartack message", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }
    /* Move to the next state so we do not send anymore ACKs */
    recvreq->req_errstate &= ~RECVREQ_RNDVRESTART_RECVED;
    recvreq->req_errstate |= RECVREQ_RNDVRESTART_ACKED;
}

/**
 * Called after the receipt of a RNDVRESTARTNOTIFY message to a request
 * that no longer matches.  This can happen if the sender detected an
 * error, but the receiver actually received all the data.  Therefore
 * send a NACK back instead of the ACK so that the sender can complete
 * its request.  This happens very rarely.  Note that we need to make
 * use of the hdr_dst_rank that we received from the notify message.
 * This is so the sending side make sure the message matches a valid
 * request on the sending side.
 */
void mca_pml_bfo_recv_request_rndvrestartnack(mca_btl_base_descriptor_t* olddes,
                                              ompi_proc_t* ompi_proc, bool repost)
{
    mca_btl_base_segment_t* segments;
    mca_pml_bfo_restart_hdr_t* hdr;  /* hdr of NOTIFY message */
    mca_pml_bfo_restart_hdr_t* nack; /* hdr of NACK message */
    mca_btl_base_descriptor_t* des;
    mca_bml_base_endpoint_t* bml_endpoint;
    mca_bml_base_btl_t* bml_btl;
    int rc;

    if (repost) {
        /* In the case where we are reposting the NACK, the information
         * is in the src area, since we are reposting a send.  In addition,
         * we get the ompi_proc from the old descriptor. */
        segments = olddes->des_src;
        ompi_proc = olddes->des_cbdata;
    } else {
        segments = olddes->des_dst;
    }
    hdr = (mca_pml_bfo_restart_hdr_t*)segments->seg_addr.pval;

    bml_endpoint = ompi_proc->proc_bml;
    assert(0 != bml_endpoint->btl_eager.arr_size);
    bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_eager);

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &des, MCA_BTL_NO_ORDER,
                       sizeof(mca_pml_bfo_restart_hdr_t),
                       MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP |
                       MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if( OPAL_UNLIKELY(NULL == des) ) {
        opal_output(0, "%s:%d Out of resources, cannot proceed", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }

    /* fill out header */
    nack = (mca_pml_bfo_restart_hdr_t*)des->des_src->seg_addr.pval;
    nack->hdr_match.hdr_common.hdr_flags = 0;
    nack->hdr_match.hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNACK;
    nack->hdr_match.hdr_ctx = hdr->hdr_match.hdr_ctx;
    nack->hdr_match.hdr_src = hdr->hdr_dst_rank;       /* Receiver rank */
    nack->hdr_match.hdr_seq = hdr->hdr_match.hdr_seq;
    nack->hdr_restartseq = hdr->hdr_restartseq;
    nack->hdr_src_req = hdr->hdr_src_req;
    nack->hdr_dst_req.pval = 0;

    bfo_hdr_hton(nack, MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNACK, ompi_proc);

    /* Initialize descriptor.  Save away ompi_proc in case we need
     * to respost this fragmnet. */
    des->des_cbfunc = mca_pml_bfo_recv_restart_completion;
    des->des_cbdata = ompi_proc;

    opal_output_verbose(30, mca_pml_bfo_output,
                        "RNDVRESTARTNACK: sending to sender, "
                        "PML=%d, RQS=%d, CTX=%d, SRC=%d, peer=%d",
                        nack->hdr_match.hdr_seq, nack->hdr_restartseq,
                        nack->hdr_match.hdr_ctx, nack->hdr_match.hdr_src,
                        ompi_proc->proc_name.vpid);

    rc = mca_bml_base_send(bml_btl, des, MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNACK);
    if( OPAL_UNLIKELY( rc < 0 ) ) {
        opal_output(0, "[%s:%d] Cannot send rndvrestartnack message", __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }
}


/**
 * Reset all the receive request fields to match what a request
 * looks like when it is first started.   This gets called when
 * the rendezvous/rget message is being restarted.
 */
void mca_pml_bfo_recv_request_reset(mca_pml_bfo_recv_request_t* match) {
    int i;

    assert(true != match->req_recv.req_base.req_pml_complete);

    /* Free up any resources that were reserved for this receive.  This
     * was copied from the receive completion code.  */
    for(i = 0; i < (int)match->req_rdma_cnt; i++) {
        mca_mpool_base_registration_t* btl_reg = match->req_rdma[i].btl_reg;
        if( NULL != btl_reg  && btl_reg->mpool != NULL) {
            btl_reg->mpool->mpool_deregister( btl_reg->mpool, btl_reg );
        }
    }
    match->req_rdma_cnt = 0;

    /* This code is mostly copied from mca_pml_bfo_recv_req_start.
     * Note 1: Leave req_bytes_expected as the original value.  No
     * need to adjust this as it is set when convertor is created.
     * Note 2: Leave req_bytes_delivered as the original value.
     * This is created when the convertor is created and represents
     * the expected bytes from the user. */
    assert(0 == match->req_events);
    match->req_errstate = 0;
    match->req_lock = 0;
    match->req_pipeline_depth = 0;
    match->req_bytes_received = 0;
    match->req_rdma_idx = 0;
    match->req_rdma_offset = 0;
    match->req_send_offset = 0;
    match->req_pending = false;
    match->req_ack_sent = false;
    match->req_restartseq++;

    /* These really should not need to be set, but this matches some
     * of the initialization within MCA_PML_BASE_RECV_START. */
    match->req_recv.req_base.req_pml_complete = false;
    match->req_recv.req_base.req_ompi.req_complete = false;
    match->req_recv.req_base.req_ompi.req_state = OMPI_REQUEST_ACTIVE;

    /* Reset the convertor */
    opal_convertor_set_position(&match->req_recv.req_base.req_convertor,
                                &match->req_rdma_offset);
    return;
}

/*
 * Completion callback for RNDVRESTARTACK, RNDVRESTARTNACK and RECVERRNOTIFY.
 */
void mca_pml_bfo_recv_restart_completion( mca_btl_base_module_t* btl,
                                          struct mca_btl_base_endpoint_t* ep,
                                          struct mca_btl_base_descriptor_t* des,
                                          int status )
{
    if(OPAL_UNLIKELY(OMPI_SUCCESS != status)) {
        mca_pml_bfo_common_hdr_t* common = des->des_src->seg_addr.pval;
        mca_pml_bfo_restart_hdr_t* restart;  /* RESTART header */
        mca_pml_bfo_recv_request_t* recvreq;

        switch (common->hdr_type) {
        case MCA_PML_BFO_HDR_TYPE_RNDVRESTARTACK:
            restart = (mca_pml_bfo_restart_hdr_t*)des->des_src->seg_addr.pval;
            recvreq = (mca_pml_bfo_recv_request_t*) restart->hdr_dst_req.pval;
            opal_output_verbose(30, mca_pml_bfo_output,
                                "RNDVRESTARTACK: completion failed: try again "
                                "PML:req=%d,hdr=%d RQS:req=%d,hdr=%d CTX:req=%d,hdr=%d "
                                "src_req=%p, dst_req=%p, peer=%d",
                                recvreq->req_msgseq, restart->hdr_match.hdr_seq,
                                recvreq->req_restartseq, restart->hdr_restartseq,
                                recvreq->req_recv.req_base.req_comm->c_contextid,
                                restart->hdr_match.hdr_ctx,
                                recvreq->remote_req_send.pval,
                                (void *)recvreq,
                                recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);

            /* Adjust the states back to avoid assert errors */
            recvreq->req_errstate &= ~RECVREQ_RNDVRESTART_ACKED;
            recvreq->req_errstate |= RECVREQ_RNDVRESTART_RECVED;
            mca_pml_bfo_recv_request_rndvrestartack(recvreq, MCA_PML_BFO_HDR_TYPE_RNDVRESTARTACK,
                                                    status, btl);
            break;
        case MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNACK:
            opal_output_verbose(30, mca_pml_bfo_output,
                                "RNDVRESTARTNACK: completion failed: try again "
                                "des=%p ", (void *)des);
            /* Just blast it again.  No request associated with it. */
            mca_pml_bfo_recv_request_rndvrestartnack(des, NULL, true);
            break;
        case MCA_PML_BFO_HDR_TYPE_RECVERRNOTIFY:
            restart = (mca_pml_bfo_restart_hdr_t*)des->des_src->seg_addr.pval;
            recvreq = (mca_pml_bfo_recv_request_t*) restart->hdr_dst_req.pval;
            /* With just two BTLs, this should never happen as we are
             * typically sending the RECVERRNOTIFY message on the
             * working BTL.  But, just in case, if we get an error,
             * send it again. */
            opal_output_verbose(30, mca_pml_bfo_output,
                                "RECVERRNOTIFY: completion failed: try again, "
                                "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                                recvreq->req_msgseq, recvreq->req_restartseq,
                                recvreq->remote_req_send.pval,
                                (void *)recvreq,
                                recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
            mca_pml_bfo_recv_request_recverrnotify(recvreq, MCA_PML_BFO_HDR_TYPE_RECVERRNOTIFY,
                                                   status);
            break;
        default:
            opal_output(0, "[%s:%d] Unknown callback error", __FILE__, __LINE__);
            orte_errmgr.abort(-1, NULL);
        }
    }
}

/*
 * Remove a btl for future communication on an endpoint.
 */
void mca_pml_bfo_map_out_btl(struct mca_btl_base_module_t* btl,
                             ompi_proc_t *errproc, char *btlname)
{
    mca_bml_base_endpoint_t* ep;
    bool remove = false;
    int i;

    ep = (mca_bml_base_endpoint_t*)errproc->proc_bml;

    /* The bml_del_proc_btl function does not indicate if it
     * actually removed a btl, so let me check up front.  This is
     * done so that we can only print out messages when a btl is
     * actually going to be removed. These arrays are small so it
     * is OK to walk through all of them even though it may be
     * redundant. */
    for( i = 0; i < (int)ep->btl_eager.arr_size; i++ ) {
        if( ep->btl_eager.bml_btls[i].btl == btl ) {
            remove = true;
        }
    }
    for( i = 0; i < (int)ep->btl_send.arr_size; i++ ) {
        if( ep->btl_send.bml_btls[i].btl == btl ) {
            remove = true;
        }
    }
    for( i = 0; i < (int)ep->btl_rdma.arr_size; i++ ) {
        if( ep->btl_rdma.bml_btls[i].btl == btl ) {
            remove = true;
        }
    }

    if (true == remove) {
        mca_bml.bml_del_proc_btl(errproc, btl);

        orte_notifier.log(ORTE_NOTIFIER_CRIT, 1,
                          "BTL %s error: rank=%d mapping out %s "
                          "to rank=%d on node=%s",
                          btl->btl_component->btl_version.mca_component_name,
                          ORTE_PROC_MY_NAME->vpid,
                          btlname, errproc->proc_name.vpid,
                          errproc->proc_hostname);

        opal_output_verbose(10, mca_pml_bfo_output,
                            "BTL %s error: rank=%d mapping out %s "
                            "to rank=%d on node=%s \n",
                            btl->btl_component->btl_version.mca_component_name,
                            ORTE_PROC_MY_NAME->vpid,
                            btlname, errproc->proc_name.vpid,
                            errproc->proc_hostname);

        /* Need to search for any pending packets associated
         * with this endpoint and remove them.  We may also
         * have to restarts depending on the state of the
         * requests. */
        mca_pml_bfo_error_pending_packets(btl, ep);

        if ((ep->btl_eager.arr_size == 0) &&
            (ep->btl_send.arr_size == 0) &&
            (ep->btl_rdma.arr_size == 0)) {
            opal_output(0, "%s:%d: No more interfaces, aborting",
                        __FILE__, __LINE__);
            orte_errmgr.abort(-1, NULL);
        }
    }
}

void mca_pml_bfo_failover_error_handler(struct mca_btl_base_module_t* btl,
                    int32_t flags, ompi_proc_t *errproc, char *btlname)
{ 
    ompi_proc_t** procs; 
    size_t p, num_procs; 

    /* If we are in here, we know that the we were called
     * with the flags == MCA_BTL_ERROR_FLAGS_NONFATAL so no
     * need to check it in here. */
    assert(flags & MCA_BTL_ERROR_FLAGS_NONFATAL);

    procs = ompi_proc_all(&num_procs);

    if(NULL == procs) {
        opal_output(0, "%s:%d: Out of memory, giving up.",
                    __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
    }

    if (NULL == btlname) {
        btlname = "unknown";
    }

    /* If the process to map out is not specified then map out the
     * entire BTL.  Otherwise, only map out the BTL for the specific
     * remote process. */
    if (NULL == errproc) {
        for( p = 0; p < num_procs; p++ ) {
            mca_pml_bfo_map_out_btl(btl, procs[p], btlname);
        }
    } else {
        mca_pml_bfo_map_out_btl(btl, errproc, btlname);
    }
    free(procs);
}

/**
 * This function is called since when we are mapping out a BML.  This
 * will walk through the four PML lists and dispatch with the
 * fragments/requests.  There are four different lists and each one is
 * handled slighty differently.  In all cases, we first see if the
 * message is associated with the endpoint that is being mapped out.
 * If not, then just leave it alone and put it back on the list.  If
 * it is associated with the endpoint, then a each list handles it
 * slighlty differently.  Also, in some cases, we actually adjust the
 * pointers to the BMLs in the messages as they may have changed when
 * the BML is mapped out.  That is because this is called after we
 * have mapped out the offending BML and adjusted the array of
 * available BMLs.
 */
static void mca_pml_bfo_error_pending_packets(mca_btl_base_module_t* btl,
                                              mca_bml_base_endpoint_t* ep) {
    int32_t i, s;

    /* The pckt_pending list contains both ACK and FIN messages.
     * ACKs can be sent over any BTL associated with the endpoint.
     * Therefore, the bml_btl entry for ACKS is NULL and they do
     * not need to be adjusted.  It is also worth noting that
     * the ACK will be the only outstanding message associated
     * with a request so we can just let nature takes it course.
     *
     * FIN messages do have a BML associated with them, but they
     * can also be sent over any BTL.  Therefore, adjust the bml
     * pointer in the pckt to ensure it points at a valid BML.
     */

    s = (int32_t)opal_list_get_size(&mca_pml_bfo.pckt_pending);
    for(i = 0; i < s; i++) {
        mca_pml_bfo_pckt_pending_t *pckt;
        opal_output_verbose(0, mca_pml_bfo_output,
                            "INFO: pckt_pending list has %d entries", s);
#if 1
        /* TODO: Error out until code is tested */
        opal_output_verbose(0, mca_pml_bfo_output,
                            "%s:%d: Support not implemented, aborting",
                    __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
#endif
        OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
        pckt = (mca_pml_bfo_pckt_pending_t*)
            opal_list_remove_first(&mca_pml_bfo.pckt_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);

        /* My guess is that this can happen in the threaded
         * case where the other thread removed some packets
         * after we determined the size of the list. */
        if(NULL == pckt)
            break;

        /* If there is no bml stored on the packet, then just
         * put it back on the list as there is nothing to adjust.
         * This appears to be true with ACK packets. */
        if (NULL == pckt->bml_btl) {
            OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
            opal_list_append(&mca_pml_bfo.pckt_pending,
                             (opal_list_item_t*)pckt);
            OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
            continue;
        }

        /* Now see if this endpoint matches the one we are mapping
         * out.  If so, adjust the bml entry so to ensure it is
         * not pointing at a stale bml.  We do not really care
         * which BML it is pointing at as long as it is valid.
         * In either case, then put entry back on the list. */
        if (pckt->proc->proc_bml == ep) {
            opal_output_verbose(15, mca_pml_bfo_output,
                                "INFO: Found matching pckt on pckt_pending list, adjusting bml");
            pckt->bml_btl = mca_bml_base_btl_array_get_next(&ep->btl_eager);
        }
        OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
        opal_list_append(&mca_pml_bfo.pckt_pending,
                         (opal_list_item_t*)pckt);
        OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);

    }

    /* This next list holds rdma fragments.  We need to walk through
     * the list and see if any are associated with the endpoint
     * we are mapping out.  If not, then just put back on the
     * list.  If they are, then we need to error them out.  One issue
     * is that we need to deal with the case where there may be more
     * then one pending rdma fragment for a request. */
    s = (int32_t)opal_list_get_size(&mca_pml_bfo.rdma_pending);
    for(i = 0; i < s; i++) {
        mca_pml_bfo_rdma_frag_t* frag;
        mca_pml_bfo_send_request_t* sendreq;
        mca_pml_bfo_recv_request_t* recvreq;
        opal_output_verbose(0, mca_pml_bfo_output,
                            "INFO: rdma_pending list has %d entries", s);
#if 1
        /* TODO: Error out until code is tested */
        opal_output_verbose(0, mca_pml_bfo_output,
                            "%s:%d: Support not implemented, aborting",
                    __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
#endif
        OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
        frag = (mca_pml_bfo_rdma_frag_t*)
            opal_list_remove_first(&mca_pml_bfo.rdma_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);

        /* My guess is that this can happen in the threaded
         * case where the other thread removed some packets
         * after we determined the size of the list. */
        if(NULL == frag)
            break;

        /* Check to see if it matches our endpoint.  If it does,
         * then check if it matches the BTL that is being mapped
         * out.  If it does not, then just readjust the BML pointer.
         * If it does, then we need to do something with it. */
        if (frag->rdma_ep != ep) {
            OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
            opal_list_append(&mca_pml_bfo.rdma_pending,
                             (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
            continue;
        }

        /* If we are here, then we know we are working on the same
         * endpoint.  Now check the BTL. */
        if (frag->rdma_btl != btl) {
            opal_output_verbose(15, mca_pml_bfo_output,
                                "INFO: Found matching frag on rdma_pending list, adjusting bml");
            /* The BTL this RDMA is associated with is not the
             * one that is getting mapped out, so just adjust the
             * BML pointer and put back on the list. */
            frag->rdma_bml = mca_bml_base_btl_array_find(&ep->btl_rdma, frag->rdma_btl);
            OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
            opal_list_append(&mca_pml_bfo.rdma_pending,
                             (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
            continue;
        }

        /* Now we call the restart routine.  This is just like if we got
         * a completion event after calling an RDMA write.  This will
         * take care of figuring out if we need to restart the request
         * or wait for any outstanding events to complete.  */
        if(frag->rdma_state == MCA_PML_BFO_RDMA_PUT) {
            opal_output_verbose(15, mca_pml_bfo_output,
                                "INFO: Found matching PUT frag on rdma_pending list, restarting");
            sendreq = frag->rdma_req;
            mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,
                                                       MCA_PML_BFO_HDR_TYPE_PUT, 2, btl);
            MCA_PML_BFO_RDMA_FRAG_RETURN(frag);
        } else {
            opal_output_verbose(15, mca_pml_bfo_output,
                                "INFO: Found matching RGET frag on rdma_pending list, sending reqerror");
            /* This is just like what we do on an rget completion event */
            recvreq = (mca_pml_bfo_recv_request_t*)frag->rdma_req;
            mca_pml_bfo_recv_request_recverrnotify(recvreq, MCA_PML_BFO_HDR_TYPE_RGET, 2);

            /* See if the request has received a RNDVRESTARTNOTIFY */
            if( OPAL_UNLIKELY(recvreq->req_errstate)) {
                if (recvreq->req_errstate & RECVREQ_RNDVRESTART_RECVED) {
                    mca_pml_bfo_recv_request_rndvrestartack(recvreq,
                                                            MCA_PML_BFO_HDR_TYPE_RGET,
                                                            2, btl);
                }
            }
            MCA_PML_BFO_RDMA_FRAG_RETURN(frag);
        }
    }

    s = opal_list_get_size(&mca_pml_bfo.send_pending);
    /* Look for pending events on our endpoint */
    for(i = 0; i < s; i++) {
        mca_pml_bfo_send_request_t* sendreq;
        ompi_proc_t* proc;
        mca_bml_base_endpoint_t* bml_endpoint;
        opal_output_verbose(0, mca_pml_bfo_output,
                            "INFO: send_pending list has %d entries", s);
#if 1
        /* TODO: Error out until code is tested */
        opal_output_verbose(0, mca_pml_bfo_output,
                            "%s:%d: Support not implemented, aborting",
                    __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
#endif
        OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
        sendreq = (mca_pml_bfo_send_request_t*)
            opal_list_remove_first(&mca_pml_bfo.send_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);

        /* My guess is that this can happen in the threaded
         * case where the other thread removed some packets
         * after we determined the size of the list. */
        if(NULL == sendreq)
            break;

        proc = (ompi_proc_t*)sendreq->req_send.req_base.req_proc;
        bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml;

        /* Check to see if it matches our endpoint.  If it does not,
         * then just put it back on the list as there is nothing
         * we need to do with it. */
        if (bml_endpoint != ep) {
            OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
            opal_list_append(&mca_pml_bfo.send_pending,
                             (opal_list_item_t*)sendreq);
            OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
            continue;
        }

        switch(sendreq->req_pending) {
        case MCA_PML_BFO_SEND_PENDING_SCHEDULE:
            /* If this send request is using the endpoint that received
             * the error, then let us error it out.  In the case
             * where there is only one fragment left to be scheduled
             * and it would have gone over the good BTL, this is
             * not necessary.  But, we will use simplicity here
             * and assume that some of the fragments are still
             * scheduled to go over the broken BTL. */
            sendreq->req_error++;
            mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,
                                                       MCA_PML_BFO_HDR_TYPE_FRAG, 2, btl);
            break;
        case MCA_PML_BFO_SEND_PENDING_START:
            /* If the request has not even started, then just put it back
             * on the list.  Nothing else to do with it. */
            OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
            opal_list_append(&mca_pml_bfo.send_pending,
                             (opal_list_item_t*)sendreq);
            OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
            break;
        default:
            opal_output(0, "[%s:%d] wrong send request type\n",
                    __FILE__, __LINE__);
            break;
        }
    }

    s = (int)opal_list_get_size(&mca_pml_bfo.recv_pending);
    for(i = 0; i < s; i++) {
        mca_pml_bfo_recv_request_t* recvreq;
        ompi_proc_t* proc;
        mca_bml_base_endpoint_t* bml_endpoint;
        opal_output_verbose(0, mca_pml_bfo_output,
                            "INFO: recv_pending list has %d entries", s);
#if 1
        /* TODO: Error out until code is tested */
        opal_output_verbose(0, mca_pml_bfo_output,
                            "%s:%d: Support not implemented, aborting",
                    __FILE__, __LINE__);
        orte_errmgr.abort(-1, NULL);
#endif
        OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
        recvreq = (mca_pml_bfo_recv_request_t*)
            opal_list_remove_first(&mca_pml_bfo.recv_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);

        /* My guess is that this can happen in the threaded
         * case where the other thread removed some packets
         * after we determined the size of the list. */
        if(NULL == recvreq)
            break;

        proc = (ompi_proc_t*)recvreq->req_recv.req_base.req_proc;
        bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml;

        if (bml_endpoint != ep) {
            OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
            opal_list_append(&mca_pml_bfo.recv_pending,
                             (opal_list_item_t*)recvreq);
            OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
            continue;
        }

        mca_pml_bfo_recv_request_recverrnotify(recvreq, MCA_PML_BFO_HDR_TYPE_PUT, 2);
    }
}

/**
 * Call each time we get a completion event on ACK or PUT message.
 * These types of messages are receive control type messages.  This
 * function is only called if the underlying BTL supports failover.
 * Otherwise, there is no need for this check.
 */
void mca_pml_bfo_check_recv_ctl_completion_status(mca_btl_base_module_t* btl,
                                                  struct mca_btl_base_descriptor_t* des,
                                                  int status)
{
    mca_pml_bfo_common_hdr_t * common = des->des_src->seg_addr.pval;
    mca_pml_bfo_rdma_hdr_t* hdr; /* PUT header */
    struct mca_btl_base_descriptor_t* rdma_des;
    mca_pml_bfo_recv_request_t* recvreq;

    if(OPAL_UNLIKELY(OMPI_SUCCESS != status)) {
        switch (common->hdr_type) {
        case MCA_PML_BFO_HDR_TYPE_ACK:
            recvreq = des->des_cbdata;
                
            /* Record the error.  Send RECVERRNOTIFY if necessary. */
            if (recvreq->req_errstate) {
                opal_output_verbose(30, mca_pml_bfo_output,
                                    "ACK: completion failed, error already seen, "
                                    "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                                    recvreq->req_msgseq, recvreq->req_restartseq,
                                    recvreq->remote_req_send.pval, (void *)recvreq,
                                    recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
            } else {
                opal_output_verbose(30, mca_pml_bfo_output,
                                    "ACK: completion failed, sending RECVERRNOTIFY to sender, "
                                    "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                                    recvreq->req_msgseq, recvreq->req_restartseq,
                                    recvreq->remote_req_send.pval, (void *)recvreq,
                                    recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
                mca_pml_bfo_recv_request_recverrnotify(recvreq, MCA_PML_BFO_HDR_TYPE_ACK, status);
            }
            break;

        case MCA_PML_BFO_HDR_TYPE_PUT:
            hdr = (mca_pml_bfo_rdma_hdr_t*)des->des_src->seg_addr.pval;
            rdma_des = hdr->hdr_des.pval;
            recvreq = des->des_cbdata;
            if ((NULL != rdma_des->des_cbdata) && (recvreq == rdma_des->des_cbdata)) {
                /* We now record the error, send the RECVERRNOTIFY if
                 * necessary, and free the descriptor.  Prior to this,
                 * we want to ensure that we have not reached the case
                 * where the PUT message actually made it over and we
                 * have already received a FIN back.  We first check to
                 * see if the RDMA descriptor cbdata is pointing to
                 * NULL.  If it is, this means that the PUT message must
                 * have made it over and a corresponding FIN already
                 * made it back and freed the RDMA descriptor.  Second,
                 * if it is non-null, we make sure that it is pointing
                 * to the same request as the PUT descriptor is.  If
                 * it is not, again we assume that the FIN came back
                 * and freed it.  And we can count on the fact that the
                 * recvreq has not been freed or reused as it is held
                 * until this very completion event occurs.  */
                if (recvreq->req_errstate) {
                    opal_output_verbose(30, mca_pml_bfo_output,
                                        "PUT: completion failed, error already seen, "
                                        "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                                        recvreq->req_msgseq, recvreq->req_restartseq,
                                        recvreq->remote_req_send.pval, (void *)recvreq,
                                        recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
                } else {
                    opal_output_verbose(30, mca_pml_bfo_output,
                                        "PUT: completion failed, sending RECVERRNOTIFY to sender, "
                                        "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                                        recvreq->req_msgseq, recvreq->req_restartseq,
                                        recvreq->remote_req_send.pval, (void *)recvreq,
                                        recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
                    mca_pml_bfo_recv_request_recverrnotify(recvreq, MCA_PML_BFO_HDR_TYPE_PUT, status);
                }
#if 0
                /* TODO: Add descriptor to receive request so it can
                 * be freed only when receive request is freed and
                 * only if needed. */
                btl->btl_free(btl, rdma_des);
#endif
            }
            break;
        default:
            orte_errmgr.abort(-1, NULL);
        }
    }

    switch (common->hdr_type) {
    case MCA_PML_BFO_HDR_TYPE_ACK:
        recvreq = des->des_cbdata;
        recvreq->req_events--;
        assert(recvreq->req_events >= 0);
        if(OPAL_UNLIKELY (recvreq->req_errstate & RECVREQ_RNDVRESTART_RECVED)) {
            opal_output_verbose(30, mca_pml_bfo_output,
                                "ACK: completion: recvreq in error, outstanding events=%d "
                                "PML=%d, RQS=%d, src_req=%p, dst_req=%p, status=%d, peer=%d",
                                recvreq->req_events, recvreq->req_msgseq, recvreq->req_restartseq,
                                recvreq->remote_req_send.pval, (void *)recvreq, status,
                                recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
            if (0 == recvreq->req_events) {
                mca_pml_bfo_recv_request_rndvrestartack(recvreq, MCA_PML_BFO_HDR_TYPE_ACK,
                                                        status, btl);
            }
            return;
        }
        recv_request_pml_complete_check(recvreq);
        break;
    case MCA_PML_BFO_HDR_TYPE_PUT:
        recvreq = des->des_cbdata;
        recvreq->req_events--;
        assert(recvreq->req_events >= 0);
        if(OPAL_UNLIKELY(recvreq->req_errstate & RECVREQ_RNDVRESTART_RECVED)) {
            opal_output_verbose(30, mca_pml_bfo_output,
                                "PUT: completion: recvreq in error, outstanding events=%d "
                                "PML=%d, RQS=%d, src_req=%p, dst_req=%p, status=%d, peer=%d",
                                recvreq->req_events, recvreq->req_msgseq, recvreq->req_restartseq,
                                recvreq->remote_req_send.pval, (void *)recvreq, status,
                                recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
            if (0 == recvreq->req_events) {
                mca_pml_bfo_recv_request_rndvrestartack(recvreq, MCA_PML_BFO_HDR_TYPE_PUT,
                                                        status, btl);
            }
            return;
        }
        recv_request_pml_complete_check(recvreq);
        break;
    }
}

/**
 * Register four functions to handle extra PML message types that
 * are utilized when a failover occurs.
 */
int mca_pml_bfo_register_callbacks(void) {
    int rc;
    /* The following four functions are utilized when failover
     * support for openib is enabled. */
    rc = mca_bml.bml_register( MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNOTIFY,
                               mca_pml_bfo_recv_frag_callback_rndvrestartnotify,
                               NULL );
    if(OMPI_SUCCESS != rc)
        return rc;

    rc = mca_bml.bml_register( MCA_PML_BFO_HDR_TYPE_RNDVRESTARTACK,
                               mca_pml_bfo_recv_frag_callback_rndvrestartack,
                               NULL );
    if(OMPI_SUCCESS != rc)
        return rc;

    rc = mca_bml.bml_register( MCA_PML_BFO_HDR_TYPE_RNDVRESTARTNACK,
                               mca_pml_bfo_recv_frag_callback_rndvrestartnack,
                               NULL );
    if(OMPI_SUCCESS != rc)
        return rc;

    rc = mca_bml.bml_register( MCA_PML_BFO_HDR_TYPE_RECVERRNOTIFY,
                               mca_pml_bfo_recv_frag_callback_recverrnotify,
                               NULL );
    if(OMPI_SUCCESS != rc)
        return rc;

    return rc;
}

/**
 * Update a few fields when we are restarting either a RNDV or
 * RGET type message.
 */
void mca_pml_bfo_update_rndv_fields(mca_pml_bfo_hdr_t* hdr,
                                    mca_pml_bfo_send_request_t* sendreq, char *type)
{
    hdr->hdr_common.hdr_flags |= MCA_PML_BFO_HDR_FLAGS_RESTART;
    hdr->hdr_rndv.hdr_dst_req = sendreq->req_recv;
    hdr->hdr_rndv.hdr_restartseq = sendreq->req_restartseq;
    opal_output_verbose(30, mca_pml_bfo_output,
                        "%s: restarting: PML=%d, RQS=%d, CTX=%d, SRC=%d, "
                        "src_req=%p, dst_req=%p, peer=%d",
                        type, (uint16_t)sendreq->req_send.req_base.req_sequence,
                        sendreq->req_restartseq,
                        sendreq->req_send.req_base.req_comm->c_contextid,
                        sendreq->req_send.req_base.req_comm->c_my_rank, (void *)sendreq,
                        sendreq->req_recv.pval, sendreq->req_send.req_base.req_peer);
}

/**
 * The following set of functions are all called when it is determined
 * that the cached bml_btl->btl does not match the btl handed back
 * by the callback function.  This means that the bml_btl array has
 * been shuffled and the bml_btl matching the btl has to be found
 * back.  If it cannot be found, then just find a different one to
 * use.  
 */
void mca_pml_bfo_update_eager_bml_btl_recv_ctl(mca_bml_base_btl_t** bml_btl,
                                               mca_btl_base_module_t* btl,
                                               struct mca_btl_base_descriptor_t* des)
{
    if ((*bml_btl)->btl != btl) {
        mca_pml_bfo_common_hdr_t * common = des->des_src->seg_addr.pval;
        mca_pml_bfo_ack_hdr_t* ack;  /* ACK header */
        mca_pml_bfo_recv_request_t* recvreq;
        char *type;

        switch (common->hdr_type) {
        case MCA_PML_BFO_HDR_TYPE_ACK:
            ack = (mca_pml_bfo_ack_hdr_t*)des->des_src->seg_addr.pval;
            recvreq = (mca_pml_bfo_recv_request_t*) ack->hdr_dst_req.pval;
            type = "ACK";
            break;
        case MCA_PML_BFO_HDR_TYPE_PUT:
            recvreq = des->des_cbdata;
            type = "PUT";
            break;
        default:
            /* In theory, this can never happen. */
            opal_output(0, "%s:%d FATAL ERROR, unknown header (hdr=%d)",
                        __FILE__, __LINE__, common->hdr_type);
            orte_errmgr.abort(-1, NULL);
        }

        mca_pml_bfo_find_recvreq_eager_bml_btl(bml_btl, btl, recvreq, type);
    }
}

void mca_pml_bfo_find_sendreq_eager_bml_btl(mca_bml_base_btl_t** bml_btl,
                                            mca_btl_base_module_t* btl,
                                            mca_pml_bfo_send_request_t* sendreq,
                                            char* type)
{
    if ((*bml_btl)->btl != btl) {
        opal_output_verbose(25, mca_pml_bfo_output,
                            "%s completion: BML does not match BTL, find it back, "
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                            type, (uint16_t)sendreq->req_send.req_base.req_sequence,
                            sendreq->req_restartseq, (void *)sendreq,
                            sendreq->req_recv.pval,
                            sendreq->req_send.req_base.req_peer);
        *bml_btl = mca_bml_base_btl_array_find(&sendreq->req_endpoint->btl_eager, btl);
        if (NULL == *bml_btl) {
            opal_output_verbose(25, mca_pml_bfo_output,
                                "%s completion: BML is gone, find another one, "
                                "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                                type, (uint16_t)sendreq->req_send.req_base.req_sequence,
                                sendreq->req_restartseq, (void *)sendreq,
                                sendreq->req_recv.pval,
                                sendreq->req_send.req_base.req_peer);
            *bml_btl = mca_bml_base_btl_array_get_next(&sendreq->req_endpoint->btl_eager);
        }
    }
}

void mca_pml_bfo_find_sendreq_rdma_bml_btl(mca_bml_base_btl_t** bml_btl,
                                           mca_btl_base_module_t* btl,
                                           mca_pml_bfo_send_request_t* sendreq,
                                           char* type)
{
    if ((*bml_btl)->btl != btl) {
        opal_output_verbose(25, mca_pml_bfo_output,
                            "%s completion: BML does not match BTL, find it back, "
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                            type, (uint16_t)sendreq->req_send.req_base.req_sequence,
                            sendreq->req_restartseq, (void *)sendreq,
                            sendreq->req_recv.pval,
                            sendreq->req_send.req_base.req_peer);
        *bml_btl = mca_bml_base_btl_array_find(&sendreq->req_endpoint->btl_rdma, btl);
        if (NULL == *bml_btl) {
            opal_output_verbose(25, mca_pml_bfo_output,
                                "%s completion: BML is gone, find another one, "
                                "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                                type, (uint16_t)sendreq->req_send.req_base.req_sequence,
                                sendreq->req_restartseq, (void *)sendreq,
                                sendreq->req_recv.pval,
                                sendreq->req_send.req_base.req_peer);
            *bml_btl = mca_bml_base_btl_array_get_next(&sendreq->req_endpoint->btl_rdma);
        }
    }
}

void mca_pml_bfo_find_recvreq_eager_bml_btl(mca_bml_base_btl_t** bml_btl,
                                            mca_btl_base_module_t* btl,
                                            mca_pml_bfo_recv_request_t* recvreq,
                                            char* type)
{
    if ((*bml_btl)->btl != btl) {
        ompi_proc_t *proc = (ompi_proc_t*)recvreq->req_recv.req_base.req_proc;
        mca_bml_base_endpoint_t* bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml;

        opal_output_verbose(25, mca_pml_bfo_output,
                            "%s completion: BML does not match BTL, find it back, "
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                            type, recvreq->req_msgseq, recvreq->req_restartseq,
                            recvreq->remote_req_send.pval, (void *)recvreq,
                            recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);

        *bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_eager, btl);
        if (NULL == *bml_btl) {
            opal_output_verbose(25, mca_pml_bfo_output,
                                "%s completion: BML is gone, find another one, "
                                "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                                type, recvreq->req_msgseq, recvreq->req_restartseq,
                                recvreq->remote_req_send.pval, (void *)recvreq,
                                recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);

            *bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_eager);
        }
    }
}

void mca_pml_bfo_find_recvreq_rdma_bml_btl(mca_bml_base_btl_t** bml_btl,
                                           mca_btl_base_module_t* btl,
                                           mca_pml_bfo_recv_request_t* recvreq,
                                           char* type)
{
    if ((*bml_btl)->btl != btl) {
        ompi_proc_t *proc = (ompi_proc_t*)recvreq->req_recv.req_base.req_proc;
        mca_bml_base_endpoint_t* bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_bml;

        opal_output_verbose(25, mca_pml_bfo_output,
                            "%s completion: BML does not match BTL, find it back, "
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                            type, recvreq->req_msgseq, recvreq->req_restartseq,
                            recvreq->remote_req_send.pval, (void *)recvreq,
                            recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);

        *bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_rdma, btl);
        if (NULL == *bml_btl) {
            opal_output_verbose(25, mca_pml_bfo_output,
                                "%s completion: BML is gone, find another one, "
                                "PML=%d, RQS=%d, src_req=%p, dst_req=%p, peer=%d",
                                type, recvreq->req_msgseq, recvreq->req_restartseq,
                                recvreq->remote_req_send.pval, (void *)recvreq,
                                recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);

            *bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_rdma);
        }
    }
}

/**
 * The completion event for the RNDV message has returned with an
 * error. We know that the send request we are looking at is valid
 * because it cannot be completed until the sendreq->req_state value
 * reaches 0.  And for the sendreq->req_state to reach 0, the
 * completion event on the RNDV message must occur.  So, we do not
 * bother checking whether the send request is valid, because we know
 * it is, but we put a few asserts in for good measure.  We then check
 * a few fields in the request to decide what to do.  If the
 * sendreq->req_error is set, that means that something has happend
 * already to the request and we do not want to restart it.
 * Presumably, we may have received a RECVERRNOTIFY message from the
 * receiver.  We also check the sendreq->req_acked field to see if it
 * has been acked.  If it has, then again we do not restart everything
 * because obviously the RNDV message has made it to the other side.
 */
bool mca_pml_bfo_rndv_completion_status_error(struct mca_btl_base_descriptor_t* des,
                                              mca_pml_bfo_send_request_t* sendreq)
{
    assert(((mca_pml_bfo_hdr_t*)((des)->des_src->seg_addr.pval))->hdr_match.hdr_ctx ==
           (sendreq)->req_send.req_base.req_comm->c_contextid);
    assert(((mca_pml_bfo_hdr_t*)((des)->des_src->seg_addr.pval))->hdr_match.hdr_src ==
           (sendreq)->req_send.req_base.req_comm->c_my_rank);
    assert(((mca_pml_bfo_hdr_t*)((des)->des_src->seg_addr.pval))->hdr_match.hdr_seq ==
           (uint16_t)(sendreq)->req_send.req_base.req_sequence);
    if ((!(sendreq)->req_error) && (NULL == (sendreq)->req_recv.pval)) {
        (sendreq)->req_events--;
        /* Assume RNDV did not make it, so restart from the beginning. */
        mca_pml_bfo_send_request_restart(sendreq, true, MCA_PML_BFO_HDR_TYPE_RNDV);
        return true;
    }
    return false;
}

/**
 * Check to see if an error has occurred on this send request.  If it has
 * and there are no outstanding events, then we can start the restart dance.
 */
void mca_pml_bfo_completion_sendreq_has_error(mca_pml_bfo_send_request_t* sendreq,
					      int status,
					      mca_btl_base_module_t* btl,
					      int type,
					      char *description)
{
    opal_output_verbose(30, mca_pml_bfo_output,
                        "%s: completion: sendreq has error, outstanding events=%d, "
                        "PML=%d, RQS=%d, src_req=%p, dst_req=%p, status=%d, peer=%d",
                        description,
                        sendreq->req_events, (uint16_t)sendreq->req_send.req_base.req_sequence,
                        sendreq->req_restartseq, (void *)sendreq,
                        sendreq->req_recv.pval,
                        status, sendreq->req_send.req_base.req_peer);
    if (0 == sendreq->req_events) {
        mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,
                                                   type, status, btl);
    } 
}

/* If we get an error on the RGET message, then first make sure that
 * header matches the send request that we are pointing to.  This is
 * necessary, because even though the sending side got an error, the
 * RGET may have made it to the receiving side and the message transfer
 * may have completed.  This would then mean the send request has been
 * completed and perhaps in use by another communication.  So there is
 * no need to restart this request.  Therefore, ensure that we are
 * looking at the same request that the header thinks we are looking
 * at.  If not, then there is nothing else to be done. */
void mca_pml_bfo_send_ctl_completion_status_error(struct mca_btl_base_descriptor_t* des)
{
    mca_pml_bfo_send_request_t* sendreq = (mca_pml_bfo_send_request_t*)des->des_cbdata;
    mca_pml_bfo_hdr_t* hdr = des->des_src->seg_addr.pval;
    switch (hdr->hdr_common.hdr_type) {
    case MCA_PML_BFO_HDR_TYPE_RGET:
        if ((hdr->hdr_match.hdr_ctx != sendreq->req_send.req_base.req_comm->c_contextid) ||
            (hdr->hdr_match.hdr_src != sendreq->req_send.req_base.req_comm->c_my_rank) ||
            (hdr->hdr_match.hdr_seq != (uint16_t)sendreq->req_send.req_base.req_sequence)) {
            opal_output_verbose(30, mca_pml_bfo_output,
                                "RGET: completion event: dropping because no valid request "
                                "PML:exp=%d,act=%d CTX:exp=%d,act=%d SRC:exp=%d,act=%d "
                                "RQS:exp=%d,act=%d, dst_req=%p",
                                (uint16_t)sendreq->req_send.req_base.req_sequence,
                                hdr->hdr_match.hdr_seq,
                                sendreq->req_send.req_base.req_comm->c_contextid,
                                hdr->hdr_match.hdr_ctx,
                                sendreq->req_send.req_base.req_comm->c_my_rank,
                                hdr->hdr_match.hdr_src,
                                sendreq->req_restartseq, hdr->hdr_rndv.hdr_restartseq,
                                (void *)sendreq);
            return;
        }
        mca_pml_bfo_send_request_restart(sendreq, true, MCA_PML_BFO_HDR_TYPE_RGET);
        return;
    default:
        opal_output(0, "%s:%d FATAL ERROR, unknown header (hdr=%d)",
                    __FILE__, __LINE__, hdr->hdr_common.hdr_type);
        orte_errmgr.abort(-1, NULL);
    }
}
