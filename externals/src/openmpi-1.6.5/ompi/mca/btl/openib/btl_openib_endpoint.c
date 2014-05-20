/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * Copyright (c) 2006-2009 Mellanox Technologies, Inc.  All rights reserved.
 * Copyright (c) 2009      IBM Corporation.  All rights reserved.
 * Copyright (c) 2010-2011 Oracle and/or its affiliates.  All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <sys/time.h>
#include <time.h>
#include <errno.h>
#include <string.h>

#include "opal_stdint.h"
#include "opal/util/output.h"

#include "orte/util/show_help.h"

#include "ompi/types.h"
#include "ompi/class/ompi_free_list.h"

#include "btl_openib_endpoint.h"
#include "btl_openib_proc.h"
#include "btl_openib_xrc.h"
#include "btl_openib_async.h"
#include "connect/connect.h"

static void mca_btl_openib_endpoint_construct(mca_btl_base_endpoint_t* endpoint);
static void mca_btl_openib_endpoint_destruct(mca_btl_base_endpoint_t* endpoint);

static inline int acruire_wqe(mca_btl_openib_endpoint_t *ep,
        mca_btl_openib_send_frag_t *frag)
{
    int qp = to_base_frag(frag)->base.order;
    int prio = !(to_base_frag(frag)->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY);

    if(qp_get_wqe(ep, qp) < 0) {
        qp_put_wqe(ep, qp);
        opal_list_append(&ep->qps[qp].no_wqe_pending_frags[prio],
                (opal_list_item_t *)frag);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}

static int acquire_send_credit(mca_btl_openib_endpoint_t *endpoint,
        mca_btl_openib_send_frag_t *frag)
{
    mca_btl_openib_module_t *openib_btl = endpoint->endpoint_btl;
    int qp = to_base_frag(frag)->base.order;
    int prio = !(to_base_frag(frag)->base.des_flags & MCA_BTL_DES_FLAGS_PRIORITY);

    if(BTL_OPENIB_QP_TYPE_PP(qp)) {
        if(OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.sd_credits, -1) < 0) {
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.sd_credits, 1);
            opal_list_append(&endpoint->qps[qp].no_credits_pending_frags[prio],
                    (opal_list_item_t *)frag);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    } else {
        if(OPAL_THREAD_ADD32(&openib_btl->qps[qp].u.srq_qp.sd_credits, -1) < 0)
        {
            OPAL_THREAD_ADD32(&openib_btl->qps[qp].u.srq_qp.sd_credits, 1);
            OPAL_THREAD_LOCK(&openib_btl->ib_lock);
            opal_list_append(&openib_btl->qps[qp].u.srq_qp.pending_frags[prio],
                             (opal_list_item_t *)frag);
            OPAL_THREAD_UNLOCK(&openib_btl->ib_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    return OMPI_SUCCESS;
}

/* this function is called with endpoint->endpoint_lock held */
int mca_btl_openib_endpoint_post_send(mca_btl_openib_endpoint_t *endpoint,
        mca_btl_openib_send_frag_t *frag)
{
    mca_btl_openib_header_t *hdr = frag->hdr;
    mca_btl_base_descriptor_t *des = &to_base_frag(frag)->base;
    int qp, ib_rc;
    int32_t cm_return;
    bool do_rdma = false;
    size_t eager_limit;

    if(OPAL_LIKELY(des->order == MCA_BTL_NO_ORDER))
        des->order = frag->qp_idx;

    qp = des->order;

    if(acruire_wqe(endpoint, frag) != OMPI_SUCCESS)
        return OMPI_ERR_RESOURCE_BUSY;

    eager_limit = mca_btl_openib_component.eager_limit +
        sizeof(mca_btl_openib_header_coalesced_t) +
        sizeof(mca_btl_openib_control_header_t);
    if(des->des_src->seg_len + frag->coalesced_length <= eager_limit &&
            (des->des_flags & MCA_BTL_DES_FLAGS_PRIORITY)) {
        /* High priority frag. Try to send over eager RDMA */
        if(acquire_eager_rdma_send_credit(endpoint) == OMPI_SUCCESS)
            do_rdma = true;
    }

    if(!do_rdma && acquire_send_credit(endpoint, frag) != OMPI_SUCCESS) {
        qp_put_wqe(endpoint, qp);
        return OMPI_ERR_RESOURCE_BUSY;
    }

    BTL_OPENIB_GET_CREDITS(endpoint->eager_rdma_local.credits, hdr->credits);
    if(hdr->credits)
        hdr->credits |= BTL_OPENIB_RDMA_CREDITS_FLAG;

    if(!do_rdma) {
        if(BTL_OPENIB_QP_TYPE_PP(qp) && 0 == hdr->credits) {
            BTL_OPENIB_GET_CREDITS(endpoint->qps[qp].u.pp_qp.rd_credits, hdr->credits);
        }
    } else {
        hdr->credits |= (qp << 11);
    }

    BTL_OPENIB_GET_CREDITS(endpoint->qps[qp].u.pp_qp.cm_return, cm_return);
    /* cm_seen is only 8 bytes, but cm_return is 32 bytes */
    if(cm_return > 255) {
        hdr->cm_seen = 255;
        cm_return -= 255;
        OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_return, cm_return);
    } else {
        hdr->cm_seen = cm_return;
    }

    qp_reset_signal_count(endpoint, qp);
    ib_rc = post_send(endpoint, frag, do_rdma, 1);

    if(!ib_rc)
        return OMPI_SUCCESS;

    if(endpoint->nbo)
        BTL_OPENIB_HEADER_NTOH(*hdr);

    if(BTL_OPENIB_IS_RDMA_CREDITS(hdr->credits)) {
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits,
                BTL_OPENIB_CREDITS(hdr->credits));
    }

    qp_put_wqe(endpoint, qp);

    if(do_rdma) {
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens, 1);
    } else {
        if(BTL_OPENIB_QP_TYPE_PP(qp)) {
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.rd_credits,
                    hdr->credits);
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.sd_credits, 1);
        } else if BTL_OPENIB_QP_TYPE_SRQ(qp){
            mca_btl_openib_module_t *openib_btl = endpoint->endpoint_btl;
            OPAL_THREAD_ADD32(&openib_btl->qps[qp].u.srq_qp.sd_credits, 1);
        }
    }
    BTL_ERROR(("error posting send request error %d: %s\n",
               ib_rc, strerror(ib_rc)));
    return OMPI_ERROR;
}



OBJ_CLASS_INSTANCE(mca_btl_openib_endpoint_t,
                   opal_list_item_t, mca_btl_openib_endpoint_construct,
                   mca_btl_openib_endpoint_destruct);

/*
 * Initialize state of the endpoint instance.
 *
 */
static mca_btl_openib_qp_t *endpoint_alloc_qp(void)
{
    mca_btl_openib_qp_t *qp = calloc(1, sizeof(mca_btl_openib_qp_t));
    if(!qp) {
        BTL_ERROR(("Failed to allocate memory for qp"));
        return NULL;
    }

    OBJ_CONSTRUCT(&qp->lock, opal_mutex_t);

    return qp;
}

static void
endpoint_init_qp_pp(mca_btl_openib_endpoint_qp_t *ep_qp, const int qp)
{
    mca_btl_openib_qp_info_t *qp_info = &mca_btl_openib_component.qp_infos[qp];
    ep_qp->qp = endpoint_alloc_qp();
    ep_qp->qp->users++;

    /* local credits are set here such that on initial posting
     * of the receive buffers we end up with zero credits to return
     * to our peer. The peer initializes his sd_credits to reflect this
     * below. Note that this may be a problem for iWARP as the sender
     * now has credits even if the receive buffers are not yet posted
     */
    ep_qp->u.pp_qp.rd_credits = -qp_info->rd_num;

    ep_qp->u.pp_qp.rd_posted = 0;
    ep_qp->u.pp_qp.cm_sent = 0;
    ep_qp->u.pp_qp.cm_return = -qp_info->u.pp_qp.rd_rsv;
    ep_qp->u.pp_qp.cm_received = qp_info->u.pp_qp.rd_rsv;

    /* initialize the local view of credits */
    ep_qp->u.pp_qp.sd_credits = qp_info->rd_num;

    /* number of available send WQEs */
    ep_qp->qp->sd_wqe = qp_info->rd_num;
}

static void
endpoint_init_qp_srq(mca_btl_openib_endpoint_qp_t *ep_qp, const int qp)
{
    ep_qp->qp = endpoint_alloc_qp();
    ep_qp->qp->users++;

    /* number of available send WQEs */
    ep_qp->qp->sd_wqe = mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
}

static void
endpoint_init_qp_xrc(mca_btl_base_endpoint_t *ep, const int qp)
{
    int max = ep->endpoint_btl->device->ib_dev_attr.max_qp_wr -
        (mca_btl_openib_component.use_eager_rdma ?
         mca_btl_openib_component.max_eager_rdma : 0);
    mca_btl_openib_endpoint_qp_t *ep_qp = &ep->qps[qp];
    ep_qp->qp = ep->ib_addr->qp;
    ep_qp->qp->sd_wqe += mca_btl_openib_component.qp_infos[qp].u.srq_qp.sd_max;
    /* make sure that we don't overrun maximum supported by device */
    if (ep_qp->qp->sd_wqe > max)
        ep_qp->qp->sd_wqe =  max;
    ep_qp->qp->users++;
}

static void endpoint_init_qp(mca_btl_base_endpoint_t *ep, const int qp)
{
    mca_btl_openib_endpoint_qp_t *ep_qp = &ep->qps[qp];

    ep_qp->rd_credit_send_lock = 0;
    ep_qp->credit_frag = NULL;

    OBJ_CONSTRUCT(&ep_qp->no_wqe_pending_frags[0], opal_list_t);
    OBJ_CONSTRUCT(&ep_qp->no_wqe_pending_frags[1], opal_list_t);

    OBJ_CONSTRUCT(&ep_qp->no_credits_pending_frags[0], opal_list_t);
    OBJ_CONSTRUCT(&ep_qp->no_credits_pending_frags[1], opal_list_t);

    switch(BTL_OPENIB_QP_TYPE(qp)) {
        case MCA_BTL_OPENIB_PP_QP:
            endpoint_init_qp_pp(ep_qp, qp);
            break;
        case MCA_BTL_OPENIB_SRQ_QP:
            endpoint_init_qp_srq(ep_qp, qp);
            break;
        case MCA_BTL_OPENIB_XRC_QP:
            if (NULL == ep->ib_addr->qp) {
                ep->ib_addr->qp = endpoint_alloc_qp();
            }
            endpoint_init_qp_xrc(ep, qp);
            break;
        default:
            BTL_ERROR(("Wrong QP type"));
            return;
    }

    ep_qp->qp->sd_wqe_inflight = 0;
    ep_qp->qp->wqe_count = QP_TX_BATCH_COUNT;
}

void mca_btl_openib_endpoint_init(mca_btl_openib_module_t *btl,
                                  mca_btl_base_endpoint_t *ep,
                                  ompi_btl_openib_connect_base_module_t *local_cpc,
                                  mca_btl_openib_proc_modex_t *remote_proc_info,
                                  ompi_btl_openib_connect_base_module_data_t *remote_cpc_data)
{
    int qp;

    ep->endpoint_btl = btl;
    ep->use_eager_rdma = btl->device->use_eager_rdma &
        mca_btl_openib_component.use_eager_rdma;
    ep->subnet_id = btl->port_info.subnet_id;
    ep->endpoint_local_cpc = local_cpc;
    ep->endpoint_remote_cpc_data = remote_cpc_data;

    ep->rem_info.rem_lid = remote_proc_info->pm_port_info.lid;
    ep->rem_info.rem_subnet_id = remote_proc_info->pm_port_info.subnet_id;
    ep->rem_info.rem_mtu = remote_proc_info->pm_port_info.mtu;
    opal_output(-1, "Got remote LID, subnet, MTU: %d, 0x%" PRIx64 ", %d", 
                ep->rem_info.rem_lid,
                ep->rem_info.rem_subnet_id,
                ep->rem_info.rem_mtu);

    ep->rem_info.rem_vendor_id = (remote_proc_info->pm_port_info).vendor_id;
    ep->rem_info.rem_vendor_part_id = (remote_proc_info->pm_port_info).vendor_part_id;

    ep->rem_info.rem_transport_type = (remote_proc_info->pm_port_info).transport_type;

    for (qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        endpoint_init_qp(ep, qp);
    }
}

static void mca_btl_openib_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{
    /* setup qp structures */
    endpoint->qps = (mca_btl_openib_endpoint_qp_t*)
        calloc(mca_btl_openib_component.num_qps,
                sizeof(mca_btl_openib_endpoint_qp_t));
    if (MCA_BTL_XRC_ENABLED) {
        endpoint->rem_info.rem_qps = (mca_btl_openib_rem_qp_info_t*)
            calloc(1, sizeof(mca_btl_openib_rem_qp_info_t));
        endpoint->rem_info.rem_srqs = (mca_btl_openib_rem_srq_info_t*)
            calloc(mca_btl_openib_component.num_xrc_qps,
                    sizeof(mca_btl_openib_rem_srq_info_t));
    } else {
        endpoint->rem_info.rem_qps = (mca_btl_openib_rem_qp_info_t*)
            calloc(mca_btl_openib_component.num_qps,
                    sizeof(mca_btl_openib_rem_qp_info_t));
        endpoint->rem_info.rem_srqs = NULL;
    }

    endpoint->ib_addr = NULL;
    endpoint->xrc_recv_qp_num = 0;
    endpoint->endpoint_btl = 0;
    endpoint->endpoint_proc = 0;
    endpoint->endpoint_local_cpc = NULL;
    endpoint->endpoint_remote_cpc_data = NULL;
    endpoint->endpoint_initiator = false;
    endpoint->endpoint_tstamp = 0.0;
    endpoint->endpoint_state = MCA_BTL_IB_CLOSED;
    endpoint->endpoint_retries = 0;
    OBJ_CONSTRUCT(&endpoint->endpoint_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&endpoint->pending_lazy_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->pending_get_frags, opal_list_t);
    OBJ_CONSTRUCT(&endpoint->pending_put_frags, opal_list_t);

    endpoint->get_tokens = mca_btl_openib_component.ib_qp_ous_rd_atom;

    /* initialize RDMA eager related parts */
    endpoint->eager_recv_count = 0;
    memset(&endpoint->eager_rdma_remote, 0,
           sizeof(mca_btl_openib_eager_rdma_remote_t));
    memset(&endpoint->eager_rdma_local, 0,
           sizeof(mca_btl_openib_eager_rdma_local_t));
    OBJ_CONSTRUCT(&endpoint->eager_rdma_local.lock, opal_mutex_t);

    endpoint->rem_info.rem_lid = 0;
    endpoint->rem_info.rem_subnet_id = 0;
    endpoint->rem_info.rem_mtu = 0;
    endpoint->nbo = false;
    endpoint->use_eager_rdma = false;
    endpoint->eager_rdma_remote.tokens = 0;
    endpoint->eager_rdma_local.credits = 0;
    endpoint->endpoint_cts_mr = NULL;
    endpoint->endpoint_cts_frag.super.super.base.super.registration = NULL;
    endpoint->endpoint_cts_frag.super.super.base.super.ptr = NULL;
    endpoint->endpoint_posted_recvs = false;
    endpoint->endpoint_cts_received = false;
    endpoint->endpoint_cts_sent = false;
}

/*
 * Destroy a endpoint
 *
 */

static void mca_btl_openib_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
    bool pval_clean = false;
    int qp;

    /* If the CPC has an endpoint_finalize function, call it */
    if (NULL != endpoint->endpoint_local_cpc->cbm_endpoint_finalize) {
        endpoint->endpoint_local_cpc->cbm_endpoint_finalize(endpoint);
    }

    /* Release CTS buffer */
    ompi_btl_openib_connect_base_free_cts(endpoint);

    /* Release memory resources */
    do {
        /* Make sure that mca_btl_openib_endpoint_connect_eager_rdma ()
         * was not in "connect" or "bad" flow (failed to allocate memory)
         * and changed the pointer back to NULL
         */
        if(!opal_atomic_cmpset_ptr(&endpoint->eager_rdma_local.base.pval, NULL,
                    (void*)1)) {
            if ((void*)1 != endpoint->eager_rdma_local.base.pval &&
                    NULL != endpoint->eager_rdma_local.base.pval) {
                endpoint->endpoint_btl->super.btl_mpool->mpool_free(endpoint->endpoint_btl->super.btl_mpool,
                        endpoint->eager_rdma_local.base.pval,
                        (mca_mpool_base_registration_t*)endpoint->eager_rdma_local.reg);
                pval_clean=true;
            }
        } else {
            pval_clean=true;
        }
    } while (!pval_clean);

    /* Close opened QPs if we have them*/
   for(qp = 0; qp < mca_btl_openib_component.num_qps; qp++) {
        MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(&endpoint->qps[qp].no_credits_pending_frags[0]);
        MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(&endpoint->qps[qp].no_credits_pending_frags[1]);
        OBJ_DESTRUCT(&endpoint->qps[qp].no_credits_pending_frags[0]);
        OBJ_DESTRUCT(&endpoint->qps[qp].no_credits_pending_frags[1]);

        MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(
                &endpoint->qps[qp].no_wqe_pending_frags[0]);
        MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(
                &endpoint->qps[qp].no_wqe_pending_frags[1]);
        OBJ_DESTRUCT(&endpoint->qps[qp].no_wqe_pending_frags[0]);
        OBJ_DESTRUCT(&endpoint->qps[qp].no_wqe_pending_frags[1]);


        if(--endpoint->qps[qp].qp->users != 0)
            continue;

        if(endpoint->qps[qp].qp->lcl_qp != NULL)
            if(ibv_destroy_qp(endpoint->qps[qp].qp->lcl_qp))
                BTL_ERROR(("Failed to destroy QP:%d\n", qp));

        free(endpoint->qps[qp].qp);
    }

    /* free the qps */
    free(endpoint->qps);
    endpoint->qps = NULL;

    free(endpoint->rem_info.rem_qps);
    free(endpoint->rem_info.rem_srqs);

    /* unregister xrc recv qp */
#if HAVE_XRC
    if (0 != endpoint->xrc_recv_qp_num) {
        if(ibv_unreg_xrc_rcv_qp(endpoint->endpoint_btl->device->xrc_domain,
                    endpoint->xrc_recv_qp_num)) {
            BTL_ERROR(("Failed to unregister XRC recv QP:%d\n", endpoint->xrc_recv_qp_num));
        }
    }
#endif

    OBJ_DESTRUCT(&endpoint->endpoint_lock);
    /* Clean pending lists */
    MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(&endpoint->pending_lazy_frags);
    OBJ_DESTRUCT(&endpoint->pending_lazy_frags);

    MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(&endpoint->pending_get_frags);
    OBJ_DESTRUCT(&endpoint->pending_get_frags);

    MCA_BTL_OPENIB_CLEAN_PENDING_FRAGS(&endpoint->pending_put_frags);
    OBJ_DESTRUCT(&endpoint->pending_put_frags);
}


/*
 * Called when the connect module has created all the qp's on an
 * endpoint and needs to have some receive buffers posted.
 */
int mca_btl_openib_endpoint_post_recvs(mca_btl_openib_endpoint_t *endpoint)
{
    int qp;

    for (qp = 0; qp < mca_btl_openib_component.num_qps; ++qp) {
        if (BTL_OPENIB_QP_TYPE_PP(qp)) {
            mca_btl_openib_endpoint_post_rr_nolock(endpoint, qp);
        } else {
            mca_btl_openib_post_srr(endpoint->endpoint_btl, qp);
        }
    }

    return OMPI_SUCCESS;
}

static void cts_sent(mca_btl_base_module_t* btl,
                     struct mca_btl_base_endpoint_t* ep,
                     struct mca_btl_base_descriptor_t* des,
                     int status)
{
    /* Nothing to do/empty function (we can't pass in a NULL pointer
       for the des_cbfunc) */
    OPAL_OUTPUT((-1, "CTS send to %s completed",
                 ep->endpoint_proc->proc_ompi->proc_hostname));
}

/*
 * Send CTS control fragment
 */
void mca_btl_openib_endpoint_send_cts(mca_btl_openib_endpoint_t *endpoint) 
{
    mca_btl_openib_send_control_frag_t *sc_frag;
    mca_btl_base_descriptor_t *base_des;
    mca_btl_openib_frag_t *openib_frag;
    mca_btl_openib_com_frag_t *com_frag;
    mca_btl_openib_control_header_t *ctl_hdr;

    OPAL_OUTPUT((-1, "SENDING CTS to %s on qp index %d (QP num %d)",
                 endpoint->endpoint_proc->proc_ompi->proc_hostname,
                 mca_btl_openib_component.credits_qp,
                 endpoint->qps[mca_btl_openib_component.credits_qp].qp->lcl_qp->qp_num));
    sc_frag = alloc_control_frag(endpoint->endpoint_btl);
    if (OPAL_UNLIKELY(NULL == sc_frag)) {
        BTL_ERROR(("Failed to allocate control buffer"));
        mca_btl_openib_endpoint_invoke_error(endpoint);
        return;
    }

    /* I dislike using the "to_<foo>()" macros; I prefer using the
       explicit member fields to ensure I get the types right.  Since
       this is not a performance-criticial part of the code, it's
       ok. */
    com_frag = &(sc_frag->super.super);
    openib_frag = &(com_frag->super);
    base_des = &(openib_frag->base);

    base_des->des_cbfunc = cts_sent;
    base_des->des_cbdata = NULL;
    base_des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY|MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    base_des->order = mca_btl_openib_component.credits_qp;
    openib_frag->segment.seg_len = sizeof(mca_btl_openib_control_header_t);
    com_frag->endpoint = endpoint;

    sc_frag->hdr->tag = MCA_BTL_TAG_BTL;
    sc_frag->hdr->cm_seen = 0;
    sc_frag->hdr->credits = 0;

    ctl_hdr = (mca_btl_openib_control_header_t*)
        openib_frag->segment.seg_addr.pval;
    ctl_hdr->type = MCA_BTL_OPENIB_CONTROL_CTS;

    /* Send the fragment */
    OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
    if (OMPI_SUCCESS != mca_btl_openib_endpoint_post_send(endpoint, sc_frag)) {
        BTL_ERROR(("Failed to post CTS send"));
        mca_btl_openib_endpoint_invoke_error(endpoint);
    }
    endpoint->endpoint_cts_sent = true;
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
}

/*
 * Called when the CPC has established a connection on an endpoint
 */
void mca_btl_openib_endpoint_cpc_complete(mca_btl_openib_endpoint_t *endpoint)
{
    /* If the CPC uses the CTS protocol, then start it up */
    if (endpoint->endpoint_local_cpc->cbm_uses_cts) {
        int transport_type_ib_p = 0;
        /* Post our receives, which will make credit management happy
           (i.e., rd_credits will be 0) */
        if (OMPI_SUCCESS != mca_btl_openib_endpoint_post_recvs(endpoint)) {
            BTL_ERROR(("Failed to post receive buffers"));
            mca_btl_openib_endpoint_invoke_error(endpoint);
            return;
        }
        endpoint->endpoint_posted_recvs = true;

        /* If this is IB, send the CTS immediately.  If this is iWARP,
           then only send the CTS if this endpoint was the initiator
           of the connection (the receiver will send its CTS when it
           receives this side's CTS).  Also send the CTS if we already
           received the peer's CTS (e.g., if this process was slow to
           call cpc_complete(). */
#if defined(HAVE_STRUCT_IBV_DEVICE_TRANSPORT_TYPE)
        transport_type_ib_p = (IBV_TRANSPORT_IB == endpoint->endpoint_btl->device->ib_dev->transport_type);
#endif
        OPAL_OUTPUT((-1, "cpc_complete to peer %s: is IB %d, initiatior %d, cts received: %d",
                     endpoint->endpoint_proc->proc_ompi->proc_hostname,
                     transport_type_ib_p,
                     endpoint->endpoint_initiator,
                     endpoint->endpoint_cts_received));
        if (transport_type_ib_p ||
            endpoint->endpoint_initiator ||
            endpoint->endpoint_cts_received) {
            mca_btl_openib_endpoint_send_cts(endpoint);

            /* If we've already got the CTS from the other side, then
               mark us as connected */
            if (endpoint->endpoint_cts_received) {
                OPAL_OUTPUT((-1, "cpc_complete to %s -- already got CTS, so marking endpoint as complete",
                             endpoint->endpoint_proc->proc_ompi->proc_hostname));
                mca_btl_openib_endpoint_connected(endpoint);
            }
        }

        OPAL_OUTPUT((-1, "cpc_complete to %s -- done",
                     endpoint->endpoint_proc->proc_ompi->proc_hostname));
        return;
    }

    /* Otherwise, just set the endpoint to "connected" */
    mca_btl_openib_endpoint_connected(endpoint);
}

/*
 * called when the connect module has completed setup of an endpoint
 */
void mca_btl_openib_endpoint_connected(mca_btl_openib_endpoint_t *endpoint)
{
    opal_list_item_t *frag_item, *ep_item;
    mca_btl_openib_send_frag_t *frag;
    mca_btl_openib_endpoint_t *ep;
    bool master = false;

    opal_output(-1, "Now we are CONNECTED");
    if (MCA_BTL_XRC_ENABLED) {
        OPAL_THREAD_LOCK(&endpoint->ib_addr->addr_lock);
        if (MCA_BTL_IB_ADDR_CONNECTED == endpoint->ib_addr->status) {
            /* We are not xrc master */
            /* set our qp pointer to master qp */
            master = false;
        } else {
            /* I'm master of XRC */
            endpoint->ib_addr->status = MCA_BTL_IB_ADDR_CONNECTED;
            master = true;
        }
    }

    /* Run over all qps and load alternative path */
#if OPAL_HAVE_THREADS
    if (APM_ENABLED) {
        int i;
        if (MCA_BTL_XRC_ENABLED) {
            if (master) {
                mca_btl_openib_load_apm(endpoint->ib_addr->qp->lcl_qp, endpoint);
            }
        } else {
            for(i = 0; i < mca_btl_openib_component.num_qps; i++) {
                mca_btl_openib_load_apm(endpoint->qps[i].qp->lcl_qp, endpoint);
            }
        }
    }
#endif

    endpoint->endpoint_state = MCA_BTL_IB_CONNECTED;
    endpoint->endpoint_btl->device->non_eager_rdma_endpoints++;

    /* The connection is correctly setup. Now we can decrease the
       event trigger. */
    opal_progress_event_users_decrement();

    if(MCA_BTL_XRC_ENABLED) {
        while(master && !opal_list_is_empty(&endpoint->ib_addr->pending_ep)) {
            ep_item = opal_list_remove_first(&endpoint->ib_addr->pending_ep);
            ep = (mca_btl_openib_endpoint_t *)ep_item;
            if (OMPI_SUCCESS != 
                ompi_btl_openib_connect_base_start(endpoint->endpoint_local_cpc, 
                                                   ep)) {
                BTL_ERROR(("Failed to connect pending endpoint\n"));
            }
        }
        OPAL_THREAD_UNLOCK(&endpoint->ib_addr->addr_lock);
    }


    /* Process pending packet on the endpoint */

    /* While there are frags in the list, process them */
    while (!opal_list_is_empty(&(endpoint->pending_lazy_frags))) {
        frag_item = opal_list_remove_first(&(endpoint->pending_lazy_frags));
        frag = to_send_frag(frag_item);
        /* We need to post this one */

        if(OMPI_ERROR == mca_btl_openib_endpoint_post_send(endpoint, frag))
            BTL_ERROR(("Error posting send"));
    }
    OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);

    /* if upper layer called put or get before connection moved to connected
     * state then we restart them here */
    mca_btl_openib_frag_progress_pending_put_get(endpoint,
            mca_btl_openib_component.rdma_qp);
}

/*
 * Attempt to send a fragment using a given endpoint. If the endpoint is not
 * connected, queue the fragment and start the connection as required.
 */
int mca_btl_openib_endpoint_send(mca_btl_base_endpoint_t* ep,
                                 mca_btl_openib_send_frag_t* frag)
{
    int rc;

    OPAL_THREAD_LOCK(&ep->endpoint_lock);
    rc = check_endpoint_state(ep, &to_base_frag(frag)->base,
            &ep->pending_lazy_frags);

    if(OPAL_LIKELY(OMPI_SUCCESS == rc)) {
        rc = mca_btl_openib_endpoint_post_send(ep, frag);
    }
    OPAL_THREAD_UNLOCK(&ep->endpoint_lock);
    if (OPAL_UNLIKELY(OMPI_ERR_RESOURCE_BUSY == rc)) {
        rc = OMPI_SUCCESS;
    }

    return rc;
}

/**
 * Return control fragment.
 */

static void mca_btl_openib_endpoint_credits(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{

    int qp;

    mca_btl_openib_send_control_frag_t *frag = to_send_control_frag(des);

    qp = frag->qp_idx;

    /* we don't acquire a WQE for credit message - so decrement.
     * Note: doing it for QP used for credit management */
    qp_get_wqe(ep, des->order);

    if(check_send_credits(ep, qp) || check_eager_rdma_credits(ep))
        mca_btl_openib_endpoint_send_credits(ep, qp);
    else {
        BTL_OPENIB_CREDITS_SEND_UNLOCK(ep, qp);
        /* check one more time if credits are available after unlock */
        send_credits(ep, qp);
    }
}

/**
 * Return credits to peer
 */

void mca_btl_openib_endpoint_send_credits(mca_btl_openib_endpoint_t* endpoint,
        const int qp)
{
    mca_btl_openib_module_t* openib_btl = endpoint->endpoint_btl;
    mca_btl_openib_send_control_frag_t* frag;
    mca_btl_openib_rdma_credits_header_t *credits_hdr;
    int rc;
    bool do_rdma = false;
    int32_t cm_return;

    frag = endpoint->qps[qp].credit_frag;

    if(OPAL_UNLIKELY(NULL == frag)) {
        frag = alloc_control_frag(openib_btl);
        frag->qp_idx = qp;
        endpoint->qps[qp].credit_frag = frag;
        /* set those once and forever */
        to_base_frag(frag)->base.order = mca_btl_openib_component.credits_qp;
        to_base_frag(frag)->base.des_cbfunc = mca_btl_openib_endpoint_credits;
        to_base_frag(frag)->base.des_cbdata = NULL;
        to_base_frag(frag)->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;;
        to_com_frag(frag)->endpoint = endpoint;
        frag->hdr->tag = MCA_BTL_TAG_BTL;
        to_base_frag(frag)->segment.seg_len =
            sizeof(mca_btl_openib_rdma_credits_header_t);
    }

    assert(frag->qp_idx == qp);
    credits_hdr = (mca_btl_openib_rdma_credits_header_t*)
        to_base_frag(frag)->segment.seg_addr.pval;
    if(OMPI_SUCCESS == acquire_eager_rdma_send_credit(endpoint)) {
        do_rdma = true;
    } else {
        if(OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_sent, 1) >
                (mca_btl_openib_component.qp_infos[qp].u.pp_qp.rd_rsv - 1)) {
            OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_sent, -1);
            BTL_OPENIB_CREDITS_SEND_UNLOCK(endpoint, qp);
            return;
        }
     }

    BTL_OPENIB_GET_CREDITS(endpoint->qps[qp].u.pp_qp.rd_credits, frag->hdr->credits);

    frag->hdr->cm_seen = 0;
    BTL_OPENIB_GET_CREDITS(endpoint->qps[qp].u.pp_qp.cm_return, cm_return);
    if(cm_return > 255) {
        frag->hdr->cm_seen = 255;
        cm_return -= 255;
        OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_return, cm_return);
    } else {
        frag->hdr->cm_seen = cm_return;
    }

    BTL_OPENIB_GET_CREDITS(endpoint->eager_rdma_local.credits, credits_hdr->rdma_credits);
    credits_hdr->qpn = qp;
    credits_hdr->control.type = MCA_BTL_OPENIB_CONTROL_CREDITS;

    if(endpoint->nbo)
         BTL_OPENIB_RDMA_CREDITS_HEADER_HTON(*credits_hdr);

    qp_reset_signal_count(endpoint, qp);
    if((rc = post_send(endpoint, frag, do_rdma, 1)) == 0)
        return;

    if(endpoint->nbo) {
        BTL_OPENIB_HEADER_NTOH(*frag->hdr);
        BTL_OPENIB_RDMA_CREDITS_HEADER_NTOH(*credits_hdr);
    }
    BTL_OPENIB_CREDITS_SEND_UNLOCK(endpoint, qp);
    OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.rd_credits,
            frag->hdr->credits);
    OPAL_THREAD_ADD32(&endpoint->eager_rdma_local.credits,
            credits_hdr->rdma_credits);
    if(do_rdma)
        OPAL_THREAD_ADD32(&endpoint->eager_rdma_remote.tokens, 1);
    else
        OPAL_THREAD_ADD32(&endpoint->qps[qp].u.pp_qp.cm_sent, -1);

    BTL_ERROR(("error posting send request errno %d says %s", rc,
                strerror(errno)));
}

/* local callback function for completion of eager rdma connect */
static void mca_btl_openib_endpoint_eager_rdma_connect_cb(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_btl_openib_device_t *device = endpoint->endpoint_btl->device;
    OPAL_THREAD_ADD32(&device->non_eager_rdma_endpoints, -1);
    assert(device->non_eager_rdma_endpoints >= 0);
    MCA_BTL_IB_FRAG_RETURN(descriptor);
}

/* send the eager rdma connect message to the remote endpoint */
static int mca_btl_openib_endpoint_send_eager_rdma(
    mca_btl_base_endpoint_t* endpoint)
{
    mca_btl_openib_module_t* openib_btl = endpoint->endpoint_btl;
    mca_btl_openib_eager_rdma_header_t *rdma_hdr;
    mca_btl_openib_send_control_frag_t* frag;
    int rc;

    frag = alloc_control_frag(openib_btl);
    if(NULL == frag) {
        return -1;
    }

    to_base_frag(frag)->base.des_cbfunc =
        mca_btl_openib_endpoint_eager_rdma_connect_cb;
    to_base_frag(frag)->base.des_cbdata = NULL;
    to_base_frag(frag)->base.des_flags |= MCA_BTL_DES_FLAGS_PRIORITY|MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    to_base_frag(frag)->base.order = mca_btl_openib_component.credits_qp;
    to_base_frag(frag)->segment.seg_len =
        sizeof(mca_btl_openib_eager_rdma_header_t);
    to_com_frag(frag)->endpoint = endpoint;

    frag->hdr->tag = MCA_BTL_TAG_BTL;
    rdma_hdr = (mca_btl_openib_eager_rdma_header_t*)to_base_frag(frag)->segment.seg_addr.pval;
    rdma_hdr->control.type = MCA_BTL_OPENIB_CONTROL_RDMA;
    rdma_hdr->rkey = endpoint->eager_rdma_local.reg->mr->rkey;
    rdma_hdr->rdma_start.lval = ompi_ptr_ptol(endpoint->eager_rdma_local.base.pval);
    BTL_VERBOSE(("sending rkey %" PRIu32 ", rdma_start.lval %" PRIx64 
                 ", pval %p, ival %" PRIu32 " type %d and sizeof(rdma_hdr) %d\n",
                 rdma_hdr->rkey,
                 rdma_hdr->rdma_start.lval,
                 rdma_hdr->rdma_start.pval,
                 rdma_hdr->rdma_start.ival,
                 rdma_hdr->control.type,
                 (int) sizeof(mca_btl_openib_eager_rdma_header_t)
                 ));

    if(endpoint->nbo) {
        BTL_OPENIB_EAGER_RDMA_CONTROL_HEADER_HTON((*rdma_hdr));

        BTL_VERBOSE(("after HTON: sending rkey %" PRIu32 ", rdma_start.lval %" PRIx64 ", pval %p, ival %" PRIu32 "\n",
                     rdma_hdr->rkey,
                     rdma_hdr->rdma_start.lval,
                     rdma_hdr->rdma_start.pval,
                     rdma_hdr->rdma_start.ival
                     ));
    }
    rc = mca_btl_openib_endpoint_send(endpoint, frag);
    if (OMPI_SUCCESS == rc ||OMPI_ERR_RESOURCE_BUSY == rc)
        return OMPI_SUCCESS;

    MCA_BTL_IB_FRAG_RETURN(frag);
    BTL_ERROR(("Error sending RDMA buffer: %s", strerror(errno)));
    return rc;
}

/* Setup eager RDMA buffers and notify the remote endpoint*/
void mca_btl_openib_endpoint_connect_eager_rdma(
        mca_btl_openib_endpoint_t* endpoint)
{
    mca_btl_openib_module_t* openib_btl = endpoint->endpoint_btl;
    char *buf;
    mca_btl_openib_recv_frag_t *headers_buf;
    int i;
    uint32_t flag = MCA_MPOOL_FLAGS_CACHE_BYPASS;

    /* Set local rdma pointer to 1 temporarily so other threads will not try
     * to enter the function */
    if(!opal_atomic_cmpset_ptr(&endpoint->eager_rdma_local.base.pval, NULL,
                (void*)1))
        return;

    headers_buf = (mca_btl_openib_recv_frag_t*)
        malloc(sizeof(mca_btl_openib_recv_frag_t) *
            mca_btl_openib_component.eager_rdma_num);

    if(NULL == headers_buf)
       goto unlock_rdma_local;

#if HAVE_DECL_IBV_ACCESS_SO
    /* Solaris implements the Relaxed Ordering feature defined in the
       PCI Specification. With this in mind any memory region which
       relies on a buffer being written in a specific order, for
       example the eager rdma connections created in this routinue,
       must set a strong order flag when registering the memory for
       rdma operations.

       The following flag will be interpreted and the appropriate
       steps will be taken when the memory is registered in
       openib_reg_mr(). */
    flag |= MCA_MPOOL_FLAGS_SO_MEM;
#endif

     buf = openib_btl->super.btl_mpool->mpool_alloc(openib_btl->super.btl_mpool,
            openib_btl->eager_rdma_frag_size *
            mca_btl_openib_component.eager_rdma_num,
            mca_btl_openib_component.buffer_alignment,
            flag,
            (mca_mpool_base_registration_t**)&endpoint->eager_rdma_local.reg);

    if(!buf)
       goto free_headers_buf;

    buf = buf + openib_btl->eager_rdma_frag_size -
        sizeof(mca_btl_openib_footer_t) - openib_btl->super.btl_eager_limit -
        sizeof(mca_btl_openib_header_t);

    for(i = 0; i < mca_btl_openib_component.eager_rdma_num; i++) {
        ompi_free_list_item_t *item;
        mca_btl_openib_recv_frag_t * frag;
        mca_btl_openib_frag_init_data_t init_data;

        item = (ompi_free_list_item_t*)&headers_buf[i];
        item->registration = (void*)endpoint->eager_rdma_local.reg;
        item->ptr = buf + i * openib_btl->eager_rdma_frag_size;
        OBJ_CONSTRUCT(item, mca_btl_openib_recv_frag_t);

        init_data.order = mca_btl_openib_component.credits_qp;
        init_data.list = NULL;

        mca_btl_openib_frag_init(item, &init_data);
        frag = to_recv_frag(item);
        to_base_frag(frag)->type = MCA_BTL_OPENIB_FRAG_EAGER_RDMA;
        to_com_frag(frag)->endpoint = endpoint;
        frag->ftr = (mca_btl_openib_footer_t*)
            ((char*)to_base_frag(frag)->segment.seg_addr.pval +
             mca_btl_openib_component.eager_limit);

        MCA_BTL_OPENIB_RDMA_MAKE_REMOTE(frag->ftr);
    }

    endpoint->eager_rdma_local.frags = headers_buf;

    endpoint->eager_rdma_local.rd_win =
        mca_btl_openib_component.eager_rdma_num >> 2;
    endpoint->eager_rdma_local.rd_win =
        endpoint->eager_rdma_local.rd_win?endpoint->eager_rdma_local.rd_win:1;

    /* set local rdma pointer to real value */
    opal_atomic_cmpset_ptr(&endpoint->eager_rdma_local.base.pval, (void*)1,
            buf);

    if(mca_btl_openib_endpoint_send_eager_rdma(endpoint) == OMPI_SUCCESS) {
        mca_btl_openib_device_t *device = endpoint->endpoint_btl->device;
        mca_btl_openib_endpoint_t **p;
        OBJ_RETAIN(endpoint);
        assert(((opal_object_t*)endpoint)->obj_reference_count == 2);
        do {
            p = &device->eager_rdma_buffers[device->eager_rdma_buffers_count];
        } while(!opal_atomic_cmpset_ptr(p, NULL, endpoint));

        OPAL_THREAD_ADD32(&openib_btl->eager_rdma_channels, 1);
        /* from this point progress function starts to poll new buffer */
        OPAL_THREAD_ADD32(&device->eager_rdma_buffers_count, 1);
        return;
    }

    openib_btl->super.btl_mpool->mpool_free(openib_btl->super.btl_mpool,
           buf, (mca_mpool_base_registration_t*)endpoint->eager_rdma_local.reg);
free_headers_buf:
    free(headers_buf);
unlock_rdma_local:
    /* set local rdma pointer back to zero. Will retry later */
    opal_atomic_cmpset_ptr(&endpoint->eager_rdma_local.base.pval,
            endpoint->eager_rdma_local.base.pval, NULL);
    endpoint->eager_rdma_local.frags = NULL;
}

/*
 * Invoke an error on the btl associated with an endpoint.  If we
 * don't have an endpoint, then just use the first one on the
 * component list of BTLs.
 */
void *mca_btl_openib_endpoint_invoke_error(void *context)
{
    mca_btl_openib_endpoint_t *endpoint = (mca_btl_openib_endpoint_t*) context;
    mca_btl_openib_module_t *btl = NULL;

    if (NULL == endpoint) {
        int i;
        for (i = 0; i < mca_btl_openib_component.ib_num_btls; ++i) {
            if (NULL != mca_btl_openib_component.openib_btls[i] &&
                NULL != mca_btl_openib_component.openib_btls[i]->error_cb) {
                btl = mca_btl_openib_component.openib_btls[i];
                break;
            }
        }
    } else {
        btl = endpoint->endpoint_btl;
    }

    /* If we didn't find a BTL, then just bail :-( */
    if (NULL == btl || NULL == btl->error_cb) {
        orte_show_help("help-mpi-btl-openib.txt",
                       "cannot raise btl error", true,
                       orte_process_info.nodename,
                       __FILE__, __LINE__);
        exit(1);
    }

    /* Invoke the callback to the upper layer */
    btl->error_cb(&(btl->super), MCA_BTL_ERROR_FLAGS_FATAL, NULL, NULL);

    /* Will likely never get here */
    return NULL;
}
