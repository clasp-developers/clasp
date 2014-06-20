/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2007      The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include <errno.h>
#include <string.h>

#include "ompi_config.h"
#include "opal/prefetch.h"
#include "ompi/types.h"

#include "btl_ofud.h"
#include "btl_ofud_endpoint.h"
#include "btl_ofud_frag.h"


static void mca_btl_ud_endpoint_construct(mca_btl_base_endpoint_t* endpoint);
static void mca_btl_ud_endpoint_destruct(mca_btl_base_endpoint_t* endpoint);


/* First, we check the downcounter on the endpoint.
   If it is 0, we queue this frag on the endpoint.
   Otherwise, we check the BTL downcounter.
   If it is 0, we queue this frag on the BTL.
   Otherwise, we post the send. */
#define CHECK_FRAG_QUEUES(sd_wqe, lock, queue, frag)                        \
do {                                                                        \
    if(OPAL_UNLIKELY(OPAL_THREAD_ADD32(&(sd_wqe), -1) < 0)) {               \
        OPAL_THREAD_ADD32(&(sd_wqe), 1);                                    \
        OPAL_THREAD_LOCK(&(lock));                                          \
        opal_list_append(&(queue),                                          \
                (opal_list_item_t*)(frag));                                 \
        OPAL_THREAD_UNLOCK(&(lock));                                        \
        return OMPI_SUCCESS;                                                \
    }                                                                       \
} while(0);


/*
 * Post a send to the work queue
 */

int mca_btl_ud_endpoint_post_send(mca_btl_ud_module_t* ud_btl,
                                  mca_btl_ud_frag_t* frag)
{
    struct ibv_qp* ib_qp;
    struct ibv_send_wr* bad_wr;
    struct ibv_send_wr* wr = &frag->wr_desc.sr_desc;
    mca_btl_ud_endpoint_t* endpoint = frag->endpoint;
    int ret;

    /* Have to be careful here - UD adds a 40 byte header, but it is not
       included on the sending side. */
    frag->sg_entry.length = frag->segment.seg_len + sizeof(mca_btl_ud_header_t);
    wr->send_flags = IBV_SEND_SIGNALED;

    CHECK_FRAG_QUEUES(ud_btl->sd_wqe,
            ud_btl->ud_lock, ud_btl->pending_frags, frag);

    /* We avoid locking here by allowing our stripe counter to count
       until it wraps around uint32_t.  This keeps the mod operation
       out of the critical section, allowing us to use OPAL_THREAD_ADD32
       instead of a full mutex. */
    ib_qp = ud_btl->ib_qp[ud_btl->ib_qp_next % MCA_BTL_UD_NUM_QP];
    OPAL_THREAD_ADD32(((int32_t*)&ud_btl->ib_qp_next), 1);

    wr->wr.ud.ah = endpoint->rmt_ah;
    wr->wr.ud.remote_qpn = endpoint->rem_addr.qp_num;

    if(frag->sg_entry.length <= ud_btl->ib_inline_max) {
        wr->send_flags =
            IBV_SEND_SIGNALED|IBV_SEND_INLINE;
    }

    /*frag->hdr->src_qpnum = ud_btl->addr.qp_num;*/

    MCA_BTL_UD_START_TIME(ibv_post_send);
    if(OPAL_UNLIKELY((ret = ibv_post_send(ib_qp, wr, &bad_wr)))) {
#if 0
        opal_output(0, "ep->sd_wqe %d btl->sd_wqe %d len %d ib_qp_next %d",
                endpoint->sd_wqe, ud_btl->sd_wqe,
                frag->sg_entry.length, ud_btl->ib_qp_next);
#endif
        BTL_ERROR(("error posting send request: %d %s\n", ret, strerror(ret)));

    }
    MCA_BTL_UD_END_TIME(ibv_post_send);

    return OMPI_SUCCESS;
}


OBJ_CLASS_INSTANCE(mca_btl_ud_endpoint_t,
                   opal_list_item_t, mca_btl_ud_endpoint_construct,
                   mca_btl_ud_endpoint_destruct);

/*
 * Construct/destruct an endpoint structure.
 */

static void mca_btl_ud_endpoint_construct(mca_btl_base_endpoint_t* endpoint)
{
#if OPAL_ENABLE_DEBUG
    memset(&endpoint->rem_addr, 0, sizeof(struct mca_btl_ud_addr_t));
#endif
}

static void mca_btl_ud_endpoint_destruct(mca_btl_base_endpoint_t* endpoint)
{
}

