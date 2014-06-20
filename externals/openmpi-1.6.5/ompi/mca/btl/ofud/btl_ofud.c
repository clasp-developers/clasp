/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <errno.h>
#include <string.h>

#include "ompi_config.h"
#include "opal/class/opal_bitmap.h"
#include "opal/prefetch.h"
#include "opal/util/output.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/rdma/mpool_rdma.h"

#include "btl_ofud.h"
#include "btl_ofud_frag.h"
#include "btl_ofud_proc.h"
#include "btl_ofud_endpoint.h"


mca_btl_ud_module_t mca_btl_ofud_module = {
    {
        &mca_btl_ofud_component.super,
        0, /* eager_limit */
        0, /* min_send_size */
        0, /* max_send_size */
        0, /* rdma_pipeline_send_length */
        0, /* rdma_pipeline_frag_size */
        0, /* min_rdma_pipeline_size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        MCA_BTL_FLAGS_SEND,
        mca_btl_ud_add_procs,
        mca_btl_ud_del_procs,
        NULL,
        mca_btl_ud_finalize,
        mca_btl_ud_alloc,
        mca_btl_ud_free,
        mca_btl_ud_prepare_src,
        NULL, /*mca_btl_ud_prepare_dst */
        mca_btl_ud_send,
        NULL, /* send immediate */
        NULL, /*mca_btl_ud_put */
        NULL, /*mca_btl_ud_get */
        mca_btl_base_dump,
        NULL, /* mpool */
        NULL, /* register error */
        mca_btl_ud_ft_event
    }
};



/*
 *  Add procs to this BTL module, receiving endpoint information from the modex.
 */

int mca_btl_ud_add_procs(struct mca_btl_base_module_t* btl,
                         size_t nprocs,
                         struct ompi_proc_t **ompi_procs,
                         struct mca_btl_base_endpoint_t** peers,
                         opal_bitmap_t* reachable)
{
    mca_btl_ud_module_t* ud_btl = (mca_btl_ud_module_t*)btl;
    struct ibv_ah_attr ah_attr;
    int i, rc;

    /* Set up the endpoint lookup table if it hasn't been already */
    /* We do this here so we can initialize the table to a reasonable size
       based on nprocs */
#if 0
    if(NULL == ud_btl->ep_lookup) {
        ud_btl->ep_lookup = malloc(sizeof(opal_hash_table_t));
        OBJ_CONSTRUCT(ud_btl->ep_lookup, opal_hash_table_t);
        opal_hash_table_init(ud_btl->ep_lookup, nprocs);
    }
#endif

    for(i = 0; i < (int)nprocs; i++) {
        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_ud_proc_t* ib_proc;
        mca_btl_base_endpoint_t* ib_peer;

        if(NULL == (ib_proc = mca_btl_ud_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }


        /* The btl_proc datastructure is shared by all IB BTL instances that are
         * trying to reach this destination. Cache the peer instance on the
         * btl_proc.
         */
        ib_peer = OBJ_NEW(mca_btl_ud_endpoint_t);
        if(NULL == ib_peer) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        OPAL_THREAD_LOCK(&ib_proc->proc_lock);
        rc = mca_btl_ud_proc_insert(ib_proc, ib_peer);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(ib_peer);
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            continue;
        }

        BTL_VERBOSE(("modex_recv QP num %d, LID = %d",
                ib_peer->rem_addr.qp_num, ib_peer->rem_addr.lid));

        /* Set up IB address handles for the endpoint */
        ah_attr.is_global = 0;
        ah_attr.dlid = ib_peer->rem_addr.lid;
        ah_attr.sl = mca_btl_ofud_component.ib_service_level;
        ah_attr.src_path_bits = mca_btl_ofud_component.ib_src_path_bits;
        ah_attr.port_num = ud_btl->ib_port_num;

        ib_peer->rmt_ah = ibv_create_ah(ud_btl->ib_pd, &ah_attr);
        if(NULL == ib_peer->rmt_ah) {
            BTL_ERROR(("error creating address handle: %s\n", strerror(errno)));
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            continue;
        }

        /* Insert a pointer to the endpoint in the BTL lookup table */
#if 0
        opal_hash_table_set_value_uint64(ud_btl->ep_lookup,
                ((uint64_t)ib_peer->rem_addr.lid << 32) |
                        ib_peer->rem_addr.qp_num,
                ib_peer);
#endif

        opal_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
        peers[i] = ib_peer;
    }

    return OMPI_SUCCESS;
}


/*
 * Delete the proc as reachable from this btl module
 */

int mca_btl_ud_del_procs(struct mca_btl_base_module_t* btl,
                         size_t nprocs,
                         struct ompi_proc_t** procs,
                         struct mca_btl_base_endpoint_t** peers)
{
    size_t i;

    for(i = 0; i < nprocs; i++) {
        mca_btl_ud_endpoint_t* endpoint = (mca_btl_ud_endpoint_t*)peers[i];
        mca_btl_ud_proc_t* proc = mca_btl_ud_proc_lookup_ompi(procs[i]);
#if 0
        opal_hash_table_remove_value_uint64(ud_btl->ep_lookup,
                ((uint64_t)endpoint->rem_addr.lid << 32) |
                        endpoint->rem_addr.qp_num);
#endif
        if(NULL != proc) {
            mca_btl_ud_proc_remove(proc, endpoint);
        }

        OBJ_RELEASE(endpoint);
    }

    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 *
 * When allocating a segment we pull a pre-alllocated segment
 * from one of two free lists, an eager list and a max list
 */

mca_btl_base_descriptor_t* mca_btl_ud_alloc(struct mca_btl_base_module_t* btl,
                                            struct mca_btl_base_endpoint_t* endpoint,
                                            uint8_t order,
                                            size_t size,
                                            uint32_t flags)
{
    mca_btl_ud_frag_t* frag = NULL;
    int rc;

    if(OPAL_LIKELY(size <= mca_btl_ofud_module.super.btl_eager_limit)) {
        MCA_BTL_UD_ALLOC_FRAG(btl, frag, rc);
    }

    if(NULL == frag) {
        return NULL;
    }

    frag->base.order = MCA_BTL_NO_ORDER;
    frag->base.des_flags = flags;
    frag->segment.seg_len = size;
    return (mca_btl_base_descriptor_t*)frag;
}


/**
 * Return a segment
 *
 * Return the segment to the appropriate
 *  preallocated segment list
 */

int mca_btl_ud_free(struct mca_btl_base_module_t* btl,
                    mca_btl_base_descriptor_t* des)
{
    mca_btl_ud_frag_t* frag = (mca_btl_ud_frag_t*)des;

    if(OPAL_LIKELY(frag->type == MCA_BTL_UD_FRAG_SEND)) {
        MCA_BTL_UD_RETURN_FRAG(btl, frag);
    } else if(frag->type == MCA_BTL_UD_FRAG_USER && frag->ud_reg != NULL) {
        btl->btl_mpool->mpool_deregister(btl->btl_mpool,
                (mca_mpool_base_registration_t*)frag->ud_reg);
        MCA_BTL_UD_RETURN_USER_FRAG(btl, frag);
    }

    return OMPI_SUCCESS;
}


/**
 * register user buffer or pack
 * data into pre-registered buffer and return a
 * descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 *
 * prepare source's behavior depends on the following:
 * Has a valid memory registration been passed to prepare_src?
 *  if so we attempt to use the pre-registred user-buffer, if the memory
 *  registration is to small (only a portion of the user buffer) then we must
 *  reregister the user buffer
 * Has the user requested the memory to be left pinned?
 *  if so we insert the memory registration into a memory tree for later
 *  lookup, we may also remove a previous registration if a MRU (most recently
 *  used) list of registions is full, this prevents resources from being
 *  exhausted.
 * Is the requested size larger than the btl's max send size?
 *  if so and we aren't asked to leave the registration pinned then we
 *  register the memory if the user's buffer is contiguous.
 * Otherwise we choose from two free lists of pre-registered memory in which
 * to pack the data into.
 *
 */

mca_btl_base_descriptor_t* mca_btl_ud_prepare_src(
                                    struct mca_btl_base_module_t* btl,
                                    struct mca_btl_base_endpoint_t* endpoint,
                                    mca_mpool_base_registration_t* registration,
                                    struct opal_convertor_t* convertor,
                                    uint8_t order,
                                    size_t reserve,
                                    size_t* size,
                                    uint32_t flags)
{
    mca_btl_ud_frag_t* frag = NULL;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    if(opal_convertor_need_buffers(convertor) == 0 && reserve == 0 &&
            (registration != NULL || max_data > btl->btl_max_send_size)) {
        /* The user buffer is contigous and we are asked to send more than
           the max send size. */

        MCA_BTL_UD_ALLOC_USER_FRAG(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }

        iov.iov_len = max_data;
        iov.iov_base = NULL;

        opal_convertor_pack(convertor, &iov, &iov_count, &max_data);

        frag->segment.seg_len = max_data;
        frag->segment.seg_addr.pval = iov.iov_base;
        frag->base.des_flags = flags;
        frag->base.order = MCA_BTL_NO_ORDER;

        if(NULL == registration) {
            rc = btl->btl_mpool->mpool_register(btl->btl_mpool, iov.iov_base,
                max_data, 0, &registration);
            if(OMPI_SUCCESS != rc || NULL == registration) {
                MCA_BTL_UD_RETURN_USER_FRAG(btl, frag);
            }
            return NULL;
        }

        frag->ud_reg = (mca_btl_ud_reg_t*)registration;

        frag->sg_entry.lkey = frag->ud_reg->mr->lkey;
        frag->sg_entry.addr = (unsigned long)iov.iov_base;

        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        *size = max_data;
        return &frag->base;
    }

    if(max_data + reserve > btl->btl_eager_limit) {
        max_data = btl->btl_eager_limit - reserve;
    }

    MCA_BTL_UD_ALLOC_FRAG(btl, frag, rc);
    if(OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    iov.iov_len = max_data;
    iov.iov_base = (unsigned char*)frag->segment.seg_addr.pval + reserve;

    rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data);
    if(OPAL_UNLIKELY(rc < 0)) {
        MCA_BTL_UD_RETURN_FRAG(btl, frag);
        return NULL;
    }

    frag->segment.seg_len = max_data + reserve;
    frag->sg_entry.length =
        max_data + reserve + sizeof(mca_btl_ud_header_t);

    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = flags;
    frag->base.order = MCA_BTL_NO_ORDER;
    *size  = max_data;

    return &frag->base;
}


int mca_btl_ud_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_ud_module_t* ud_btl = (mca_btl_ud_module_t*)btl;
    int32_t i;

    for(i = 0; i < MCA_BTL_UD_NUM_QP; i++) {
        ibv_destroy_qp(ud_btl->ib_qp[i]);
    }

    ibv_dealloc_pd(ud_btl->ib_pd);
    
    OBJ_DESTRUCT(&ud_btl->ud_lock);
    OBJ_DESTRUCT(&ud_btl->pending_frags);
    OBJ_DESTRUCT(&ud_btl->send_frags);
    OBJ_DESTRUCT(&ud_btl->user_frags);
    OBJ_DESTRUCT(&ud_btl->recv_frags);
    mca_mpool_base_module_destroy(ud_btl->super.btl_mpool);
    return OMPI_SUCCESS;
}


/*
 *  Initiate a send.
 */

int mca_btl_ud_send(struct mca_btl_base_module_t* btl,
                    struct mca_btl_base_endpoint_t* endpoint,
                    struct mca_btl_base_descriptor_t* descriptor,
                    mca_btl_base_tag_t tag)
{
    int rc;

    mca_btl_ud_frag_t* frag = (mca_btl_ud_frag_t*)descriptor;
    MCA_BTL_UD_START_TIME(post_send);
    frag->endpoint = endpoint;
    frag->hdr->tag = tag;

    rc = mca_btl_ud_endpoint_post_send((mca_btl_ud_module_t*)btl, frag);

    MCA_BTL_UD_END_TIME(post_send);
    return rc;
}


/*
 * RDMA Memory Pool (de)register callbacks
 */

static int mca_btl_ud_reg_mr(void* reg_data, void* base, size_t size,
        mca_mpool_base_registration_t* reg)
{
    mca_btl_ud_module_t* mod = (mca_btl_ud_module_t*)reg_data;
    mca_btl_ud_reg_t* ud_reg = (mca_btl_ud_reg_t*)reg;

    ud_reg->mr = ibv_reg_mr(mod->ib_pd, base, size, IBV_ACCESS_LOCAL_WRITE |
            IBV_ACCESS_REMOTE_WRITE | IBV_ACCESS_REMOTE_READ);

    if(NULL == ud_reg->mr)
        return OMPI_ERR_OUT_OF_RESOURCE;

    return OMPI_SUCCESS;
}


static int mca_btl_ud_dereg_mr(void* reg_data,
        mca_mpool_base_registration_t* reg)
{
    mca_btl_ud_reg_t* ud_reg = (mca_btl_ud_reg_t*)reg;

    if(ud_reg->mr != NULL) {
        if(ibv_dereg_mr(ud_reg->mr)) {
            opal_output(0, "%s: error unpinning UD memory: %s\n",
                    __func__, strerror(errno));
            return OMPI_ERROR;
        }
    }

    ud_reg->mr = NULL;
    return OMPI_SUCCESS;
}


/*
 * Create a single UD queue pair.  Since UD is connectionless, the QP is
 * useable immediately.
 */

/* TODO - can remove cq/psn args now with only one type of frag */
static int mca_btl_ud_init_qp(mca_btl_ud_module_t* ud_btl,
                              struct ibv_cq* cq,
                              struct ibv_qp** qp,
                              uint32_t lcl_psn)
{
    struct ibv_qp_attr qp_attr;
    struct ibv_qp_init_attr qp_init_attr;

    memset(&qp_init_attr, 0, sizeof(struct ibv_qp_init_attr));

    qp_init_attr.send_cq = cq;
    qp_init_attr.recv_cq = cq;
    qp_init_attr.cap.max_send_wr = mca_btl_ofud_component.sd_num;
    qp_init_attr.cap.max_recv_wr = mca_btl_ofud_component.rd_num;
    qp_init_attr.cap.max_send_sge = 1;
    qp_init_attr.cap.max_recv_sge = 1;
    /* TODO - find the best value for max_inline_data */
    qp_init_attr.cap.max_inline_data = 200;
    qp_init_attr.qp_type = IBV_QPT_UD;
    
    *qp = ibv_create_qp(ud_btl->ib_pd, &qp_init_attr);
    if(NULL == *qp) {
        BTL_ERROR(("error creating QP: %s\n", strerror(errno)));
        return OMPI_ERROR;
    }

    if(0 == (ud_btl->ib_inline_max = qp_init_attr.cap.max_inline_data)) {
        BTL_ERROR(("ibv_create_qp: returned 0 byte(s) for max inline data"));
    }

    BTL_VERBOSE(("ib_inline_max %lu\n", (unsigned long) ud_btl->ib_inline_max));

    qp_attr.qp_state = IBV_QPS_INIT;
    qp_attr.pkey_index = mca_btl_ofud_component.ib_pkey_ix;
    qp_attr.qkey = mca_btl_ofud_component.ib_qkey;
    qp_attr.port_num = ud_btl->ib_port_num;

    if(ibv_modify_qp(*qp, &qp_attr,
                     IBV_QP_STATE | IBV_QP_PKEY_INDEX |
                     IBV_QP_PORT | IBV_QP_QKEY)) {
        BTL_ERROR(("error modifying QP to INIT: %s", strerror(errno)));
        goto destroy_qp;
    }

    qp_attr.qp_state = IBV_QPS_RTR;
    if(ibv_modify_qp(*qp, &qp_attr, IBV_QP_STATE)) {
        BTL_ERROR(("error modifing QP to RTR: %s",  strerror(errno)));
        goto destroy_qp;
    }

    qp_attr.qp_state    = IBV_QPS_RTS;
    qp_attr.sq_psn 	    = lcl_psn;
    if (ibv_modify_qp(*qp, &qp_attr, IBV_QP_STATE | IBV_QP_SQ_PSN)) {
        BTL_ERROR(("error modifying QP to RTS: %s", strerror(errno)));
        goto destroy_qp;
    }

    return OMPI_SUCCESS;

destroy_qp:
    ibv_destroy_qp(*qp);
    *qp = NULL;
    return OMPI_ERROR;
}


/*
 * Initialize the btl module by allocating a protection domain,
 *  memory pool, completion queue, and free lists
 */

int mca_btl_ud_module_init(mca_btl_ud_module_t *ud_btl)
{
    struct mca_mpool_base_resources_t mpool_resources;
    struct ibv_context *ctx = ud_btl->ib_dev_context;
    struct ibv_recv_wr* bad_wr;
    mca_btl_ud_frag_t* frag;
    ompi_free_list_item_t* item;
    uint32_t length,length_payload;
    int32_t rc, i;

    ud_btl->sd_wqe = mca_btl_ofud_component.sd_num;

    ud_btl->ib_pd = ibv_alloc_pd(ctx);
    if(NULL == ud_btl->ib_pd) {
        BTL_ERROR(("error allocating PD for %s: %s\n",
                ibv_get_device_name(ud_btl->ib_dev), strerror(errno)));
        return OMPI_ERROR;
    }
        
    mpool_resources.reg_data = (void*)ud_btl;
    mpool_resources.sizeof_reg = sizeof(mca_btl_ud_reg_t);
    mpool_resources.register_mem = mca_btl_ud_reg_mr;
    mpool_resources.deregister_mem = mca_btl_ud_dereg_mr;
    ud_btl->super.btl_mpool =
            mca_mpool_base_module_create(mca_btl_ofud_component.ud_mpool_name,
                    &ud_btl->super, &mpool_resources);

    if(NULL == ud_btl->super.btl_mpool) {
        BTL_ERROR(("error creating IB mpool for %s: %s\n",
                    ibv_get_device_name(ud_btl->ib_dev), strerror(errno)));
        goto dealloc_pd;
    }

    /* Create the completion queue */
    length = mca_btl_ofud_component.rd_num + mca_btl_ofud_component.sd_num;

    ud_btl->ib_cq = ibv_create_cq(ctx, length, NULL, NULL, 0);
    if(NULL == ud_btl->ib_cq) {
        BTL_ERROR(("error creating CQ for %s: %s\n",
                ibv_get_device_name(ud_btl->ib_dev), strerror(errno)));
        goto mpool_destroy;
    }

    /* Set up our packet sequence numbers */
    ud_btl->addr.psn = lrand48() & 0xffffff;

    /* Set up the QPs for this BTL */
    for(i = 0; i < MCA_BTL_UD_NUM_QP; i++) {
        if(OMPI_SUCCESS != mca_btl_ud_init_qp(ud_btl,
                ud_btl->ib_cq, &ud_btl->ib_qp[i], ud_btl->addr.psn)) {
            goto qp_destroy;
        }
    }

    /* Place our QP numbers in our local address information */
    ud_btl->addr.qp_num = ud_btl->ib_qp[0]->qp_num;
    ud_btl->ib_qp_next = 0;

    /*ud_btl->rd_posted = mca_btl_ofud_component.rd_num_init;*/

    /* Initialize pool of receive fragments first, since an error may occur */
    /* TODO - no need for a free list with a static buffer count */
    OBJ_CONSTRUCT(&ud_btl->recv_frags, ompi_free_list_t);
    length = sizeof(mca_btl_ud_frag_t) + sizeof(mca_btl_ud_header_t) +
            ud_btl->super.btl_eager_limit + 2 * MCA_BTL_IB_FRAG_ALIGN;

    length_payload=sizeof(mca_btl_ud_frag_t) + sizeof(mca_btl_ud_header_t) +
        ud_btl->super.btl_eager_limit + 2 * MCA_BTL_IB_FRAG_ALIGN -
        sizeof(mca_btl_ud_recv_frag_t);

    ompi_free_list_init_new(&ud_btl->recv_frags,
                        length + sizeof(mca_btl_ud_ib_header_t),
                        opal_cache_line_size,
                        OBJ_CLASS(mca_btl_ud_recv_frag_t),
                        length_payload,opal_cache_line_size,
                        mca_btl_ofud_component.rd_num,
                        mca_btl_ofud_component.rd_num,
                        mca_btl_ofud_component.rd_num,
                        ud_btl->super.btl_mpool);
#if 0
    ompi_free_list_init_new(&ud_btl->recv_frags,
                        length + sizeof(mca_btl_ud_ib_header_t),
                        opal_cache_line_size,
                        OBJ_CLASS(mca_btl_ud_recv_frag_t),
                        length_payload,opal_cache_line_size,
                         mca_btl_ofud_component.rd_num_init,
                         mca_btl_ofud_component.rd_num_max,
                         mca_btl_ofud_component.rd_num_inc,
                        ud_btl->super.btl_mpool);
#endif

    /* Post receive descriptors */
    for(i = 0; i < mca_btl_ofud_component.rd_num; i++) {
        OMPI_FREE_LIST_GET(&ud_btl->recv_frags, item, rc);
        frag = (mca_btl_ud_frag_t*)item;

        if(NULL == frag) {
            BTL_ERROR(("error getting receive buffer from free list\n"));
            goto obj_destruct;
        }

        frag->type = MCA_BTL_UD_FRAG_RECV;
        frag->sg_entry.length = mca_btl_ofud_module.super.btl_eager_limit +
                sizeof(mca_btl_ud_header_t) + sizeof(mca_btl_ud_ib_header_t);
        if(ibv_post_recv(ud_btl->ib_qp[0],
                &frag->wr_desc.rd_desc, &bad_wr)) {
            BTL_ERROR(("error posting recv, errno %s\n", strerror(errno)));
            goto obj_destruct;
        }
    }

    /* No more errors anticipated - initialize everything else */
    OBJ_CONSTRUCT(&ud_btl->ud_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ud_btl->pending_frags, opal_list_t);
    OBJ_CONSTRUCT(&ud_btl->send_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&ud_btl->user_frags, ompi_free_list_t);

    ompi_free_list_init_new(&ud_btl->send_frags,
                        length,
                        opal_cache_line_size,
                        OBJ_CLASS(mca_btl_ud_send_frag_t),
                        length_payload,opal_cache_line_size,
                        mca_btl_ofud_component.sd_num >> 1,
                        -1,
                        mca_btl_ofud_component.sd_num << 2,
                        ud_btl->super.btl_mpool);

    /* Initialize pool of user fragments */
    length = sizeof(mca_btl_ud_frag_t) +
            sizeof(mca_btl_ud_header_t) + 2 * MCA_BTL_IB_FRAG_ALIGN;

    length_payload = sizeof(mca_btl_ud_frag_t) +
        sizeof(mca_btl_ud_header_t) + 2 * MCA_BTL_IB_FRAG_ALIGN-
        sizeof(mca_btl_ud_user_frag_t);

    ompi_free_list_init_new(&ud_btl->user_frags,
                        length,
                        opal_cache_line_size,
                        OBJ_CLASS(mca_btl_ud_user_frag_t),
                        length_payload,opal_cache_line_size,
                        mca_btl_ofud_component.sd_num >> 1,
                        -1,
                        mca_btl_ofud_component.sd_num << 2,
                        ud_btl->super.btl_mpool);

    return OMPI_SUCCESS;

obj_destruct:
    OBJ_DESTRUCT(&ud_btl->recv_frags);
qp_destroy:
    for(i = 0; i < MCA_BTL_UD_NUM_QP; i++) {
        ibv_destroy_qp(ud_btl->ib_qp[i]);
    }
mpool_destroy:
    mca_mpool_base_module_destroy(ud_btl->super.btl_mpool);
dealloc_pd:
    ibv_dealloc_pd(ud_btl->ib_pd);
    return OMPI_ERROR;
}


int mca_btl_ud_ft_event(int state) {
    return OMPI_SUCCESS;
}


