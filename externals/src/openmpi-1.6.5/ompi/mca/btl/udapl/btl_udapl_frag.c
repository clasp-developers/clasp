/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "btl_udapl.h"
#include "btl_udapl_frag.h" 
#include "ompi/mca/mpool/rdma/mpool_rdma.h"


static void mca_btl_udapl_frag_common_constructor(mca_btl_udapl_frag_t* frag) 
{
    mca_btl_udapl_reg_t* reg =
        (mca_btl_udapl_reg_t*)frag->base.super.registration;

#if OPAL_ENABLE_DEBUG
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
#endif

    frag->registration = reg; 
    frag->segment.seg_addr.pval = (unsigned char*)frag->base.super.ptr;
    frag->ftr = NULL;

    /* Don't understand why yet, but there are cases where reg is NULL -
       that is, this memory has not been registered.  So be careful not
       to dereference a NULL pointer. */
    if(NULL != reg) {
        /* Save the LMR context so we can set up LMR subset triplets later */
        frag->triplet.lmr_context = reg->lmr_triplet.lmr_context;
    }
}

static void mca_btl_udapl_frag_eager_constructor(mca_btl_udapl_frag_t* frag) 
{ 
    frag->segment.seg_len = mca_btl_udapl_module.super.btl_eager_limit;
    frag->size = mca_btl_udapl_component.udapl_eager_frag_size;
    mca_btl_udapl_frag_common_constructor(frag); 
}

static void mca_btl_udapl_frag_max_constructor(mca_btl_udapl_frag_t* frag) 
{ 
    frag->segment.seg_len = mca_btl_udapl_module.super.btl_max_send_size;
    frag->size = mca_btl_udapl_component.udapl_max_frag_size;
    mca_btl_udapl_frag_common_constructor(frag); 
}

static void mca_btl_udapl_frag_user_constructor(mca_btl_udapl_frag_t* frag) 
{ 
    mca_btl_udapl_frag_common_constructor(frag); 
    frag->segment.seg_len = 0;
    frag->segment.seg_addr.pval = NULL;
    frag->ftr = NULL;
    frag->size = 0;
    frag->registration = NULL;
}

static void mca_btl_udapl_frag_eager_rdma_constructor(mca_btl_udapl_frag_t* frag) 
{ 
    mca_btl_udapl_frag_eager_constructor(frag);
    frag->segment.seg_len = mca_btl_udapl_module.super.btl_eager_limit;
    frag->size = mca_btl_udapl_component.udapl_eager_frag_size;
    frag->rdma_ftr = (mca_btl_udapl_rdma_footer_t *)
        ((char *)(frag->segment.seg_addr.pval) +
                frag->size -
                sizeof(mca_btl_udapl_rdma_footer_t));
    frag->rdma_ftr->active=0;
}

static void mca_btl_udapl_frag_common_destructor(mca_btl_udapl_frag_t* frag)
{
#if OPAL_ENABLE_DEBUG
    frag->ftr = NULL;
    frag->size = 0; 
    frag->registration = NULL; 
    frag->segment.seg_len = 0;
    frag->segment.seg_addr.pval = NULL; 
    
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
#endif
}


OBJ_CLASS_INSTANCE(
    mca_btl_udapl_frag_t, 
    mca_btl_base_descriptor_t, 
    NULL, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_udapl_frag_eager_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_udapl_frag_eager_constructor, 
    mca_btl_udapl_frag_common_destructor); 

OBJ_CLASS_INSTANCE(
    mca_btl_udapl_frag_max_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_udapl_frag_max_constructor, 
    mca_btl_udapl_frag_common_destructor); 

OBJ_CLASS_INSTANCE(
    mca_btl_udapl_frag_user_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_udapl_frag_user_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_udapl_frag_eager_rdma_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_udapl_frag_eager_rdma_constructor, 
    mca_btl_udapl_frag_common_destructor); 
