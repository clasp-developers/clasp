/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "btl_self_frag.h"

static inline void mca_btl_self_frag_constructor(mca_btl_self_frag_t* frag)
{
    frag->segment.seg_addr.pval = frag+1;
    frag->segment.seg_len       = (uint32_t)frag->size;
    frag->base.des_src          = &frag->segment;
    frag->base.des_src_cnt      = 1;
    frag->base.des_dst          = &frag->segment;
    frag->base.des_dst_cnt      = 1;
    frag->base.des_flags        = 0;
}

static void mca_btl_self_frag_eager_constructor(mca_btl_self_frag_t* frag)
{
    frag->size = mca_btl_self.btl_eager_limit;
    mca_btl_self_frag_constructor(frag);
}

static void mca_btl_self_frag_send_constructor(mca_btl_self_frag_t* frag)
{
    frag->size = mca_btl_self.btl_max_send_size;
    mca_btl_self_frag_constructor(frag);
}

static void mca_btl_self_frag_rdma_constructor(mca_btl_self_frag_t* frag)
{
    frag->size = 0;
    frag->segment.seg_addr.pval = frag+1;
    frag->segment.seg_len = (uint32_t)frag->size;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
}

OBJ_CLASS_INSTANCE( mca_btl_self_frag_eager_t,
                    mca_btl_base_descriptor_t,
                    mca_btl_self_frag_eager_constructor,
                    NULL );

OBJ_CLASS_INSTANCE( mca_btl_self_frag_send_t,
                    mca_btl_base_descriptor_t,
                    mca_btl_self_frag_send_constructor,
                    NULL );

OBJ_CLASS_INSTANCE( mca_btl_self_frag_rdma_t,
                    mca_btl_base_descriptor_t,
                    mca_btl_self_frag_rdma_constructor,
                    NULL );
