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
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_portals.h" 
#include "btl_portals_frag.h" 


static void
mca_btl_portals_frag_common_send_constructor(mca_btl_portals_frag_t* frag) 
{ 
    frag->base.des_flags = 0;
    frag->base.des_dst = 0;
    frag->base.des_dst_cnt = 0;
    frag->base.des_src = frag->segments;
    frag->base.des_src_cnt = 2;

    frag->segments[0].seg_addr.pval = frag + 1;
    frag->segments[0].seg_len = frag->size;
    frag->segments[0].seg_key.key64 = 0;

    frag->md_h = PTL_INVALID_HANDLE;
}


static void
mca_btl_portals_frag_eager_constructor(mca_btl_portals_frag_t* frag) 
{ 
    frag->size = mca_btl_portals_module.super.btl_eager_limit;  
    mca_btl_portals_frag_common_send_constructor(frag); 
    frag->type = BTL_PORTALS_FRAG_TYPE_EAGER;
}


static void
mca_btl_portals_frag_eager_destructor(mca_btl_portals_frag_t* frag)
{
    if (PTL_INVALID_HANDLE == frag->md_h) {
        PtlMDUnlink(frag->md_h);
        frag->md_h = PTL_INVALID_HANDLE;
    }
}


static void
mca_btl_portals_frag_max_constructor(mca_btl_portals_frag_t* frag) 
{ 
    frag->size = mca_btl_portals_module.super.btl_max_send_size; 
    mca_btl_portals_frag_common_send_constructor(frag); 
    frag->type = BTL_PORTALS_FRAG_TYPE_MAX;
}


static void
mca_btl_portals_frag_user_constructor(mca_btl_portals_frag_t* frag) 
{ 
    frag->base.des_flags = 0;
    frag->base.des_dst = 0;
    frag->base.des_dst_cnt = 0;
    frag->base.des_src = 0;
    frag->base.des_src_cnt = 0;
    frag->size = 0; 
    frag->type = BTL_PORTALS_FRAG_TYPE_USER;
}

static void
mca_btl_portals_frag_recv_constructor(mca_btl_portals_frag_t* frag) 
{ 
    frag->base.des_flags = 0;
    frag->base.des_dst = &frag->segments[0];
    frag->base.des_dst_cnt = 1;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->size = 0; 
}


OBJ_CLASS_INSTANCE(
    mca_btl_portals_frag_t, 
    mca_btl_base_descriptor_t, 
    NULL, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_portals_frag_eager_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_portals_frag_eager_constructor, 
    mca_btl_portals_frag_eager_destructor);

OBJ_CLASS_INSTANCE(
    mca_btl_portals_frag_max_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_portals_frag_max_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_portals_frag_user_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_portals_frag_user_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_portals_frag_recv_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_portals_frag_recv_constructor, 
    NULL); 

