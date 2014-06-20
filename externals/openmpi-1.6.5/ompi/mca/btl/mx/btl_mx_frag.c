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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_mx_frag.h" 

static void mca_btl_mx_frag_common_constructor(mca_btl_mx_frag_t* frag) 
{ 
    frag->base.des_src     = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst     = NULL;
    frag->base.des_dst_cnt = 0;
}

static void mca_btl_mx_frag_eager_constructor(mca_btl_mx_frag_t* frag) 
{ 
    frag->size = mca_btl_mx_module.super.btl_eager_limit;  
    mca_btl_mx_frag_common_constructor(frag); 
}

static void mca_btl_mx_frag_max_constructor(mca_btl_mx_frag_t* frag) 
{ 
    frag->size = mca_btl_mx_module.super.btl_max_send_size; 
    mca_btl_mx_frag_common_constructor(frag); 
}

static void mca_btl_mx_frag_user_constructor(mca_btl_mx_frag_t* frag) 
{ 
    frag->size = 0; 
    mca_btl_mx_frag_common_constructor(frag); 
}


OBJ_CLASS_INSTANCE(
    mca_btl_mx_frag_t, 
    mca_btl_base_descriptor_t, 
    NULL, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_mx_frag_eager_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_mx_frag_eager_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_mx_frag_max_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_mx_frag_max_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_mx_frag_user_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_mx_frag_user_constructor, 
    NULL); 

