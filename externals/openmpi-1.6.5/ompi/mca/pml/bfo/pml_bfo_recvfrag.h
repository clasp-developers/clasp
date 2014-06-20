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
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 *  @file
 */

#ifndef MCA_PML_BFO_RECVFRAG_H
#define MCA_PML_BFO_RECVFRAG_H

#include "ompi/mca/btl/btl.h"
#include "pml_bfo_hdr.h"

BEGIN_C_DECLS

struct mca_pml_bfo_buffer_t {
    size_t len;
    void * addr;
};
typedef struct mca_pml_bfo_buffer_t mca_pml_bfo_buffer_t;


struct mca_pml_bfo_recv_frag_t {
    ompi_free_list_item_t super;
    mca_pml_bfo_hdr_t hdr;
    size_t num_segments;
    mca_btl_base_module_t* btl;
    mca_btl_base_segment_t segments[MCA_BTL_DES_MAX_SEGMENTS];
    mca_pml_bfo_buffer_t buffers[MCA_BTL_DES_MAX_SEGMENTS];
    unsigned char addr[1];
};
typedef struct mca_pml_bfo_recv_frag_t mca_pml_bfo_recv_frag_t;

OBJ_CLASS_DECLARATION(mca_pml_bfo_recv_frag_t);


#define MCA_PML_BFO_RECV_FRAG_ALLOC(frag,rc)                    \
do {                                                            \
    ompi_free_list_item_t* item;                                \
    OMPI_FREE_LIST_WAIT(&mca_pml_bfo.recv_frags, item, rc);     \
    frag = (mca_pml_bfo_recv_frag_t*)item;                      \
} while(0)

    
#define MCA_PML_BFO_RECV_FRAG_INIT(frag, hdr, segs, cnt, btl )          \
do {                                                                    \
    size_t i, _size;                                                    \
    mca_btl_base_segment_t* macro_segments = frag->segments;            \
    mca_pml_bfo_buffer_t* buffers = frag->buffers;                      \
    unsigned char* _ptr = (unsigned char*)frag->addr;                   \
    /* init recv_frag */                                                \
    frag->btl = btl;                                                    \
    frag->hdr = *(mca_pml_bfo_hdr_t*)hdr;                               \
    frag->num_segments = 1;                                             \
    _size = segs[0].seg_len;                                            \
    for( i = 1; i < cnt; i++ ) {                                        \
        _size += segs[i].seg_len;                                       \
    }                                                                   \
    /* copy over data */                                                \
    if(_size <= mca_pml_bfo.unexpected_limit ) {                        \
        macro_segments[0].seg_addr.pval = frag->addr;                   \
    } else {                                                            \
        buffers[0].len = _size;                                         \
        buffers[0].addr = (char*)                                       \
            mca_pml_bfo.allocator->alc_alloc( mca_pml_bfo.allocator,    \
                                              buffers[0].len,           \
                                              0, NULL);                 \
        _ptr = (unsigned char*)(buffers[0].addr);                       \
        macro_segments[0].seg_addr.pval = buffers[0].addr;              \
    }                                                                   \
    macro_segments[0].seg_len = _size;                                  \
    for( i = 0; i < cnt; i++ ) {                                        \
        memcpy( _ptr, segs[i].seg_addr.pval, segs[i].seg_len);          \
        _ptr += segs[i].seg_len;                                        \
    }                                                                   \
 } while(0)


#define MCA_PML_BFO_RECV_FRAG_RETURN(frag)                              \
do {                                                                    \
    if( frag->segments[0].seg_len > mca_pml_bfo.unexpected_limit ) {    \
        /* return buffers */                                            \
        mca_pml_bfo.allocator->alc_free( mca_pml_bfo.allocator,         \
                                         frag->buffers[0].addr );       \
    }                                                                   \
    frag->num_segments = 0;                                             \
                                                                        \
    /* return recv_frag */                                              \
    OMPI_FREE_LIST_RETURN(&mca_pml_bfo.recv_frags,                      \
                          (ompi_free_list_item_t*)frag);                \
 } while(0)


/**
 *  Callback from BTL on receipt of a recv_frag (match).
 */

extern void mca_pml_bfo_recv_frag_callback_match( mca_btl_base_module_t *btl, 
                                                  mca_btl_base_tag_t tag,
                                                  mca_btl_base_descriptor_t* descriptor,
                                                  void* cbdata );
                                                                 
/**
 *  Callback from BTL on receipt of a recv_frag (rndv).
 */

extern void mca_pml_bfo_recv_frag_callback_rndv( mca_btl_base_module_t *btl, 
                                                 mca_btl_base_tag_t tag,
                                                 mca_btl_base_descriptor_t* descriptor,
                                                 void* cbdata );
/**
 *  Callback from BTL on receipt of a recv_frag (rget).
 */

extern void mca_pml_bfo_recv_frag_callback_rget( mca_btl_base_module_t *btl, 
                                                 mca_btl_base_tag_t tag,
                                                 mca_btl_base_descriptor_t* descriptor,
                                                 void* cbdata );

/**
 *  Callback from BTL on receipt of a recv_frag (ack).
 */

extern void mca_pml_bfo_recv_frag_callback_ack( mca_btl_base_module_t *btl, 
                                                mca_btl_base_tag_t tag,
                                                mca_btl_base_descriptor_t* descriptor,
                                                void* cbdata );
/**
 *  Callback from BTL on receipt of a recv_frag (frag).
 */

extern void mca_pml_bfo_recv_frag_callback_frag( mca_btl_base_module_t *btl, 
                                                 mca_btl_base_tag_t tag,
                                                 mca_btl_base_descriptor_t* descriptor,
                                                 void* cbdata );
/**
 *  Callback from BTL on receipt of a recv_frag (put).
 */

extern void mca_pml_bfo_recv_frag_callback_put( mca_btl_base_module_t *btl, 
                                                mca_btl_base_tag_t tag,
                                                mca_btl_base_descriptor_t* descriptor,
                                                void* cbdata );
/**
 *  Callback from BTL on receipt of a recv_frag (fin).
 */

extern void mca_pml_bfo_recv_frag_callback_fin( mca_btl_base_module_t *btl, 
                                                mca_btl_base_tag_t tag,
                                                mca_btl_base_descriptor_t* descriptor,
                                                void* cbdata );

                                              
END_C_DECLS

#endif

