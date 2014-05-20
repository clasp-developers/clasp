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

#ifndef OMPI_BTL_PORTALS_FRAG_H
#define OMPI_BTL_PORTALS_FRAG_H

BEGIN_C_DECLS

/**
 * Portals send fragment derived type
 */
struct mca_btl_portals_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_base_segment_t segments[1]; 
    /* needed for retransmit case */
    struct mca_btl_base_endpoint_t *endpoint; 
    /* needed for retransmit case */
    mca_btl_base_header_t hdr;
    /* handle to use for communication */
    ptl_handle_md_t md_h;
    /* size of the allocated memory region -- not the amount of data
       we need to send */
    size_t size; 

    enum { BTL_PORTALS_FRAG_TYPE_EAGER, 
           BTL_PORTALS_FRAG_TYPE_MAX,
           BTL_PORTALS_FRAG_TYPE_USER } type;
    unsigned char  data[16];
};
typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_t);

typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_eager_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_eager_t); 

typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_max_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_max_t); 

typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_user_t; 
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_user_t); 

typedef struct mca_btl_portals_frag_t mca_btl_portals_frag_recv_t;
OBJ_CLASS_DECLARATION(mca_btl_portals_frag_recv_t); 

/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */
#define OMPI_BTL_PORTALS_FRAG_ALLOC_EAGER(btl_macro, frag, rc)     \
{                                                                  \
                                                                   \
    ompi_free_list_item_t *item;                                        \
    OMPI_FREE_LIST_GET(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_eager, item, rc); \
    frag = (mca_btl_portals_frag_t*) item;                         \
    if (rc == OMPI_ERR_TEMP_OUT_OF_RESOURCE) {                     \
        OMPI_BTL_PORTALS_FRAG_ALLOC_MAX(btl_macro, frag, rc);      \
    }                                                              \
}


#define OMPI_BTL_PORTALS_FRAG_RETURN_EAGER(btl_macro, frag)        \
{                                                                  \
    assert(BTL_PORTALS_FRAG_TYPE_EAGER == frag->type);                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_eager, \
        (ompi_free_list_item_t*)(frag));                                \
}


#define OMPI_BTL_PORTALS_FRAG_ALLOC_MAX(btl_macro, frag, rc)       \
{                                                                  \
                                                                   \
    ompi_free_list_item_t *item_macro;                                        \
    OMPI_FREE_LIST_GET(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_max, item_macro, rc); \
    frag = (mca_btl_portals_frag_t*) item_macro;                         \
}


#define OMPI_BTL_PORTALS_FRAG_RETURN_MAX(btl_macro, frag)          \
{                                                                  \
    assert(BTL_PORTALS_FRAG_TYPE_MAX == frag->type);                    \
    OMPI_FREE_LIST_RETURN(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_max, \
        (ompi_free_list_item_t*)(frag));                                \
}


#define OMPI_BTL_PORTALS_FRAG_ALLOC_USER(btl_macro, frag, rc)      \
{                                                                  \
    ompi_free_list_item_t *item;                                        \
    OMPI_FREE_LIST_WAIT(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_user, item, rc); \
    frag = (mca_btl_portals_frag_t*) item;                         \
}


#define OMPI_BTL_PORTALS_FRAG_RETURN_USER(btl_macro, frag)         \
{                                                                  \
    assert(BTL_PORTALS_FRAG_TYPE_USER == frag->type);                    \
    OMPI_FREE_LIST_RETURN(&((mca_btl_portals_module_t*)btl_macro)->portals_frag_user, \
        (ompi_free_list_item_t*)(frag));                                \
}


END_C_DECLS
#endif
