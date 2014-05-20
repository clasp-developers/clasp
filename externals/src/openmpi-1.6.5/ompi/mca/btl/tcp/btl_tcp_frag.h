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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_TCP_FRAG_H
#define MCA_BTL_TCP_FRAG_H


#define MCA_BTL_TCP_FRAG_ALIGN (8)
#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif

#include "btl_tcp.h" 
#include "btl_tcp_hdr.h"

BEGIN_C_DECLS

#define MCA_BTL_TCP_FRAG_IOVEC_NUMBER  4

/**
 * TCP fragment derived type.
 */
struct mca_btl_tcp_frag_t {
    mca_btl_base_descriptor_t base; 
    mca_btl_base_segment_t segments[2]; 
    struct mca_btl_base_endpoint_t *endpoint; 
    struct mca_btl_tcp_module_t* btl;
    mca_btl_tcp_hdr_t hdr;
    struct iovec iov[MCA_BTL_TCP_FRAG_IOVEC_NUMBER + 1];
    struct iovec *iov_ptr;
    size_t iov_cnt;
    size_t iov_idx;
    size_t size; 
    int rc;
    ompi_free_list_t* my_list;
}; 
typedef struct mca_btl_tcp_frag_t mca_btl_tcp_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_tcp_frag_t); 

typedef struct mca_btl_tcp_frag_t mca_btl_tcp_frag_eager_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_tcp_frag_eager_t); 

typedef struct mca_btl_tcp_frag_t mca_btl_tcp_frag_max_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_tcp_frag_max_t); 

typedef struct mca_btl_tcp_frag_t mca_btl_tcp_frag_user_t; 
    
OBJ_CLASS_DECLARATION(mca_btl_tcp_frag_user_t); 


/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */

#define MCA_BTL_TCP_FRAG_ALLOC_EAGER(frag, rc)                             \
{                                                                          \
    ompi_free_list_item_t *item;                                           \
    OMPI_FREE_LIST_GET(&mca_btl_tcp_component.tcp_frag_eager, item, rc);   \
    frag = (mca_btl_tcp_frag_t*) item;                                     \
}

#define MCA_BTL_TCP_FRAG_ALLOC_MAX(frag, rc)                               \
{                                                                          \
    ompi_free_list_item_t *item;                                           \
    OMPI_FREE_LIST_GET(&mca_btl_tcp_component.tcp_frag_max, item, rc);     \
    frag = (mca_btl_tcp_frag_t*) item;                                     \
}

#define MCA_BTL_TCP_FRAG_ALLOC_USER(frag, rc)                              \
{                                                                          \
    ompi_free_list_item_t *item;                                           \
    OMPI_FREE_LIST_GET(&mca_btl_tcp_component.tcp_frag_user, item, rc);    \
    frag = (mca_btl_tcp_frag_t*) item;                                     \
}

#define MCA_BTL_TCP_FRAG_RETURN(frag)                                      \
{                                                                          \
    OMPI_FREE_LIST_RETURN(frag->my_list, (ompi_free_list_item_t*)(frag));  \
}

#define MCA_BTL_TCP_FRAG_INIT_DST(frag,ep)                                 \
do {                                                                       \
    frag->rc = 0;                                                          \
    frag->btl = ep->endpoint_btl;                                          \
    frag->endpoint = ep;                                                   \
    frag->iov[0].iov_len = sizeof(frag->hdr);                              \
    frag->iov[0].iov_base = (IOVBASE_TYPE*)&frag->hdr;                     \
    frag->iov_cnt = 1;                                                     \
    frag->iov_idx = 0;                                                     \
    frag->iov_ptr = frag->iov;                                             \
    frag->base.des_src = NULL;                                             \
    frag->base.des_dst_cnt = 0;                                            \
    frag->base.des_dst = frag->segments;                                   \
    frag->base.des_dst_cnt = 1;                                            \
} while(0)


bool mca_btl_tcp_frag_send(mca_btl_tcp_frag_t*, int sd);
bool mca_btl_tcp_frag_recv(mca_btl_tcp_frag_t*, int sd);


END_C_DECLS
#endif
