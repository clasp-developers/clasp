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
/**
 * @file
 */
#ifndef MCA_BTL_SELF_SEND_FRAG_H
#define MCA_BTL_SELF_SEND_FRAG_H

#include <sys/types.h>
#include "ompi/class/ompi_free_list.h"
#include "btl_self.h"


/**
 * shared memory send fragment derived type.
 */
struct mca_btl_self_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segment;
    struct mca_btl_base_endpoint_t *endpoint;
    size_t size;
};
typedef struct mca_btl_self_frag_t mca_btl_self_frag_t;
typedef struct mca_btl_self_frag_t mca_btl_self_frag_eager_t;
typedef struct mca_btl_self_frag_t mca_btl_self_frag_send_t;
typedef struct mca_btl_self_frag_t mca_btl_self_frag_rdma_t;

OBJ_CLASS_DECLARATION(mca_btl_self_frag_eager_t);
OBJ_CLASS_DECLARATION(mca_btl_self_frag_send_t);
OBJ_CLASS_DECLARATION(mca_btl_self_frag_rdma_t);

#define MCA_BTL_SELF_FRAG_ALLOC_EAGER(frag, rc)                              \
{                                                                            \
    ompi_free_list_item_t* item;                                             \
    OMPI_FREE_LIST_GET(&mca_btl_self_component.self_frags_eager, item, rc);  \
    frag = (mca_btl_self_frag_t*)item;                                       \
}

#define MCA_BTL_SELF_FRAG_RETURN_EAGER(frag)                                 \
{                                                                            \
    OMPI_FREE_LIST_RETURN(&mca_btl_self_component.self_frags_eager,          \
                          (ompi_free_list_item_t*)(frag));                   \
    frag->segment.seg_addr.pval = frag+1;                                    \
}

#define MCA_BTL_SELF_FRAG_ALLOC_SEND(frag, rc)                               \
{                                                                            \
    ompi_free_list_item_t* item;                                             \
    OMPI_FREE_LIST_GET(&mca_btl_self_component.self_frags_send, item, rc);   \
    frag = (mca_btl_self_frag_t*)item;                                       \
}

#define MCA_BTL_SELF_FRAG_RETURN_SEND(frag)                                  \
{                                                                            \
    OMPI_FREE_LIST_RETURN( &mca_btl_self_component.self_frags_send,          \
                           (ompi_free_list_item_t*)(frag));                  \
    frag->segment.seg_addr.pval = frag+1;                                    \
}

#define MCA_BTL_SELF_FRAG_ALLOC_RDMA(frag, rc)                               \
{                                                                            \
    ompi_free_list_item_t* item;                                             \
    OMPI_FREE_LIST_GET(&mca_btl_self_component.self_frags_rdma, item, rc);   \
    frag = (mca_btl_self_frag_t*)item;                                       \
}

#define MCA_BTL_SELF_FRAG_RETURN_RDMA(frag)                                  \
{                                                                            \
    OMPI_FREE_LIST_RETURN(&mca_btl_self_component.self_frags_rdma,           \
                          (ompi_free_list_item_t*)(frag));                   \
    frag->segment.seg_addr.pval = frag+1;                                    \
}

#endif

