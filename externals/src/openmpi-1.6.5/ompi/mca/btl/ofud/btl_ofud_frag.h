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
 * Copyright (c) 2006      Sandia National Laboratories. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_UD_FRAG_H
#define MCA_BTL_UD_FRAG_H

#define MCA_BTL_IB_FRAG_ALIGN (8)

#include <infiniband/verbs.h>

#include "ompi/mca/mpool/rdma/mpool_rdma.h"

#include "btl_ofud.h"


BEGIN_C_DECLS


/**
 * Fragment types
 */
typedef enum {
    MCA_BTL_UD_FRAG_SEND,
    MCA_BTL_UD_FRAG_USER,
    MCA_BTL_UD_FRAG_RECV
} mca_btl_ud_frag_type_t;


struct mca_btl_ud_reg_t {
    mca_mpool_base_registration_t base;
    struct ibv_mr* mr;
};
typedef struct mca_btl_ud_reg_t mca_btl_ud_reg_t;


/* UD adds a 40 byte global routing header */
/* This works in strange ways - the sending side does not need to explicitly
   include this data in sg lists.  Then, on the receiving side, the extra 40
   bytes magically appear. */
struct mca_btl_ud_ib_header_t {
    uint8_t ib_grh[40];
};
typedef struct mca_btl_ud_ib_header_t mca_btl_ud_ib_header_t;

struct mca_btl_ud_header_t {
    /*uint32_t src_qpnum;*/
    mca_btl_base_tag_t tag;
};
typedef struct mca_btl_ud_header_t mca_btl_ud_header_t;


/**
 * IB send fragment derived type.
 */

struct mca_btl_ud_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segment;
    
    struct mca_btl_base_endpoint_t* endpoint;

    mca_btl_ud_frag_type_t type;
    
    union{
        struct ibv_recv_wr rd_desc;
        struct ibv_send_wr sr_desc;
    } wr_desc;
    struct ibv_sge sg_entry;

    /* When this is a send frag, hdr points right after this, as expected.
       But when this is a receive frag, we have an extra 40 bytes provided
       by IB, so this points 40 bytes past the end of the frag. */
    mca_btl_ud_header_t* hdr;

    mca_btl_ud_reg_t* ud_reg;
};
typedef struct mca_btl_ud_frag_t mca_btl_ud_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_ud_frag_t);

typedef struct mca_btl_ud_frag_t mca_btl_ud_send_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_ud_send_frag_t);

typedef struct mca_btl_ud_frag_t mca_btl_ud_user_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_ud_user_frag_t);

typedef struct mca_btl_ud_frag_t mca_btl_ud_recv_frag_t;
OBJ_CLASS_DECLARATION(mca_btl_ud_recv_frag_t);


/*
 * Allocate/return a UD/IB send/user fragment
 */

#define MCA_BTL_UD_ALLOC_FRAG(btl, frag, rc)                                \
{                                                                           \
    ompi_free_list_item_t *item;                                            \
    OMPI_FREE_LIST_GET(&((mca_btl_ud_module_t*)btl)->send_frags, item, rc); \
    frag = (mca_btl_ud_frag_t*) item;                                       \
}

#define MCA_BTL_UD_RETURN_FRAG(btl, frag)                                   \
{                                                                           \
    OMPI_FREE_LIST_RETURN(                                                  \
            &((mca_btl_ud_module_t*)btl)->send_frags,                       \
            (ompi_free_list_item_t*)(frag));                                \
}


#define MCA_BTL_UD_ALLOC_USER_FRAG(btl, frag, rc)                           \
{                                                                           \
    ompi_free_list_item_t *item;                                            \
    OMPI_FREE_LIST_GET(&((mca_btl_ud_module_t*)btl)->user_frags, item, rc); \
    frag = (mca_btl_ud_frag_t*) item;                                       \
}

#define MCA_BTL_UD_RETURN_USER_FRAG(btl, frag)                              \
{                                                                           \
    OMPI_FREE_LIST_RETURN(                                                  \
            &((mca_btl_ud_module_t*)btl)->user_frags,                       \
            (ompi_free_list_item_t*)(frag));                                \
}


struct mca_btl_ud_module_t;

END_C_DECLS
#endif
