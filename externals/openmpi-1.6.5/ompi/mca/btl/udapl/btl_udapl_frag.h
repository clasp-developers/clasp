/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
 * Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_UDAPL_FRAG_H
#define MCA_BTL_UDAPL_FRAG_H


#define MCA_BTL_UDAPL_FRAG_ALIGN (8)
#include "ompi_config.h"

BEGIN_C_DECLS

typedef enum {
    MCA_BTL_UDAPL_SEND,
    MCA_BTL_UDAPL_RECV,
    MCA_BTL_UDAPL_PUT,
    MCA_BTL_UDAPL_GET,
    MCA_BTL_UDAPL_CONN_RECV,
    MCA_BTL_UDAPL_CONN_SEND,
    MCA_BTL_UDAPL_RDMA_WRITE,
    MCA_BTL_UDAPL_FRAG_EAGER_RDMA,
    MCA_BTL_UDAPL_IGNORE
} mca_btl_udapl_frag_type_t;

typedef enum {
    MCA_BTL_UDAPL_CONTROL_NOOP,
    MCA_BTL_UDAPL_CONTROL_RDMA_CONNECT,
    MCA_BTL_UDAPL_CONTROL_RDMA_CREDIT,
    MCA_BTL_UDAPL_CONTROL_SR_CREDIT
} mca_btl_udapl_control_t;

/* Control message header */
struct mca_btl_udapl_control_header_t {
    mca_btl_udapl_control_t type; 
};
typedef struct mca_btl_udapl_control_header_t mca_btl_udapl_control_header_t;

/**
 * uDAPL btl footer.
 * This is put after the payload packet so the PML header can be aligned.
 * Must be aligned on MCA_BTL_UDAPL_FRAG_ALIGN byte boundary.
 */
struct mca_btl_udapl_footer_t {
    mca_btl_base_tag_t tag;
};
typedef struct mca_btl_udapl_footer_t mca_btl_udapl_footer_t;

/**
 * uDAPL BTL rdma footer.
 * This is used in addtion to the uDAPL BTL footer. The two are seperate to
 * allow for any padding that may be required between the two.
 */
struct mca_btl_udapl_rdma_footer_t {
    uint32_t size;
    volatile uint8_t active;/* 0 = not in use; 1 = data is available to be
			     * received; this should always be the last entry
			     * in this structure
			     */
    char pad[3];        /* pad out be aligned on MCA_BTL_UDAPL_FRAG_ALIGN byte boundary */
};
typedef struct mca_btl_udapl_rdma_footer_t mca_btl_udapl_rdma_footer_t;

/**
 * uDAPL fragment derived type.
 */
struct mca_btl_udapl_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segment;

    struct mca_btl_udapl_module_t* btl;
    struct mca_btl_base_endpoint_t* endpoint; 
    DAT_LMR_TRIPLET triplet;
    struct mca_btl_udapl_reg_t* registration;
    
    mca_btl_udapl_footer_t* ftr;
    mca_btl_udapl_rdma_footer_t* rdma_ftr;
    size_t size; 
    mca_btl_udapl_frag_type_t type;
    uint32_t pad; /* Padding the structure to be evenly divisble by MCA_BTL_UDAPL_FRAG_ALIGN */
}; 
typedef struct mca_btl_udapl_frag_t mca_btl_udapl_frag_t; 
OBJ_CLASS_DECLARATION(mca_btl_udapl_frag_t); 


typedef struct mca_btl_udapl_frag_t mca_btl_udapl_frag_eager_t; 
OBJ_CLASS_DECLARATION(mca_btl_udapl_frag_eager_t); 

typedef struct mca_btl_udapl_frag_t mca_btl_udapl_frag_max_t; 
OBJ_CLASS_DECLARATION(mca_btl_udapl_frag_max_t); 

typedef struct mca_btl_udapl_frag_t mca_btl_udapl_frag_user_t; 
OBJ_CLASS_DECLARATION(mca_btl_udapl_frag_user_t); 

typedef struct mca_btl_udapl_frag_t mca_btl_udapl_frag_eager_rdma_t; 
OBJ_CLASS_DECLARATION(mca_btl_udapl_frag_eager_rdma_t);

    
/*
 * Macros to allocate/return descriptors from module specific
 * free list(s).
 */

#define MCA_BTL_UDAPL_FRAG_ALLOC_EAGER(btl, frag, rc)              \
{                                                                  \
    ompi_free_list_item_t *item;                                   \
    OMPI_FREE_LIST_GET(&((mca_btl_udapl_module_t*)btl)->udapl_frag_eager, item, rc); \
    frag = (mca_btl_udapl_frag_t*) item;                           \
}

#define MCA_BTL_UDAPL_FRAG_RETURN_EAGER(btl, frag)                 \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_udapl_module_t*)btl)->udapl_frag_eager, \
        (ompi_free_list_item_t*)(frag));                           \
}

#define MCA_BTL_UDAPL_FRAG_ALLOC_EAGER_RECV(btl, frag, rc)              \
{                                                                  \
    ompi_free_list_item_t *item;                                   \
    OMPI_FREE_LIST_GET(&((mca_btl_udapl_module_t*)btl)->udapl_frag_eager_recv, item, rc); \
    frag = (mca_btl_udapl_frag_t*) item;                           \
}

#define MCA_BTL_UDAPL_FRAG_ALLOC_MAX(btl, frag, rc)                \
{                                                                  \
    ompi_free_list_item_t *item;                                   \
    OMPI_FREE_LIST_GET(&((mca_btl_udapl_module_t*)btl)->udapl_frag_max, item, rc); \
    frag = (mca_btl_udapl_frag_t*) item;                           \
}

#define MCA_BTL_UDAPL_FRAG_RETURN_MAX(btl, frag)                   \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_udapl_module_t*)btl)->udapl_frag_max, \
        (ompi_free_list_item_t*)(frag));                           \
}

#define MCA_BTL_UDAPL_FRAG_ALLOC_MAX_RECV(btl, frag, rc)                \
{                                                                  \
    ompi_free_list_item_t *item;                                   \
    OMPI_FREE_LIST_GET(&((mca_btl_udapl_module_t*)btl)->udapl_frag_max_recv, item, rc); \
    frag = (mca_btl_udapl_frag_t*) item;                           \
}

#define MCA_BTL_UDAPL_FRAG_ALLOC_USER(btl, frag, rc)               \
{                                                                  \
    ompi_free_list_item_t *item;                                   \
    OMPI_FREE_LIST_GET(&((mca_btl_udapl_module_t*)btl)->udapl_frag_user, item, rc); \
    frag = (mca_btl_udapl_frag_t*) item;                           \
}

#define MCA_BTL_UDAPL_FRAG_RETURN_USER(btl, frag)                  \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_udapl_module_t*)btl)->udapl_frag_user, \
        (ompi_free_list_item_t*)(frag)); \
}

#define MCA_BTL_UDAPL_FRAG_ALLOC_CONTROL(btl, frag, rc)              \
{                                                                  \
    ompi_free_list_item_t *item;                                   \
    OMPI_FREE_LIST_GET(&((mca_btl_udapl_module_t*)btl)->udapl_frag_control, item, rc); \
    frag = (mca_btl_udapl_frag_t*) item;                           \
}

#define MCA_BTL_UDAPL_FRAG_RETURN_CONTROL(btl, frag)                 \
{                                                                  \
    OMPI_FREE_LIST_RETURN(&((mca_btl_udapl_module_t*)btl)->udapl_frag_control, \
        (ompi_free_list_item_t*)(frag));                           \
}

/*
 * Calculate the pad value P required to align the given size S
 */
#define MCA_BTL_UDAPL_FRAG_CALC_ALIGNMENT_PAD(P,S) do {               \
    (P) = ((S) % MCA_BTL_UDAPL_FRAG_ALIGN) == 0 ?                \
        0 : (MCA_BTL_UDAPL_FRAG_ALIGN - ((S) % MCA_BTL_UDAPL_FRAG_ALIGN));   \
} while (0);

END_C_DECLS
#endif
