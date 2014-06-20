/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_BTL_SM_SEND_FRAG_H
#define MCA_BTL_SM_SEND_FRAG_H

#include <sys/types.h>
#include "ompi_config.h"
#include "ompi/class/ompi_free_list.h"
#include "btl_sm.h"


#define MCA_BTL_SM_FRAG_TYPE_MASK ((uintptr_t)0x3)
#define MCA_BTL_SM_FRAG_SEND ((uintptr_t)0x0)
#define MCA_BTL_SM_FRAG_ACK ((uintptr_t)0x1)
#define MCA_BTL_SM_FRAG_PUT ((uintptr_t)0x2)
#define MCA_BTL_SM_FRAG_GET ((uintptr_t)0x3)

#define MCA_BTL_SM_FRAG_STATUS_MASK ((uintptr_t)0x4)

struct mca_btl_sm_frag_t;

struct mca_btl_sm_hdr_t {
    struct mca_btl_sm_frag_t *frag;
    size_t len;
    int my_smp_rank;
    mca_btl_base_tag_t tag;
};
typedef struct mca_btl_sm_hdr_t mca_btl_sm_hdr_t;

/**
 * shared memory send fragment derived type.
 */
struct mca_btl_sm_frag_t {
    mca_btl_base_descriptor_t base;
    mca_btl_base_segment_t segment;
    struct mca_btl_base_endpoint_t *endpoint;
    size_t size;
    /* pointer written to the FIFO, this is the base of the shared memory region */
    mca_btl_sm_hdr_t *hdr;
    ompi_free_list_t* my_list;
};
typedef struct mca_btl_sm_frag_t mca_btl_sm_frag_t;
typedef struct mca_btl_sm_frag_t mca_btl_sm_frag1_t;
typedef struct mca_btl_sm_frag_t mca_btl_sm_frag2_t;
typedef struct mca_btl_sm_frag_t mca_btl_sm_user_t;


OBJ_CLASS_DECLARATION(mca_btl_sm_frag_t);
OBJ_CLASS_DECLARATION(mca_btl_sm_frag1_t);
OBJ_CLASS_DECLARATION(mca_btl_sm_frag2_t);
OBJ_CLASS_DECLARATION(mca_btl_sm_user_t);

#define MCA_BTL_SM_FRAG_ALLOC_EAGER(frag, rc)                           \
{                                                                       \
    ompi_free_list_item_t* item;                                        \
    OMPI_FREE_LIST_GET(&mca_btl_sm_component.sm_frags_eager, item, rc); \
    frag = (mca_btl_sm_frag_t*)item;                                    \
}

#define MCA_BTL_SM_FRAG_ALLOC_MAX(frag, rc)                             \
{                                                                       \
    ompi_free_list_item_t* item;                                        \
    OMPI_FREE_LIST_GET(&mca_btl_sm_component.sm_frags_max, item, rc);   \
    frag = (mca_btl_sm_frag_t*)item;                                    \
}

#define MCA_BTL_SM_FRAG_ALLOC_USER(frag, rc)                             \
{                                                                       \
	ompi_free_list_item_t* item;                                        \
	OMPI_FREE_LIST_GET(&mca_btl_sm_component.sm_frags_user, item, rc);   \
	frag = (mca_btl_sm_frag_t*)item;                                    \
}


#define MCA_BTL_SM_FRAG_RETURN(frag)                                      \
{                                                                         \
    OMPI_FREE_LIST_RETURN(frag->my_list, (ompi_free_list_item_t*)(frag)); \
}
#endif
